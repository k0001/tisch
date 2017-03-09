{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tisch.Run
  ( -- * Connection
    Conn
  , Conn'
  , unConn
  , connect
  , connect'
  , close
    -- * Permissions
  , Perm(..)
  , Allow
  , Forbid
  , DropPerm
  , withoutPerm
    -- * Transaction
  , IsolationLevel(..)
  , withReadOnlyTransaction
  , withReadWriteTransaction
  , withSavepoint
    -- * Query
  , runQuery1
  , runQuery
    -- * Insert
  , runInsert1
  , runInsert
  , runInsertNoCountCheck
  , runInsertRaw1
  , runInsertRaw
  , runInsertRawNoCountCheck
    -- ** Returning
  , runInsertReturning1
  , runInsertReturning
  , runInsertReturningNoCountCheck
  , runInsertRawReturning1
  , runInsertRawReturning
  , runInsertRawReturningNoCountCheck
    -- * Update
  , runUpdate
  , runUpdateRaw
    -- * Delete
  , runDelete
  , runDeleteRaw
    -- * Exception
  , ErrNumRows(..)
    -- * Parsing results
  , O.QueryRunnerColumnDefault(..)
  , qrcFromField
  , qrcFieldParser
  , qrcFieldParserMap
  , qrcMap
  , qrcMapMay
  , qrcPrism
  , qrcWrapped
  ) where

import           Control.Lens
import           Control.Monad (when)
import           Control.Monad.IO.Class
import qualified Control.Monad.Catch as Cx
import           Data.Int
import qualified Data.List.NonEmpty as NEL
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as PP
import           Data.Typeable (Typeable)
import qualified Data.ByteString.Char8 as B8
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import           GHC.Exts (Constraint)
import qualified Opaleye as O
import qualified Opaleye.Internal.Unpackspec as OI
import qualified Opaleye.Internal.RunQuery as OI

import Tisch.Internal.Table
  (Table, TableRW, PgR, PgW, HsI, RawTable(..), rawTableRW, pgWfromHsI, pgWfromPgR)
import Tisch.Internal.Kol (Kol(..))
import Tisch.Internal.Query (Query(..))
import Tisch.Internal.Debug (renderSqlQuery')

--------------------------------------------------------------------------------

-- | 'Perm' is only used at the type level to index 'Conn'.
data Perm
  = Fetch
    -- ^ Allow fetching a value from the database
    -- (i.e., @SELECT@, @... RETURNING@).
  | Update
    -- ^ Allow updating data in the database (i.e., @UPDATE@)
  | Insert
    -- ^ Allow inserting new data to the database (i.e., @INSERT@)
  | Delete
    -- ^ Allow deleting data from the database (i.e., @DELETE@)
  | Transact
    -- ^ Allow starting and finishing transactions (i.e., @BEGIN@,
    -- @COMMIT@, @ROLLBACK@).
  | Savepoint
    -- ^ Allow creating transactions savepoints and rolling back to them.
  deriving (Eq, Ord, Show)

-- | @'Conn' perms@ is just a wrapper around @postgresql-simple@'s
-- 'Pg.Connection' that carries, in @perms@, type-level information about which
-- kind of operations can be performed on this connection.
--
-- The problem with 'Pg.Connection' is that it allows us to perform any operation
-- on it (reading, inserting, commiting a transaction, etc.), but sometimes we
-- would prefer to limit the type of operations this connection might support.
-- For example, we might want to forbid transactions from being commited because
-- they are being handled by someone else. @'Conn' perms@ carries type-level
-- information about this.
--
-- Note that 'Conn' is not thread-safe, you are encouraged to maintain a
-- multithreaded pool of 'Conn' instead. See "Data.Pool" from the @ex-pool@
-- package.
newtype Conn (perms :: [Perm]) = Conn Pg.Connection

unConn :: Conn ps -> Pg.Connection
unConn (Conn conn) = conn

-- | A type synonym for a 'Conn' with all the permissions enabled.
type Conn' = Conn ['Fetch, 'Insert, 'Update, 'Delete, 'Transact]

-- | @'Allow' p ps@ ensures that @p@ is present in @ps@.
--
-- The kind of @p@ can be 'Perm' or @['Perm']@.
type family Allow (p :: k) (ps :: [Perm]) :: Constraint where
  Allow ('[] :: [Perm]) ps = ()
  Allow ((p ': ps) :: [Perm]) qs = (Allow p qs, Allow ps qs)
  Allow (p :: Perm) (p ': ps) = ()
  Allow (p :: Perm) (q ': ps) = Allow p ps

-- | @'Forbid'' p ps@ ensures that @p@ is not present in @ps@.
--
-- The kind of @p@ can be 'Perm' or @['Perm']@.
type family Forbid (p :: k) (ps :: [Perm]) :: Constraint where
  Forbid ('[] :: [Perm]) ps = ()
  Forbid ((p ': ps) :: [Perm]) qs = (Forbid p qs, Forbid ps qs)
  Forbid (p :: Perm) (p ': ps) =
     "Tisch.Run.Forbid" ~
     "Forbid: The forbidden permission is allowed"
  Forbid (p :: Perm) (q ': ps) = Forbid p ps
  Forbid (p :: Perm) '[] = ()

-- | @'DropPerm' p ps@ removes @p@ from @ps@ if present.
--
-- The kind of @p@ can be 'Perm' or @['Perm']@.
type family DropPerm (p :: k) (ps :: [Perm]) :: [Perm] where
  DropPerm ('[] :: [Perm]) ps = ps
  DropPerm ((p ': ps) :: [Perm]) qs = DropPerm p (DropPerm ps qs)
  DropPerm (p :: Perm) (p ': ps) = DropPerm p ps
  DropPerm (p :: Perm) (q ': ps) = q ': DropPerm p ps
  DropPerm (p :: Perm) '[] = '[]

-- | Drop a permission from the connection.
withoutPerm
  :: (MonadIO m, Cx.MonadMask m, Allow p ps, ps' ~ DropPerm p ps)
  => proxy (p :: k)
  -- ^ @k@ may be 'Perm' or @['Perm']@.
  -> Conn ps
  -> (Conn ps' -> m a)
  -- ^ The usage of @'Conn' ps@ is undefined within this function,
  -- and @'Conn' ps'@ mustn't escape the scope of this function.
  -> m a
withoutPerm _ (Conn conn) f = f (Conn conn)

-- | Open a new connection.
connect :: MonadIO m => Pg.ConnectInfo -> m Conn'
connect = connect' . Pg.postgreSQLConnectionString

-- | Like 'connect', except it takes a @libpq@ connection string instead of a
-- 'Pg.ConnectInfo'.
connect' :: MonadIO m => B8.ByteString -> m Conn'
connect' = liftIO . fmap Conn . Pg.connectPostgreSQL

-- | Colse a connection.
--
-- Warning: Using the given @'Conn' ps@ after calling 'close' will result in
-- a runtime exception.
close :: (MonadIO m, Cx.MonadMask m) => Conn ps -> m ()
close (Conn conn) = liftIO (Pg.close conn)

--------------------------------------------------------------------------------

-- | Like 'Pg.IsolationLevel', but without support for default values.
data IsolationLevel = ReadCommitted | RepeatableRead | Serializable
  deriving (Eq, Ord, Show, Enum, Bounded)

pgIsolationLevel :: IsolationLevel -> Pg.IsolationLevel
pgIsolationLevel ReadCommitted = Pg.ReadCommitted
pgIsolationLevel RepeatableRead = Pg.RepeatableRead
pgIsolationLevel Serializable = Pg.Serializable

---

-- | Execute the given callback within a read-only transaction with the given
-- isolation level. The transaction is rolled-back afterwards, as there wouldn't
-- be anything to commit anyway, even in case of execeptions.
withReadOnlyTransaction
  :: (MonadIO m, Cx.MonadMask m, Allow 'Transact ps, Forbid 'Savepoint ps,
      ps' ~ DropPerm ['Transact, 'Insert, 'Update, 'Delete] ps)
  => IsolationLevel
  -> Conn ps
  -> (Conn ps' -> m a)
  -- ^ The usage of @'Conn' ps@ is undefined within this function,
  -- as well as the usage of @'Conn' ps'@ outside this function.
  -> m a
withReadOnlyTransaction il (Conn conn) f = Cx.mask $ \restore -> do
  let tmode = Pg.TransactionMode (pgIsolationLevel il) Pg.ReadOnly
  liftIO $ Pg.beginMode tmode conn
  a <- restore (f (Conn conn)) `Cx.onException` liftIO (Pg.rollback conn)
  a <$ liftIO (Pg.rollback conn)

-- | Execute the given callback within a read-write transaction with the given
-- isolation level, rolling back the transaction in case of exceptions,
-- and either commiting or rolling back the transaction otherwise, as requested
-- by the passed in callback.
withReadWriteTransaction
 :: (MonadIO m, Cx.MonadMask m, Allow 'Transact ps, Forbid 'Savepoint ps,
     ps' ~ ('Savepoint ': DropPerm 'Transact ps))
 => IsolationLevel
 -> Conn ps
 -> (Conn ps' -> m (Either a b))
 -- ^ The usage of @'Conn' ps@ is undefined within this function,
 -- as well as the usage of @'Conn' ps'@ outside this function.
 -- A 'Left' return value rollbacks the transaction, 'Right' commits it.
 -> m (Either a b)
withReadWriteTransaction il (Conn conn) f = Cx.mask $ \restore -> do
  let tmode = Pg.TransactionMode (pgIsolationLevel il) Pg.ReadWrite
  liftIO $ Pg.beginMode tmode conn
  eab <- restore (f (Conn conn)) `Cx.onException` liftIO (Pg.rollback conn)
  eab <$ liftIO (either (const Pg.rollback) (const Pg.commit) eab conn)

---

-- | You can use this function within `withTransaction` as a sort of nested
-- transaction.
withSavepoint
  :: (MonadIO m, Cx.MonadMask m, Allow 'Savepoint ps, Forbid 'Transact ps)
  => Conn ps
  -> (Conn ps -> m (Either a b))
  -- ^ A 'Left' return value rollbacks the savepoint, 'Right' keeps it.
  -> m (Either a b)
withSavepoint (Conn conn) f = Cx.mask $ \restore -> do
  sp <- liftIO $ Pg.newSavepoint conn
  let abort = liftIO $ Pg.rollbackToAndReleaseSavepoint conn sp
  eab <- restore (f (Conn conn)) `Cx.onException` abort
  eab <$ either (const abort) (const (return ())) eab

--------------------------------------------------------------------------------

-- | Query and fetch zero or more resulting rows.
runQuery
 :: (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v r, Allow 'Fetch ps)
 => Conn ps -> Query d () v -> m [r] -- ^
runQuery (Conn conn) = liftIO . O.runQuery conn . unQuery

-- | Query and fetch zero or one resulting row.
--
-- Throws 'ErrNumRows' if there is more than one row in the result.
runQuery1
 :: forall v d r m ps
  . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v r, Allow 'Fetch ps)
 => Conn ps -> Query d () v -> m (Maybe r) -- ^
runQuery1 pc q = do
    rs <- runQuery pc q
    case rs of
      [r] -> return (Just r)
      []   -> return Nothing
      _   -> Cx.throwM $ ErrNumRows 1 (fromIntegral (length rs)) $
                let OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v r
                in renderSqlQuery' u q

--------------------------------------------------------------------------------

-- | Insert rows into a 'Table'.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- the number of passed in rows. Use 'runInsertNoCountCheck' if you don't want
-- this behavior (hint: you probably want this behavior).
runInsert
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps, TableRW t)
  => Conn ps -> Table t -> [HsI t] -> m () -- ^
runInsert conn t = runInsertRaw conn (rawTableRW t) . map pgWfromHsI

-- | Insert a single row into a 'Table'.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- one. Use 'runInsertNoCountCheck' if you don't want this behavior (hint: you
-- probably want this behavior).
runInsert1
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps, TableRW t)
  => Conn ps -> Table t -> HsI t -> m () -- ^
runInsert1 conn t = runInsertRaw1 conn (rawTableRW t) . pgWfromHsI

-- | Like 'runInsert', but instead of possibly throwing 'ErrNumRows' it returns
-- the number of affected rows, which might be different than the number of
-- passed in rows.
runInsertNoCountCheck
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps, TableRW t)
  => Conn ps -> Table t -> [HsI t] -> m Int64 -- ^
runInsertNoCountCheck conn t =
  runInsertRawNoCountCheck conn (rawTableRW t) . map pgWfromHsI


-- | Like 'runInsert', but takes a 'RawTable' instead of a 'Table'.
runInsertRaw
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps)
  => Conn ps -> RawTable d w v -> [w] -> m () -- ^
runInsertRaw conn t = \case
  [] -> return ()
  ws -> do
    nAffected <- runInsertRawNoCountCheck conn t ws
    let nExpected = fromIntegral (length ws) :: Int64
    when (nExpected /= nAffected) $ do
       let sql = O.arrangeInsertManySql (unRawTable t) (NEL.fromList ws)
       Cx.throwM (ErrNumRows nExpected nAffected (Just sql))


-- | Like 'runInsertNoCountCheck', but takes a 'RawTable' instead of a 'Table'.
runInsertRawNoCountCheck
  :: (MonadIO m, Allow 'Insert ps)
  => Conn ps -> RawTable d w v -> [w] -> m Int64 -- ^
runInsertRawNoCountCheck (Conn conn) (RawTable t) = \case
  [] -> return 0
  ws -> liftIO (O.runInsertMany conn t ws)

-- | Like 'runInsert1', but takes a 'RawTable' instead of a 'Table'.
runInsertRaw1
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps)
  => Conn ps -> RawTable d w v -> w -> m () -- ^
runInsertRaw1 pc t w = runInsertRaw pc t [w]

--------------------------------------------------------------------------------

-- | Insert many rows, returning data from the rows actually inserted.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- the number of passed in rows. Use 'runInsertReturningNoCountCheck' if you
-- don't want this behavior (hint: you probably want this behavior).
runInsertReturning
  :: (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v r, TableRW t,
      Allow ['Insert, 'Fetch] ps)
  => Conn ps -> Table t -> (PgR t -> v) -> [HsI t] -> m [r] -- ^
runInsertReturning conn t g =
  runInsertRawReturning conn (rawTableRW t) g . map pgWfromHsI


-- | Like 'runInsertReturning', but instead of possibly throwing 'ErrNumRows' it
-- returns the number of affected rows, which might be different than the number
-- of passed in rows.
runInsertReturningNoCountCheck
  :: (MonadIO m, PP.Default O.QueryRunner v r, TableRW t,
      Allow ['Insert, 'Fetch] ps)
  => Conn ps -> Table t -> (PgR t -> v) -> [HsI t] -> m [r] -- ^
runInsertReturningNoCountCheck conn t g =
  runInsertRawReturningNoCountCheck conn (rawTableRW t) g . map pgWfromHsI

-- | Insert one row, returning data from the one row actually inserted.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- one. Use 'runInsertReturningNoCountCheck' if you don't want this behavior
-- (hint: you probably want this behavior).
runInsertReturning1
  :: (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v r, TableRW t,
      Allow ['Insert, 'Fetch] ps)
  => Conn ps -> Table t -> (PgR t -> v) -> HsI t -> m r -- ^
runInsertReturning1 conn t g =
  runInsertRawReturning1 conn (rawTableRW t) g . pgWfromHsI

-- | Like 'runInsertReturning' but takes a 'RawTable' instead of a 'Table'.
runInsertRawReturning
  :: forall m ps w v v' r d
   . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v' r,
      Allow ['Insert, 'Fetch] ps)
  => Conn ps -> RawTable d w v -> (v -> v') -> [w] -> m [r] -- ^
runInsertRawReturning conn t g = \case
  [] -> return []
  ws -> do
    rs <- runInsertRawReturningNoCountCheck conn t g ws
    let nExpected = fromIntegral (length ws) :: Int64
        nAffected = fromIntegral (length rs) :: Int64
    if nExpected == nAffected
       then return rs
       else do
         let OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v' r
             sql = O.arrangeInsertManyReturningSql
                      u (unRawTable t) (NEL.fromList ws) g
         Cx.throwM (ErrNumRows nExpected nAffected (Just sql))


-- | Like 'runInsertReturningNoCountCheck' but takes a 'RawTable' instead of a
-- 'Table'.
runInsertRawReturningNoCountCheck
  :: (MonadIO m, PP.Default O.QueryRunner v' r, Allow ['Insert, 'Fetch] ps)
  => Conn ps -> RawTable d w v -> (v -> v') -> [w] -> m [r] -- ^
runInsertRawReturningNoCountCheck (Conn conn) (RawTable t) g = \case
  [] -> return []
  ws -> liftIO $ O.runInsertManyReturning conn t ws g

-- | Like 'runInsertReturning1' but takes a 'RawTable' instead of a 'Table'.
runInsertRawReturning1
  :: (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v' r,
      Allow ['Insert, 'Fetch] ps)
  => Conn ps -> RawTable d w v -> (v -> v') -> w -> m r -- ^
runInsertRawReturning1 pc t g w = do
   -- Pattern matching on [r] is safe here, see 'runInsertReturning'.
   [r] <- runInsertRawReturning pc t g [w]
   return r

--------------------------------------------------------------------------------

-- | Like 'runUpdate' but takes a 'RawTable' instead of a 'Table'.
runUpdateRaw
  :: (MonadIO m, Allow 'Update ps)
  => Conn ps -> RawTable d w r -> (r -> w) -> (r -> Kol O.PGBool) -> m Int64 -- ^
runUpdateRaw (Conn conn) (RawTable t) upd fil =
  liftIO (O.runUpdate conn t upd (unKol . fil))

-- | Updates all of the rows in the given 'Table' that satisfy the given
-- predicate. Returns the number of actually updated rows.
runUpdate
  :: (TableRW t, MonadIO m, Allow 'Update ps, TableRW t)
  => Conn ps
  -> Table t
  -> (PgW t -> PgW t)        -- ^ Upgrade current values to new values.
  -> (PgR t -> Kol O.PGBool) -- ^ Whether a row should be updated.
  -> m Int64                 -- ^ Number of updated rows.
runUpdate pc t upd = runUpdateRaw pc (rawTableRW t) (upd . pgWfromPgR)

--------------------------------------------------------------------------------

-- | Like 'runDelete' but takes a 'RawTable' instead of a 'Table'.
runDeleteRaw
  :: (MonadIO m, Allow 'Delete ps)
  => Conn ps -> RawTable d w r -> (r -> Kol O.PGBool) -> m Int64 -- ^
runDeleteRaw (Conn conn) (RawTable t) fil =
  liftIO (O.runDelete conn t (unKol . fil))

-- | Deletes all of the rows in the given 'Table' that satisfy the given
-- predicate. Returns the number of deleted rows.
runDelete
  :: (TableRW t, MonadIO m, Allow 'Delete ps, TableRW t)
  => Conn ps
  -> Table t
  -> (PgR t -> Kol O.PGBool) -- ^ Whether a row should be deleted.
  -> m Int64
runDelete pc = runDeleteRaw pc . rawTableRW

--------------------------------------------------------------------------------
-- Exceptions

-- | Exception thrown to indicate a number of rows different from the expected.
data ErrNumRows = ErrNumRows
  { _errNumRowsExpected :: Int64
  , _errNumRowsActual   :: Int64
  , _errNumRowsSQL      :: Maybe String -- ^ Nothing means empty query.
  } deriving (Typeable, Show)
instance Cx.Exception ErrNumRows

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This might belong in Opaleye

qrcFromField :: Pg.FromField b => O.QueryRunnerColumn a b
qrcFromField = O.fieldQueryRunnerColumn

qrcFieldParser :: Pg.FieldParser b -> O.QueryRunnerColumn a b
qrcFieldParser = OI.QueryRunnerColumn (P.rmap (const ()) OI.unpackspecColumn)

qrcFieldParserMap
  :: OI.QueryRunnerColumnDefault a b
  => (Pg.FieldParser b -> Pg.FieldParser b')
  -> O.QueryRunnerColumn a b'
qrcFieldParserMap g =
  let OI.QueryRunnerColumn u fp = O.queryRunnerColumnDefault
   in OI.QueryRunnerColumn u (g fp)

qrcMap
  :: OI.QueryRunnerColumnDefault a b
  => (b -> b') -> O.QueryRunnerColumn a b'
qrcMap g = qrcFieldParserMap (fmap (fmap (fmap g)))

qrcMapMay
  :: (OI.QueryRunnerColumnDefault a b, Show b, Typeable b')
  => (b -> Maybe b') -> O.QueryRunnerColumn a b'
qrcMapMay g = qrcFieldParserMap $ \fp0 -> \f mb -> do
   b <- fp0 f mb
   case g b of
      Just b' -> return b'
      Nothing -> Pg.returnError Pg.ConversionFailed f (show b)

qrcPrism
  :: (OI.QueryRunnerColumnDefault a b, Show b, Typeable b')
  => Prism' b b' -> O.QueryRunnerColumn a b'
qrcPrism p = qrcMapMay (preview p)

qrcWrapped
  :: (Wrapped b, OI.QueryRunnerColumnDefault a (Unwrapped b))
  => OI.QueryRunnerColumn a b
qrcWrapped = qrcMap (review _Wrapped')

--------------------------------------------------------------------------------

-- data Db d m a = Db !(Set Perms) (m a)
--
-- instance Monad m => Monad (Db d m) where
--   return = Db Set.empty . return
--   (>>=) (Db p1 ma) f =
--      let Db p2 mb = ma >>= f
--      in Db (Set.union p1 p2) mb
--
-- data Conny d = Conny !(TMVar Pg.Connection) !(TVar (Set Perms))
--
-- runDb :: Conny d -> Db d m a -> m a

