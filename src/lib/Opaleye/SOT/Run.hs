{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.SOT.Run
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
  , withTransactionRead
  , withTransactionReadWrite
  , withSavepoint
    -- * Query
  , runQuery
  , runQuery1
    -- * Insert
  , runInsert
  , runInsert'
  , runInsert1
  , runInsertT
  , runInsertT'
  , runInsertT1
    -- ** Returning
  , runInsertReturning
  , runInsertReturning'
  , runInsertReturning1
    -- * Update
  , runUpdate
  , runUpdateT
    -- * Delete
  , runDelete
  , runDeleteT
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
import           Data.Foldable
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

import           Opaleye.SOT.Internal

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
     "Opaleye.SOT.Run.Forbid" ~
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

-- | Return a new connection.
connect :: MonadIO m => Pg.ConnectInfo -> m Conn'
connect = connect' . Pg.postgreSQLConnectionString

-- | Like 'connect', except it takes a @libpq@ connection string instead of a
-- 'Pg.ConnectInfo'.
connect' :: MonadIO m => B8.ByteString -> m Conn'
connect' = liftIO . fmap Conn . Pg.connectPostgreSQL

-- | Warning: Using the given @'Conn' ps@ after calling 'close' will result in
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
withTransactionRead
  :: (MonadIO m, Cx.MonadMask m, Allow 'Transact ps, Forbid 'Savepoint ps,
      ps' ~ DropPerm ['Transact, 'Insert, 'Update, 'Delete] ps)
  => IsolationLevel
  -> Conn ps
  -> (Conn ps' -> m a)
  -- ^ The usage of @'Conn' ps@ is undefined within this function,
  -- as well as the usage of @'Conn' ps'@ outside this function.
  -> m a
withTransactionRead il (Conn conn) f = Cx.mask $ \restore -> do
  let tmode = Pg.TransactionMode (pgIsolationLevel il) Pg.ReadOnly
  liftIO $ Pg.beginMode tmode conn
  a <- restore (f (Conn conn)) `Cx.onException` liftIO (Pg.rollback conn)
  a <$ liftIO (Pg.rollback conn)

-- | Execute the given callback within a read-write transaction with the given
-- isolation level, rolling back the transaction in case of exceptions,
-- and either commiting or rolling back the transaction otherwise, as requested
-- by the passed in callback.
withTransactionReadWrite
 :: (MonadIO m, Cx.MonadMask m, Allow 'Transact ps, Forbid 'Savepoint ps,
     ps' ~ ('Savepoint ': DropPerm 'Transact ps))
 => IsolationLevel
 -> Conn ps
 -> (Conn ps' -> m (Either a b))
 -- ^ The usage of @'Conn' ps@ is undefined within this function,
 -- as well as the usage of @'Conn' ps'@ outside this function.
 -- A 'Left' return value rollbacks the transaction, 'Right' commits it.
 -> m (Either a b)
withTransactionReadWrite il (Conn conn) f = Cx.mask $ \restore -> do
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
 => Conn ps -> O.Query v -> m [r] -- ^
runQuery (Conn conn) = liftIO . O.runQuery conn

-- | Query and fetch zero or one resulting row.
--
-- Throws 'ErrNumRows' if there is more than one row in the result.
runQuery1
 :: forall v r m ps
  . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v r, Allow 'Fetch ps)
 => Conn ps -> O.Query v -> m (Maybe r) -- ^
runQuery1 pc q = do
    rs <- runQuery pc q
    case rs of
      [r] -> return (Just r)
      []  -> return Nothing
      _   -> Cx.throwM (ErrNumRows 1 (fromIntegral (length rs)) sql)
  where
    sql = let OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v r
          in  O.showSqlForPostgresExplicit u q

--------------------------------------------------------------------------------

-- | Like 'runInsert', but easier to use if you are querying a single 'Tabla'.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- the number of passed in rows. Use 'runInsertT'' if you don't want this
-- behavior (hint: you probably want this behavior).
runInsertT
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps, Tabla t, Foldable f)
  => Conn ps -> T t -> f (HsI t) -> m () -- ^
runInsertT conn t = runInsert conn (table t) . map pgWfromHsI . toList

-- | Like 'runInsert1', but easier to use if you are querying a single 'Tabla'.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- one. Use 'runInsert'' if you don't want this behavior (hint: you probably
-- want this behavior).
runInsertT1
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps, Tabla t)
  => Conn ps -> T t -> HsI t -> m () -- ^
runInsertT1 conn t = runInsert1 conn (table t) . pgWfromHsI

-- | Like 'runInsert'', but easier to use if you are querying a single 'Tabla'.
--
-- Returns the number of affected rows, which might be different than the number
-- of passed in rows. If you want this situation to throw an exception, use
-- 'runInsertT' (hint: you probably want to use 'runInsertT').
runInsertT'
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps, Tabla t, Foldable f)
  => Conn ps -> T t -> f (HsI t) -> m Int64 -- ^
runInsertT' conn t = runInsert' conn (table t) . map pgWfromHsI . toList

-- | Insert many rows.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- the number of passed in rows. Use 'runInsert'' if you don't want this
-- behavior (hint: you probably want this behavior).
runInsert
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps, Foldable f)
  => Conn ps -> O.Table w v -> f w -> m () -- ^
runInsert conn t fs = case toList fs of
  [] -> return ()
  ws -> do
    nAffected <- runInsert' conn t ws
    let nExpected = fromIntegral (length ws) :: Int64
    when (nExpected == nAffected) $ do
       let sql = O.arrangeInsertManySql t (NEL.fromList ws)
       Cx.throwM (ErrNumRows nExpected nAffected (Just sql))


-- | Like 'runInsert', but doesn't check that the number of affected rows equals
-- the number of passed in rows.
--
-- Returns the number of affected rows, which might be different than the number
-- of passed in rows. If you want this situation to throw an exception, use
-- 'runInsert' (hint: you probably want to use 'runInsert').
runInsert'
  :: (MonadIO m, Allow 'Insert ps, Foldable f)
  => Conn ps -> O.Table w v -> f w -> m Int64 -- ^
runInsert' (Conn conn) t fs = case toList fs of
  [] -> return 0
  ws -> liftIO (O.runInsertMany conn t ws)

-- | Insert one row.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- one. Use 'runInsert'' if you don't want this behavior (hint: you probably
-- want this behavior).
runInsert1
  :: (MonadIO m, Cx.MonadThrow m, Allow 'Insert ps)
  => Conn ps -> O.Table w v -> w -> m () -- ^
runInsert1 pc t w = runInsert pc t [w]

--------------------------------------------------------------------------------

-- | Insert many rows, returning data from the rows actually inserted.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- the number of passed in rows. Use 'runInsertReturning'' if you don't want
-- this behavior (hint: you probably want this behavior).
runInsertReturning
  :: forall m ps w v r f
   . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v r,
      Allow ['Insert, 'Fetch] ps, Foldable f)
  => Conn ps -> O.Table w v -> f w -> m [r] -- ^
runInsertReturning conn t fs = case toList fs of
  [] -> return []
  ws -> do
    rs <- runInsertReturning' conn t ws
    let nExpected = fromIntegral (length ws) :: Int64
        nAffected = fromIntegral (length rs) :: Int64
    if nExpected == nAffected
       then return rs
       else do
         let OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v r
             sql = O.arrangeInsertManyReturningSql u t (NEL.fromList ws) id
         Cx.throwM (ErrNumRows nExpected nAffected (Just sql))


-- Like 'runInsertReturning', but doesn't check that the number of affected
-- rows equals the number of passed in rows.
--
-- Returns the number of affected rows, which might be different than the number
-- of passed in rows. If you want this situation to throw an exception, use
-- 'runInsertReturning' (hint: you probably want to use 'runInsertReturning').
runInsertReturning'
  :: (MonadIO m, PP.Default O.QueryRunner v r, Allow ['Insert, 'Fetch] ps,
      Foldable f)
  => Conn ps -> O.Table w v -> f w -> m [r] -- ^
runInsertReturning' (Conn conn) t fs = case toList fs of
   [] -> return []
   ws -> liftIO $ O.runInsertManyReturning conn t ws id

-- | Insert one row, returning data from the one row actually inserted.
--
-- Throws 'ErrNumRows' if the number of actually affected rows is different than
-- one. Use 'runInsertReturning'' if you don't want this behavior (hint: you
-- probably want this behavior).
runInsertReturning1
  :: forall m v w r ps
   . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v r,
      Allow ['Insert, 'Fetch] ps)
  => Conn ps -> O.Table w v -> w -> m r -- ^
runInsertReturning1 pc t w = do
   -- Pattern matching on [r] is safe here, see 'runInsertReturning'.
   [r] <- runInsertReturning pc t [w]
   return r

--------------------------------------------------------------------------------

-- | Like @opaleye@'s 'O.runUpdate', but the predicate is expected to
-- return a @'Kol' 'O.PGBool'@. Returns the number of affected rows.
--
-- It is recommended that you use 'runUpdateT' if you are trying to update
-- a table that is an instance of 'Tabla'. The result is the same, but
-- this function might be less convenient to use.
runUpdate
  :: (MonadIO m, Allow 'Update ps)
  => Conn ps -> O.Table w r -> (r -> w) -> (r -> Kol O.PGBool) -> m Int64 -- ^
runUpdate (Conn conn) t upd fil = liftIO (O.runUpdate conn t upd (unKol . fil))

-- | Like 'runUpdate', but specifically designed to work well with 'Tabla'.
runUpdateT
  :: (Tabla t, MonadIO m, Allow 'Update ps)
  => Conn ps
  -> T t
  -> (PgW t -> PgW t)        -- ^ Upgrade current values to new values.
  -> (PgR t -> Kol O.PGBool) -- ^ Whether a row should be updated.
  -> m Int64                 -- ^ Number of updated rows.
runUpdateT pc t upd = runUpdate pc (table t) (upd . pgWfromPgR)

--------------------------------------------------------------------------------

-- | Like @opaleye@'s 'O.runDelete', but the predicate is expected to return
-- a @'Kol' 'O.PGBool'@. Returns the number of affected rows.
--
-- It is recommended that you use 'runDeleteT' if you are trying to update
-- a table that is an instance of 'Tabla', the result is the same, but
-- this function might be less convenient to use.
runDelete
  :: (MonadIO m, Allow 'Delete ps)
  => Conn ps -> O.Table w r -> (r -> Kol O.PGBool) -> m Int64 -- ^
runDelete (Conn conn) t fil = liftIO (O.runDelete conn t (unKol . fil))

-- | Like 'runDelete', but specifically designed to work well with 'Tabla'.
runDeleteT
  :: (Tabla t, MonadIO m, Allow 'Delete ps)
  => Conn ps
  -> T t
  -> (PgR t -> Kol O.PGBool) -- ^ Whether a row should be deleted.
  -> m Int64
runDeleteT pc = runDelete pc . table

--------------------------------------------------------------------------------
-- Exceptions

-- | Exception thrown when indicating less rows than expected are available.
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
