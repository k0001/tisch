{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Opaleye.SOT.Run
{-  ( -- * Connection
    PgConn
  , connect
  , connect'
  , close
  , Perm(..)
  , Allow
  , Forbid
  , dropPerm
  , unsafePgConn
    -- * Transaction
  , begin
  , commit
  , rollback
    -- * Query
  , runQueryMany
  , runQuery1
  , runQueryHead
    -- * Insert
  , runInsertMany
  , runInsert1
    -- ** Returning
  , runInsertReturningMany
  , runInsertReturning1
  , runInsertReturningHead
    -- * Update
  , runUpdate
  , runUpdateTabla
  , runUpdateTabla'
    -- * Exception
  , ErrTooManyRows(..)
  , ErrNoRows(..)
  , ErrPgConnPerms(..)
  , ErrPgConnClosed(..)
  ) -}  where

import           Control.Concurrent.MVar
import           Control.Monad (when, void)
import           Control.Monad.IO.Class
import qualified Control.Monad.Catch as Cx
import           Control.Lens
import           Data.Foldable
import           Data.Int (Int64)
import qualified Data.Profunctor.Product.Default as PP
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Data.Singletons.Decide
import           Data.Typeable (Typeable)
import qualified Data.ByteString.Char8 as B8
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import           GHC.Exts (Constraint)
import qualified Opaleye as O
import qualified Opaleye.Internal.RunQuery as OI

import           Opaleye.SOT.Internal

--------------------------------------------------------------------------------

$(singletons [d|
   -- | 'Perm' is only used at the type level to index 'PgConn'.
   data Perm
     = Read -- ^ Allow reading (i.e., @SELECT@)
     | Write -- ^ Allow writing  (i.e., @INSERT@, @UPDATE@)
     | TransactionBegin
       -- ^ Allow beginning transactions (i.e., @BEGIN@)
     | TransactionFinish
       -- ^ Allow finishing transactions (i.e., @COMMIT@, @ROLLBACK@)
     deriving (Eq, Ord, Show)
 |])

-- | @'PgConn' perms@ is just a wrapper around @postgresql-simple@'s
-- 'Pg.Connection' that carries, in @perms@, type-level information about which
-- kind of operations can be performed on this connection.
--
-- The problem with 'Pg.Connection' is that it allows us to perform any operation
-- on it (reading, inserting, commiting a transaction, etc.), but sometimes we
-- would prefer to limit the type of operations this connection might support.
-- For example, we might want to forbid transactions from being commited because
-- they are being handled by someone else. @'PgConn' perms@ carries type-level
-- information about this. For example, a @'PgConn' '['Write',
-- 'TransactionBegin']@ is suitable for performing inserts, updates, and begin
-- transactions, hereas @'PgConn' '['Write'] can perform inserts and updates, but
-- cannot work with transactions.
newtype PgConn (perms :: [Perm]) = PgConn (MVar (Maybe (Pg.Connection, Set Perm)))

-- | @'Allow' p ps@ ensures that @p@ is present in @ps@.
type family Allow (p :: Perm) (ps :: [Perm]) :: Constraint where
  Allow p '[] =
     "Opaleye.SOT.Run.Allow" ~
     "Allow: The required permission is forbidden"
  Allow p (p ': ps) = ()
  Allow p (q ': ps) = Allow p ps

-- | @'Forbid' p ps@ ensures that @p@ is not present in @ps@.
type family Forbid (p :: Perm) (ps :: [Perm]) :: Constraint where
  Forbid p (p ': ps) =
     "Opaleye.SOT.Run.Forbid" ~
     "Forbid: The forbidden permission is allowed"
  Forbid p (q ': ps) = Forbid p ps
  Forbid p '[] = ()

-- | @'DropPerm' p ps@ removes @p@ from @ps@ if present.
type family DropPerm (p :: Perm) (ps :: [Perm]) :: [Perm] where
  DropPerm p (p ': ps) = DropPerm p ps
  DropPerm p (q ': ps) = q ': DropPerm p ps
  DropPerm p '[]       = '[]


data ReadOrTake = ReadOrTake_Read | ReadOrTake_Take

-- | Internal. This may thrown 'ErrPgConnClosed' or 'ErrPgConnPerms'
readOrTakePgConn
  :: forall m ps
   . (MonadIO m, SingI ps)
  => ReadOrTake -> PgConn ps -> m Pg.Connection
readOrTakePgConn rot = \(PgConn mv) -> liftIO $ do
   let f = case rot of
             ReadOrTake_Read -> readMVar
             ReadOrTake_Take -> takeMVar
   mx <- f mv
   case mx of
     Nothing -> Cx.throwM ErrPgConnClosed
     Just (conn, aperms) -> do
       let eperms = Set.fromList (fromSing (sing :: Sing ps))
       if eperms /= aperms
          then Cx.throwM $ ErrPgConnPerms eperms aperms
          else return conn

-- | Internal. This may thrown 'ErrPgConnClosed' or 'ErrPgConnPerms'
readPgConn :: (MonadIO m, SingI ps) => PgConn ps -> m Pg.Connection
readPgConn = readOrTakePgConn ReadOrTake_Read

-- | Internal. This may thrown 'ErrPgConnClosed' or 'ErrPgConnPerms'
takePgConn :: (MonadIO m, SingI ps) => PgConn ps -> m Pg.Connection
takePgConn = readOrTakePgConn ReadOrTake_Take

-- | Internal. Make sure to fix @ps'@ to some specific type.
-- If @ps'@ is @[]@ then the connection will be closed.
putPgConn
  :: forall m ps ps'
   . (MonadIO m, SingI ps')
  => Pg.Connection -> PgConn ps -> m (PgConn ps')
putPgConn conn (PgConn mv) = liftIO $ do
   let perms = Set.fromList (fromSing (sing :: Sing ps'))
   putMVar mv (Just (conn, perms))
   return (PgConn mv)

-- | Internal. Make sure to fix @ps'@ to some specific type.
-- If @ps'@ is @[]@ then the connection will be closed.
withPgConn
  :: forall m ps ps' a
   . (MonadIO m, Cx.MonadMask m, SingI ps, SingI ps')
  => (Pg.Connection -> m a) -> PgConn ps -> m (a, PgConn ps')
withPgConn k pc@(PgConn mv) = Cx.bracket
  (takePgConn pc)
  (\conn -> putPgConn conn pc <&> asTypeOf pc)
  (\conn -> do
      a <- k conn
      return (a, PgConn mv :: PgConn ps'))

-- | Internal.
closePgConn :: (MonadIO m) => PgConn ps -> m (PgConn '[])
closePgConn pc@(PgConn mv) = liftIO $ Cx.mask $ \restore -> do
  mx <- takeMVar mv
  case mx of
     Nothing -> do
        putMVar mv Nothing
        restore (Cx.throwM ErrPgConnClosed)
     Just (conn, perms) -> do
        flip Cx.onException (putMVar mv mx) $ restore $ do
           Pg.close conn
           when (Set.member TransactionFinish perms) $ do
              putStrLn "Warning: Opaleye.SOT.Run.closePgConn - \
                       \Closed connection on transaction"
  putMVar mv Nothing
  return (PgConn mv)

-- | Drop a permission from the connection.
dropPerm
  :: (MonadIO m, Cx.MonadMask m, SingI ps, SingI (DropPerm p ps))
  => proxy (p :: Perm) -> PgConn ps -> m (PgConn (DropPerm p ps)) -- ^
dropPerm _ = fmap snd . withPgConn return

connect
  :: MonadIO m
  => Pg.ConnectInfo
  -> m (PgConn ['Read, 'Write, 'TransactionBegin], IO ()) -- ^
connect = connect' . Pg.postgreSQLConnectionString

-- | Like 'connect', except it takes a @libpq@ connection string instead of a
-- 'Pg.ConnectInfo'.
connect'
  :: MonadIO m
  => B8.ByteString -- ^ @libpq@ connection string.
  -> m (PgConn ['Read, 'Write, 'TransactionBegin], IO ())
connect' connStr = liftIO $ Cx.mask $ \restore -> do
  conn <- Pg.connectPostgreSQL connStr
  flip Cx.onException (Pg.close conn) $ restore $ do
     mv <- newEmptyMVar
     let pc = PgConn mv
     _ <- putPgConn conn pc <&> asTypeOf pc
     return (pc, void (closePgConn pc))

--------------------------------------------------------------------------------
-- Transactions

begin
  :: (MonadIO m, Cx.MonadMask m,
      Allow 'TransactionBegin ps,
      SingI ps, SingI ('TransactionFinish ': DropPerm 'TransactionBegin ps))
  => Pg.TransactionMode
  -> PgConn ps
  -> m (PgConn ('TransactionFinish ': DropPerm 'TransactionBegin ps)) -- ^
begin tmode = fmap snd . withPgConn (liftIO . Pg.beginMode tmode)

commit
  :: (MonadIO m, Cx.MonadMask m,
      Allow 'TransactionFinish ps,
      SingI ps, SingI ('TransactionBegin ': DropPerm 'TransactionFinish ps))
  => PgConn ps
  -> m (PgConn ('TransactionBegin ': DropPerm 'TransactionFinish ps)) -- ^
commit = fmap snd . withPgConn (liftIO . Pg.commit)

rollback
  :: (MonadIO m, Cx.MonadMask m,
      Allow 'TransactionFinish ps,
      SingI ps, SingI ('TransactionBegin ': DropPerm 'TransactionFinish ps))
  => PgConn ps
  -> m (PgConn ('TransactionBegin ': DropPerm 'TransactionFinish ps)) -- ^
rollback = fmap snd . withPgConn (liftIO . Pg.rollback)

--------------------------------------------------------------------------------

-- -- | Query and fetch zero or more resulting rows.
-- runQueryMany
--  :: (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs, Allow 'Read ps)
--  => (hs -> Either SomeException r) -> O.Query v -> PgConn ps -> m [r] -- ^
-- runQueryMany f q (PgConn conn) = do
--   traverse (either Cx.throwM return . f) =<< liftIO (O.runQuery conn q)

-- -- | Query and fetch zero or one resulting row.
-- --
-- -- Throws 'ErrTooManyRows' if there is more than one row in the result.
-- runQuery1
--  :: forall v hs r m ps
--   . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs, Allow 'Read ps)
--  => (hs -> Either SomeException r) -> O.Query v
--  -> PgConn ps -> m (Maybe r) -- ^
-- runQuery1 f q pc = do
--     rs <- runQueryMany f q pc
--     case rs of
--       [r] -> return (Just r)
--       []  -> return Nothing
--       _   -> Cx.throwM $ ErrTooManyRows (length rs) sql
--   where
--     OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
--     sql = O.showSqlForPostgresExplicit u q

-- -- | Query and fetch one resulting row.
-- --
-- -- Throws 'ErrTooManyRows' if there is more than one row in the result, and
-- -- 'ErrNoRows' if there is no row in the result.
-- runQueryHead
--  :: forall v hs r m ps
--   . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs, Allow 'Read ps)
--  => (hs -> Either SomeException r) -> O.Query v -> PgConn ps -> m r -- ^
-- runQueryHead f q pc = do
--     rs <- runQueryMany f q pc
--     case rs of
--       [r] -> return r
--       []  -> Cx.throwM $ ErrNoRows sql
--       _   -> Cx.throwM $ ErrTooManyRows (length rs) sql
--   where
--     OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
--     sql = O.showSqlForPostgresExplicit u q

-- --------------------------------------------------------------------------------

-- -- | Insert zero or more rows.
-- runInsertMany
--   :: (MonadIO m, Allow 'Write ps)
--   => O.Table w v -> [w] -> PgConn ps -> m Int64 -- ^
-- runInsertMany t ws (PgConn conn) = liftIO (O.runInsertMany conn t ws)

-- -- | Insert one row.
-- runInsert1
--   :: (MonadIO m, Allow 'Write ps)
--   => O.Table w v -> w -> PgConn ps -> m Int64 -- ^
-- runInsert1 t w = runInsertMany t [w]

-- --------------------------------------------------------------------------------

-- -- | Insert zero or more rows, returning data from the rows actually inserted.
-- runInsertReturningMany
--   :: (MonadIO m, PP.Default O.QueryRunner v hs,
--       Allow 'Write ps, Allow 'Read ps)
--   => (hs -> Either SomeException r) -> O.Table w v -> w
--   -> PgConn ps -> m [r] -- ^
-- runInsertReturningMany f t w (PgConn conn) = liftIO $ do
--    traverse (either Cx.throwM return . f) =<< O.runInsertReturning conn t w id

-- -- | Insert 1 row, returning data from the zero or one rows actually inserted.
-- --
-- -- Throws 'ErrTooManyRows' if there is more than one row in the result.
-- runInsertReturning1
--   :: forall m v hs w r ps
--    . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs,
--       Allow 'Write ps, Allow 'Read ps)
--   => (hs -> Either SomeException r) -> O.Table w v -> w
--   -> PgConn ps -> m (Maybe r) -- ^
-- runInsertReturning1 f t w conn = do
--    rs <- runInsertReturningMany f t w conn
--    case rs of
--      [r] -> return (Just r)
--      []  -> return Nothing
--      _   -> Cx.throwM $ ErrTooManyRows (length rs) sql
--   where
--     OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
--     sql = O.arrangeInsertReturningSql u t w id

-- -- | Insert 1 row, returning data from the one row actually inserted.
-- --
-- -- Throws 'ErrTooManyRows' if there is more than one row in the result, and
-- -- 'ErrNoRows' if there is no row in the result.
-- runInsertReturningHead
--   :: forall m hs w v r ps
--    . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs,
--       Allow 'Write ps, Allow 'Read ps)
--   => (hs -> Either SomeException r) -> O.Table w v -> w
--   -> PgConn ps -> m r -- ^
-- runInsertReturningHead f t w conn = do
--    rs <- runInsertReturningMany f t w conn
--    case rs of
--      [r] -> return r
--      []  -> Cx.throwM $ ErrNoRows sql
--      _   -> Cx.throwM $ ErrTooManyRows (length rs) sql
--   where
--     OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
--     sql = O.arrangeInsertReturningSql u t w id

-- --------------------------------------------------------------------------------

-- -- | Like Opaleye's 'O.runUpdate', but the predicate is expected to
-- -- return a @('GetKol' w 'O.PGBool')@.
-- --
-- -- It is recommended that you use 'runUpdateTabla' if you are trying to update
-- -- a table that is an instance of 'Tabla'. The result is the same, but the
-- -- this function might be less convenient to use.
-- runUpdate
--   :: (MonadIO m, GetKol gkb O.PGBool, Allow 'Write ps)
--   => O.Table w r -> (r -> w) -> (r -> gkb) -> PgConn ps -> m Int64 -- ^
-- runUpdate t upd fil (PgConn conn) = liftIO $ do
--     O.runUpdate conn t upd (unKol . getKol . fil)

-- -- | Like 'runUpdate', but specifically designed to work well with 'Tabla'.
-- runUpdateTabla'
--   :: forall t m gkb ps
--    . (Tabla t, MonadIO m, GetKol gkb O.PGBool, Allow 'Write ps)
--   => (PgW t -> PgW t) -- ^ Upgrade current values to new values.
--   -> (PgR t -> gkb)   -- ^ Whether a row should be updated.
--   -> PgConn ps
--   -> m Int64          -- ^ Number of updated rows.
-- runUpdateTabla' = runUpdateTabla (T::T t)

-- -- | Like 'runUpdateTabla'', but takes @t@ explicitely for the times when
-- -- it can't be inferred.
-- runUpdateTabla
--   :: (Tabla t, MonadIO m, GetKol gkb O.PGBool, Allow 'Write ps)
--   => T t -> (PgW t -> PgW t) -> (PgR t -> gkb) -> PgConn ps -> m Int64 -- ^
-- runUpdateTabla t upd = runUpdate (table t) (upd . update')

--------------------------------------------------------------------------------
-- Exceptions

-- | Exception thrown when the expected 'PgConn' permissions don't match the
-- actual permissions,
data ErrPgConnPerms = ErrPgConnPerms
  { _errPgConnPerms_expected :: !(Set Perm)
    -- ^ Expected permissions (i.e., those indexing 'PgConn').
  , _errPgConnPerms_actual :: !(Set Perm)
    -- ^ Actual current permissions.
  } deriving (Typeable, Show)
instance Cx.Exception ErrPgConnPerms

-- | Exception thrown when trying to use a 'PgConn' that has already been
-- closed.
data ErrPgConnClosed = ErrPgConnClosed deriving (Typeable, Show)
instance Cx.Exception ErrPgConnClosed

-- | Exception thrown when indicating more rows than expected are available.
data ErrTooManyRows = ErrTooManyRows Int String -- ^ Number of rows, SQL string
   deriving (Typeable, Show)
instance Cx.Exception ErrTooManyRows

-- | Exception thrown when indicating no rows are available.
data ErrNoRows = ErrNoRows String -- ^ SQL string
   deriving (Typeable, Show)
instance Cx.Exception ErrNoRows

--------------------------------------------------------------------------------
-- Misc

class (kparam ~ 'KProxy, SingKind kparam)
  => FromSings (kparam :: KProxy k) (as :: [k]) where
  fromSings :: proxy as -> [DemoteRep kparam]
instance (kparam ~ 'KProxy, SingKind kparam)
  => FromSings (kparam :: KProxy k) '[] where
  fromSings _ = []
instance (kparam ~ 'KProxy, SingKind kparam, SingI a, FromSings kparam as)
  => FromSings (kparam :: KProxy k) (a ': as) where
  fromSings (_ :: proxy (a ': as))
     = fromSing (sing :: Sing a) : fromSings (Proxy :: Proxy as)
