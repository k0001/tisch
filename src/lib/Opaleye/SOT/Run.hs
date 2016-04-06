{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.SOT.Run
  ( -- * Connection
    PgConn
  , connect
  , connect'
  , close
    -- * Permissions
  , Perm(..)
  , SPerm
  , Allow
  , Forbid
  , withoutPerm
    -- * Transaction
  , withTransaction
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
  , ErrPgConnClosed(..)
  , ErrPgConnInUse(..)
  ) where

import           Control.Concurrent.MVar hiding (withMVar)
import           Control.Monad ((<=<))
import           Control.Monad.IO.Class
import qualified Control.Monad.Catch as Cx
import           Data.Int (Int64)
import qualified Data.Profunctor.Product.Default as PP
import           Data.Singletons.Prelude
import           Data.Singletons.TH
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
    | Transaction
      -- ^ Allow starting and finishing transactions (i.e., @BEGIN@,
      -- @COMMIT@, @ROLLBACK@).
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
--
-- Note that 'PgConn' is not thread-safe, you are encouraged to maintain a
-- multithreaded pool of 'PgConn' instead. See "Data.Pool" from the @ex-pool@
-- package.
newtype PgConn (perms :: [Perm]) = PgConn (MVar (Maybe Pg.Connection))

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

-- | Internal. @'withPgConn' x f@ ensures that @x@ is not
-- usable outside @f@ while @f@ is running.
withPgConn
  :: (MonadIO m, Cx.MonadMask m)
  => PgConn ps -> (Pg.Connection -> m a) -> m a
withPgConn (PgConn mv) f = Cx.bracket
   (liftIO (takeMVar mv))
   (liftIO . putMVar mv)
   (maybe (Cx.throwM ErrPgConnClosed) f)

-- | Drop a permission from the connection.
withoutPerm
  :: forall proxy m p ps a
   . (MonadIO m, Cx.MonadMask m, Allow p ps, Forbid p (DropPerm p ps))
  => proxy (p :: Perm)
  -> PgConn ps
  -> (forall ps'. Forbid p ps' => PgConn ps' -> m a)
  -- ^ The usage of @'PgConn' ps@ is undefined within this function,
  -- and @'PgConn' ps'@ mustn't escape the scope of this function.
  -> m a
withoutPerm _ pc f = withPgConn pc $ \conn -> Cx.bracket
  (liftIO $ newMVar (Just conn))
  (maybe (Cx.throwM ErrPgConnInUse) return <=< liftIO . tryTakeMVar)
  (\mv' -> f (PgConn mv' :: PgConn (DropPerm p ps)))

-- | Return a new connection and a finalizer for it.
connect
  :: MonadIO m
  => Pg.ConnectInfo
  -> m (PgConn ['Read, 'Write, 'Transaction]) -- ^
connect = connect' . Pg.postgreSQLConnectionString

-- | Like 'connect', except it takes a @libpq@ connection string instead of a
-- 'Pg.ConnectInfo'.
connect'
  :: MonadIO m
  => B8.ByteString -- ^ @libpq@ connection string.
  -> m (PgConn ['Read, 'Write, 'Transaction])
connect' bs = liftIO $ do
  conn <- Pg.connectPostgreSQL bs
  PgConn <$> newMVar (Just conn)

-- | Warning: Using the given @'PgConn' ps@ after calling 'close' will result in
-- a runtime exception.
close :: (MonadIO m, Cx.MonadMask m) => PgConn ps -> m ()
close pc = withPgConn pc (liftIO . Pg.close)

withTransaction
  :: forall m ps a b
   . (MonadIO m, Cx.MonadMask m,
      Allow 'Transaction ps, Forbid 'Transaction (DropPerm 'Transaction ps))
  => PgConn ps
  -> Pg.TransactionMode
  -> (forall ps'. Forbid 'Transaction ps' => PgConn ps' -> m (Either a b))
  -- ^ The usage of @'PgConn' ps@ is undefined within this function,
  -- and @'PgConn' ps'@ mustn't escape the scope of this function.
  -- A 'Left' return value rollbacks the transaction, 'Right' commits it.
  -> m (Either a b)
withTransaction pc tmode f = withPgConn pc $ \conn -> Cx.bracket
  (liftIO $ newMVar (Just conn))
  (maybe (Cx.throwM ErrPgConnInUse) return <=< liftIO . tryTakeMVar)
  (\mv' -> Cx.mask $ \restore -> do
     let pc' = PgConn mv' :: PgConn (DropPerm 'Transaction ps)
     liftIO $ Pg.beginMode tmode conn
     eab <- restore (f pc') `Cx.onException` liftIO (Pg.rollback conn)
     liftIO $ either (const Pg.rollback) (const Pg.commit) eab conn
     return eab)

--------------------------------------------------------------------------------

-- | Query and fetch zero or more resulting rows.
runQueryMany
 :: (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs, Allow 'Read ps)
 => PgConn ps -> (hs -> Either Cx.SomeException r) -> O.Query v -> m [r] -- ^
runQueryMany pc f q = liftIO $ withPgConn pc $ \conn ->
  traverse (either Cx.throwM return . f) =<< O.runQuery conn q

-- | Query and fetch zero or one resulting row.
--
-- Throws 'ErrTooManyRows' if there is more than one row in the result.
runQuery1
 :: forall v hs r m ps
  . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs, Allow 'Read ps)
 => PgConn ps -> (hs -> Either Cx.SomeException r) -> O.Query v
 -> m (Maybe r) -- ^
runQuery1 pc f q = do
    rs <- runQueryMany pc f q
    case rs of
      [r] -> return (Just r)
      []  -> return Nothing
      _   -> Cx.throwM $ ErrTooManyRows (length rs) sql
  where
    sql = let OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
          in  O.showSqlForPostgresExplicit u q

-- | Query and fetch one resulting row.
--
-- Throws 'ErrTooManyRows' if there is more than one row in the result, and
-- 'ErrNoRows' if there is no row in the result.
runQueryHead
 :: forall v hs r m ps
  . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs, Allow 'Read ps)
 => PgConn ps -> (hs -> Either Cx.SomeException r) -> O.Query v
 -> m r -- ^
runQueryHead pc f q = do
    rs <- runQueryMany pc f q
    case rs of
      [r] -> return r
      []  -> Cx.throwM $ ErrNoRows sql
      _   -> Cx.throwM $ ErrTooManyRows (length rs) sql
  where
    sql = let OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
          in  O.showSqlForPostgresExplicit u q


--------------------------------------------------------------------------------

-- | Insert zero or more rows.
runInsertMany
  :: (MonadIO m, Allow 'Write ps)
  => PgConn ps -> O.Table w v -> [w] -> m Int64 -- ^
runInsertMany pc t ws = liftIO $ withPgConn pc $ \conn ->
  O.runInsertMany conn t ws

-- | Insert one row.
runInsert1
  :: (MonadIO m, Allow 'Write ps)
  => PgConn ps -> O.Table w v -> w -> m Int64 -- ^
runInsert1 pc t w = runInsertMany pc t [w]

--------------------------------------------------------------------------------

-- | Insert zero or more rows, returning data from the rows actually inserted.
runInsertReturningMany
  :: (MonadIO m, PP.Default O.QueryRunner v hs,
      Allow 'Write ps, Allow 'Read ps)
  => PgConn ps -> (hs -> Either Cx.SomeException r) -> O.Table w v -> w
  -> m [r] -- ^
runInsertReturningMany pc f t w = liftIO $ withPgConn pc $ \conn ->
   traverse (either Cx.throwM return . f) =<< O.runInsertReturning conn t w id

-- | Insert 1 row, returning data from the zero or one rows actually inserted.
--
-- Throws 'ErrTooManyRows' if there is more than one row in the result.
runInsertReturning1
  :: forall m v hs w r ps
   . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs,
      Allow 'Write ps, Allow 'Read ps)
  => PgConn ps -> (hs -> Either Cx.SomeException r) -> O.Table w v -> w
  -> m (Maybe r) -- ^
runInsertReturning1 pc f t w = do
   rs <- runInsertReturningMany pc f t w
   case rs of
     [r] -> return (Just r)
     []  -> return Nothing
     _   -> Cx.throwM $ ErrTooManyRows (length rs) sql
  where
    sql = let OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
          in  O.arrangeInsertReturningSql u t w id

-- | Insert 1 row, returning data from the one row actually inserted.
--
-- Throws 'ErrTooManyRows' if there is more than one row in the result, and
-- 'ErrNoRows' if there is no row in the result.
runInsertReturningHead
  :: forall m hs w v r ps
   . (MonadIO m, Cx.MonadThrow m, PP.Default O.QueryRunner v hs,
      Allow 'Write ps, Allow 'Read ps)
  => PgConn ps -> (hs -> Either Cx.SomeException r) -> O.Table w v -> w
  -> m r -- ^
runInsertReturningHead pc f t w = do
   rs <- runInsertReturningMany pc f t w
   case rs of
     [r] -> return r
     []  -> Cx.throwM $ ErrNoRows sql
     _   -> Cx.throwM $ ErrTooManyRows (length rs) sql
  where
    sql = let OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
          in  O.arrangeInsertReturningSql u t w id

--------------------------------------------------------------------------------

-- | Like Opaleye's 'O.runUpdate', but the predicate is expected to
-- return a @('GetKol' w 'O.PGBool')@.
--
-- It is recommended that you use 'runUpdateTabla' if you are trying to update
-- a table that is an instance of 'Tabla'. The result is the same, but the
-- this function might be less convenient to use.
runUpdate
  :: (MonadIO m, GetKol gkb O.PGBool, Allow 'Write ps)
  => PgConn ps -> O.Table w r -> (r -> w) -> (r -> gkb) -> m Int64 -- ^
runUpdate pc t upd fil = liftIO $ withPgConn pc $ \conn ->
    O.runUpdate conn t upd (unKol . getKol . fil)

-- | Like 'runUpdate', but specifically designed to work well with 'Tabla'.
runUpdateTabla'
  :: forall t m gkb ps
   . (Tabla t, MonadIO m, GetKol gkb O.PGBool, Allow 'Write ps)
  => PgConn ps
  -> (PgW t -> PgW t) -- ^ Upgrade current values to new values.
  -> (PgR t -> gkb)   -- ^ Whether a row should be updated.
  -> m Int64          -- ^ Number of updated rows.
runUpdateTabla' pc = runUpdateTabla pc (T::T t)

-- | Like 'runUpdateTabla'', but takes @t@ explicitely for the times when
-- it can't be inferred.
runUpdateTabla
  :: (Tabla t, MonadIO m, GetKol gkb O.PGBool, Allow 'Write ps)
  => PgConn ps -> T t -> (PgW t -> PgW t) -> (PgR t -> gkb) -> m Int64 -- ^
runUpdateTabla pc t upd = runUpdate pc (table t) (upd . update')

--------------------------------------------------------------------------------
-- Exceptions

-- | Exception thrown when indicating more rows than expected are available.
data ErrTooManyRows = ErrTooManyRows Int String -- ^ Number of rows, SQL string
  deriving (Typeable, Show)
instance Cx.Exception ErrTooManyRows

-- | Exception thrown when indicating no rows are available.
data ErrNoRows = ErrNoRows String -- ^ SQL string
  deriving (Typeable, Show)
instance Cx.Exception ErrNoRows

-- | Exception thrown when trying to use a closed connection.
data ErrPgConnClosed = ErrPgConnClosed
  deriving (Typeable, Show)
instance Cx.Exception ErrPgConnClosed

-- | Exception thrown when a connection is being used when it shouldn't be.
data ErrPgConnInUse = ErrPgConnInUse
  deriving (Typeable, Show)
instance Cx.Exception ErrPgConnInUse
