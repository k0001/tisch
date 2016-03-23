{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Opaleye.SOT.Run
  ( commit
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
  ) where

import           Control.Exception (Exception, SomeException)
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader, ask)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Int (Int64)
import qualified Data.Profunctor.Product.Default as PP
import           Data.Typeable (Typeable)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Opaleye as O
import qualified Opaleye.Internal.RunQuery as OI
import           Opaleye.SOT.Internal

--------------------------------------------------------------------------------

-- | Exception type indicating too many rows.
data ErrTooManyRows = ErrTooManyRows Int String -- ^ Number of rows, SQL string
   deriving (Typeable, Show)
instance Exception ErrTooManyRows

-- | Exception type indicating no rows.
data ErrNoRows = ErrNoRows String -- ^ SQL string
   deriving (Typeable, Show)
instance Exception ErrNoRows

--------------------------------------------------------------------------------
-- Working with the DB connection

commit :: (MonadIO m, MonadReader Pg.Connection m) => m ()
commit = (liftIO . Pg.commit) =<< ask

--------------------------------------------------------------------------------

-- | Query and fetch zero or more resulting rows.
runQueryMany
 :: (MonadIO m, MonadThrow m, MonadReader Pg.Connection m,
     PP.Default O.QueryRunner v hs)
 => (hs -> Either SomeException r) -> O.Query v -> m [r] -- ^
runQueryMany f q = ask >>= \conn -> do
  traverse (either throwM return . f) =<< liftIO (O.runQuery conn q)

-- | Query and fetch zero or one resulting row.
--
-- Throws 'ErrTooManyRows' if there is more than one row in the result.
runQuery1
 :: forall v hs r m
  . (MonadIO m, MonadThrow m, MonadReader Pg.Connection m,
     PP.Default O.QueryRunner v hs)
 => (hs -> Either SomeException r) -> O.Query v -> m (Maybe r) -- ^
runQuery1 f q = do
    rs <- runQueryMany f q
    case rs of
      [r] -> return (Just r)
      []  -> return Nothing
      _   -> throwM $ ErrTooManyRows (length rs) sql
  where
    OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
    sql = O.showSqlForPostgresExplicit u q

-- | Query and fetch one resulting row.
--
-- Throws 'ErrTooManyRows' if there is more than one row in the result, and
-- 'ErrNoRows' if there is no row in the result.
runQueryHead
 :: forall v hs r m
  . (MonadIO m, MonadThrow m, MonadReader Pg.Connection m,
     PP.Default O.QueryRunner v hs)
 => (hs -> Either SomeException r) -> O.Query v -> m r -- ^
runQueryHead f q = do
    rs <- runQueryMany f q
    case rs of
      [r] -> return r
      []  -> throwM $ ErrNoRows sql
      _   -> throwM $ ErrTooManyRows (length rs) sql
  where
    OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
    sql = O.showSqlForPostgresExplicit u q

--------------------------------------------------------------------------------

-- | Insert zero or more rows.
runInsertMany
  :: (MonadIO m, MonadReader Pg.Connection m)
  => O.Table w v -> [w] -> m Int64 -- ^
runInsertMany t ws = ask >>= \conn -> liftIO (O.runInsertMany conn t ws)

-- | Insert one row.
runInsert1
  :: (MonadIO m, MonadReader Pg.Connection m)
  => O.Table w v -> w -> m Int64 -- ^
runInsert1 t w = runInsertMany t [w]

--------------------------------------------------------------------------------

-- | Insert zero or more rows, returning data from the rows actually inserted.
runInsertReturningMany
  :: (MonadIO m, MonadReader Pg.Connection m, PP.Default O.QueryRunner v hs)
  => (hs -> Either SomeException r) -> O.Table w v -> w -> m [r] -- ^
runInsertReturningMany f t w = ask >>= \conn -> liftIO $ do
   traverse (either throwM return . f) =<< O.runInsertReturning conn t w id

-- | Insert 1 row, returning data from the zero or one rows actually inserted.
--
-- Throws 'ErrTooManyRows' if there is more than one row in the result.
runInsertReturning1
  :: forall m v hs w r
   . (MonadIO m, MonadThrow m, MonadReader Pg.Connection m,
      PP.Default O.QueryRunner v hs)
  => (hs -> Either SomeException r) -> O.Table w v -> w -> m (Maybe r) -- ^
runInsertReturning1 f t w = do
   rs <- runInsertReturningMany f t w
   case rs of
     [r] -> return (Just r)
     []  -> return Nothing
     _   -> throwM $ ErrTooManyRows (length rs) sql
  where
    OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
    sql = O.arrangeInsertReturningSql u t w id

-- | Insert 1 row, returning data from the one row actually inserted.
--
-- Throws 'ErrTooManyRows' if there is more than one row in the result, and
-- 'ErrNoRows' if there is no row in the result.
runInsertReturningHead
  :: forall m hs w v r
   . (MonadIO m, MonadThrow m, MonadReader Pg.Connection m,
      PP.Default O.QueryRunner v hs)
  => (hs -> Either SomeException r) -> O.Table w v -> w -> m r -- ^
runInsertReturningHead f t w = do
   rs <- runInsertReturningMany f t w
   case rs of
     [r] -> return r
     []  -> throwM $ ErrNoRows sql
     _   -> throwM $ ErrTooManyRows (length rs) sql
  where
    OI.QueryRunner u _ _ = PP.def :: OI.QueryRunner v hs
    sql = O.arrangeInsertReturningSql u t w id

--------------------------------------------------------------------------------

-- | Like Opaleye's 'O.runUpdate', but the predicate is expected to
-- return a @('GetKol' w 'O.PGBool')@.
--
-- It is recommended that you use 'runUpdateTabla' if you are trying to update
-- a table that is an instance of 'Tabla'. The result is the same, but the
-- this function might be less convenient to use.
runUpdate
  :: (MonadIO m, MonadReader Pg.Connection m, GetKol gkb O.PGBool)
  => O.Table w r -> (r -> w) -> (r -> gkb) -> m Int64 -- ^
runUpdate t upd fil = ask >>= \conn -> liftIO $ do
    O.runUpdate conn t upd (unKol . getKol . fil)

-- | Like 'runUpdate', but specifically designed to work well with 'Tabla'.
runUpdateTabla'
  :: forall t m gkb
   . (Tabla t, MonadIO m, MonadReader Pg.Connection m, GetKol gkb O.PGBool)
  => (PgW t -> PgW t) -- ^ Upgrade current values to new values.
  -> (PgR t -> gkb)   -- ^ Whether a row should be updated.
  -> m Int64          -- ^ Number of updated rows.
runUpdateTabla' = runUpdateTabla (T::T t)

-- | Like 'runUpdateTabla'', but takes @t@ explicitely for the times when
-- it can't be inferred.
runUpdateTabla
  :: (Tabla t, MonadIO m, MonadReader Pg.Connection m, GetKol gkb O.PGBool)
  => T t -> (PgW t -> PgW t) -> (PgR t -> gkb) -> m Int64 -- ^
runUpdateTabla t upd = runUpdate (table t) (upd . update')
