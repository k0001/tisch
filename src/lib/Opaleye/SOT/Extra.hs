{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module exports some tools that are not directly related to
-- @opaleye-sot@, yet they complement it.
module Opaleye.SOT.Extra
  ( -- * Field parsers.
    pgFromFieldMap
  , pgFromFieldWrapped
  , pgFromFieldMapMay
  , pgFromFieldPrism
  , pgGuardTypeName
  , pgGuardNotNull
  , pgTextFromFieldNoTypeCheck
  , pgCharFromFieldNoTypeCheck
  ) where

import           Control.Lens
import           Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Typeable (Typeable)
import qualified Database.PostgreSQL.Simple.FromField as Pg

--------------------------------------------------------------------------------

pgFromFieldMap :: Pg.FromField a => (a -> b) -> Pg.FieldParser b
pgFromFieldMap f = \a mb -> fmap f (Pg.fromField a mb)
{-# INLINABLE pgFromFieldMap #-}

pgFromFieldWrapped
  :: (Wrapped b, Unwrapped b ~ a, Pg.FromField a) => Pg.FieldParser b
pgFromFieldWrapped = pgFromFieldMap (review _Wrapped')
{-# INLINABLE pgFromFieldWrapped #-}

pgFromFieldMapMay
  :: (Pg.FromField a, Show a, Typeable b) => (a -> Maybe b) -> Pg.FieldParser b
pgFromFieldMapMay f = \a mb -> do
    t <- Pg.fromField a mb
    case f t of
       Just x -> return x
       Nothing -> Pg.returnError Pg.ConversionFailed a (show t)
{-# INLINABLE pgFromFieldMapMay #-}

pgFromFieldPrism
  :: (Pg.FromField a, Show a, Typeable b) => Prism' a b -> Pg.FieldParser b
pgFromFieldPrism p = pgFromFieldMapMay (preview p)
{-# INLINABLE pgFromFieldPrism #-}

pgGuardTypeName :: B8.ByteString -> Pg.Field -> Pg.Conversion ()
pgGuardTypeName ty f = do
    ty' <- Pg.typname <$> Pg.typeInfo f
    when (ty' /= ty) $ Pg.returnError Pg.Incompatible f
       ("Expected " ++ show ty ++ ", got " ++ show ty')
{-# INLINABLE pgGuardTypeName #-}

pgGuardNotNull :: Pg.FieldParser B8.ByteString
pgGuardNotNull = \f mb -> case mb of
   Nothing -> Pg.returnError Pg.UnexpectedNull f ""
   Just b -> return b
{-# INLINABLE pgGuardNotNull #-}

-- | Like the 'Pg.FromField' instance for 'Text.Text' but doesn't check the
-- 'Pg.Field' type.
pgTextFromFieldNoTypeCheck :: Pg.FieldParser Text.Text
pgTextFromFieldNoTypeCheck = \f mb -> do
   b <- pgGuardNotNull f mb
   case Text.decodeUtf8' b of
      Left e -> Pg.conversionError e
      Right t -> return t
{-# INLINABLE pgTextFromFieldNoTypeCheck #-}

-- | Like the 'Pg.FromField' instance for 'Char' but doesn't check the
-- 'Pg.Field' type.
pgCharFromFieldNoTypeCheck :: Pg.FieldParser Char
pgCharFromFieldNoTypeCheck = \f mb -> do
   b <- pgGuardNotNull f mb
   case Text.decodeUtf8' b of
      Left e -> Pg.conversionError e
      Right t -> case Text.compareLength t 1 of
         EQ -> return (Text.head t)
         _  -> Pg.returnError Pg.ConversionFailed f (show t)
{-# INLINABLE pgCharFromFieldNoTypeCheck #-}
