{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This is an internal module. You are very discouraged from using it directly.
module Opaleye.SOT.Internal where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Fix (MonadFix(..))
import           Data.Data (Data)
import           Data.Kind
import           Data.Foldable
import           Data.Typeable (Typeable)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Time
import qualified Data.UUID
import           Data.Int
import           Data.Proxy (Proxy(..))
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as PP
import qualified Data.Promotion.Prelude.List as List (Map)
import           Data.Promotion.Prelude.Bool (If)
import           Data.Singletons
import           Data.Type.Equality
import           Data.Tagged
import           Data.Word
import           GHC.Exts (Constraint)
import qualified GHC.OverloadedLabels as GHC
import           GHC.Generics (Generic)
import           GHC.Float (float2Double)
import qualified GHC.TypeLits as GHC
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as OI
import qualified Opaleye.Internal.HaskellDB.PrimQuery as OI
import qualified Opaleye.Internal.PGTypes as OI
import qualified Opaleye.Internal.RunQuery as OI
import qualified Opaleye.Internal.Join as OI
import qualified Opaleye.Internal.TableMaker as OI

import qualified Opaleye.SOT.Internal.Profunctors as PP
import           Opaleye.SOT.Internal.Record (Record(RNil, RCons))
import qualified Opaleye.SOT.Internal.Record as Record
import           Opaleye.SOT.Internal.Singletons ((:&&&$$$))

-------------------------------------------------------------------------------

-- | Only 'PgPrimType' instances are allowed as indexes to @opaleye@'s
-- 'O.Column'.
--
-- You probably won't be adding new 'PgPrimType' instances yourself,
-- unless you are trying to represent a concrete PostgreSQL data type, but even
-- then you might get away with creating 'PgTyped' instances instead.
class PgPrimType (a :: k) where
  pgPrimTypeName :: proxy a -> String

instance GHC.TypeError
  ( 'GHC.Text "Invalid PgPrimType (can't be Nullable): "
       'GHC.:<>: 'GHC.ShowType (O.Nullable a)
  ) => PgPrimType (O.Nullable a) where
  pgPrimTypeName = error "impossible"

instance forall a. PgPrimType a => PgPrimType (O.PGArray a) where
  pgPrimTypeName _ = pgPrimTypeName (Proxy :: Proxy a) ++ "[]"

instance PgPrimType O.PGBool where pgPrimTypeName _ = "boolean"
instance PgPrimType O.PGBytea where pgPrimTypeName _ = "bytea"
instance PgPrimType O.PGCitext where pgPrimTypeName _ = "citext"
instance PgPrimType O.PGDate where pgPrimTypeName _ = "date"
instance PgPrimType O.PGFloat4 where pgPrimTypeName _ = "float4"
instance PgPrimType O.PGFloat8 where pgPrimTypeName _ = "float8"
instance PgPrimType O.PGInt2 where pgPrimTypeName _ = "int2"
instance PgPrimType O.PGInt4 where pgPrimTypeName _ = "int4"
instance PgPrimType O.PGInt8 where pgPrimTypeName _ = "int8"
instance PgPrimType O.PGJsonb where pgPrimTypeName _ = "jsonb"
instance PgPrimType O.PGJson where pgPrimTypeName _ = "json"
instance PgPrimType O.PGNumeric where pgPrimTypeName _ = "numeric"
instance PgPrimType O.PGText where pgPrimTypeName _ = "text"
instance PgPrimType O.PGTimestamptz where pgPrimTypeName _ = "timestamptz"
instance PgPrimType O.PGTimestamp where pgPrimTypeName _ = "timestamp"
instance PgPrimType O.PGTime where pgPrimTypeName _ = "time"
instance PgPrimType O.PGUuid where pgPrimTypeName _ = "uuid"

-- | Only 'PgTyped' instances are allowed as indexes to 'Kol' and 'Koln'.
--
-- The @'PgType' a ~ 'PgType' ('PgType' a)@ guarantees that @'PgType' a@ is a
-- fixpoint.
class (PgPrimType (PgType a), PgTyped (PgType a), PgType a ~ PgType (PgType a))
  => PgTyped (a :: k) where
  -- | @'PgType' a@ indicates the primitive PostgreSQL column type that will
  -- ultimately be used as the index to @opaleye@'s 'O.Column'. This could be
  -- @a@ itself, in the case of primitive types such as 'O.PGInt4', or it could
  -- be something else.
  --
  -- The motivation for this is the same as for “newtype wrappers” in Haskell.
  -- Say in you have this newtype in Haskell:
  --
  -- @
  -- newtype UserId = UserId 'Int32'
  -- @
  --
  -- Now 'UserId' and 'Int32' are different types, even though they share the
  -- same underlying representation 'Int32'. With 'PgTyped' you can achieve
  -- something similar:
  --
  -- @
  -- instance 'PgTyped' UserId where
  --   type 'PgType' UserId = 'O.PGInt4'
  -- @
  --
  -- With that in place, you won't be able to accidentally mistake @'Kol'
  -- 'O.PGInt4'@ for @'Kol' UserId@ values, yet you will be able to easily
  -- reuse most of the machinery available to @'Kol' 'O.PGInt4'@, except:
  --
  -- * You will need a 'O.QueryRunnerColumnDefault' instance to fetch @'Kol'
  --   UserId@ values from the database as @UserId@:
  --
  --   @
  --   instance 'O.QueryRunnerColumnDefault' 'O.PGInt4' UserId
  --   @
  --
  --   You might find 'queryRunnerColumnFromWrapped' useful for simple cases
  --   like @UserId@.
  --
  --   Notice that the instance mentions 'O.PGInt4' directly, not our 'PgTyped'
  --   @UserId@. This is fine, not much would be gained by making the difference
  --   between them at this point, as you still need to do the 'Pg.FromRow'
  --   parsing and any inconsistencies will be uncovered there.
  --
  -- * If you want to reuse the exising 'Num' or 'Fractional' instances for
  --   @'Kol' 'O.PGInt4', you will need to explicitely ask for it by
  --   instantiating @'PgNum' UserId@ and/or @'PgFractional' UserId@. Most
  --   likely you won't need this for cases such as @UserId@, since you
  --   shouldn't be doing arithmetic with user identifiers anyway.
  type PgType a :: Type

instance PgTyped O.PGBool where type PgType O.PGBool = O.PGBool
instance PgTyped O.PGBytea where type PgType O.PGBytea = O.PGBytea
instance PgTyped O.PGCitext where type PgType O.PGCitext = O.PGCitext
instance PgTyped O.PGDate where type PgType O.PGDate = O.PGDate
instance PgTyped O.PGFloat4 where type PgType O.PGFloat4 = O.PGFloat4
instance PgTyped O.PGFloat8 where type PgType O.PGFloat8 = O.PGFloat8
instance PgTyped O.PGInt2 where type PgType O.PGInt2 = O.PGInt2
instance PgTyped O.PGInt4 where type PgType O.PGInt4 = O.PGInt4
instance PgTyped O.PGInt8 where type PgType O.PGInt8 = O.PGInt8
instance PgTyped O.PGJsonb where type PgType O.PGJsonb = O.PGJsonb
instance PgTyped O.PGJson where type PgType O.PGJson = O.PGJson
instance PgTyped O.PGNumeric where type PgType O.PGNumeric = O.PGNumeric
instance PgTyped O.PGText where type PgType O.PGText = O.PGText
instance PgTyped O.PGTimestamptz where type PgType O.PGTimestamptz = O.PGTimestamptz
instance PgTyped O.PGTimestamp where type PgType O.PGTimestamp = O.PGTimestamp
instance PgTyped O.PGTime where type PgType O.PGTime = O.PGTime
instance PgTyped O.PGUuid where type PgType O.PGUuid = O.PGUuid
instance PgTyped a => PgTyped (O.PGArray a) where type PgType (O.PGArray a) = O.PGArray (PgType a)

-------------------------------------------------------------------------------

-- | Like @opaleye@'s 'OI.PGOrd', but applies to a 'PgTyped'.
class (PgTyped a, O.PGOrd (PgType a)) => PgOrd (a :: k)
instance (PgTyped a, O.PGOrd (PgType a)) => PgOrd a

-------------------------------------------------------------------------------

-- | A @'PgNum' a@ instance gives you a @'Num' ('Kol' a)@ instance for free.
class (PgTyped a, OI.PGNum (PgType a)) => PgNum (a :: k)

instance PgNum O.PGInt2
instance PgNum O.PGInt4
instance PgNum O.PGInt8
instance PgNum O.PGFloat4
instance PgNum O.PGFloat8

instance (PgNum a, Num (O.Column (PgType a))) => Num (Kol a) where
  fromInteger = Kol . fromInteger
  (*) = liftKol2 (*)
  (+) = liftKol2 (+)
  (-) = liftKol2 (-)
  abs = liftKol1 abs
  negate = liftKol1 negate
  signum = liftKol1 signum

instance (PgTyped a, Num (Kol a)) => Num (Koln a) where
  fromInteger = fromKol . fromInteger
  (*) kna knb = bindKoln kna (\ka -> bindKoln knb (\kb -> fromKol (ka * kb)))
  (+) kna knb = bindKoln kna (\ka -> bindKoln knb (\kb -> fromKol (ka + kb)))
  (-) kna knb = bindKoln kna (\ka -> bindKoln knb (\kb -> fromKol (ka - kb)))
  abs = mapKoln abs
  negate = mapKoln negate
  signum = mapKoln signum

-------------------------------------------------------------------------------

-- | A @'PgFractional' a@ instance gives you a @'Fractional' ('Kol' a)@ instance
-- for free.
class (PgTyped a, PgNum a, OI.PGFractional (PgType a)) => PgFractional (a :: k)

instance PgFractional O.PGFloat4
instance PgFractional O.PGFloat8

instance
    ( PgTyped a, PgFractional a
    , Fractional (O.Column (PgType a))
    , Num (O.Column (PgType a))
    ) => Fractional (Kol a) where
  fromRational = Kol . fromRational
  (/) = liftKol2 (/)

instance (PgTyped a, Num (Koln a), Fractional (Kol a)) => Fractional (Koln a) where
  fromRational = fromKol . fromRational
  (/) kna knb = bindKoln kna (\ka -> bindKoln knb (\kb -> fromKol (ka / kb)))

-------------------------------------------------------------------------------

-- | Like @opaleye@'s @('O.Column' x)@, but with @x@ guaranteed to be not
-- 'O.Nullable'. If you need to have a 'O.Nullable' column type, use 'Koln'
-- instead.
--
-- Build using 'kol' or 'Kol'.
--
-- /Notice that 'Kol' is very different from 'Col': 'Col' is used to describe/
-- /the properties of a column at compile time. 'Kol' is used at runtime/
-- /for manipulating with values stored in columns./
--
-- We do not use @opaleye@'s @'O.Column' x@, instead we use @'Kol' y@ where @x ~
-- 'PgType' y@. This is where we drift a bit appart from Opaleye. See
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/97
data Kol (a :: k) = PgTyped a => Kol { unKol :: O.Column (PgType a) }

deriving instance (PgTyped a, Show (O.Column (PgType a))) => Show (Kol a)

-- | Like 'unsaferCoerceKol', but with a guarantee that the underlying 'PgType's
-- are the same.
unsafeCoerceKol :: (PgTyped a, PgTyped b, PgType a ~ PgType b) => Kol a -> Kol b
unsafeCoerceKol = unsaferCoerceKol

-- | Like 'unsaferCoerceExplicitKol', but without any explicit mention of the
-- target type.
unsaferCoerceKol :: (PgTyped a, PgTyped b) => Kol a -> Kol b
unsaferCoerceKol = liftKol1 O.unsafeCoerceColumn

-- | Unsafely but explicitely coerce one column type to another one by
-- appending a target type name like @::int4@ to the PostgreSQL value, even if
-- it is not guaranteed that the @int4@ type can properly hold the value. Use
-- 'unsafeCoerceKol' if you don't want the explicit casting behavior.
unsaferCoerceExplicitKol :: forall a b. (PgTyped a, PgTyped b) => Kol a -> Kol b
unsaferCoerceExplicitKol =
  liftKol1 (O.unsafeCast (pgPrimTypeName (Proxy @(PgType b))))

-- | Converts an unary function on Opaleye's 'O.Column' to an unary function
-- taking any of 'Kol' and 'Koln' as argument, with the result type fully
-- determined by it.
liftKol1
  :: (PgTyped a, PgTyped b)
  => (O.Column (PgType a) -> O.Column (PgType b))
  -> (Kol a -> Kol b) -- ^
liftKol1 f = Kol . f . unKol

-- | Converts a binary function on Opaleye's 'O.Column's to an binary function
-- taking any of 'Kol' and 'Koln' as arguments, with the result type fully
-- determined by them.
liftKol2
  :: (PgTyped a, PgTyped b, PgTyped c)
  => (O.Column (PgType a) -> O.Column (PgType b) -> O.Column (PgType c))
  -> (Kol a -> Kol b -> Kol c)
liftKol2 f = \ka kb -> Kol (f (unKol ka) (unKol kb))

-- | Converts a ternary function on Opaleye's 'O.Column's to an ternary function
-- on 'Kol'.
--
-- /Hint/: You can further compose the result of this function with 'op3'
-- to widen the range of accepted argument types.
liftKol3
  :: (PgTyped a, PgTyped b, PgTyped c, PgTyped d)
  => (O.Column (PgType a) -> O.Column (PgType b) -> O.Column (PgType c) -> O.Column (PgType d))
  -> (Kol a -> Kol b -> Kol c -> Kol d)
liftKol3 f = \ka kb kc -> Kol (f (unKol ka) (unKol kb) (unKol kc))

instance
    ( Profunctor p, PP.Default p (O.Column (PgType a)) (O.Column b)
    ) => PP.Default p (Kol a) (O.Column b) where
  def = P.lmap unKol PP.def

instance forall p a b.
    ( PgTyped b, Profunctor p, PP.Default p (O.Column a) (O.Column (PgType b))
    ) => PP.Default p (O.Column a) (Kol b) where
  def = P.rmap Kol (PP.def :: p (O.Column a) (O.Column (PgType b)))

instance forall p a b.
    ( PgTyped b
    , Profunctor p, PP.Default p (O.Column (PgType a)) (O.Column (PgType b))
    ) => PP.Default p (Kol a) (Kol b) where
  def = P.dimap unKol Kol (PP.def :: p (O.Column (PgType a)) (O.Column (PgType b)))

instance
    ( PP.Default O.QueryRunner (O.Column (PgType a)) b
    ) => PP.Default O.QueryRunner (Kol a) b where
  def = P.lmap unKol PP.def

-- | Convert a Haskell value to its 'Kol' representation.
--
-- A a default implementation of 'kol' is given for 'Wrapped' instances (see
-- 'kolWrapped').
--
-- Notice that as described in the documentation for 'PgTyped', @b@ here could
-- be any 'PgTyped'. For example, following the @UserId@ example mentioned in
-- the documentation for 'PgTyped', you can have:
--
-- @
-- instance 'ToKol' @UserId@ @UserId@
-- @
--
-- And that instance would give you @'kol' :: @UserId@ -> 'Kol' @UserId@@.
-- However, for that to work, a 'ToKol' instance relating the @UserId@ on the
-- left (the Haskell value) to the primitive type of the @UserId@ on the right
-- (i.e., 'PgType' @UserId@ ~ 'O.PGInt4') must also exist:
--
-- @
-- instance 'ToKol' @UserId@ 'O.PGInt4'
-- @
--
-- If you ensure that @UserId@ is an instance of 'Wrapped', then that's all you
-- need to say: both instances will get default implementations of 'kol'.
-- Otherwise, you'll need to implement 'kol' yourself.
--
-- Law 1 - @b@ is only nominal:
--
-- @
-- ('upcastKol' ('kol' (x :: a) :: 'Kol' b) :: Kol ('PgType' b))
--    '==' ('kol' (x :: a) :: 'Kol' ('PgType' b))
-- @
class (PgTyped b, ToKol a (PgType b)) => ToKol (a :: Type) (b :: kb) where
  -- | Convert a constant Haskell value (say, a 'Bool') to its equivalent
  -- PostgreSQL representation (for example, to a @'Kol' 'O.PGBool'@, or any
  -- other compatible 'PgTyped').
  --
  -- Some example simplified types:
  --
  -- @
  -- 'kol' :: 'Bool' -> 'Kol' 'O.PGBool'
  -- 'kol' :: 'Int32' -> 'Kol' 'O.PGInt4'
  -- @
  kol :: a -> Kol (b :: kb)
  default kol :: (Wrapped a, PgTyped b, ToKol (Unwrapped a) (PgType b)) => a -> Kol b
  kol = -- Downcasting here is safe due to the Law 1 of 'ToKol'.
        unsafeDowncastKol . kol . view _Wrapped'

instance (ToKol a b) => ToKol (Tagged t a) b
instance ToKol String O.PGText where kol = Kol . O.pgString
instance ToKol Data.Text.Text O.PGText where kol = Kol . O.pgStrictText
instance ToKol Data.Text.Lazy.Text O.PGText where kol = Kol . O.pgLazyText
instance ToKol Char O.PGText where kol = Kol . O.pgString . (:[])
instance ToKol Bool O.PGBool where kol = Kol . O.pgBool
instance ToKol Word8 O.PGInt2 where kol = Kol . pgInt2 . fromIntegral
instance ToKol Word8 O.PGInt4 where kol = Kol . O.pgInt4 . fromIntegral
instance ToKol Word8 O.PGInt8 where kol = Kol . O.pgInt8 . fromIntegral
instance ToKol Word16 O.PGInt4 where kol = Kol . O.pgInt4 . fromIntegral
instance ToKol Word16 O.PGInt8 where kol = Kol . O.pgInt8 . fromIntegral
instance ToKol Word32 O.PGInt8 where kol = Kol . O.pgInt8 . fromIntegral
instance ToKol Int8 O.PGInt2 where kol = Kol . pgInt2 . fromIntegral
instance ToKol Int8 O.PGInt4 where kol = Kol . O.pgInt4 . fromIntegral
instance ToKol Int8 O.PGInt8 where kol = Kol . O.pgInt8 . fromIntegral
instance ToKol Int16 O.PGInt2 where kol = Kol . pgInt2 . fromIntegral
instance ToKol Int16 O.PGInt4 where kol = Kol . O.pgInt4 . fromIntegral
instance ToKol Int16 O.PGInt8 where kol = Kol . O.pgInt8 . fromIntegral
instance ToKol Int32 O.PGInt4 where kol = Kol . O.pgInt4 . fromIntegral
instance ToKol Int32 O.PGInt8 where kol = Kol . O.pgInt8 . fromIntegral
instance ToKol Int64 O.PGInt8 where kol = Kol . O.pgInt8
instance ToKol Float O.PGFloat4 where kol = Kol . pgFloat4
instance ToKol Float O.PGFloat8 where kol = Kol . pgFloat8
instance ToKol Double O.PGFloat8 where kol = Kol . O.pgDouble
instance ToKol Data.ByteString.ByteString O.PGBytea where kol = Kol . O.pgStrictByteString
instance ToKol Data.ByteString.Lazy.ByteString O.PGBytea where kol = Kol . O.pgLazyByteString
instance ToKol Data.Time.UTCTime O.PGTimestamptz where kol = Kol . O.pgUTCTime
instance ToKol Data.Time.LocalTime O.PGTimestamp where kol = Kol . O.pgLocalTime
instance ToKol Data.Time.TimeOfDay O.PGTime where kol = Kol . O.pgTimeOfDay
instance ToKol Data.Time.Day O.PGDate where kol = Kol . O.pgDay
instance ToKol Data.UUID.UUID O.PGUuid where kol = Kol . O.pgUUID
instance ToKol (Data.CaseInsensitive.CI String) O.PGCitext where kol = Kol . O.unsafeCoerceColumn . O.pgString . Data.CaseInsensitive.original
instance ToKol (Data.CaseInsensitive.CI Data.Text.Text) O.PGCitext where kol = Kol . O.pgCiStrictText
instance ToKol (Data.CaseInsensitive.CI Data.Text.Lazy.Text) O.PGCitext where kol = Kol . O.pgCiLazyText
instance ToKol Aeson.Value O.PGJson where kol = Kol . O.pgLazyJSON . Aeson.encode
instance ToKol Aeson.Value O.PGJsonb where kol = Kol . O.pgLazyJSONB . Aeson.encode

instance forall a b. ToKol a b => ToKol [a] (O.PGArray b) where
  kol = kolArray . map (kol :: a -> Kol b)
--
-- | Build a @'Kol' ('O.PGArray' x)@ from any 'Foldable'.
--
-- The return type is not fixed to @'Kol' ('O.PGArray' x)@ so that you can
-- easily use 'kolArray' as part of the implementation for 'kol' (see instance
-- @'ToKol' [a] ('O.PGArray' p)@ as an example of this).
kolArray
 :: forall f a as
 .  (Foldable f, PgTyped as, PgType as ~ O.PGArray (PgType a))
 => f (Kol a) -> Kol as
kolArray xs = Kol $ O.unsafeCast
   (pgPrimTypeName (Proxy :: Proxy (O.PGArray (PgType a))))
   (OI.Column (OI.ArrayExpr (map (OI.unColumn . unKol) (toList xs))))

---
instance Monoid (Kol O.PGText) where
  mempty = kol ""
  mappend = liftKol2 (OI.binOp OI.OpCat)

instance Monoid (Kol O.PGCitext) where
  mempty = kol (Data.CaseInsensitive.mk "")
  mappend ka kb = unsaferCoerceKol
    (mappend (unsaferCoerceKol ka :: Kol O.PGText)
             (unsaferCoerceKol kb :: Kol O.PGText))

instance Monoid (Kol O.PGBytea) where
  mempty = kol Data.ByteString.empty
  mappend = liftKol2 (OI.binOp OI.OpCat)

---
-- | Like @opaleye@'s @'O.Column' ('O.Nullable' x)@, but with @x@ guaranteed
-- to be not-'O.Nullable'.
--
-- Think of @'Koln' a@ as @'Maybe' ('Kol' a)@, with 'nul' being analogous to
-- the 'Nothing' constructor and 'koln' being analogous to the 'Just'
-- constructor.
--
-- Build safely using 'nul', 'koln', 'fromKol' or 'Koln'.
--
-- /Notice that 'Koln' is very different from 'Col': 'Col' is used to describe/
-- /the properties of a column at compile time. 'Koln' is used at runtime/
-- /for manipulating with values stored in columns./
--
-- We do not use @opaleye@'s @'O.Column' ('O.Nullable' x)@, instead we use
-- @'Koln' y@ where @x ~ 'PgType' y@. This is where we drift a bit appart from
-- Opaleye. See https://github.com/tomjaguarpaw/haskell-opaleye/issues/97
data Koln (a :: k) = PgTyped a => Koln { unKoln :: O.Column (O.Nullable (PgType a)) }

deriving instance Show (O.Column (O.Nullable a)) => Show (Koln a)

-- | Build a 'Koln' from a Haskell term. This is like the 'Just' constructor for
-- 'Maybe'
koln :: (ToKol a b) => a -> Koln b
koln = fromKol . kol

-- | PostgreSQL's @NULL@ value. This is like the 'Nothing' constructor for
-- 'Maybe'
nul :: PgTyped a => Koln a
nul = Koln O.null

-- | Convert a 'Kol' to a 'Koln'.
fromKol :: PgTyped a => Kol a -> Koln a
fromKol = Koln . O.toNullable . unKol

-- | Convert a 'Koln' to a 'Kol'.
--
-- This function behaves as 'Data.Maybe.fromMaybe': If @'Koln' a@ is @NULL@,
-- then return the first argument, otherwise it returns the @'Kol' a@ underlying
-- the given @'Koln' a@.
fromKoln :: PgTyped a => Kol a -> Koln a -> Kol a
fromKoln ka0 na = Kol (O.matchNullable (unKol ka0) id (unKoln na))

-- | Like 'fmap' for 'Maybe'.
--
-- Apply the given function to the underlying @('Kol' a)@ only as long as the
-- given @('Koln' a)@ is not @NULL@, otherwise, evaluates to @NULL@.
mapKoln :: (PgTyped a, PgTyped b) => (Kol a -> Kol b) -> Koln a -> Koln b
mapKoln f kna = bindKoln kna (fromKol . f)

-- | 'mapKoln' with the arguments flipped.
forKoln :: (PgTyped a, PgTyped b) => Koln a -> (Kol a -> Kol b) -> Koln b
forKoln kna f = mapKoln f kna

-- | Monadic bind like the one for 'Maybe'.
--
-- Apply the given function to the underlying @('Kol' a)@ only as long as the
-- given @('Koln' a)@ is not @NULL@, otherwise, evaluates to @NULL@.
bindKoln :: (PgTyped a, PgTyped b) => Koln a -> (Kol a -> Koln b) -> Koln b
bindKoln kna f = Koln $
  O.matchNullable O.null (unKoln . f . Kol) (unKoln kna)

-- | Like @('<|>') :: 'Maybe' a -> 'Maybe' a -> 'Maybe' a@.
--
-- Evaluates to the first argument if it is not @NULL@, otherwise
-- evaluates to the second argument.
altKoln :: PgTyped a => Koln a -> Koln a -> Koln a
altKoln kna0 kna1 = Koln $
  O.matchNullable (unKoln kna1) O.toNullable (unKoln kna0)

-- | OVERLAPPABLE.
instance {-# OVERLAPPABLE #-} forall p x a.
    ( P.Profunctor p, PgTyped a
    , PP.Default p x (O.Column (O.Nullable (PgType a)))
    ) => PP.Default p x (Koln a) where
  def = P.rmap Koln (PP.def :: p x (O.Column (O.Nullable (PgType a))))
  {-# INLINE def #-}

-- | OVERLAPPABLE.
instance {-# OVERLAPPABLE #-}
    ( P.Profunctor p
    , PP.Default p (O.Column (O.Nullable (PgType a))) x
    ) => PP.Default p (Koln a) x where
  def = P.lmap unKoln PP.def
  {-# INLINE def #-}

instance
    ( P.Profunctor p, PgTyped b
    , PP.Default p (O.Column (O.Nullable (PgType a))) (O.Column (O.Nullable (PgType b)))
    ) => PP.Default p (Koln a) (Koln b) where
  def = P.dimap unKoln Koln (PP.def :: p (O.Column (O.Nullable (PgType a)))
                                         (O.Column (O.Nullable (PgType b))))
  {-# INLINE def #-}

-- | OVERLAPPABLE.
instance {-# OVERLAPPABLE #-}
    ( O.QueryRunnerColumnDefault pg hs
    ) => O.QueryRunnerColumnDefault pg (Maybe hs) where
  queryRunnerColumnDefault = OI.QueryRunnerColumn u (fmap (fmap (fmap Just)) fp)
    where OI.QueryRunnerColumn u fp = O.queryRunnerColumnDefault

---
instance (PgTyped a, Monoid (Kol a)) => Monoid (Koln a) where
  mempty = fromKol mempty
  mappend = \na nb ->
    bindKoln na (\ka -> bindKoln nb (\kb -> fromKol (mappend ka kb)))

-------------------------------------------------------------------------------

-- | @'CastKol' a b@ says that @'Kol' a@ can be safely cast to @'Kol' b@
-- using 'castKol'.
--
-- Notice that an explicit cast will be performed on the PostgreSQL side. For
-- example, using @'castKol' :: 'Kol' 'O.PGUuid' -> 'Kol' 'O.PGText'@ will
-- explicitely add @::text@ to the value in an @uuid@ column. This allows for
-- much more interesting and predictable conversions between different types
-- compared to 'unsafeCoerceKol'.
class (PgTyped a, PgTyped b) => CastKol (a :: ka) (b :: kb) where

instance CastKol O.PGCitext O.PGText
instance CastKol O.PGText O.PGCitext
instance CastKol O.PGUuid O.PGText
instance CastKol O.PGUuid O.PGCitext
instance CastKol O.PGInt2 O.PGText
instance CastKol O.PGInt2 O.PGCitext
instance CastKol O.PGInt2 O.PGInt4
instance CastKol O.PGInt2 O.PGInt8
instance CastKol O.PGInt4 O.PGText
instance CastKol O.PGInt4 O.PGCitext
instance CastKol O.PGInt4 O.PGInt8
instance CastKol O.PGInt8 O.PGText
instance CastKol O.PGInt8 O.PGCitext

-- Shooting yourself in the foot? I will help you.

type family TypeErrorRange a b :: Constraint where
  TypeErrorRange a b = GHC.TypeError
    ('GHC.Text "If really want to cast " 'GHC.:<>: 'GHC.ShowType a 'GHC.:<>:
     'GHC.Text " to " 'GHC.:<>: 'GHC.ShowType b 'GHC.:<>:
     'GHC.Text " then use 'unsafeCastKol'." 'GHC.:$$:
     'GHC.Text "You will get a runtime error if the " 'GHC.:<>: 'GHC.ShowType a
     'GHC.:<>: 'GHC.Text " value is out of the range of " 'GHC.:<>: 'GHC.ShowType b)
instance TypeErrorRange O.PGInt4 O.PGInt2 => CastKol O.PGInt4 O.PGInt2
instance TypeErrorRange O.PGInt8 O.PGInt2 => CastKol O.PGInt8 O.PGInt2
instance TypeErrorRange O.PGInt8 O.PGInt4 => CastKol O.PGInt8 O.PGInt4

type family TypeErrorTimeCasting a b :: Constraint where
  TypeErrorTimeCasting a b = GHC.TypeError
    ('GHC.Text "Do not cast " 'GHC.:<>: 'GHC.ShowType a 'GHC.:<>:
     'GHC.Text " to " 'GHC.:<>: 'GHC.ShowType b 'GHC.:$$:
     'GHC.Text "It probably doesn't do what you think it does." 'GHC.:$$:
     'GHC.Text "Read section 8.5 of the PostgreSQL documentation instead.")
instance TypeErrorTimeCasting O.PGDate O.PGTimestamp => CastKol O.PGDate O.PGTimestamp
instance TypeErrorTimeCasting O.PGDate O.PGTimestamptz => CastKol O.PGDate O.PGTimestamptz
instance TypeErrorTimeCasting O.PGTimestamp O.PGDate => CastKol O.PGTimestamp O.PGDate
instance TypeErrorTimeCasting O.PGTimestamp O.PGTime => CastKol O.PGTimestamp O.PGTime
instance TypeErrorTimeCasting O.PGTimestamp O.PGTimestamptz => CastKol O.PGTimestamp O.PGTimestamptz
instance TypeErrorTimeCasting O.PGTimestamptz O.PGDate => CastKol O.PGTimestamptz O.PGDate
instance TypeErrorTimeCasting O.PGTimestamptz O.PGTime => CastKol O.PGTimestamptz O.PGTime
instance TypeErrorTimeCasting O.PGTimestamptz O.PGTimestamp => CastKol O.PGTimestamptz O.PGTimestamp

-- | Safely and explicitely cast one column type to another one. See 'CastKol'.
castKol :: CastKol a b => Kol a -> Kol b
castKol = unsaferCoerceExplicitKol

-- | Safe upcasting.
upcastKol :: PgTyped a => Kol a -> Kol (PgType a)
upcastKol = unsafeCoerceKol

-- | Unsafe downcasting.
unsafeDowncastKol :: PgTyped a => Kol (PgType a) -> Kol a
unsafeDowncastKol = unsafeCoerceKol

-------------------------------------------------------------------------------

-- | Whether to read a plain value or possibly @NULL@.
data RN = R  -- ^ Read plain value.
        | RN -- ^ Possibly read @NULL@.

-- | Whether to write a specific value or possibly @DEFAULT@.
data WD = W  -- ^ Write a specific value.
        | WD -- ^ Possibly write @DEFAULT@. See 'WDef'.
--- | Whether to write @DEFAULT@ or a specific value when writing to a column.

--------------------------------------------------------------------------------

-- | Whether to write a @DEFAUT@ value or a specific value into a database column.
--
-- 'WDef' is isomorphic to 'Maybe'. It exists mainly to avoid accidentally
-- mixing the two of them together.
data WDef a
  = WDef   -- ^ Write @DEFAULT@.
  | WVal a -- ^ Write the specified value.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable,
            Data, Generic, Typeable)

-- | Case analysis for 'WDef'.
--
-- Like 'maybe', evaluates to the first argument if 'WDef',
-- otherwise applies the given function to the @a@ in 'WVal'.
wdef :: b -> (a -> b) -> WDef a -> b
wdef b f = \w -> case w of { WDef -> b; WVal a -> f a }
{-# INLINE wdef #-}

instance Applicative WDef where
  pure = WVal
  {-# INLINE pure #-}
  (<*>) (WVal f) (WVal a) = WVal (f a)
  (<*>) _        _        = WDef
  {-# INLINE (<*>) #-}

instance Alternative WDef where
  empty = WDef
  {-# INLINE empty #-}
  (<|>) WDef wb = wb
  (<|>) wa   _  = wa
  {-# INLINE (<|>) #-}

instance Monad WDef where
  return = pure
  {-# INLINE return #-}
  (>>=) (WVal a) k = k a
  (>>=) _        _ = WDef
  {-# INLINE (>>=) #-}

instance MonadPlus WDef where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadFix WDef where
  mfix f = let a = f (unWVal a) in a
    where unWVal (WVal x) = x
          unWVal WDef     = error "mfix WDef: WDef"

-- | Like @'Maybe' a@.
instance Aeson.FromJSON a => Aeson.FromJSON (WDef a) where
  parseJSON v = fmap (maybe WDef WVal) (Aeson.parseJSON v)
  {-# INLINE parseJSON #-}

-- | Like @'Maybe' a@.
instance Aeson.ToJSON a => Aeson.ToJSON (WDef a) where
  toJSON = Aeson.toJSON . wdef Nothing Just
  {-# INLINE toJSON #-}
  toEncoding = Aeson.toEncoding . wdef Nothing Just
  {-# INLINE toEncoding #-}

--------------------------------------------------------------------------------

-- | Column description.
--
-- This is only used as a promoted datatype expected to have kind
-- @'Col' 'Symbol' 'WD' 'RN' 'Type' 'Type'@.
--
-- * @name@: Column name.
--
-- * @wd@: Whether @DEFAULT@ can be written to this column ('WD') or not ('W').
--
-- * @rn@: Whether @NULL@ might be read from this column ('RN') or not ('R').
--
-- * @pgType@: Type of the column value used in Opaleye queries as index to
--   'Kol' or 'Koln'. This must be an instance of 'PgTyped'.
--
-- * @hsType@: Type of the column value used in Haskell outside Opaleye
--   queries. Hint: don't use something like @'Maybe' 'Bool'@ here if you
--   want to indicate that this is an optional 'Bool' column. Instead, use
--   'Int' here and 'RN' in the @rn@ field.
--
-- /Notice that 'Col' is very different from 'Kol' and 'Koln': 'Kol' and 'Koln'/
-- /are used at runtime for manipulating values stored in columns, 'Col' is used/
-- /to describe the properties of a column at compile time./
data Col name wd rn pgType hsType
   = Col name wd rn pgType hsType

--

type family Col_Name (col :: Col Symbol WD RN Type Type) :: Symbol where
  Col_Name ('Col n w r p h) = n
data Col_NameSym0 (col :: TyFun (Col Symbol WD RN Type Type) Symbol)
type instance Apply Col_NameSym0 col = Col_Name col

type family Col_PgType (col :: Col Symbol WD RN Type Type) :: Type where
  Col_PgType ('Col n w r p h) = p
data Col_PgTypeSym0 (col :: TyFun (Col Symbol WD RN Type Type) Type)
type instance Apply Col_PgTypeSym0 col = Col_PgType col

type family Col_PgR (col :: Col Symbol WD RN Type Type) :: Type where
  Col_PgR ('Col n w 'R  p h) = Kol p
  Col_PgR ('Col n w 'RN p h) = Koln p
data Col_PgRSym0 (col :: TyFun (Col Symbol WD RN Type Type) Type)
type instance Apply Col_PgRSym0 col = Col_PgR col

type family Col_PgRN (col :: Col Symbol WD RN Type Type) :: Type where
  Col_PgRN ('Col n w r p h) = Koln p
data Col_PgRNSym0 (col :: TyFun (Col Symbol WD RN Type Type) Type)
type instance Apply Col_PgRNSym0 col = Col_PgRN col

type family Col_PgW (col :: Col Symbol WD RN Type Type) :: Type where
  Col_PgW ('Col n 'W  r p h) = Col_PgR ('Col n 'W r p h)
  Col_PgW ('Col n 'WD r p h) = WDef (Col_PgR ('Col n 'WD r p h))
data Col_PgWSym0 (col :: TyFun (Col Symbol WD RN Type Type) Type)
type instance Apply Col_PgWSym0 col = Col_PgW col

type family Col_HsR (col :: Col Symbol WD RN Type Type) :: Type where
  Col_HsR ('Col n w 'R  p h) = h
  Col_HsR ('Col n w 'RN p h) = Maybe h
data Col_HsRSym0 (col :: TyFun (Col Symbol WD RN Type Type) Type)
type instance Apply Col_HsRSym0 col = Col_HsR col

type family Col_HsI (col :: Col Symbol WD RN Type Type) :: Type where
  Col_HsI ('Col n 'W  r p h) = Col_HsR ('Col n 'W r p h)
  Col_HsI ('Col n 'WD r p h) = WDef (Col_HsR ('Col n 'WD r p h))
data Col_HsISym0 (col :: TyFun (Col Symbol WD RN Type Type) Type)
type instance Apply Col_HsISym0 col = Col_HsI col

--------------------------------------------------------------------------------
-- Note: By using newtype wrappers, instead of just type synonyms for 'Record',
-- we can provide nicer error messages to the users at the small cost of a bit
-- more complicated implementation for us (e.g., the implementation of `col`
-- could be generalized otherwise).

-- | Expected output type for 'O.runQuery' on a @('PgR' t)@.
--
-- Important: If you are expecting a @('PgR' t)@ on the right side
-- of a 'O.leftJoin', you will need to use @('Maybe' ('PgR' t))@.
--
-- Mnemonic: Haskell Read.
newtype HsR t = HsR { unHsR :: Record (Cols_NamedHsR t) }
type Cols_NamedHsR t = List.Map (Col_NameSym0 :&&&$$$ Col_HsRSym0) (Cols t)

deriving instance Eq (Record (Cols_NamedHsR t)) => Eq (HsR t)
deriving instance Ord (Record (Cols_NamedHsR t)) => Ord (HsR t)
deriving instance Show (Record (Cols_NamedHsR t)) => Show (HsR t)
deriving instance Generic (Record (Cols_NamedHsR t)) => Generic (HsR t)
instance (Aeson.FromJSON (Record (Cols_NamedHsR t)), Generic (HsR t)) => Aeson.FromJSON (HsR t)
instance (Aeson.ToJSON (Record (Cols_NamedHsR t)), Generic (HsR t)) => Aeson.ToJSON (HsR t)

instance Profunctor p => PP.Default p (HsR t) (HsR t) where
  def = P.rmap id PP.def
  {-# INLINE def #-}

---
-- | @'HsI' t@ is the Haskell representation of Haskell values to be inserted to
-- the database, as taken by "Opaleye.SOT.Run.runInsertTabla".
--
-- An @'HsI' t@ can always be converted to a @'PgW' t@ using 'pgWfromHsI', in
-- case you need that for with the more general "Opaleye.SOT.Run.runInsert".
--
-- Mnemonic: Haskell Insert.
newtype HsI t = HsI { unHsI :: Record (Cols_NamedHsI t) }
type Cols_NamedHsI t = List.Map (Col_NameSym0 :&&&$$$ Col_HsISym0) (Cols t)

deriving instance Eq (Record (Cols_NamedHsI t)) => Eq (HsI t)
deriving instance Ord (Record (Cols_NamedHsI t)) => Ord (HsI t)
deriving instance Show (Record (Cols_NamedHsI t)) => Show (HsI t)
deriving instance Generic (Record (Cols_NamedHsI t)) => Generic (HsI t)
instance (Aeson.FromJSON (Record (Cols_NamedHsI t)), Generic (HsI t)) => Aeson.FromJSON (HsI t)
instance (Aeson.ToJSON (Record (Cols_NamedHsI t)), Generic (HsI t)) => Aeson.ToJSON (HsI t)

instance Profunctor p => PP.Default p (HsI t) (HsI t) where
  def = P.rmap id PP.def
  {-# INLINE def #-}


---
-- | Output type of @'queryTabla' ('T' :: 'T' t)@.
--
-- Mnemonic: PostGresql Read.
newtype PgR t = PgR { unPgR :: Record (Cols_NamedPgR t) }
type Cols_NamedPgR t = List.Map (Col_NameSym0 :&&&$$$ Col_PgRSym0) (Cols t)

instance Profunctor p => PP.Default p (PgR t) (PgR t) where
  def = P.rmap id PP.def
  {-# INLINE def #-}

instance
  ( Profunctor p
  , PP.Default p (Record (Cols_NamedPgR t)) (Record (Cols_NamedHsR t))
  ) => PP.Default p (PgR t) (HsR t) where
  def = P.dimap unPgR HsR PP.def
  {-# INLINE def #-}

instance
  ( Profunctor p
  , PP.Default p (Record (Cols_NamedPgR t)) (Record (Cols_NamedPgRN t))
  ) => PP.Default p (PgR t) (PgRN t) where
  def = P.dimap unPgR PgRN PP.def
  {-# INLINE def #-}

---
-- | Like @('PgRN' t)@ but every field is 'Koln', as in the
-- output type of the right hand side of a 'O.leftJoin' with @'('table' t)@.
--
-- Mnemonic: PostGresql Read Nulls.
newtype PgRN t = PgRN { unPgRN :: Record (Cols_NamedPgRN t) }
type Cols_NamedPgRN t = List.Map (Col_NameSym0 :&&&$$$ Col_PgRNSym0) (Cols t)

instance Profunctor p => PP.Default p (PgRN t) (PgRN t) where
  def = P.rmap id PP.def
  {-# INLINE def #-}

instance
  ( Profunctor p
  , PP.Default p (Record (Cols_NamedPgRN t)) (Maybe (Record (Cols_NamedHsR t)))
  ) => PP.Default p (PgRN t) (Maybe (HsR t)) where
  def = P.dimap unPgRN (fmap HsR) PP.def
  {-# INLINE def #-}

---
-- | Representation of PostgreSQL values to be written to the database. This
-- type can be used as input for "Opaleye.SOT.Run.runInsert" and similar.
--
-- An @'HsI' t@ can always be converted to a @'PgW' t@ using 'pgWfromHsI', in
--
-- Mnemonic: PostGresql Write.
newtype PgW t = PgW { unPgW :: Record (Cols_NamedPgW t) }
type Cols_NamedPgW t = List.Map (Col_NameSym0 :&&&$$$ Col_PgWSym0) (Cols t)

instance Profunctor p => PP.Default p (PgW t) (PgW t) where
  def = P.rmap id PP.def
  {-# INLINE def #-}

--------------------------------------------------------------------------------

-- | All these constraints need to be satisfied by tools that work with 'Tabla'.
-- It's easier to just write all the constraints once here and make 'ITabla' a
-- superclass of 'Tabla'. Moreover, they enforce some sanity constraints on our
-- 'Tabla' so that we can get early compile time errors.
type ITabla t
  = ( All PgTyped (List.Map (Col_PgTypeSym0) (Cols t))
    , KnownSymbol (SchemaName t)
    , KnownSymbol (TableName t)
    , RDistributeColProps (Cols t)
    , Record.RMap FnPgWfromPgRField (Cols_NamedPgR t) (Cols_NamedPgW t)
    , Record.RMap FnPgWfromHsIField (Cols_NamedHsI t) (Cols_NamedPgW t)
    , PP.Default OI.ColumnMaker (PgR t) (PgR t)
    , PP.ProductProfunctorAdaptor
         O.TableProperties
         (Record (List.Map (Col_NameSym0 :&&&$$$ Col_PropsSym0) (Cols t)))
         (Record (Cols_NamedPgW t))
         (Record (Cols_NamedPgR t))
    )

-- | Tabla means table in spanish.
--
-- An instance of this class can uniquely describe a PostgreSQL table and
-- how to convert back and forth between it and its Haskell representation
-- used when writing Opaleye queries.
--
-- The @t@ type is only used as a tag for the purposes of uniquely identifying
-- this 'Tabla'.
class ITabla t => Tabla (t :: k) where
  -- | 'T' is a 'Proxy'-like type we will use to pass around @t@. The
  -- constructors for @'T' t@ are never used by @opaleye-sot@.
  --
  -- For consistency, readability, discoverability and ease of maintenance, it
  -- is recommended that you name your constructor for @'T' t@ the same as @t@.
  -- So for example, if you have @t ~ TUser@, then @data T TUser = TUser@ will
  -- allow you to use the term-level constructor @TUser@ to represent the
  -- @TUser@ type.
  data T t :: Type
  -- | Some kind of unique identifier used for telling appart the database where
  -- this table exists from other databases, so as to avoid accidentally mixing
  -- tables from different databases in queries.
  type Database t :: Type
  -- | PostgreSQL schema name where to find the table (defaults to @"public"@,
  -- PostgreSQL's default schema name).
  type SchemaName t :: Symbol
  type SchemaName t = "public"
  -- | Table name.
  type TableName t :: Symbol
  -- | Columns in this table. See the documentation for 'Col'.
  type Cols t :: [Col Symbol WD RN Type Type]

--------------------------------------------------------------------------------

-- | Helper function to safely build an @'HsI' t@.
--
-- The type of this function isn't easy to understand, but an example should
-- clarify its usage. We will asume we have a @TPerson@ which is an instance of
-- 'Table', and @Person@ datatype as follows:
--
-- @
-- data TPerson
--
-- instance 'Table' TPerson where
--   data 'T' TPerson = TPerson
--   type 'Database' TPerson = ... not important ...
--   type 'SchemaName' TPerson = ... not important ...
--   type 'TableName' TPerson = ... not important ...
--   type 'Cols' TPerson
--     = '[ ''Col' "name" 'W' 'R' 'O.PGText' 'Data.Text.Text'
--        , ''Col' "age" 'W' 'R' 'O.PGInt4' 'Int32'
--        ]
--
-- data Person = Person
--   { _personName :: 'Text'
--   , _personAge :: 'Int'
--   }
-- @
--
-- With that in place, and with the 'OverloadedLabels' GHC extension enabled,
-- we can use 'mkHsI' as follows:
--
-- @
-- personToHsI :: Person -> 'HsI' TPerson
-- personToHsI person =
--   'mkHsI' TPerson
--     ('hsi' #name (_personName person))
--     ('hsi' #age (_personAge age))
-- @
--
-- The column names must appear in the same order as they do in @'Cols'
-- TPerson@, otherwise you will get a compiler error.
--
-- As long as your column name is also a valid Haskell name, you can use the
-- 'OverloadedLabels' syntax as above to specify the name of the column.
-- However, if your column is name is more esoteric, you can just replace the
-- call to 'hsi' with 'Tagged', specifying the column name as 'Symbol' between
-- quotes:
--
-- @
-- personToHsI :: Person -> 'HsI' TPerson
-- personToHsI person =
--   'mkHsI' TPerson
--     ('Tagged' \@ "name" (_personName person))
--     ('Tagged' \@ "age" (_personAge age))
-- @
--
-- Note: Technically, you can use the 'Tagged' without specifying the column
-- name and it will work just fine. However, for maintenance purposes (i.e., in
-- case you change the order of your columns in the future), we highly recommend
-- being explicit about the column name.
--
-- Note: You are not required to use this function to build an @'HsI' t@ if
-- working with 'HsI', 'Record.RCons' and 'Record.RNil' (not exported, from
-- "Opaleye.SOT.Internal.Record") are sufficient to you, this is
-- just a convenience.
mkHsI :: T t -> MkHsI t
mkHsI _ = Record.rBuildSymbol
{-# INLINE mkHsI #-}

-- | See 'mkHsI'.
type MkHsI t = Record.RBuild' ('[] :: [(Symbol,Type)])
                              (Cols_CNamedFunArgs Col_HsISym0 (HsI t) (Cols t))
                           => (Cols_CNamedFunArgs Col_HsISym0 (HsI t) (Cols t))

-- | Just like @'Proxy' c@, but with the different name to prevent clashing the
-- 'GHC.IsLabel' instance with third party code. Used by 'hsi'.
data C (c :: Symbol) = C

-- | Used by 'hsi'.
instance GHC.IsLabel (c :: Symbol) (C c) where
  fromLabel _ = C
  {-# INLINE fromLabel #-}

-- | Helper function for building an 'HsI'. To construct a 'C' use the GHC's
-- @OverloadedLabels@ extension (e.g., @#foo == ('C' :: 'C' "foo"))@.
--
-- See 'mkHsI' for a full example.
hsi :: C (c :: Symbol) -> x -> Tagged c x
hsi _ = Tagged
{-# INLINE hsi #-}

instance Record.RBuild' axs (Record (Cols_NamedHsI t)) => Record.RBuild' axs (HsI t) where
  rBuild' raxs = HsI (Record.rBuild' raxs)
  {-# INLINE rBuild' #-}

-- | Used by 'MkHsI'.
type family Cols_CNamedFunArgs
    (f :: TyFun (Col Symbol WD RN Type Type) Type -> Type)
    (z :: Type) (cols :: [Col Symbol WD RN Type Type]) :: Type
 where
  Cols_CNamedFunArgs f z '[] = z
  Cols_CNamedFunArgs f z (x ': xs) =
    Tagged (Col_Name x) (Apply f x) -> Cols_CNamedFunArgs f z xs

--------------------------------------------------------------------------------

-- | To be used with 'Record.ApplyAB'.
data FnPgWfromHsIField = FnPgWfromHsIField
instance Record.ApplyAB FnPgWfromHsIField x x where
  applyAB _ = id
instance (ToKol a b) => Record.ApplyAB FnPgWfromHsIField a (Kol b) where
  applyAB _ = kol
instance (ToKol a b) => Record.ApplyAB FnPgWfromHsIField (WDef a) (WDef (Kol b)) where
  applyAB _ = fmap kol
instance (ToKol a b) => Record.ApplyAB FnPgWfromHsIField (Maybe a) (Koln b) where
  applyAB _ = maybe nul koln
instance (ToKol a b) => Record.ApplyAB FnPgWfromHsIField (WDef (Maybe a)) (WDef (Koln b)) where
  applyAB _ = fmap (maybe nul koln)

-- | Convert a custom Haskell type to a representation appropiate for /inserting/
-- it as a new row using 'Opaleye.SOT.Run.runInsert'.
pgWfromHsI :: Tabla t => HsI t -> PgW t
pgWfromHsI = PgW . Record.rMap FnPgWfromHsIField . unHsI
{-# INLINE pgWfromHsI #-}

--------------------------------------------------------------------------------

-- | To be used with 'Record.ApplyAB'.
data FnPgWfromPgRField = FnPgWfromPgRField
instance Record.ApplyAB FnPgWfromPgRField x x where
  applyAB _ = id
instance Record.ApplyAB FnPgWfromPgRField (Kol a) (WDef (Kol a)) where
  applyAB _ = WVal
instance Record.ApplyAB FnPgWfromPgRField (Koln a) (WDef (Koln a)) where
  applyAB _ = WVal

-- | Convert a @('PgR' t)@ resulting from a 'O.queryTable'-like operation
-- to a @('PgW' t)@ that can be used in a 'Opaleye.SOT.runUpdate'-like
-- operation.
pgWfromPgR :: Tabla t => PgR t -> PgW t
pgWfromPgR = PgW . Record.rMap FnPgWfromPgRField . unPgR
{-# INLINE pgWfromPgR #-}

--------------------------------------------------------------------------------

-- | Column properties: Write (no default), Read (not nullable).
colProps_wr :: PgTyped a => String -> O.TableProperties (Kol a) (Kol a)
colProps_wr = P.dimap unKol Kol . O.required

-- | Column properties: Write (no default), Read (nullable).
colProps_wrn :: PgTyped a => String -> O.TableProperties (Koln a) (Koln a)
colProps_wrn = P.dimap unKoln Koln . O.required

-- | Column properties: Write (optional default), Read (not nullable).
colProps_wdr :: PgTyped a => String -> O.TableProperties (WDef (Kol a)) (Kol a)
colProps_wdr = P.dimap (wdef Nothing Just . fmap unKol) Kol . O.optional

-- | Column properties: Write (optional default), Read (nullable).
colProps_wdrn :: PgTyped a => String -> O.TableProperties (WDef (Koln a)) (Koln a)
colProps_wdrn = P.dimap (wdef Nothing Just . fmap unKoln) Koln . O.optional

--------------------------------------------------------------------------------

-- | 'O.TableProperties' for a single column in 'Tabla' @t@.
type Col_Props (col :: Col Symbol WD RN Type Type)
  = O.TableProperties (Col_PgW col) (Col_PgR col)
data Col_PropsSym0 (col :: TyFun (Col Symbol WD RN Type Type) Type)
type instance Apply Col_PropsSym0 t = Col_Props t

class ICol_Props (col :: Col Symbol WD RN Type Type) where
  colProps :: proxy col -> Col_Props col

-- | 'colProps' is equivalent 'colProps_wr'.
instance forall n p h. (KnownSymbol n, PgTyped p) => ICol_Props ('Col n 'W 'R p h) where
  colProps _ = colProps_wr (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wrn'.
instance forall n p h. (KnownSymbol n, PgTyped p) => ICol_Props ('Col n 'W 'RN p h) where
  colProps _ = colProps_wrn (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wdr'.
instance forall n p h. (KnownSymbol n, PgTyped p) => ICol_Props ('Col n 'WD 'R p h) where
  colProps _ = colProps_wdr (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wdrn'.
instance forall n p h. (KnownSymbol n, PgTyped p) => ICol_Props ('Col n 'WD 'RN p h) where
  colProps _ = colProps_wdrn (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}

class RDistributeColProps (cols :: [Col Symbol WD RN Type Type]) where
  rDistributeColProps
    :: Proxy cols
    -> Record (List.Map (Col_NameSym0 :&&&$$$ Col_PropsSym0) cols)
instance RDistributeColProps '[] where
  rDistributeColProps _ = RNil
instance (RDistributeColProps cols, ICol_Props ('Col n w r p h))
  => RDistributeColProps ('Col n w r p h ': cols) where
  rDistributeColProps (_ :: Proxy ('Col n w r p h ': cols)) =
     RCons (Tagged @n (colProps (Proxy @('Col n w r p h))))
           (rDistributeColProps (Proxy @cols))

--------------------------------------------------------------------------------

-- | Build the Opaleye 'O.Table' for a 'Tabla'.
--
-- If you will be querying the resulting 'O.Table' right away, it is simpler to
-- use 'queryTabla' directly.
table :: Tabla t => T t -> O.Table (PgW t) (PgR t)
table (_ :: T t) = O.TableWithSchema
  (symbolVal (Proxy :: Proxy (SchemaName t)))
  (symbolVal (Proxy :: Proxy (TableName t)))
  (P.dimap unPgW PgR (PP.ppa (rDistributeColProps (Proxy :: Proxy (Cols t)))))

-- | Query all of the rows in a 'Tabla.
--
-- This is like @opaleye@'s own 'O.queryTable', but for specialized for a
-- 'Tabla'.
queryTabla :: Tabla t => T t -> O.Query (PgR t)
queryTabla = O.queryTable . table
{-# INLINE queryTabla #-}

--------------------------------------------------------------------------------

class ColLens n x a b | x n -> a b where
  -- | 'Lens'' into the value in a column.
  --
  -- Mnemonic: the COLumn value.
  --
  -- See 'GHC.IsLabel' for alternative APIs for this.
  --
  -- Notice that this lens is more polymorphic than it needs to be, as
  -- @'Lens' x x a a@ would usually suffice for our needs. However, we need to make this fully
  -- polymorphic over @a@ because otherwise `Control.Lens.set` won't
  -- readily pick our 'GHC.IsLabel' implementation for 'col' when required.
  col :: proxy (n :: Symbol) -> Lens x x a b

instance (Tabla t, x ~ Cols_NamedHsR t, Record.RLens n x x a b) => ColLens n (HsR t) a b where
  col prx = iso unHsR HsR . Record.rLens prx
  {-# INLINE col #-}

instance (Tabla t, x ~ Cols_NamedHsI t, Record.RLens n x x a b) => ColLens n (HsI t) a b where
  col prx = iso unHsI HsI . Record.rLens prx
  {-# INLINE col #-}

instance (Tabla t, x ~ Cols_NamedPgR t, Record.RLens n x x a b) => ColLens n (PgR t) a b where
  col prx = iso unPgR PgR . Record.rLens prx
  {-# INLINE col #-}

instance (Tabla t, x ~ Cols_NamedPgRN t, Record.RLens n x x a b) => ColLens n (PgRN t) a b where
  col prx = iso unPgRN PgRN . Record.rLens prx
  {-# INLINE col #-}

instance (Tabla t, x ~ Cols_NamedPgW t, Record.RLens n x x a b) => ColLens n (PgW t) a b where
  col prx = iso unPgW PgW . Record.rLens prx
  {-# INLINE col #-}

--------------------------------------------------------------------------------
-- Lenses into column values through OverloadedLabels:

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (HsR t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((HsR t) -> f (HsR t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (HsI t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((HsI t) -> f (HsI t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (PgR t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((PgR t) -> f (PgR t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (PgRN t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((PgRN t) -> f (PgRN t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (PgW t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((PgW t) -> f (PgW t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

--------------------------------------------------------------------------------
-- Projection of column values through OverloadedLabels:

type family Col_ByName (n :: Symbol) (cols :: [Col Symbol WD RN Type Type]) :: Col Symbol WD RN Type Type where
  Col_ByName n (c ': cs) = If (Col_Name c == n) c (Col_ByName n cs)
  Col_ByName n '[] = GHC.TypeError
    ('GHC.Text "Cols_ByName: No column named " 'GHC.:<>: 'GHC.ShowType n)


-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (HsR t) a a) => GHC.IsLabel n (HsR t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (HsI t) a a) => GHC.IsLabel n (HsI t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (PgR t) a a) => GHC.IsLabel n (PgR t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (PgRN t) a a) => GHC.IsLabel n (PgRN t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (PgW t) a a) => GHC.IsLabel n (PgW t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

--------------------------------------------------------------------------------

-- | Like 'Prelude.bool', @'matchBool' f t x@ evaluates to @f@ if @x@ is false,
-- otherwise it evaluates to @t@.
matchBool :: PgTyped a => Kol a -> Kol a -> Kol O.PGBool -> Kol a
matchBool = liftKol3 (\f' t' x' -> O.ifThenElse x' t' f')

--------------------------------------------------------------------------------
-- Booleans.

-- | Logical NOT.
--
-- Note: This function can take any of 'Kol' and 'Koln' argument, with the
-- return type being fully determined by it. The valid combinations are:
--
-- @
-- 'lnot' :: 'Kol'  'O.PGBool' -> 'Kol'  'O.PGBool'
-- 'lnot' :: 'Koln' 'O.PGBool' -> 'Koln' 'O.PGBool'
-- @
lnot :: Kol O.PGBool -> Kol O.PGBool
lnot = liftKol1 O.not

-- | Logical OR. See 'eq' for possible argument types.
lor :: Kol O.PGBool -> Kol O.PGBool -> Kol O.PGBool
lor = liftKol2 (O..||)

-- | Whether any of the given 'O.PGBool's is true.
--
-- Notice that 'lor' is more general that 'lors', as it doesn't restrict @kol@.
--
-- Mnemonic reminder: Logical ORs.
lors :: Foldable f => f (Kol O.PGBool) -> Kol O.PGBool
lors = foldl' lor (kol False)

-- Logical AND. See 'eq' for possible argument types.
land :: Kol O.PGBool -> Kol O.PGBool -> Kol O.PGBool
land = liftKol2 (O..&&)

-- | Whether all of the given 'O.PGBool's are true.
--
-- Notice that 'land' is more general that 'lands', as it doesn't restrict
-- @kol@.
--
-- Mnemonic reminder: Logical ANDs.
lands :: Foldable f => f (Kol O.PGBool) -> Kol O.PGBool
lands = foldl' land (kol True)

--------------------------------------------------------------------------------
-- Equality

-- | A @'PgEq' a@ instance states that @a@ can be compared for equality.
class PgTyped a => PgEq (a :: k)

instance PgEq O.PGBool
instance PgEq O.PGBytea
instance PgEq O.PGCitext
instance PgEq O.PGDate
instance PgEq O.PGFloat4
instance PgEq O.PGFloat8
instance PgEq O.PGInt2
instance PgEq O.PGInt4
instance PgEq O.PGInt8
instance PgEq O.PGJsonb
instance PgEq O.PGJson
instance PgEq O.PGNumeric
instance PgEq O.PGText
instance PgEq O.PGTimestamptz
instance PgEq O.PGTimestamp
instance PgEq O.PGTime
instance PgEq O.PGUuid

-- | Whether two column values are equal.
--
-- Note: This function can take any combination of 'Kol' and 'Koln' arguments,
-- with the return type being fully determined by them. The valid combinations
-- are:
--
-- @
-- 'eq' :: 'Kol'  x -> 'Kol'  x -> 'Kol'  'O.PGBool'
-- 'eq' :: 'Kol'  x -> 'Koln' x -> 'Koln' 'O.PGBool'
-- 'eq' :: 'Koln' x -> 'Kol'  x -> 'Koln' 'O.PGBool'
-- 'eq' :: 'Koln' x -> 'Koln' x -> 'Koln' 'O.PGBool'
-- @
--
-- Mnemonic reminder: EQual.
eq :: PgEq a => Kol a -> Kol a -> Kol O.PGBool
eq = liftKol2 (O..==)

-- | Whether the given value is a member of the given collection.
--
-- Notice that a combination 'eq' and 'or' is more general that 'member', as
-- they don't restrict @kol'@.
member :: (PgEq a, Foldable f) => Kol a -> f (Kol a) -> Kol O.PGBool
member a = lors . map (eq a) . toList

--------------------------------------------------------------------------------
-- Ordering

-- | Whether the first argument is less than the second. See 'eq' for possible
-- argument types.
--
-- Mnemonic reminder: Less Than.
lt :: PgOrd a => Kol a -> Kol a -> Kol O.PGBool
lt = liftKol2 (O..<)

-- | Whether the first argument is less than or equal to the second. See 'eq'
-- for possible argument types.
--
-- Mnemonic reminder: Less Than or Equal.
lte :: PgOrd a => Kol a -> Kol a -> Kol O.PGBool
lte = liftKol2 (O..<=)

-- | Whether the first argument is greater than the second. See 'eq' for
-- possible argument types.
--
-- Mnemonic reminder: Greater Than.
gt :: PgOrd a => Kol a -> Kol a -> Kol O.PGBool
gt = liftKol2 (O..>)

-- | Whether the first argument is greater than or equal to the second. See 'eq'
-- for possible argument types.
--
-- Mnemonic reminder: Greater Than or Equal.
gte :: PgOrd a => Kol a -> Kol a -> Kol O.PGBool
gte = liftKol2 (O..>=)

--------------------------------------------------------------------------------

-- | Whether a 'Koln' is 'nul' (@NULL@).
isNull :: Koln a -> Kol O.PGBool
isNull = Kol . O.isNull . unKoln

-- | Restrict query results to a particular condition.
--
-- This is analogous to 'Control.Monad.guard' for 'MonadPlus'.
--
-- Hint: Many times you will have a @'Koln' 'O.PGBool'@ instead of a @'Kol'
-- 'O.PGBool'@. In order to use that with 'restrict' you will have to convert it
-- to a @'Kol' 'O.PGBool'@ first, deciding whether you want 'nul' (@NULL@) to
-- mean “true” or “false”.
--
-- To treat @NULL@ as true:
--
-- @
-- 'fromKoln' ('kol' 'True') :: 'Koln' 'O.PGBool' -> 'Kol' 'O.PGBool'
-- @
--
-- To treat @NULL@ as false:
--
-- @
-- 'fromKoln' ('kol' 'False') :: 'Koln' 'O.PGBool' -> 'Kol' 'O.PGBool'
-- @
restrict :: O.QueryArr (Kol O.PGBool) ()
restrict = O.restrict <<^ unKol

-- | Perform an SQL @LEFT JOIN@.
--
-- @'leftJoin' t1 t2 f@ returns all of the rows from @t1@ (the left table),
-- possibly paired with the rows from @f2@ (the right table) in case @f@ is
-- true.
leftJoin
  :: ( PP.Default O.Unpackspec a a
     , PP.Default O.Unpackspec b b
     , PP.Default OI.NullMaker b nb )
  => O.Query a -- ^ Left table.
  -> O.Query b -- ^ Right table.
  -> ((a, b) -> Kol O.PGBool)
  -> O.Query (a, nb) -- ^
leftJoin qa qb fil =
  O.leftJoinExplicit PP.def PP.def PP.def qa qb (unKol . fil)

--------------------------------------------------------------------------------
-- Ordering

-- | Ascending order, no @NULL@s involved.
asc :: PgOrd b => (a -> Kol b) -> O.Order a
asc f = O.asc (unKol . f)

-- | Ascending order, @NULL@s last.
ascnl :: PgOrd b => (a -> Koln b) -> O.Order a
ascnl f = O.asc (unsafeUnNullableColumn . unKoln . f)

-- | Ascending order, @NULL@s first.
ascnf :: PgOrd b => (a -> Koln b) -> O.Order a
ascnf f = O.ascNullsFirst (unsafeUnNullableColumn . unKoln . f)

-- | Descending order, no @NULL@s involved.
desc :: PgOrd b => (a -> Kol b) -> O.Order a
desc f = O.desc (unKol . f)

-- | Descending order, @NULL@s first.
descnf :: PgOrd b => (a -> Koln b) -> O.Order a
descnf f = O.desc (unsafeUnNullableColumn . unKoln . f)

-- | Descending order, @NULL@s last.
descnl :: PgOrd b => (a -> Koln b) -> O.Order a
descnl f = O.descNullsLast (unsafeUnNullableColumn . unKoln . f)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This belongs in Opaleye

unsafeUnNullableColumn :: O.Column (O.Nullable a) -> O.Column a
unsafeUnNullableColumn = O.unsafeCoerceColumn

pgFloat4 :: Float -> O.Column O.PGFloat4
pgFloat4 = OI.literalColumn . OI.DoubleLit . float2Double

pgFloat8 :: Float -> O.Column O.PGFloat8
pgFloat8 = OI.literalColumn . OI.DoubleLit . float2Double

pgInt2 :: Int16 -> O.Column O.PGInt2
pgInt2 = OI.literalColumn . OI.IntegerLit . fromIntegral

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.QueryRunnerColumnDefault O.PGFloat4 Float where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.PGFractional O.PGFloat4 where
  pgFromRational = pgFloat4 . fromRational

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.PGNum O.PGFloat4 where
  pgFromInteger = pgFloat4 . fromInteger

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.PGNum O.PGInt2 where
  pgFromInteger = pgInt2 . fromInteger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Misc

-- | Apply a same constraint to all the types in the list.
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

