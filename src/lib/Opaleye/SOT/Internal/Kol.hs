{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This is an internal module. You are very discouraged from using it directly.
module Opaleye.SOT.Internal.Kol
 ( PGArrayn
 , PgPrimType(..)
 , PgTyped(..)
 , upcastKol
 , unsafeDowncastKol
 , Kol(..)
 , unsafeCoerceKol
 , unsaferCoerceKol
 , unsaferCastKol
 , liftKol1
 , liftKol2
 , liftKol3
 , ToKol(..)
 , kolArray
 , PgNum
 , PgIntegral
 , PgFractional
 , PgFloating
 , modulo
 , itruncate
 , iround
 , ifloor
 , iceil
 , euler's
 , CastKol
 , castKol
 , matchBool
 , lnot
 , lor
 , lors
 , land
 , lands
 , PgEq
 , eq
 , member
 , PgOrd
 , lt
 , lte
 , gt
 , gte
 , PgBitwise
 , bwand
 , bwor
 , bwxor
 , bwnot
 , bwsl
 , bwsr
 , toTimestamptz
 , toTimestamp
 , timestamptzEpoch
 , timestampCentury
 , timestampDay
 , timestampDayOfTheWeek
 , timestampDayOfTheWeekISO8601
 , timestampDayOfTheYear
 , timestampDecade
 , timestampHour
 , timestampMicroseconds
 , timestampMillenium
 , timestampMilliseconds
 , timestampMinute
 , timestampMonth
 , timestampQuarter
 , timestampSecond
 , timestampWeekISO8601
 , timestampYear
 , timestampYearISO8601
 , unsafeFunExpr__date_part
 , now
 ) where

import           Control.Lens
import           Data.Fixed (Fixed(..))
import qualified Data.Fixed as Fixed
import           Data.Kind
import           Data.Foldable
import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import           Data.Int
import           Data.Proxy (Proxy(..))
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as PP
import           Data.Scientific (Scientific)
import           Data.Tagged
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Time
import           Data.Typeable (Typeable)
import qualified Data.UUID
import           Data.Word
import qualified Database.PostgreSQL.Simple.Types as Pg
import           GHC.Exts (Constraint)
import qualified GHC.TypeLits as GHC
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as OI
import qualified Opaleye.Internal.RunQuery as OI
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HDB

import Opaleye.SOT.Internal.Compat
  (AnyColumn(..), PGNumeric, PGNumericScale,
   unsafeFunExpr, pgFloat4, pgFloat8, pgInt2, pgScientific, pgFixed)

-------------------------------------------------------------------------------

-- | Like 'PGArray', but the values inside the array are expected to be
-- nullable.
data PGArrayn (a :: Type)

instance forall a b.
  ( O.QueryRunnerColumnDefault a b, Typeable b
  ) => O.QueryRunnerColumnDefault (PGArrayn a) [Maybe b] where
    queryRunnerColumnDefault =
      let OI.QueryRunnerColumn c f = O.queryRunnerColumnDefault
            :: OI.QueryRunnerColumn (O.Nullable a) (Maybe b)
          h :: O.Column (PGArrayn a) -> O.Column (O.Nullable a)
          h = O.unsafeCoerceColumn
      in OI.QueryRunnerColumn (P.lmap h c)
           ((fmap . fmap . fmap) Pg.fromPGArray (OI.arrayFieldParser f))

-------------------------------------------------------------------------------

-- | Only 'PgPrimType' instances are allowed as indexes to @opaleye@'s 'O.Column'.
--
-- You probably won't be adding new 'PgPrimType' instances yourself,
-- unless you are trying to represent a concrete PostgreSQL data type, but even
-- then you might get away with creating 'PgTyped' instances instead.
class PgPrimType (a :: k) where
  pgPrimTypeName :: proxy a -> String

instance {-# OVERLAPPING #-}
  ( GHC.TypeError
     ('GHC.Text "Invalid PgPrimType (can't be Nullable): "
      'GHC.:<>: 'GHC.ShowType (O.Nullable a) )
  ) => PgPrimType (O.Nullable a)
  where pgPrimTypeName = undefined

instance forall a. PgPrimType a => PgPrimType (O.PGArray a) where
  pgPrimTypeName _ = pgPrimTypeName (Proxy :: Proxy a) ++ "[]"

instance forall a. PgPrimType a => PgPrimType (PGArrayn a) where
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
instance PgPrimType O.PGText where pgPrimTypeName _ = "text"
instance PgPrimType O.PGTimestamptz where pgPrimTypeName _ = "timestamptz"
instance PgPrimType O.PGTimestamp where pgPrimTypeName _ = "timestamp"
instance PgPrimType O.PGTime where pgPrimTypeName _ = "time"
instance PgPrimType O.PGUuid where pgPrimTypeName _ = "uuid"

-- | Notice: 'pgPrimTypeName' is always @"numeric"@ and the @scale@ argument is
-- not rendered in SQL.This is good, don't worry too much. See 'PGNumeric'
instance PgPrimType (PGNumeric scale) where pgPrimTypeName _ = "numeric"

instance
  ( GHC.TypeError
      ('GHC.Text "Opaleye.PGNumeric is not supported," 'GHC.:$$:
       'GHC.Text "Use Opaleye.SOT.PGNumeric instead.")
  ) => PgPrimType O.PGNumeric where pgPrimTypeName = undefined

-- | Only 'PgTyped' instances are allowed as indexes to 'Kol' and 'Koln'.
--
-- The @'PgType' a ~ 'PgType' ('PgType' a)@ guarantees that @'PgType' a@ is a
-- fixpoint.
--
-- Law 1: The @a@ in @'PgType' a@ is only nominal (i.e., it is possible to
-- safely downcast @'PgType' a@ to @a@ and upcast it back to @'PgType' a@
-- provided the column's value didn't change):
--
-- @
-- 'upcastKol' . 'unsafeDowncastKol' = 'id'
-- 'unsafeDowncastKol' . 'upcastKol' = 'id'
-- @
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
  --   You might find 'Opaleye.SOT.Run.qrcWrapped' useful for simple cases like
  --   @UserId@.
  --
  --   Notice that the instance mentions 'O.PGInt4' directly, not our 'PgTyped'
  --   @UserId@. This is fine, not much would be gained by making the difference
  --   between them at this point, as you still need to do the 'Pg.FromRow'
  --   parsing and any inconsistencies will be uncovered there.
  --
  -- * If you want to reuse the exising 'Num', 'Fractional' or similar instances
  --   for @'Kol' 'O.PGInt4', you will need to explicitely ask for it by
  --   instantiating @'PgNum' UserId@, @'PgFractional' UserId@, etc.
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
instance PgTyped O.PGText where type PgType O.PGText = O.PGText
instance PgTyped O.PGTimestamptz where type PgType O.PGTimestamptz = O.PGTimestamptz
instance PgTyped O.PGTimestamp where type PgType O.PGTimestamp = O.PGTimestamp
instance PgTyped O.PGTime where type PgType O.PGTime = O.PGTime
instance PgTyped O.PGUuid where type PgType O.PGUuid = O.PGUuid
instance PgTyped (PGNumeric s) where type PgType (PGNumeric s) = PGNumeric s

instance PgTyped a => PgTyped (O.PGArray a) where type PgType (O.PGArray a) = O.PGArray (PgType a)
instance PgTyped a => PgTyped (PGArrayn a) where type PgType (PGArrayn a) = PGArrayn (PgType a)

-------------------------------------------------------------------------------

-- | A 'PgOrd' instance says that @a@ has an ordering. See 'orderBy'.
class (PgTyped a, O.PGOrd (PgType a)) => PgOrd (a :: k)
instance (PgTyped a, O.PGOrd (PgType a)) => PgOrd a

-------------------------------------------------------------------------------
-- Various numerical operations

-- | A @'PgNum' a@ instance gives you a @'Num' ('Kol' a)@ instance for free.
class (PgTyped a, OI.PGNum (PgType a)) => PgNum (a :: k)

instance PgNum O.PGInt2
instance PgNum O.PGInt4
instance PgNum O.PGInt8
instance PgNum O.PGFloat4
instance PgNum O.PGFloat8
instance GHC.KnownNat s => PgNum (PGNumeric s)

instance (PgNum a, Num (O.Column (PgType a))) => Num (Kol a) where
  fromInteger = Kol . fromInteger
  (*) = liftKol2 (*)
  (+) = liftKol2 (+)
  (-) = liftKol2 (-)
  abs = liftKol1 abs
  negate = liftKol1 negate
  signum = liftKol1 signum

-- | Sql operator @%@
modulo :: PgNum a => Kol a -> Kol a -> Kol a
modulo = liftKol2 (OI.binOp HDB.OpMod)

-------------------------------------------------------------------------------

-- | A 'PgIntegral' is guaranteed to be an integral type.
class PgTyped a => PgIntegral (a :: k)

instance PgIntegral O.PGInt2
instance PgIntegral O.PGInt4
instance PgIntegral O.PGInt8
instance PgIntegral (PGNumeric 0)

itruncate :: (PgFloating a, PgIntegral b) => Kol a -> Kol b
itruncate = liftKol1 (unsafeFunExpr "trunc" . pure . AnyColumn)

iround :: (PgFloating a, PgIntegral b) => Kol a -> Kol b
iround = liftKol1 (unsafeFunExpr "round" . pure . AnyColumn)

iceil :: (PgFloating a, PgIntegral b) => Kol a -> Kol b
iceil = liftKol1 (unsafeFunExpr "ceil" . pure . AnyColumn)

ifloor :: (PgFloating a, PgIntegral b) => Kol a -> Kol b
ifloor = liftKol1 (unsafeFunExpr "floor" . pure . AnyColumn)

-------------------------------------------------------------------------------

-- | A @'PgFractional' a@ instance gives you a @'Fractional' ('Kol' a)@ instance
-- for free.
class (PgTyped a, PgNum a, OI.PGFractional (PgType a)) => PgFractional (a :: k)

instance PgFractional O.PGFloat4
instance PgFractional O.PGFloat8
instance GHC.KnownNat s => PgFractional (PGNumeric s)

instance
    ( PgTyped a, PgFractional a
    , Fractional (O.Column (PgType a))
    , Num (O.Column (PgType a))
    ) => Fractional (Kol a) where
  fromRational = Kol . fromRational
  (/) = liftKol2 (/)

-------------------------------------------------------------------------------
-- | A 'PgFloating' instance gives you 'Floating'-support.
class (PgTyped a, PgFractional a) => PgFloating (a :: k)

instance PgFloating O.PGFloat4
instance PgFloating O.PGFloat8

-- | @2.718281828459045@
euler's :: PgFloating a => Kol a
euler's = unsaferCastKol
  (kol (2.718281828459045 :: Double) :: Kol O.PGFloat8)
{-# INLINE euler's #-}

instance
    ( PgTyped a
    , PgFloating a
    , Fractional (Kol a)
    , PgFloating (Kol a)
    ) => Floating (Kol a) where
  pi = Kol (unsafeFunExpr "pi" [])
  exp = liftKol1 (unsafeFunExpr "exp" . pure . AnyColumn)
  log = liftKol1 (unsafeFunExpr "log" . pure . AnyColumn)
  sqrt = liftKol1 (unsafeFunExpr "sqrt" . pure . AnyColumn)
  (**) = liftKol2 (\base ex -> unsafeFunExpr "power" [AnyColumn base, AnyColumn ex])
  logBase = liftKol2 (\base n -> unsafeFunExpr "log" [AnyColumn base, AnyColumn n])
  sin = liftKol1 (unsafeFunExpr "sin" . pure . AnyColumn)
  cos = liftKol1 (unsafeFunExpr "cos" . pure . AnyColumn)
  tan = liftKol1 (unsafeFunExpr "tan" . pure . AnyColumn)
  asin = liftKol1 (unsafeFunExpr "asin" . pure . AnyColumn)
  acos = liftKol1 (unsafeFunExpr "acos" . pure . AnyColumn)
  atan = liftKol1 (unsafeFunExpr "atan" . pure . AnyColumn)
  -- Not the most efficient implementations, but PostgreSQL doesn't provide
  -- builtin support for hyperbolic functions. We add these for completeness,
  -- so that we can implement the 'Floating' typeclass in full.
  sinh x = ((euler's ** x) - (euler's ** (negate x))) / fromInteger 2
  cosh x = ((euler's ** x) + (euler's ** (negate x))) / fromInteger 2
  tanh x = ((euler's ** x) - (euler's ** (negate x)))
         / ((euler's ** x) + (euler's ** (negate x)))
  asinh x = log (x + sqrt ((x ** fromInteger 2) + fromInteger 1))
  acosh x = log (x + sqrt ((x ** fromInteger 2) - fromInteger 1))
  atanh x = log ((fromInteger 1 + x) / (fromInteger 1 - x)) / fromInteger 2

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

-- | Like 'unsaferCastKol', but without any explicit mention of the
-- target type.
unsaferCoerceKol :: (PgTyped a, PgTyped b) => Kol a -> Kol b
unsaferCoerceKol = liftKol1 O.unsafeCoerceColumn

-- | Unsafely but explicitely coerce one column type to another one by
-- appending a target type name like @::int4@ to the PostgreSQL value, even if
-- it is not guaranteed that the @int4@ type can properly hold the value. Use
-- 'unsafeCoerceKol' if you don't want the explicit casting behavior.
unsaferCastKol :: forall a b. (PgTyped a, PgTyped b) => Kol a -> Kol b
unsaferCastKol =
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
-- And that instance would give you @'kol' :: UserId -> 'Kol' UserId@.
-- However, for that to work, a 'ToKol' instance relating the @UserId@ on the
-- left (the Haskell value) to the primitive type of the @UserId@ on the right
-- (i.e., 'PgType' @UserId@ ~ 'O.PGInt4') must also exist:
--
-- @
-- instance 'ToKol' @UserId@ 'O.PGInt4'
-- @
--
-- If @UserId@ is an instance of 'Wrapped', then that's all you need to say:
-- Both instances will get default implementations of 'kol'.  Otherwise, you'll
-- need to implement 'kol' yourself.
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
  kol = -- Downcasting here is safe due to the Law 1 of 'PgTyped'.
        unsafeDowncastKol . kol . view _Wrapped'

-- | Law 1 of 'PgTyped' grants this instance.
instance {-# OVERLAPPABLE #-} forall a b. (ToKol a b, PgTyped b) => ToKol a b where
  kol = unsafeDowncastKol . kol

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

instance GHC.KnownNat s => ToKol Integer (PGNumeric s) where kol = Kol . fromInteger
instance GHC.KnownNat s => ToKol Scientific (PGNumeric s) where kol = Kol . pgScientific
instance GHC.KnownNat s => ToKol Rational (PGNumeric s) where kol = Kol . fromRational
instance
  ( GHC.KnownNat s
  , Fixed.HasResolution e, GHC.CmpNat s (PGNumericScale e GHC.+ 1) ~ 'LT
  ) => ToKol (Fixed e) (PGNumeric s) where kol = Kol . pgFixed

instance forall a b. ToKol a b => ToKol [a] (O.PGArray b) where
  kol = kolArray . map (kol :: a -> Kol b)

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
   (OI.Column (HDB.ArrayExpr (map (OI.unColumn . unKol) (toList xs))))

-- | Conversions from 'Int' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( PgTyped a
  , GHC.TypeError
      ('GHC.Text "ToKol conversions from Int to Kol are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Int is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one Int8, Int16, Int32, Int64 from Data.Int.")
  ) => ToKol Int a where kol = undefined

-- | Conversions from 'Word' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( PgTyped a
  , GHC.TypeError
      ('GHC.Text "ToKol conversions from Word to Kol are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Word is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one of Word8, Word16, Word32 from Data.Word.")
  ) => ToKol Word a where kol = undefined

---

instance Monoid (Kol O.PGText) where
  mempty = kol ""
  mappend = liftKol2 (OI.binOp HDB.OpCat)

instance Monoid (Kol O.PGCitext) where
  mempty = kol (Data.CaseInsensitive.mk "")
  mappend ka kb = unsaferCoerceKol
    (mappend (unsaferCoerceKol ka :: Kol O.PGText)
             (unsaferCoerceKol kb :: Kol O.PGText))

instance Monoid (Kol O.PGBytea) where
  mempty = kol Data.ByteString.empty
  mappend = liftKol2 (OI.binOp HDB.OpCat)

instance forall a. PgTyped a => Monoid (Kol (O.PGArray a)) where
  mempty = kolArray ([] :: [Kol a])
  mappend = liftKol2
    (\x y -> unsafeFunExpr "array_cat" [AnyColumn x, AnyColumn y])

-- instance PgBitwise O.PGBitstring ?

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
instance CastKol O.PGInt2 (PGNumeric s)
instance CastKol O.PGInt4 O.PGText
instance CastKol O.PGInt4 O.PGCitext
instance CastKol O.PGInt4 O.PGInt8
instance CastKol O.PGInt4 (PGNumeric s)
instance CastKol O.PGInt8 (PGNumeric s)

-- Shooting yourself in the foot? I will help you.

type family TypeErrorRange a b :: Constraint where
  TypeErrorRange a b = GHC.TypeError
    ('GHC.Text "If really want to explicitly cast " 'GHC.:<>: 'GHC.ShowType a 'GHC.:<>:
     'GHC.Text " to " 'GHC.:<>: 'GHC.ShowType b 'GHC.:<>:
     'GHC.Text " then use 'unsaferCastKol'." 'GHC.:$$:
     'GHC.Text "You will get a runtime error if the " 'GHC.:<>: 'GHC.ShowType a
     'GHC.:<>: 'GHC.Text " value is out of the range of " 'GHC.:<>: 'GHC.ShowType b)
instance TypeErrorRange O.PGInt4 O.PGInt2 => CastKol O.PGInt4 O.PGInt2
instance TypeErrorRange O.PGInt8 O.PGInt2 => CastKol O.PGInt8 O.PGInt2
instance TypeErrorRange O.PGInt8 O.PGInt4 => CastKol O.PGInt8 O.PGInt4

type family TypeErrorTimeCasting a b c :: Constraint where
  TypeErrorTimeCasting a b c = GHC.TypeError
    ('GHC.Text "Do not cast " 'GHC.:<>: 'GHC.ShowType a 'GHC.:<>:
     'GHC.Text " to " 'GHC.:<>: 'GHC.ShowType b 'GHC.:$$: 'GHC.Text c)
instance TypeErrorTimeCasting O.PGDate O.PGTimestamp "Read Section 8.5 of the PostgreSQL documentation." => CastKol O.PGDate O.PGTimestamp
instance TypeErrorTimeCasting O.PGDate O.PGTimestamptz "Read Section 8.5 of the PostgreSQL documentation." => CastKol O.PGDate O.PGTimestamptz
instance TypeErrorTimeCasting O.PGTimestamp O.PGDate "Use timestampDate." => CastKol O.PGTimestamp O.PGDate
instance TypeErrorTimeCasting O.PGTimestamp O.PGTime "Use timestampTime." => CastKol O.PGTimestamp O.PGTime
instance TypeErrorTimeCasting O.PGTimestamp O.PGTimestamptz "Use toTimestamptz instead." => CastKol O.PGTimestamp O.PGTimestamptz
instance TypeErrorTimeCasting O.PGTimestamptz O.PGDate "Use (timestampDate . toTimestamp zone)" => CastKol O.PGTimestamptz O.PGDate
instance TypeErrorTimeCasting O.PGTimestamptz O.PGTime "Use (timestampTime . toTimestamp zone)" => CastKol O.PGTimestamptz O.PGTime
instance TypeErrorTimeCasting O.PGTimestamptz O.PGTimestamp "Use toTimestamp instead." => CastKol O.PGTimestamptz O.PGTimestamp

-- | Safely and explicitely cast one column type to another one. See 'CastKol'.
castKol :: CastKol a b => Kol a -> Kol b
castKol = unsaferCastKol

-- | Safe upcasting.
upcastKol :: PgTyped a => Kol a -> Kol (PgType a)
upcastKol = unsafeCoerceKol

-- | Unsafe downcasting.
unsafeDowncastKol :: PgTyped a => Kol (PgType a) -> Kol a
unsafeDowncastKol = unsafeCoerceKol

--------------------------------------------------------------------------------
-- Booleans.

-- | Like 'Prelude.bool', @'matchBool' f t x@ evaluates to @f@ if @x@ is false,
-- otherwise it evaluates to @t@.
matchBool :: PgTyped a => Kol a -> Kol a -> Kol O.PGBool -> Kol a
matchBool = liftKol3 (\f' t' x' -> O.ifThenElse x' t' f')

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
instance PgEq O.PGText
instance PgEq O.PGTimestamptz
instance PgEq O.PGTimestamp
instance PgEq O.PGTime
instance PgEq O.PGUuid

-- | Whether two column values are equal.
--
-- Mnemonic reminder: EQual.
eq :: PgEq a => Kol a -> Kol a -> Kol O.PGBool
eq = liftKol2 (O..==)

-- | Whether the given value is a member of the given collection.
member :: (PgEq a, Foldable f) => Kol a -> f (Kol a) -> Kol O.PGBool
member a = lors . map (eq a) . toList

--------------------------------------------------------------------------------
-- Ordering

-- | Whether the first argument is less than the second.
--
-- Mnemonic reminder: Less Than.
lt :: PgOrd a => Kol a -> Kol a -> Kol O.PGBool
lt = liftKol2 (O..<)

-- | Whether the first argument is less than or equal to the second.
--
-- Mnemonic reminder: Less Than or Equal.
lte :: PgOrd a => Kol a -> Kol a -> Kol O.PGBool
lte = liftKol2 (O..<=)

-- | Whether the first argument is greater than the second.
--
-- Mnemonic reminder: Greater Than.
gt :: PgOrd a => Kol a -> Kol a -> Kol O.PGBool
gt = liftKol2 (O..>)

-- | Whether the first argument is greater than or equal to the second.
--
-- Mnemonic reminder: Greater Than or Equal.
gte :: PgOrd a => Kol a -> Kol a -> Kol O.PGBool
gte = liftKol2 (O..>=)

--------------------------------------------------------------------------------
-- Bitwise

-- | Only 'PgBitwise' instance can be used with bitwise operators 'btwand',
-- 'bword', 'bwxor', 'bwnot', 'bwsl' and 'bwsr'.
class PgTyped a => PgBitwise (a :: k)

instance PgBitwise O.PGInt2
instance PgBitwise O.PGInt4
instance PgBitwise O.PGInt8
-- instance PgBitwise O.PGBitstring ?

-- | Bitwise AND. Sql operator: @&@
bwand :: PgBitwise a => Kol a -> Kol a -> Kol a
bwand = liftKol2 (OI.binOp HDB.OpBitAnd)

-- | Bitwise OR. Sql operator: @|@
bwor :: PgBitwise a => Kol a -> Kol a -> Kol a
bwor = liftKol2 (OI.binOp HDB.OpBitOr)

-- | Bitwise XOR. Sql operator: @#@
bwxor :: PgBitwise a => Kol a -> Kol a -> Kol a
bwxor = liftKol2 (OI.binOp HDB.OpBitXor)

-- | Bitwise NOT. Sql operator: @~@
bwnot :: PgBitwise a => Kol a -> Kol a
bwnot = liftKol1 (OI.unOp (HDB.UnOpOther "~"))

-- | Bitwise shift left. Sql operator: @<<@
--
-- @'bwsl' a n@ shifts @a@ to the right @n@ positions. Translates to @a << n@ in
-- the generated SQL.
bwsl :: (PgBitwise a, PgIntegral b) => Kol a -> Kol b -> Kol a
bwsl = liftKol2 (OI.binOp (HDB.OpOther ("<<")))

-- | Bitwise shift right. Sql operator: @>>@
--
-- @'bwsr' a n@ shifts @a@ to the right @n@ positions. Translates to @a >> n@ in
-- the generated SQL.
bwsr :: (PgBitwise a, PgIntegral b) => Kol a -> Kol b -> Kol a
bwsr = liftKol2 (OI.binOp (HDB.OpOther (">>")))

--------------------------------------------------------------------------------

-- Convert a PostgreSQL @timestamptz@ to a @timestamp@ at a given timezone.
--
-- Notice that a @timestamp@ value is usually meaningless unless you also know
-- the timezone where that @timestamp@ happens. In other words, you should
-- store the passed in @'Kol' zone@ somewhere.
--
-- Warning: Dealing with @timestamp@ values in PostgreSQL is very error prone
-- unless you really know what you are doing.  Quite likely you shouldn't be
-- using @timestamp@ values in PostgreSQL unless you are storing distant dates
-- in the future for which the precise UTC time can't be known (e.g., can you
-- tell the UTC time for January 1 4045, 00:00:00 in Peru? Me neither, as I
-- have no idea in what timezone Peru will be in year 4045, so I can't convert
-- that to UTC).
--
-- Law 1: Provided the timezone database information stays the same, the
-- following equality holds:
--
-- @
-- 'toTimestamptz' zone . 'toTimestamp' zone === 'id'
-- 'toTimestamp' zone . 'toTimestamptz' zone === 'id'
-- @
toTimestamptz
  :: ( PgTyped zone, PgType zone ~ O.PGText
     , PgTyped a, PgType a ~ O.PGTimestamptz
     , PgTyped b, PgType b ~ O.PGTimestamp )
  => Kol zone -> Kol a -> Kol b
toTimestamptz = liftKol2
  (\zone a -> unsafeFunExpr "timezone" [AnyColumn zone, AnyColumn a])

-- Convert a PostgreSQL @timestamp@ to a @timestamptz@, making the assumption
-- that the given @timestamp@ happens at the given timezone.
--
-- Law 1: Provided the timezone database information stays the same, the
-- following equality holds:
--
-- @
-- 'toTimestamptz' zone . 'toTimestamp' zone === 'id'
-- 'toTimestamp' zone . 'toTimestamptz' zone === 'id'
-- @
toTimestamp
  :: ( PgTyped zone, PgType zone ~ O.PGText
     , PgTyped a, PgType a ~ O.PGTimestamp
     , PgTyped b, PgType b ~ O.PGTimestamptz )
  => Kol zone -> Kol a -> Kol b
toTimestamp = liftKol2
  (\zone a -> unsafeFunExpr "timezone" [AnyColumn zone, AnyColumn a])

unsafeFunExpr__date_part :: (PgTyped tsy, PgTyped b) => HDB.Name -> Kol tsy -> Kol b
unsafeFunExpr__date_part n = unsaferCastKol @O.PGFloat8 . liftKol1
   (\x -> unsafeFunExpr "date_part" [AnyColumn (O.pgString n), AnyColumn x])

timestamptzEpoch :: Kol O.PGTimestamptz -> Kol O.PGFloat8
timestamptzEpoch = unsafeFunExpr__date_part "epoch"

timestampCentury :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampCentury = unsafeFunExpr__date_part "century"

timestampDay :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampDay = unsafeFunExpr__date_part "day"

timestampDayOfTheWeek :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampDayOfTheWeek = unsafeFunExpr__date_part "dow"

timestampDayOfTheWeekISO8601 :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampDayOfTheWeekISO8601 = unsafeFunExpr__date_part "isodow"

timestampDayOfTheYear :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampDayOfTheYear = unsafeFunExpr__date_part "doy"

timestampDecade :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampDecade = unsafeFunExpr__date_part "decade"

timestampHour :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampHour = unsafeFunExpr__date_part "hour"

timestampMicroseconds :: (PgIntegral b, CastKol O.PGInt4 b) => Kol O.PGTimestamp -> Kol b
timestampMicroseconds = unsafeFunExpr__date_part "microseconds"

timestampMillenium :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampMillenium = unsafeFunExpr__date_part "millenium"

timestampMilliseconds :: (PgIntegral b, CastKol O.PGInt4 b) => Kol O.PGTimestamp -> Kol b
timestampMilliseconds = unsafeFunExpr__date_part "milliseconds"

timestampMinute :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampMinute = unsafeFunExpr__date_part "minute"

timestampMonth :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampMonth = unsafeFunExpr__date_part "month"

timestampQuarter :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampQuarter = unsafeFunExpr__date_part "quarter"

timestampSecond :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampSecond = unsafeFunExpr__date_part "second"

timestampWeekISO8601 :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampWeekISO8601 = unsafeFunExpr__date_part "week"

timestampYear :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampYear = unsafeFunExpr__date_part "year"

timestampYearISO8601 :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
timestampYearISO8601 = unsafeFunExpr__date_part "isoyear"

now :: Kol O.PGTimestamptz
now = Kol (unsafeFunExpr "now" [])
