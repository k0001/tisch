{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is an internal module. You are very discouraged from using it directly.
module Tisch.Internal.Fun
 ( PgNum
 , PgIntegral
 , PgFractional
 , PgFloating
 , modulo
 , itruncate
 , iround
 , ifloor
 , iceil
 , euler's
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
 , tstzEpoch
 , tsCentury
 , tsDay
 , tsDayOfTheWeek
 , tsDayOfTheWeekISO8601
 , tsDayOfTheYear
 , tsDecade
 , tsHour
 , tsMicroseconds
 , tsMillenium
 , tsMilliseconds
 , tsMinute
 , tsMonth
 , tsQuarter
 , tsSecond
 , tsWeekISO8601
 , tsYear
 , tsYearISO8601
 , unsafeFunExpr__date_part
 , nowTransaction
 , nowStatement
 , nowClock
 ) where

import Control.Lens ()
import Data.Foldable
import qualified Data.ByteString
import qualified Data.CaseInsensitive
import Data.Semigroup (Semigroup(..))
import qualified GHC.TypeLits as GHC
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as OI
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HDB

import Tisch.Internal.Kol
  (CastKol, Kol(..), PgTyped(..), ToKol(..), liftKol1, liftKol2, liftKol3,
   unsaferCastKol, unsaferCoerceKol, kolArray)
import Tisch.Internal.Compat (PGNumeric, AnyColumn(..), unsafeFunExpr)

-------------------------------------------------------------------------------
-- Semigroups, Monoids

instance Semigroup (Kol O.PGText) where
  (<>) = liftKol2 (OI.binOp (HDB.:||))

instance Monoid (Kol O.PGText) where
  mempty = kol ""
  mappend = (<>)

---
instance Semigroup (Kol O.PGCitext) where
  (<>) ka kb = unsaferCoerceKol
    (mappend (unsaferCoerceKol ka :: Kol O.PGText)
             (unsaferCoerceKol kb :: Kol O.PGText))

instance Monoid (Kol O.PGCitext) where
  mempty = kol (Data.CaseInsensitive.mk "")
  mappend = (<>)

---
instance Semigroup (Kol O.PGBytea) where
  (<>) = liftKol2 (OI.binOp (HDB.:||))

instance Monoid (Kol O.PGBytea) where
  mempty = kol Data.ByteString.empty
  mappend = (<>)

---
instance PgTyped a => Semigroup (Kol (O.PGArray a)) where
  (<>) = liftKol2 (\x y -> unsafeFunExpr "array_cat" [AnyColumn x, AnyColumn y])

instance forall a. PgTyped a => Monoid (Kol (O.PGArray a)) where
  mempty = kolArray ([] :: [Kol a])
  mappend = (<>)

-------------------------------------------------------------------------------
-- 'Num' and related operations

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
euler's = unsaferCastKol (kol (2.718281828459045 :: Double) :: Kol O.PGFloat8)
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
instance PgEq (PGNumeric s)

-- | Whether two column values are equal.
--
-- Mnemonic reminder: EQual.
eq :: PgEq a => Kol a -> Kol a -> Kol O.PGBool
eq = liftKol2 (O..==)

-- | Whether the given value is a member of the given collection.
member :: (PgEq a, Foldable f) => Kol a -> f (Kol a) -> Kol O.PGBool
member ka fkas = Kol (O.in_ (map unKol (toList fkas)) (unKol ka))

--------------------------------------------------------------------------------
-- Ordering

-- | A 'PgOrd' instance says that @a@ has an ordering. See 'orderBy'.
class (PgTyped a, O.PGOrd (PgType a)) => PgOrd (a :: k)
instance (PgTyped a, O.PGOrd (PgType a)) => PgOrd a

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
bwand = liftKol2 (OI.binOp (HDB.:&))

-- | Bitwise OR. Sql operator: @|@
bwor :: PgBitwise a => Kol a -> Kol a -> Kol a
bwor = liftKol2 (OI.binOp (HDB.:|))

-- | Bitwise XOR. Sql operator: @#@
bwxor :: PgBitwise a => Kol a -> Kol a -> Kol a
bwxor = liftKol2 (OI.binOp (HDB.:^))

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
-- Time

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

tstzEpoch :: Kol O.PGTimestamptz -> Kol O.PGFloat8
tstzEpoch = unsafeFunExpr__date_part "epoch"

tsCentury :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsCentury = unsafeFunExpr__date_part "century"

tsDay :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsDay = unsafeFunExpr__date_part "day"

tsDayOfTheWeek :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsDayOfTheWeek = unsafeFunExpr__date_part "dow"

tsDayOfTheWeekISO8601 :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsDayOfTheWeekISO8601 = unsafeFunExpr__date_part "isodow"

tsDayOfTheYear :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsDayOfTheYear = unsafeFunExpr__date_part "doy"

tsDecade :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsDecade = unsafeFunExpr__date_part "decade"

tsHour :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsHour = unsafeFunExpr__date_part "hour"

tsMicroseconds :: (PgIntegral b, CastKol O.PGInt4 b) => Kol O.PGTimestamp -> Kol b
tsMicroseconds = unsafeFunExpr__date_part "microseconds"

tsMillenium :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsMillenium = unsafeFunExpr__date_part "millenium"

tsMilliseconds :: (PgIntegral b, CastKol O.PGInt4 b) => Kol O.PGTimestamp -> Kol b
tsMilliseconds = unsafeFunExpr__date_part "milliseconds"

tsMinute :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsMinute = unsafeFunExpr__date_part "minute"

tsMonth :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsMonth = unsafeFunExpr__date_part "month"

tsQuarter :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsQuarter = unsafeFunExpr__date_part "quarter"

tsSecond :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsSecond = unsafeFunExpr__date_part "second"

tsWeekISO8601 :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsWeekISO8601 = unsafeFunExpr__date_part "week"

tsYear :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsYear = unsafeFunExpr__date_part "year"

tsYearISO8601 :: (PgIntegral b) => Kol O.PGTimestamp -> Kol b
tsYearISO8601 = unsafeFunExpr__date_part "isoyear"

-- | Time when the current transaction started.
--
-- Sql function: @transaction_timestamp()@, @now()@.
nowTransaction :: Kol O.PGTimestamptz
nowTransaction = Kol (unsafeFunExpr "transaction_timestamp" [])

-- | Time when the current statement started.
--
-- SqlFunction: @statement_timestamp()@.
nowStatement :: Kol O.PGTimestamptz
nowStatement = Kol (unsafeFunExpr "statement_timestamp" [])

-- | Current clock time.
--
-- SqlFunction: @clock_timestamp()@.
nowClock :: Kol O.PGTimestamptz
nowClock = Kol (unsafeFunExpr "clock_timestamp" [])
