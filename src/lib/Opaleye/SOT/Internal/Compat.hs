{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Miscellaneous compatibility stuff.
module Opaleye.SOT.Internal.Compat
  ( AnyColumn(..)
  , unsafeFunExpr
  , unsafeUnNullableColumn
  , pgFloat4
  , pgFloat8
  , pgInt2
  , PGNumeric
  , PGNumericScale
  , pgRational
  , pgScientific
  , pgFixed
  ) where

import qualified Control.Exception as Ex
import           Data.Fixed (Fixed(..))
import qualified Data.Fixed as Fixed
import           Data.Int
import           Data.Proxy
import           Data.Scientific (Scientific, formatScientific)
import qualified Data.Scientific as Scientific
import qualified Database.PostgreSQL.Simple.FromField as Pg
import           GHC.Float (float2Double)
import           GHC.Real (infinity, notANumber)
import           GHC.TypeLits (Nat, KnownNat, type (+))
import qualified GHC.TypeLits as GHC
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as OI
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HDB
import qualified Opaleye.Internal.PGTypes as OI
import qualified Opaleye.Internal.RunQuery as OI

--------------------------------------------------------------------------------

data AnyColumn = forall a. AnyColumn (O.Column a)

-- | 'unsafeFunExpr "f" xs' calls a function called @"f"@ with arguments @xs@.
-- The return type must correctly be set by the caller.
unsafeFunExpr :: HDB.Name -> [AnyColumn] -> O.Column b
unsafeFunExpr fname =
  OI.Column . HDB.FunExpr fname . map (\(AnyColumn (OI.Column x)) -> x)

unsafeUnNullableColumn :: O.Column (O.Nullable a) -> O.Column a
unsafeUnNullableColumn = O.unsafeCoerceColumn

pgFloat4 :: Float -> O.Column O.PGFloat4
pgFloat4 = OI.literalColumn . HDB.DoubleLit . float2Double

pgFloat8 :: Float -> O.Column O.PGFloat8
pgFloat8 = OI.literalColumn . HDB.DoubleLit . float2Double

pgInt2 :: Int16 -> O.Column O.PGInt2
pgInt2 = OI.literalColumn . HDB.IntegerLit . fromIntegral

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.QueryRunnerColumnDefault O.PGFloat4 Float where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn

-- | Conversions to 'Int' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( GHC.TypeError
      ('GHC.Text "QueryRunnerColumnDefault conversions to Int are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Int is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one Int8, Int16, Int32, Int64 from Data.Int.")
  ) => OI.QueryRunnerColumnDefault a Int
  where queryRunnerColumnDefault = undefined

-- | Conversions to 'Word' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( GHC.TypeError
      ('GHC.Text "QueryRunnerColumnDefault conversions to Word are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Word is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one of Word8, Word16, Word32 from Data.Word.")
  ) => OI.QueryRunnerColumnDefault a Word
  where queryRunnerColumnDefault = undefined

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

-- | PostgreSQL @numeric@ type, with @scale@ indicating how many decimal digits
-- does this @numeric@ value support.
--
-- Note that @scale@ is a phantom types are only ever used in the Haskell side,
-- and never on the PostgreSQL side. That is, a @'PGNumeric' s@ type in
-- Haskell maps to a @numeric@ type without a scale specified.
--
-- 'PGNumeric' doesn't support specifying the “precission” of the PostgreSQL
-- @numeric@ type, as there's no use for that precision on the Haskell side and
-- we always support the full precision.
data PGNumeric (scale :: Nat)

-- | Maximum numeric scale for a type.
type family PGNumericScale (t :: k) :: Nat
type instance PGNumericScale Fixed.E0  = 0
type instance PGNumericScale Fixed.E1  = 1
type instance PGNumericScale Fixed.E2  = 2
type instance PGNumericScale Fixed.E3  = 3
type instance PGNumericScale Fixed.E6  = 6
type instance PGNumericScale Fixed.E9  = 9
type instance PGNumericScale Fixed.E12 = 12

instance O.PGOrd (PGNumeric s)

instance KnownNat s => OI.PGNum (PGNumeric s) where
  pgFromInteger = pgScientific . fromInteger
  {-# INLINE pgFromInteger #-}

-- | WARNING: 'pgFromRational' throws 'Ex.RatioZeroDenominator' if given a
-- positive or negative 'infinity'.
instance GHC.KnownNat s => OI.PGFractional (PGNumeric s) where
  pgFromRational = maybe (Ex.throw Ex.RatioZeroDenominator) id . pgRational
  {-# INLINE pgFromRational #-}

-- | Convert a 'Rational' to a @numeric@ column in PostgreSQL. 'notANumber' is
-- supported.
--
-- Returns 'Nothing' in case of positive or negative 'infinity'.
pgRational :: KnownNat s => Rational -> Maybe (O.Column (PGNumeric s))
pgRational x
  | x == infinity    = Nothing
  | x == (-infinity) = Nothing
  | x == notANumber  = Just (OI.literalColumn (HDB.StringLit "NaN"))
  | otherwise        = Just (pgScientific (fromRational x))

pgScientific :: forall s. KnownNat s => Scientific -> O.Column (PGNumeric s)
pgScientific = OI.literalColumn . HDB.OtherLit . formatScientific
  Scientific.Fixed (Just (fromInteger (GHC.natVal (Proxy :: Proxy s))))
{-# INLINE pgScientific #-}

pgFixed
  :: forall e s
  .  ( KnownNat s
     , Fixed.HasResolution e, GHC.CmpNat s (PGNumericScale e + 1) ~ 'LT)
  => Fixed e -> O.Column (PGNumeric s)
pgFixed = case GHC.natVal (Proxy :: Proxy s) of
  0 -> \(MkFixed x) -> OI.literalColumn (HDB.IntegerLit x)
  _ -> OI.literalColumn . HDB.OtherLit . Fixed.showFixed False
{-# INLINE pgFixed #-}

instance OI.QueryRunnerColumnDefault (PGNumeric s) Rational where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn
  {-# INLINE queryRunnerColumnDefault #-}

instance OI.QueryRunnerColumnDefault (PGNumeric s) Scientific where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn
  {-# INLINE queryRunnerColumnDefault #-}

instance OI.QueryRunnerColumnDefault (PGNumeric 0) Integer where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn
  {-# INLINE queryRunnerColumnDefault #-}

newtype WrapFixed e = WrapFixed { unWrapFixed :: Fixed e }

instance Fixed.HasResolution e => Pg.FromField (WrapFixed e) where
  fromField = fmap (fmap (fmap (WrapFixed . fromRational))) Pg.fromField
  {-# INLINE fromField #-}

instance
  ( Fixed.HasResolution e, GHC.CmpNat s (PGNumericScale e + 1) ~ 'LT
  ) => OI.QueryRunnerColumnDefault (PGNumeric s) (Fixed e) where
    queryRunnerColumnDefault = fmap unWrapFixed O.fieldQueryRunnerColumn
    {-# INLINE queryRunnerColumnDefault #-}

