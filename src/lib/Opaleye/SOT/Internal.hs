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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This is an internal module. You are very discouraged from using it directly.
module Opaleye.SOT.Internal where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Fix (MonadFix(..))
import           Data.Data (Data)
import           Data.Foldable
import           Data.Typeable (Typeable)
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Time
import qualified Data.UUID
import           Data.Int
import           Data.Proxy (Proxy(..))
import           Data.HList (Tagged(Tagged, unTagged), HList(HCons, HNil))
import qualified Data.HList as HL
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as PP
import           Data.Singletons
import qualified Data.Promotion.Prelude.List as List (Map)
import           GHC.Exts (Constraint)
import           GHC.Generics (Generic)
import           GHC.Float (float2Double)
import qualified GHC.TypeLits as GHC
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as OI
import qualified Opaleye.Internal.HaskellDB.PrimQuery as OI
import qualified Opaleye.Internal.PGTypes as OI
import qualified Opaleye.Internal.RunQuery as OI
import qualified Opaleye.Internal.Join as OI
import qualified Opaleye.Internal.TableMaker as OI

-------------------------------------------------------------------------------

-- | Hack to workaround the current represenation for nullable columns.
-- See 'Koln'.
type family NotNullable (x :: k) :: Constraint where
  NotNullable (O.Nullable x) =
     "NotNullable" ~ "NotNullable: expected `x` but got `Nullable x`"
  NotNullable x = ()

-- | Only 'PgPrimType' instances are allowed as indexes to @opaleye@'s
-- 'O.Column'.
--
-- You probably won't be adding new 'PgPrimType' instances yourself,
-- unless you are trying to represent a concrete PostgreSQL data type, but even
-- then you might get away with creating 'PgTyped' instances instead.
class NotNullable a => PgPrimType (a :: k)
instance PgPrimType O.PGBool
instance PgPrimType O.PGBytea
instance PgPrimType O.PGCitext
instance PgPrimType O.PGDate
instance PgPrimType O.PGFloat4
instance PgPrimType O.PGFloat8
instance PgPrimType O.PGInt2
instance PgPrimType O.PGInt4
instance PgPrimType O.PGInt8
instance PgPrimType O.PGJsonb
instance PgPrimType O.PGJson
instance PgPrimType O.PGNumeric
instance PgPrimType O.PGText
instance PgPrimType O.PGTimestamptz
instance PgPrimType O.PGTimestamp
instance PgPrimType O.PGTime
instance PgPrimType O.PGUuid

-- | Only 'PgTyped' instances are allowed as indexes to 'Kol' and 'Koln'.
class PgPrimType (PgType a) => PgTyped (a :: k) where
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
  type PgType a :: *

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

-------------------------------------------------------------------------------

-- | Like @opaleye@'s 'OI.PGOrd', but applies to a 'PgTyped'.
class (PgTyped a, O.PGOrd (PgType a)) => PgOrd (a :: k)
instance (PgTyped a, O.PGOrd (PgType a)) => PgOrd a

-------------------------------------------------------------------------------

-- | A @'PgNum' a@ instance gives you a @'Num' ('Kol' a)@ instance for free.
class (PgTyped a, OI.PGNum (PgType a)) => PgNum (a :: k)

instance PgNum O.PGInt4
instance PgNum O.PGInt8
instance PgNum O.PGFloat4
instance PgNum O.PGFloat8

instance (PgTyped a, PgNum a, Num (O.Column (PgType a))) => Num (Kol a) where
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
-- We do not use @('O.Column' x)@, instead we use @('Kol' y)@ where @x ~
-- 'PgType' y@. This is where we drift a bit appart from Opaleye. See
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/97
data Kol (a :: k) = PgTyped a => Kol { unKol :: O.Column (PgType a) }

deriving instance (PgTyped a, Show (O.Column (PgType a))) => Show (Kol a)

-- | Converts an unary function on Opaleye's 'O.Column' to an unary function
-- taking any of 'Kol' and 'Koln' as argument, with the result type fully
-- determined by it.
liftKol1
  :: (PgTyped a, PgTyped b, Op1 kol kol')
  => (O.Column (PgType a) -> O.Column (PgType b))
  -> (kol a -> kol' b) -- ^
liftKol1 f = op1 (Kol . f . unKol)

-- | Converts a binary function on Opaleye's 'O.Column's to an binary function
-- taking any of 'Kol' and 'Koln' as arguments, with the result type fully
-- determined by them.
liftKol2
  :: (PgTyped a, PgTyped b, PgTyped c, Op2 kol kol' kol'')
  => (O.Column (PgType a) -> O.Column (PgType b) -> O.Column (PgType c))
  -> (kol a -> kol' b -> kol'' c)
liftKol2 f = op2 (\ka kb -> Kol (f (unKol ka) (unKol kb)))

-- | Converts a ternary function on Opaleye's 'O.Column's to an ternary function
-- on 'Kol'.
--
-- /Hint/: You can further compose the result of this function with 'op3'
-- to widen the range of accepted argument types.
liftKol3
  :: (PgTyped a, PgTyped b, PgTyped c, PgTyped d, Op3 kol kol' kol'' kol''')
  => (O.Column (PgType a) -> O.Column (PgType b) -> O.Column (PgType c) -> O.Column (PgType d))
  -> (kol a -> kol' b -> kol'' c -> kol''' d)
liftKol3 f = op3 (\ka kb kc -> Kol (f (unKol ka) (unKol kb) (unKol kc)))

instance
    ( PgTyped a
    , Profunctor p, PP.Default p (O.Column (PgType a)) (O.Column b)
    ) => PP.Default p (Kol a) (O.Column b) where
  def = P.lmap unKol PP.def

instance forall p a b.
    ( PgTyped b, Profunctor p, PP.Default p (O.Column a) (O.Column (PgType b))
    ) => PP.Default p (O.Column a) (Kol b) where
  def = P.rmap Kol (PP.def :: p (O.Column a) (O.Column (PgType b)))

instance forall p a b.
    ( PgTyped a, PgTyped b
    , Profunctor p, PP.Default p (O.Column (PgType a)) (O.Column (PgType b))
    ) => PP.Default p (Kol a) (Kol b) where
  def = P.dimap unKol Kol (PP.def :: p (O.Column (PgType a)) (O.Column (PgType b)))

instance
    ( PgTyped a, PP.Default O.QueryRunner (O.Column (PgType a)) b
    ) => PP.Default O.QueryRunner (Kol a) b where
  def = P.lmap unKol PP.def

-- | Build a 'Kol'.
--
-- You need to provide a 'ToKol' instance for every Haskell type you plan to
-- convert to its PostgreSQL representation as 'Kol'.
--
-- A a default implementation of 'kol' is available for 'Wrapped'
-- instances:
--
-- @
-- default 'kol' :: ('PgTyped' b, 'PgType' b ~ p, 'Wrapped' a, 'ToKol' ('Unwrapped' a) p) => a -> 'Kol' b
-- 'kol' = 'kol' . 'view' '_Wrapped''
-- @
--
-- /Implementation notice/: This class overlaps in purpose with Opaleye's
-- 'O.Constant'. Technicaly, we don't really need to repeat those instances
-- here: we could just rely on Opaleye's 'O.Constant'. However, as of today,
-- Opaleye's 'O.Constant' provides some undesired support which we
-- deliberately want to avoid here. Namely, we don't want to support
-- converting 'Int' to 'O.PGInt4'. If this is fixed upstream,
-- we might go back to relying on 'O.Constant' if suitable. See
-- https://github.com/tomjaguarpaw/haskell-opaleye/pull/110
class PgPrimType p => ToKol (a :: *) (p :: *) where
  -- | Convert a constant Haskell value (say, a 'Bool') to its equivalent
  -- PostgreSQL representation as a @('Kol' 'O.PGBool')@.
  --
  -- Some example simplified types:
  --
  -- @
  -- 'kol' :: 'Bool' -> 'Kol' 'O.PGBool'
  -- 'kol' :: 'Int32' -> 'Kol' 'O.PGInt4'
  -- @
  kol :: (PgTyped b, PgType b ~ p) => a -> Kol (b :: kb)
  default kol
    :: (PgTyped b, PgType b ~ p, Wrapped a, ToKol (Unwrapped a) p) => a -> Kol b
  kol = kol . view _Wrapped'

instance ToKol String O.PGText where kol = Kol . O.pgString
instance ToKol Data.Text.Text O.PGText where kol = Kol . O.pgStrictText
instance ToKol Data.Text.Lazy.Text O.PGText where kol = Kol . O.pgLazyText
instance ToKol Char O.PGText where kol = Kol . O.pgString . (:[])
instance ToKol Bool O.PGBool where kol = Kol . O.pgBool
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
instance ToKol (Data.CaseInsensitive.CI Data.Text.Text) O.PGCitext where kol = Kol . O.pgCiStrictText
instance ToKol (Data.CaseInsensitive.CI Data.Text.Lazy.Text) O.PGCitext where kol = Kol . O.pgCiLazyText
instance ToKol Data.Aeson.Value O.PGJson where kol = Kol . O.pgLazyJSON . Data.Aeson.encode
instance ToKol Data.Aeson.Value O.PGJsonb where kol = Kol . O.pgLazyJSONB . Data.Aeson.encode

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
-- We do not use @'O.Column' ('O.Nullable' x)@, instead we use @'Koln' y@ where @x ~
-- 'PgType' y@. This is where we drift a bit appart from Opaleye. See
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/97
data Koln (a :: k) = PgTyped a => Koln { unKoln :: O.Column (O.Nullable (PgType a)) }

deriving instance Show (O.Column (O.Nullable a)) => Show (Koln a)

-- | Build a 'Koln' from a Haskell term. This is like the 'Just' constructor for
-- 'Maybe'
koln :: (ToKol a p, PgTyped b, PgType b ~ p) => a -> Koln b
koln = fromKol . kol

-- | PostgreSQL's @NULL@ value. This is like the 'Nothing' constructor for
-- 'Maybe'
nul :: PgTyped a => Koln a
nul = Koln O.null

-- | Convert a 'Kol' to a 'Koln'.
fromKol :: PgTyped a => Kol a -> Koln a
fromKol = Koln . O.toNullable . unKol

-- | Case analysis for 'Koln'. Like 'maybe' for 'Maybe'.
--
-- If @('Koln' a)@ is @NULL@, then evaluate to the first argument,
-- otherwise it applies the given function to the underlying @('Kol' a)@.
matchKoln :: (PgTyped a, PgTyped b) => Kol b -> (Kol a -> Kol b) -> Koln a -> Kol b
matchKoln kb0 f kna = Kol $
  O.matchNullable (unKol kb0) (unKol . f . Kol) (unKoln kna)

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
altKoln :: (PgTyped a) => Koln a -> Koln a -> Koln a
altKoln kna0 kna1 = Koln $
  O.matchNullable (unKoln kna1) O.toNullable (unKoln kna0)

-- | Converts an unary function on @opaleye@'s 'O.Nullable' 'O.Column'
-- to an unary function on 'Koln'.
--
-- /Hint/: You can further compose the result of this function with 'op1'
-- to widen the range of accepted argument types.
liftKoln1
  :: (PgTyped a, PgTyped b)
  => (O.Column (O.Nullable (PgType a)) -> O.Column (O.Nullable (PgType b)))
  -> (Koln a -> Koln b) -- ^
liftKoln1 f = Koln . f . unKoln

-- | Converts a binary function on Opaleye's 'O.Nullable' 'O.Column's
-- to a binary function on 'Koln's.
--
-- /Hint/: You can further compose the result of this function with 'op2'
-- to widen the range of accepted argument types.
liftKoln2
  :: (PgTyped a, PgTyped b, PgTyped c)
  => (O.Column (O.Nullable (PgType a)) ->
      O.Column (O.Nullable (PgType b)) ->
      O.Column (O.Nullable (PgType c)))
  -> (Koln a -> Koln b -> Koln c) -- ^
liftKoln2 f = \kna knb -> Koln (f (unKoln kna) (unKoln knb))

-- | OVERLAPPABLE.
instance {-# OVERLAPPABLE #-} forall p x a.
    ( P.Profunctor p, PgTyped a
    , PP.Default p x (O.Column (O.Nullable (PgType a)))
    ) => PP.Default p x (Koln a) where
  def = P.rmap Koln (PP.def :: p x (O.Column (O.Nullable (PgType a))))
  {-# INLINE def #-}

-- | OVERLAPPABLE.
instance {-# OVERLAPPABLE #-}
    ( P.Profunctor p, PgTyped a
    , PP.Default p (O.Column (O.Nullable (PgType a))) x
    ) => PP.Default p (Koln a) x where
  def = P.lmap unKoln PP.def
  {-# INLINE def #-}

instance
    ( P.Profunctor p, PgTyped a, PgTyped b
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

-------------------------------------------------------------------------------

-- | @'KolCoerce' a b@ says that @'Kol' a@ can be safely coerced to @'Kol' b@
-- using 'kolCoerce'.
class (PgTyped a, PgTyped b) => KolCoerce (a :: ka) (b :: kb) where
-- | Identity.
instance PgTyped a => KolCoerce a a

kolCoerce :: KolCoerce a b => Kol a -> Kol b
kolCoerce = unsafeCoerceKol

unsafeCoerceKol :: (PgTyped a, PgTyped b) => Kol a -> Kol b
unsafeCoerceKol = liftKol1 O.unsafeCoerceColumn

-------------------------------------------------------------------------------

-- | Satisfied if @a@ is one of 'Kol' or 'Koln'. We use this 'Constraint' to
-- restrict the instances that can exist for typeclasses like 'FromKol'.
type family KolLike a :: Constraint where
  KolLike Kol  = ()
  KolLike Koln = ()

class KolLike f => FromKol f where
  -- | Like 'fromKol', but generalizing the return type to either 'Kol' or
  -- 'Koln'.
  fromKol' :: PgTyped a => Kol a -> f a

-- | @'fromKol'' = 'id'@
instance FromKol Kol  where fromKol' = id
-- | @'fromKol'' = 'fromKol'@
instance FromKol Koln where fromKol' = fromKol

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

--------------------------------------------------------------------------------

-- | Column description.
--
-- This is only used as a promoted datatype expected to have kind
-- @'Col' 'GHC.Symbol' 'WD' 'RN' * *@.
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

type family Col_Name (col :: Col GHC.Symbol WD RN * *) :: GHC.Symbol where
  Col_Name ('Col n w r p h) = n
data Col_NameSym0 (col :: TyFun (Col GHC.Symbol WD RN * *) GHC.Symbol)
type instance Apply Col_NameSym0 col = Col_Name col

type family Col_PgType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgType ('Col n w r p h) = p
data Col_PgTypeSym0 (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply Col_PgTypeSym0 col = Col_PgType col

type family Col_PgRType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgRType ('Col n w 'R  p h) = Kol p
  Col_PgRType ('Col n w 'RN p h) = Koln p

type family Col_PgRNType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgRNType ('Col n w r p h) = Koln p

type family Col_PgWType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgWType ('Col n 'W  r p h) = Col_PgRType ('Col n 'W r p h)
  Col_PgWType ('Col n 'WD r p h) = WDef (Col_PgRType ('Col n 'WD r p h))

type family Col_HsRType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_HsRType ('Col n w 'R  p h) = h
  Col_HsRType ('Col n w 'RN p h) = Maybe h

type family Col_HsIType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_HsIType ('Col n 'W  r p h) = Col_HsRType ('Col n 'W r p h)
  Col_HsIType ('Col n 'WD r p h) = WDef (Col_HsRType ('Col n 'WD r p h))

---

-- | Lookup a column in @'Tabla' t@ by its name.
type Col_ByName t (c :: GHC.Symbol) = Col_ByName' c (Cols t)
type family Col_ByName' (name :: GHC.Symbol) (cols :: [Col GHC.Symbol WD RN * *]) :: Col GHC.Symbol WD RN * * where
  Col_ByName' n ('Col n  w r p h ': xs) = 'Col n w r p h
  Col_ByName' n ('Col n' w r p h ': xs) = Col_ByName' n xs

type HasColName t (c :: GHC.Symbol) =  HasColName' c (Cols t)
type family HasColName' (name :: GHC.Symbol) (cols :: [Col GHC.Symbol WD RN * *]) :: Constraint where
  HasColName' n ('Col n  w r p h ': xs) = ()
  HasColName' n ('Col n' w r p h ': xs) = HasColName' n xs

---

-- | Payload for @('HsR' t)@
type Cols_HsR t = List.Map (Col_HsRFieldSym1 t) (Cols t)
type Col_HsRField t (col :: Col GHC.Symbol WD RN * *)
  = Tagged (TC t (Col_Name col)) (Col_HsRType col)
data Col_HsRFieldSym1 t (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_HsRFieldSym1 t) col = Col_HsRField t col

-- | Payload for @('HsI' t)@
type Cols_HsI t = List.Map (Col_HsIFieldSym1 t) (Cols t)
type Col_HsIField t (col :: Col GHC.Symbol WD RN * *)
  = Tagged (TC t (Col_Name col)) (Col_HsIType col)
data Col_HsIFieldSym1 t (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_HsIFieldSym1 t) col = Col_HsIField t col

-- | Payload for @('PgR' t)@
type Cols_PgR t = List.Map (Col_PgRSym1 t) (Cols t)
type family Col_PgR t (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgR t ('Col n w r p h) = Tagged (TC t n) (Col_PgRType ('Col n w r p h))
data Col_PgRSym1 t (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_PgRSym1 t) col = Col_PgR t col

-- | Payload for @('PgRN' t)@
type Cols_PgRN t = List.Map (Col_PgRNSym1 t) (Cols t)
type family Col_PgRN t (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgRN t ('Col n w r p h) = Tagged (TC t n) (Col_PgRNType ('Col n w r p h))
data Col_PgRNSym1 t (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_PgRNSym1 t) col = Col_PgRN t col

-- | Type of the 'HL.Record' columns when inserting or updating a row. Also,
-- payload for @('PgW' t)@.
type Cols_PgW t = List.Map (Col_PgWSym1 t) (Cols t)
type family Col_PgW t (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgW t ('Col n w r p h) = Tagged (TC t n) (Col_PgWType ('Col n w r p h))
data Col_PgWSym1 t (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_PgWSym1 t) col = Col_PgW t col

--------------------------------------------------------------------------------

-- | Tag to be used alone or with 'Tagged' for uniquely identifying a specific
-- table in a specific schema.
data T (t :: k) = Tabla t => T

-- | Tag to be used alone or with 'Tagged' for uniquely identifying a specific
-- column in a specific table in a specific schema.
data TC (t :: k) (c :: GHC.Symbol) = Tabla t => TC

-- | Tag to be used alone or with 'Tagged' for uniquely identifying a specific
-- column in an unknown table.
data C (c :: GHC.Symbol) = C

--------------------------------------------------------------------------------

-- | All the representation of @t@ used within @opaleye-sot@ are @('Rec' t)@.
type Rec t xs = Tagged (T t) (HL.Record xs)

-- | Expected output type for 'O.runQuery' on a @('PgR' t)@.
--
-- Important: If you are expecting a @('PgR' t)@ on the right side
-- of a 'O.leftJoin', you will need to use @('Maybe' ('PgR' t))@.
--
-- Mnemonic: Haskell Read.
type HsR t = Rec t (Cols_HsR t)

-- | Output type of 'toHsI', used when inserting a new row to the table.
--
-- This type is used internally as an intermediate representation between
-- your own Haskell representation for a to-be-inserted @t@ and @('PgW' t)@.
--
-- Mnemonic: Haskell Insert.
type HsI t = Rec t (Cols_HsI t)

-- | Output type of @'queryTabla' ('T' t)@.
--
-- Mnemonic: PostGresql Read.
type PgR t = Rec t (Cols_PgR t)

-- | Like @('PgRN' t)@ but every field is 'Koln', as in the
-- output type of the right hand side of a 'O.leftJoin' with @'('table' t)@.
--
-- Mnemonic: PostGresql Read Nulls.
type PgRN t = Rec t (Cols_PgRN t)

-- | Representation of @('ToHsI' t)@ as 'Kols'. To be used when
-- writing to the database.
--
-- Mnemonic: PostGresql Write.
type PgW t = Rec t (Cols_PgW t)

--------------------------------------------------------------------------------

-- | All these constraints need to be satisfied by tools that work with 'Tabla'.
-- It's easier to just write all the constraints once here and make 'ITabla' a
-- superclass of 'Tabla'. Moreover, they enforce some sanity constraints on our
-- 'Tabla' so that we can get early compile time errors.
type ITabla t
  = ( GHC.KnownSymbol (SchemaName t)
    , GHC.KnownSymbol (TableName t)
    , All PgTyped (List.Map Col_PgTypeSym0 (Cols t))
    , HDistributeProxy (Cols t)
    , HL.HMapAux HList (FnCol_Props t) (List.Map ProxySym0 (Cols t)) (Cols_Props t)
    , HL.HMapAux HList (HL.HFmap FnPgWfromHsIField) (Cols_HsI t) (Cols_PgW t)
    , HL.HMapAux HList (HL.HFmap FnPgWfromPgRField) (Cols_PgR t) (Cols_PgW t)
    , HL.HRLabelSet (Cols_HsR t)
    , HL.HRLabelSet (Cols_HsI t)
    , HL.HRLabelSet (Cols_PgR t)
    , HL.HRLabelSet (Cols_PgRN t)
    , HL.HRLabelSet (Cols_PgW t)
    , HL.SameLength (Cols_Props t) (List.Map ProxySym0 (Cols t))
    , HL.SameLength (Cols_HsI t) (Cols_PgW t)
    , HL.SameLength (Cols_PgR t) (Cols_PgW t)
    , ProductProfunctorAdaptor O.TableProperties (HL.Record (Cols_Props t)) (HL.Record (Cols_PgW t)) (HL.Record (Cols_PgR t))
    , PP.Default OI.ColumnMaker (PgR t) (PgR t)
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
  -- | Some kind of unique identifier used for telling appart the database where
  -- this table exists from other databases, so as to avoid accidentally mixing
  -- tables from different databases in queries.
  type Database t :: *
  -- | PostgreSQL schema name where to find the table (defaults to @"public"@,
  -- PostgreSQL's default schema name).
  type SchemaName t :: GHC.Symbol
  type SchemaName t = "public"
  -- | Table name.
  type TableName t :: GHC.Symbol
  -- | Columns in this table. See the documentation for 'Col'.
  type Cols t :: [Col GHC.Symbol WD RN * *]

--------------------------------------------------------------------------------

-- | Convenience intended to be used within 'toHsI'', together with 'HL.hBuild'.
--
-- @
-- personToHsI (Person name age) = 'mkHsI' $ \\set_ -> 'HL.hBuild'
--     (set_ ('C' :: 'C' "name") name)
--     (set_ ('C' :: 'C' "age") age)
-- @

-- TODO: see if it is posisble to pack 'mkHsI'' and 'HL.hBuild' into
-- a single thing.
mkHsI
  :: (Tabla t, HL.HRearrange (HL.LabelsOf (Cols_HsI t)) xs (Cols_HsI t))
  => T t
  -> ((forall c a. (C c -> a -> Tagged (TC t c) a)) -> HList xs)
  -> HsI t -- ^
mkHsI (T::T t) k
  = Tagged
  $ HL.Record
  $ HL.hRearrange2 (Proxy :: Proxy (HL.LabelsOf (Cols_HsI t)))
  $ k (const Tagged)
{-# INLINE mkHsI #-}

--------------------------------------------------------------------------------

-- | Use with 'HL.ApplyAB' to apply convert a field in a
-- @('HList' ('Cols_HsI' t)@) to a field in a @('HList' ('Cols_PgW' t))@.
data FnPgWfromHsIField = FnPgWfromHsIField
instance HL.ApplyAB FnPgWfromHsIField x x where
  applyAB _ = id
instance (PgTyped b, PgType b ~ r, ToKol a r) => HL.ApplyAB FnPgWfromHsIField a (Kol b) where
  applyAB _ = kol
instance (PgTyped b, PgType b ~ r, ToKol a r) => HL.ApplyAB FnPgWfromHsIField (WDef a) (WDef (Kol b)) where
  applyAB _ = fmap kol
instance (PgTyped b, PgType b ~ r, ToKol a r) => HL.ApplyAB FnPgWfromHsIField (Maybe a) (Koln b) where
  applyAB _ = maybe nul koln
instance (PgTyped b, PgType b ~ r, ToKol a r) => HL.ApplyAB FnPgWfromHsIField (WDef (Maybe a)) (WDef (Koln b)) where
  applyAB _ = fmap (maybe nul koln)

--------------------------------------------------------------------------------

-- | Convert a custom Haskell type to a representation appropiate for /inserting/
-- it as a new row using 'Opaleye.SOT.Run.runInsert'.
pgWfromHsI :: Tabla t => HsI t -> PgW t
pgWfromHsI = Tagged . HL.hMap FnPgWfromHsIField . unTagged
{-# INLINE pgWfromHsI #-}

--------------------------------------------------------------------------------

-- | Use with 'HL.ApplyAB' to apply convert a field in a
-- @('HList' ('Cols_PgR' t)@) to a field in a @('HList' ('Cols_PgW' t))@.
data FnPgWfromPgRField = FnPgWfromPgRField
instance HL.ApplyAB FnPgWfromPgRField x x where
  applyAB _ = id
instance PgTyped a => HL.ApplyAB FnPgWfromPgRField (Kol a) (WDef (Kol a)) where
  applyAB _ = WVal
instance PgTyped a => HL.ApplyAB FnPgWfromPgRField (Koln a) (WDef (Koln a)) where
  applyAB _ = WVal

-- | Convert a @('PgR' t)@ resulting from a 'O.queryTable'-like operation
-- to a @('PgW' t)@ that can be used in a 'Opaleye.SOT.runUpdate'-like
-- operation.
pgWfromPgR :: Tabla t => PgR t -> PgW t
pgWfromPgR = Tagged . HL.hMap FnPgWfromPgRField . unTagged
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

-- | 'O.TableProperties' for all the columns in 'Tabla' @t@.
type Cols_Props t = List.Map (Col_PropsSym1 t) (Cols t)

-- | 'O.TableProperties' for a single column in 'Tabla' @t@.
type Col_Props t (col :: Col GHC.Symbol WD RN * *)
  = O.TableProperties (Col_PgW t col) (Col_PgR t col)
data Col_PropsSym1 t (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_PropsSym1 t) col = Col_Props t col
data Col_PropsSym0 (col :: TyFun t (TyFun (Col GHC.Symbol WD RN * *) * -> *))
type instance Apply Col_PropsSym0 t = Col_PropsSym1 t

class ICol_Props (col :: Col GHC.Symbol WD RN * *) where
  colProps :: Tabla t => Proxy t -> Proxy col -> Col_Props t col

-- | 'colProps' is equivalent 'colProps_wr'.
instance forall n p h. (GHC.KnownSymbol n, PgTyped p) => ICol_Props ('Col n 'W 'R p h) where
  colProps _ = \_ -> ppaUnTagged (colProps_wr (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wrn'.
instance forall n p h. (GHC.KnownSymbol n, PgTyped p) => ICol_Props ('Col n 'W 'RN p h) where
  colProps _ = \_ -> ppaUnTagged (colProps_wrn (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wdr'.
instance forall n p h. (GHC.KnownSymbol n, PgTyped p) => ICol_Props ('Col n 'WD 'R p h) where
  colProps _ = \_ -> ppaUnTagged (colProps_wdr (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wdrn'.
instance forall n p h. (GHC.KnownSymbol n, PgTyped p) => ICol_Props ('Col n 'WD 'RN p h) where
  colProps _ = \_ -> ppaUnTagged (colProps_wdrn (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}

-- | Use with 'HL.ApplyAB' to apply 'colProps' to each element of an 'HList'.
data FnCol_Props t = FnCol_Props

instance forall t (col :: Col GHC.Symbol WD RN * *) pcol out n w r p h
  . ( Tabla t
    , GHC.KnownSymbol n
    , ICol_Props col
    , pcol ~ Proxy col
    , col ~ 'Col n w r p h
    , out ~ Col_Props t col
    ) => HL.ApplyAB (FnCol_Props t) pcol out
    where
      applyAB _ = colProps (Proxy :: Proxy t)
      {-# INLINE applyAB #-}

--------------------------------------------------------------------------------

-- | Build the Opaleye 'O.Table' for a 'Tabla'.
table :: Tabla t => T t -> O.Table (PgW t) (PgR t)
table (T::T t) = O.TableWithSchema
   (GHC.symbolVal (Proxy :: Proxy (SchemaName t)))
   (GHC.symbolVal (Proxy :: Proxy (TableName t)))
   (ppaUnTagged $ ppa $ HL.Record
      (HL.hMapL (FnCol_Props :: FnCol_Props t)
      (hDistributeProxy (Proxy :: Proxy (Cols t)))))

-- | Like @opaleye@'s 'O.queryTable', but for a 'Tabla'.
queryTabla :: Tabla t => T t -> O.Query (PgR t)
queryTabla = O.queryTable . table

--------------------------------------------------------------------------------

-- | Lens to a column.
--
-- Mnemonic: The COLumn.
col :: forall t c xs xs' a a'
    .  HL.HLensCxt (TC t c) HL.Record xs xs' a a'
    => C c
    -> Lens (Rec t xs) (Rec t xs') a a'
col _ = _Wrapped . HL.hLens (HL.Label :: HL.Label (TC t c))
{-# INLINE col #-}

-- | Like 'col', but the column is tagged with 'TC'.
--
-- Mnemonic: the COLumn, Tagged.
--
-- TODO: Do we really need this? Can it be removed?
colt :: forall t c xs xs' a a'
      . HL.HLensCxt (TC t c) HL.Record xs xs' a a'
     => C c
     -> Lens (Rec t xs) (Rec t xs') (Tagged (TC t c) a) (Tagged (TC t c) a')
colt prx = col prx . _Unwrapped
{-# INLINE colt #-}

--------------------------------------------------------------------------------

-- | Like 'Prelude.bool', @'matchBool' f t x@ evaluates to @f@ if @x@ is false,
-- otherwise it evaluates to @t@.
matchBool
  :: (Op3 kol kol' kol'' kol''', PgTyped a)
  => kol a -> kol' a -> kol'' O.PGBool -> kol''' a
matchBool f t x = liftKol3 matchBool' f t x
  where matchBool' = \f' t' x' -> O.ifThenElse x' t' f'

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
lnot :: Op1' kol => kol O.PGBool -> kol O.PGBool
lnot = liftKol1 O.not

-- | Logical OR. See 'eq' for possible argument types.
lor :: Op2 kol kol' kol'' => kol O.PGBool -> kol' O.PGBool -> kol'' O.PGBool
lor = liftKol2 (O..||)

-- | Whether any of the given 'O.PGBool's is true.
--
-- Notice that 'lor' is more general that 'lors', as it doesn't restrict @kol@.
--
-- Mnemonic reminder: Logical ORs.
lors :: (Op2' kol, FromKol kol, Foldable f) => f (kol O.PGBool) -> kol O.PGBool
lors = foldl' lor (fromKol' (kol False))

-- Logical AND. See 'eq' for possible argument types.
land :: Op2 kol kol' kol'' => kol O.PGBool -> kol' O.PGBool -> kol'' O.PGBool
land = liftKol2 (O..&&)

-- | Whether all of the given 'O.PGBool's are true.
--
-- Notice that 'land' is more general that 'lands', as it doesn't restrict
-- @kol@.
--
-- Mnemonic reminder: Logical ANDs.
lands :: (Op2' kol, FromKol kol, Foldable f) => f (kol O.PGBool) -> kol O.PGBool
lands = foldl' land (fromKol' (kol True))

--------------------------------------------------------------------------------
-- Equality

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
eq :: (PgTyped a, Op2 kol kol' kol'') => kol a -> kol' a -> kol'' O.PGBool
eq = liftKol2 (O..==)

-- | Whether the given value is a member of the given collection.
--
-- Notice that a combination 'eq' and 'or' is more general that 'member', as
-- they don't restrict @kol'@.
member
  :: (PgTyped a, Op2 kol kol' kol', Op2' kol', FromKol kol', Foldable f)
  => kol a -> f (kol' a) -> kol' O.PGBool -- ^
member a = lors . map (eq a) . toList

--------------------------------------------------------------------------------
-- Ordering

-- | Whether the first argument is less than the second. See 'eq' for possible
-- argument types.
--
-- Mnemonic reminder: Less Than.
lt :: (Op2 kol kol' kol'', PgOrd a) => kol a -> kol' a -> kol'' O.PGBool
lt = liftKol2 (O..<)

-- | Whether the first argument is less than or equal to the second. See 'eq'
-- for possible argument types.
--
-- Mnemonic reminder: Less Than or Equal.
lte :: (Op2 kol kol' kol'', PgOrd a) => kol a -> kol' a -> kol'' O.PGBool
lte = liftKol2 (O..<=)

-- | Whether the first argument is greater than the second. See 'eq' for
-- possible argument types.
--
-- Mnemonic reminder: Greater Than.
gt :: (Op2 kol kol' kol'', PgOrd a) => kol a -> kol' a -> kol'' O.PGBool
gt = liftKol2 (O..>)

-- | Whether the first argument is greater than or equal to the second. See 'eq'
-- for possible argument types.
--
-- Mnemonic reminder: Greater Than or Equal.
gte :: (Op2 kol kol' kol'', PgOrd a) => kol a -> kol' a -> kol'' O.PGBool
gte = liftKol2 (O..>=)

--------------------------------------------------------------------------------

-- | Look up a 'Kol' inside some kind of wrapper.
--
-- This class makes it possible to accept both @('Kol' a)@ and
-- @('Tagged' ('TC' t c) ('Kol' a))@ as arguments in various @opaleye-sot@
-- functions.
class PgTyped a => GetKol w a | w -> a where getKol :: w -> Kol a
-- | Identity.
instance PgTyped a => GetKol (Kol a) a where getKol = id
instance PgTyped a => GetKol (Tagged (TC t c) (Kol a)) a where getKol = unTagged

-- | Look up a 'Koln' inside some kind of wrapper.
--
-- This class makes it possible to accept both @('Koln' a)@ and
-- @('Tagged' ('TC' t c) ('Koln' a))@ as arguments in various @opaleye-sot@
-- functions.
class PgTyped a => GetKoln w a | w -> a where getKoln :: w -> Koln a
-- | Identity.
instance PgTyped a => GetKoln (Koln a) a where getKoln = id
instance PgTyped a => GetKoln (Tagged (TC t c) (Koln a)) a where getKoln = unTagged

--------------------------------------------------------------------------------

-- | Like Opaleye's 'O.isNull', but works for any 'GetKoln'.
isNull :: GetKoln w a => w -> Kol O.PGBool
isNull = Kol . O.isNull . unKoln . getKoln

-- | Flatten @('Koln' 'O.PGBool')@ or compatible (see 'GetKoln') to
-- @('Kol' 'O.PGBool')@. An outer @NULL@ is converted to @TRUE@.
--
-- This can be used as a function or as a 'O.QueryArr', whatever works best
-- for you. The 'O.QueryArr' support is often convenient when working with
-- 'restrict':
--
-- @
-- 'restrict' '<<<' 'nullTrue' -< ...
-- @
--
-- Simplified types:
--
-- @
-- 'nullTrue' :: 'Koln' 'O.PGBool' -> 'Kol' 'O.PGBool'
-- 'nullTrue' :: 'Koln' ('Tagged' ('TC' t c) 'O.PGBool') -> 'Kol' 'O.PGBool'
-- 'nullTrue' :: 'O.QueryArr' ('Koln' 'O.PGBool') ('Kol' 'O.PGBool')
-- 'nullTrue' :: 'O.QueryArr' ('Koln' ('Tagged' ('TC' t c) 'O.PGBool')) ('Kol' 'O.PGBool')
-- @
nullTrue :: (Arrow f, GetKoln w O.PGBool) => f w (Kol O.PGBool)
nullTrue = arr $ matchKoln (kol True) id . getKoln

-- | Like 'nullTrue', but an outer @NULL@ is converted to @FALSE@.
nullFalse :: (Arrow f, GetKoln w O.PGBool) => f w (Kol O.PGBool)
nullFalse = arr $ matchKoln (kol False) id . getKoln

-- | Like Opaleye's 'O.restric', but takes a 'Kol' as input.
--
-- @
-- 'restrict' :: 'O.QueryArr' ('Kol' 'O.PGBool') ()
-- 'restrict' :: 'O.QueryArr' ('Kol' ('Tagged' ('TC' t c) 'O.PGBool')) ()
-- @
restrict :: GetKol w O.PGBool => O.QueryArr w ()
restrict = O.restrict <<^ unKol <<^ getKol

-- | Like Opaleye's 'O.leftJoin', but the predicate is expected to
-- return a @('GetKol' w 'O.PGBool')@.
leftJoin
  :: ( PP.Default O.Unpackspec a a
     , PP.Default O.Unpackspec b b
     , PP.Default OI.NullMaker b nb
     , GetKol gkb O.PGBool )
   => O.Query a -> O.Query b -> ((a, b) -> gkb) -> O.Query (a, nb) -- ^
leftJoin = leftJoinExplicit PP.def PP.def PP.def

-- | Like Opaleye's 'O.leftJoinExplicit', but the predicate is expected to
-- return a @('GetKol' w 'O.PGBool')@.
leftJoinExplicit
  :: GetKol gkb O.PGBool
  => O.Unpackspec a a -> O.Unpackspec b b -> OI.NullMaker b nb
  -> O.Query a -> O.Query b -> ((a, b) -> gkb) -> O.Query (a, nb) -- ^
leftJoinExplicit ua ub nmb qa qb fil =
  O.leftJoinExplicit ua ub nmb qa qb (unKol . getKol . fil)

--------------------------------------------------------------------------------
-- Ordering

-- | Ascending order, no @NULL@s involved.
asc :: (GetKol w b, O.PGOrd (PgType b)) => (a -> w) -> O.Order a
asc f = O.asc (unKol . getKol . f)

-- | Ascending order, @NULL@s last.
ascnl :: (GetKoln w b, O.PGOrd (PgType b)) => (a -> w) -> O.Order a
ascnl f = O.asc (unsafeUnNullableColumn . unKoln . getKoln . f)

-- | Ascending order, @NULL@s first.
ascnf :: (GetKoln w b, O.PGOrd (PgType b)) => (a -> w) -> O.Order a
ascnf f = O.ascNullsFirst (unsafeUnNullableColumn . unKoln . getKoln . f)

-- | Descending order, no @NULL@s involved.
desc :: (GetKol w b, O.PGOrd (PgType b)) => (a -> w) -> O.Order a
desc f = O.desc (unKol . getKol . f)

-- | Descending order, @NULL@s first.
descnf :: (GetKoln w b, O.PGOrd (PgType b)) => (a -> w) -> O.Order a
descnf f = O.desc (unsafeUnNullableColumn . unKoln . getKoln . f)

-- | Descending order, @NULL@s last.
descnl :: (GetKoln w b, O.PGOrd (PgType b)) => (a -> w) -> O.Order a
descnl f = O.descNullsLast (unsafeUnNullableColumn . unKoln . getKoln . f)

--------------------------------------------------------------------------------

ppaUnTagged :: P.Profunctor p => p a b -> p (Tagged ta a) (Tagged tb b)
ppaUnTagged = P.dimap unTagged Tagged
{-# INLINE ppaUnTagged #-}

-- | A generalization of product profunctor adaptors such as 'PP.p1', 'PP.p4', etc.
--
-- The functional dependencies make type inference easier, but also forbid some
-- otherwise acceptable instances.
class P.Profunctor p => ProductProfunctorAdaptor p l ra rb | p l -> ra rb, p ra rb -> l where
  ppa :: l -> p ra rb

-- | 'HList' of length 0.
instance PP.ProductProfunctor p => ProductProfunctorAdaptor p (HList '[]) (HList '[]) (HList '[]) where
  ppa = const (P.dimap (const ()) (const HNil) PP.empty)
  {-# INLINE ppa #-}

-- | 'HList' of length 1 or more.
instance
    ( PP.ProductProfunctor p
    , ProductProfunctorAdaptor p (HList pabs) (HList as) (HList bs)
    ) => ProductProfunctorAdaptor p (HList (p a1 b1 ': pabs)) (HList (a1 ': as)) (HList (b1 ': bs)) where
  ppa = \(HCons pab1 pabs) -> P.dimap (\(HCons x xs) -> (x,xs)) (uncurry HCons) (pab1 PP.***! ppa pabs)

instance
    ( ProductProfunctorAdaptor p (HList pabs) (HList as) (HList bs)
    ) => ProductProfunctorAdaptor p (HL.Record pabs) (HL.Record as) (HL.Record bs) where
  ppa = P.dimap (\(HL.Record x) -> x) HL.Record . ppa . (\(HL.Record x) -> x)
  {-# INLINE ppa #-}

--------------------------------------------------------------------------------

-- | Orphan. 'Opaleye.SOT.Internal'.
instance (PP.ProductProfunctor p, PP.Default p a b) => PP.Default p (Tagged ta a) (Tagged tb b) where
  def = ppaUnTagged PP.def
  {-# INLINE def #-}

-- | Orphan. 'Opaleye.SOT.Internal'.
instance PP.ProductProfunctor p => PP.Default p (HList '[]) (HList '[]) where
  def = ppa HNil
  {-# INLINE def #-}

-- | Orphan. 'Opaleye.SOT.Internal'.
instance
    ( PP.ProductProfunctor p, PP.Default p a1 b1, PP.Default p (HList as) (HList bs)
    ) => PP.Default p (HList (a1 ': as)) (HList (b1 ': bs)) where
  def = P.dimap (\(HCons x xs) -> (x,xs)) (uncurry HCons) (PP.def PP.***! PP.def)

-- | Orphan. 'Opaleye.SOT.Internal'.
instance
    ( PP.ProductProfunctor p, PP.Default p (HList as) (HList bs)
    ) => PP.Default p (HL.Record as) (HL.Record bs) where
  def = P.dimap (\(HL.Record x) -> x) HL.Record PP.def
  {-# INLINE def #-}

-- Maybes on the rhs

-- | Orphan. 'Opaleye.SOT.Internal'.
instance
    ( PP.ProductProfunctor p, PP.Default p a (Maybe b)
    ) => PP.Default p (Tagged ta a) (Maybe (Tagged tb b)) where
  def = P.dimap unTagged (fmap Tagged) PP.def
  {-# INLINE def #-}

-- | Orphan. 'Opaleye.SOT.Internal'. Defaults to 'Just'.
instance PP.ProductProfunctor p => PP.Default p (HList '[]) (Maybe (HList '[])) where
  def = P.rmap Just PP.def
  {-# INLINE def #-}

-- | Orphan. 'Opaleye.SOT.Internal'.
instance
    ( PP.ProductProfunctor p
    , PP.Default p a (Maybe b)
    , PP.Default p (HList as) (Maybe (HList bs))
    ) => PP.Default p (HList (a ': as)) (Maybe (HList (b ': bs))) where
  def = P.dimap (\(HCons a as) -> (a, as))
                (\(mb, mbs) -> HCons <$> mb <*> mbs)
                (PP.def PP.***! PP.def)

-- | Orphan. 'Opaleye.SOT.Internal'.
instance
    ( PP.ProductProfunctor p, PP.Default p (HList as) (Maybe (HList bs))
    ) => PP.Default p (HL.Record as) (Maybe (HL.Record bs)) where
  def = P.dimap (\(HL.Record x) -> x) (fmap HL.Record) PP.def
  {-# INLINE def #-}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Support for overloaded unary operators working on Kol or Koln

class (Op1_ Kol Kol g g) => Op1' g
instance (Op1_ Kol Kol g g) => Op1' g

class (Op1_ Kol Kol ga gb) => Op1 ga gb
instance (Op1_ Kol Kol ga gb) => Op1 ga gb

-- | Instances of this class can be used to convert an unary function @fa a ->
-- fb b@ to an unary function @ga a -> gb b@, where @fa@, @fb@, and @ga@ can be
-- any combination of 'Kol' or 'Koln', with @gb@ fully determined by them,
-- biasing towards 'Kol'.
--
-- Thanks to the 'KolLike' superclass and the functional dependency in this
-- class, it is impossible to add new 'Op1_' instances. All the possible
-- instances are already exported from this module.
class (KolLike fa, KolLike fb, KolLike ga, KolLike gb) =>
      Op1_ fa fb ga gb | fa fb ga -> gb where
  -- | Convert an unary function @fa a -> fb b@ to an unary function @ga a -> gb
  -- b@, where @fa@, @fb@, and @ga@ can be any combination of 'Kol' or 'Koln',
  -- with @gb@ fully determined by them.
  op1 :: (PgTyped a, PgTyped b) => (fa a -> fb b) -> (ga a -> gb b)

instance Op1_ Kol  Kol  Kol  Kol  where op1 f ga = f ga
instance Op1_ Kol  Kol  Koln Koln where op1 f ga = mapKoln f ga
-- The 'Op1_' instances above are sufficient for the tools offered by this
-- module. Do we need to export the following ones?
--
-- instance Op1_ Kol  Koln Kol  Koln where op1 f ga = f ga
-- instance Op1_ Kol  Koln Koln Koln where op1 f ga = bindKoln ga f
-- instance Op1_ Koln Kol  Kol  Kol  where op1 f ga = f (fromKol ga)
-- instance Op1_ Koln Kol  Koln Kol  where op1 f ga = f ga
-- instance Op1_ Koln Koln Kol  Koln where op1 f ga = f (fromKol ga)
-- instance Op1_ Koln Koln Koln Koln where op1 f ga = f ga

---

class (Op2_ Kol Kol Kol g g g) => Op2' g
instance (Op2_ Kol Kol Kol g g g) => Op2' g

class (Op2_ Kol Kol Kol ga gb gc) => Op2 ga gb gc
instance (Op2_ Kol Kol Kol ga gb gc) => Op2 ga gb gc

-- | Instances of this class can be used to convert a binary function @fa a ->
-- fb b -> fc c@ to a binary function @ga a -> gb b -> gc c@, where @fa@, @fb@,
-- @fc@, @ga@ and @gb@ can be any combination of 'Kol' or 'Koln', with @gc@
-- fully determined by them, biasing towards 'Kol'.
--
-- Thanks to the 'KolLike' superclass and the functional dependency in this
-- class, it is impossible to add new 'Op2_' instances. All the possible
-- instances are already exported from this module.
class (KolLike fa, KolLike fb, KolLike fc) =>
      Op2_ fa fb fc ga gb gc | fa fb fc ga gb -> gc where
  -- | Convert a binary function @fa a -> fb b -> fc c@ to a binary function
  -- @ga a -> gb b -> gc c@, where @fa@, @fb@, @fc@, @ga@ and @gb@ can be any
  -- combination of 'Kol' or 'Koln', with @gc@ fully determined by them.
  op2 :: (PgTyped a, PgTyped b, PgTyped c)
      => (fa a -> fb b -> fc c)
      -> (ga a -> gb b -> gc c)

instance Op2_ Kol  Kol  Kol  Kol  Kol  Kol  where op2 f ga gb = f ga gb
instance Op2_ Kol  Kol  Kol  Kol  Koln Koln where op2 f ga gb = mapKoln (f ga) gb
instance Op2_ Kol  Kol  Kol  Koln Kol  Koln where op2 f ga gb = mapKoln (flip f gb) ga
instance Op2_ Kol  Kol  Kol  Koln Koln Koln where op2 f ga gb = bindKoln ga (flip mapKoln gb . f)

-- The 'Op2_' instances above are sufficient for the tools offered by this
-- module. Do we need to export the following ones?
--
-- instance Op2_ Kol  Kol  Koln Kol  Kol  Koln where op2 f ga gb = f ga gb
-- instance Op2_ Kol  Kol  Koln Kol  Koln Koln where op2 f ga gb = bindKoln gb (f ga)
-- instance Op2_ Kol  Kol  Koln Koln Kol  Koln where op2 f ga gb = bindKoln ga (flip f gb)
-- instance Op2_ Kol  Kol  Koln Koln Koln Koln where op2 f ga gb = bindKoln ga (bindKoln gb . f)
-- instance Op2_ Kol  Koln Kol  Kol  Kol  Kol  where op2 f ga gb = f ga (fromKol gb)
-- instance Op2_ Kol  Koln Kol  Kol  Koln Kol  where op2 f ga gb = f ga gb
-- instance Op2_ Kol  Koln Kol  Koln Kol  Koln where op2 f ga gb = mapKoln (flip f (fromKol gb)) ga
-- instance Op2_ Kol  Koln Kol  Koln Koln Koln where op2 f ga gb = mapKoln (flip f gb) ga
-- instance Op2_ Kol  Koln Koln Kol  Kol  Koln where op2 f ga gb = f ga (fromKol gb)
-- instance Op2_ Kol  Koln Koln Kol  Koln Koln where op2 f ga gb = f ga gb
-- instance Op2_ Kol  Koln Koln Koln Kol  Koln where op2 f ga gb = bindKoln ga (flip f (fromKol gb))
-- instance Op2_ Kol  Koln Koln Koln Koln Koln where op2 f ga gb = bindKoln ga (flip f gb)
-- instance Op2_ Koln Kol  Kol  Kol  Kol  Kol  where op2 f ga gb = f (fromKol ga) gb
-- instance Op2_ Koln Kol  Kol  Kol  Koln Koln where op2 f ga gb = mapKoln (f (fromKol ga)) gb
-- instance Op2_ Koln Kol  Kol  Koln Kol  Kol  where op2 f ga gb = f ga gb
-- instance Op2_ Koln Kol  Kol  Koln Koln Koln where op2 f ga gb = mapKoln (f ga) gb
-- instance Op2_ Koln Kol  Koln Kol  Kol  Koln where op2 f ga gb = f (fromKol ga) gb
-- instance Op2_ Koln Kol  Koln Kol  Koln Koln where op2 f ga gb = bindKoln gb (f (fromKol ga))
-- instance Op2_ Koln Kol  Koln Koln Kol  Koln where op2 f ga gb = f ga gb
-- instance Op2_ Koln Kol  Koln Koln Koln Koln where op2 f ga gb = bindKoln gb (f ga)
-- instance Op2_ Koln Koln Kol  Kol  Kol  Kol  where op2 f ga gb = f (fromKol ga) (fromKol gb)
-- instance Op2_ Koln Koln Kol  Kol  Koln Kol  where op2 f ga gb = f (fromKol ga) gb
-- instance Op2_ Koln Koln Kol  Koln Kol  Kol  where op2 f ga gb = f ga (fromKol gb)
-- instance Op2_ Koln Koln Kol  Koln Koln Kol  where op2 f ga gb = f ga gb
-- instance Op2_ Koln Koln Koln Kol  Kol  Koln where op2 f ga gb = f (fromKol ga) (fromKol gb)
-- instance Op2_ Koln Koln Koln Kol  Koln Koln where op2 f ga gb = f (fromKol ga) gb
-- instance Op2_ Koln Koln Koln Koln Kol  Koln where op2 f ga gb = f ga (fromKol gb)
-- instance Op2_ Koln Koln Koln Koln Koln Koln where op2 f ga gb = f ga gb

---
class (Op3_ Kol Kol Kol Kol g g g g) => Op3' g
instance (Op3_ Kol Kol Kol Kol g g g g) => Op3' g

class (Op3_ Kol Kol Kol Kol ga gb gc gd) => Op3 ga gb gc gd
instance (Op3_ Kol Kol Kol Kol ga gb gc gd) => Op3 ga gb gc gd

-- | Instances of this class can be used to convert a ternary function @fa a ->
-- fb b -> fc c -> fd d@ to a ternary function @ga a -> gb b -> gc c -> gd d@,
-- where @fa@, @fb@, @fc@, @fd@, @ga@, @gb@ and @gc@ can be any combination of
-- 'Kol' or 'Koln', with @gd@ fully determined by them, biasing towards 'Kol'.
--
-- Thanks to the 'KolLike' superclass and the functional dependency in this
-- class, it is impossible to add new 'Op3_' instances. All the possible
-- instances are already exported from this module.
class (KolLike fa, KolLike fb, KolLike fc, KolLike fd) =>
      Op3_ fa fb fc fd ga gb gc gd | fa fb fc fd ga gb gc -> gd where
  -- | Convert a ternary function @fa a -> fb b -> fc c -> fd d@ to a ternary
  -- function @ga a -> gb b -> gc c -> gd d@, where @fa@, @fb@, @fc@, @fd@,
  -- @ga@, @gb@ and @gc@ can be any combination of 'Kol' or 'Koln', with @gd@
  -- fully determined by them.
  op3 :: (PgTyped a, PgTyped b, PgTyped c, PgTyped d)
      => (fa a -> fb b -> fc c -> fd d)
      -> (ga a -> gb b -> gc c -> gd d)

instance Op3_ Kol Kol Kol Kol  Kol  Kol  Kol  Kol  where op3 f fa fb fc = f fa fb fc
instance Op3_ Kol Kol Kol Kol  Kol  Kol  Koln Koln where op3 f fa fb gc = bindKoln gc (\fc -> fromKol (f fa fb fc))
instance Op3_ Kol Kol Kol Kol  Kol  Koln Kol  Koln where op3 f fa gb fc = bindKoln gb (\fb -> fromKol (f fa fb fc))
instance Op3_ Kol Kol Kol Kol  Kol  Koln Koln Koln where op3 f fa gb gc = bindKoln gb (\fb -> bindKoln gc (\fc -> fromKol (f fa fb fc)))
instance Op3_ Kol Kol Kol Kol  Koln Kol  Kol  Koln where op3 f ga fb fc = bindKoln ga (\fa -> fromKol (f fa fb fc))
instance Op3_ Kol Kol Kol Kol  Koln Kol  Koln Koln where op3 f ga fb gc = bindKoln ga (\fa -> bindKoln gc (\fc -> fromKol (f fa fb fc)))
instance Op3_ Kol Kol Kol Kol  Koln Koln Kol  Koln where op3 f ga gb fc = bindKoln ga (\fa -> bindKoln gb (\fb -> fromKol (f fa fb fc)))
instance Op3_ Kol Kol Kol Kol  Koln Koln Koln Koln where op3 f ga gb gc = bindKoln ga (\fa -> bindKoln gb (\fb -> bindKoln gc (\fc -> fromKol (f fa fb fc))))
-- The 'Op3_' instances above are sufficient for the tools offered by this
-- module. Do we need to export more?

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This belongs in Opaleye

unsafeUnNullableColumn :: O.Column (O.Nullable a) -> O.Column a
unsafeUnNullableColumn = O.unsafeCoerceColumn

pgFloat4 :: Float -> O.Column O.PGFloat4
pgFloat4 = OI.literalColumn . OI.DoubleLit . float2Double

pgFloat8 :: Float -> O.Column O.PGFloat8
pgFloat8 = OI.literalColumn . OI.DoubleLit . float2Double

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.QueryRunnerColumnDefault O.PGFloat4 Float where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.PGFractional O.PGFloat4 where
  pgFromRational = pgFloat4 . fromRational

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.PGNum O.PGFloat4 where
  pgFromInteger = pgFloat4 . fromInteger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Misc

-- | Apply a same constraint to all the types in the list.
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

---

-- | Defunctionalized 'Proxy'. To be used with 'Apply'.
data ProxySym0 (a :: TyFun k *)
type instance Apply ProxySym0 a = Proxy a

class HDistributeProxy (xs :: [k]) where
  hDistributeProxy :: Proxy xs -> HList (List.Map ProxySym0 xs)
instance HDistributeProxy ('[] :: [k]) where
  hDistributeProxy _ = HNil
  {-# INLINE hDistributeProxy #-}
instance forall (x :: k) (xs :: [k]). HDistributeProxy xs => HDistributeProxy (x ': xs) where
  hDistributeProxy _ = HCons (Proxy :: Proxy x) (hDistributeProxy (Proxy :: Proxy xs))

