{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is an internal module. You are very discouraged from using it directly.
module Opaleye.SOT.Internal.Koln
  ( Koln(..)
  , koln
  , nul
  , fromKol
  , fromKoln
  , matchKoln
  , mapKoln
  , forKoln
  , liftKoln2
  , bindKoln
  , altKoln
  , isNull
  , kolArrayn
  ) where

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as PP
import           Data.Proxy (Proxy(..))
import           Data.Foldable
import qualified GHC.TypeLits as GHC
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as OI
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HDB
import qualified Opaleye.Internal.RunQuery as OI

import Opaleye.SOT.Internal.Kol
  (Kol(..), PgTyped(..), ToKol(..), PGArrayn, pgPrimTypeName)

--------------------------------------------------------------------------------

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

-- | Build a 'Koln' from a Haskell term, where 'Nothing' means @NULL@.
koln :: ToKol a b => Maybe a -> Koln b
koln = maybe nul (fromKol . kol)

-- | PostgreSQL's @NULL@ value. This is like the 'Nothing' constructor for
-- 'Maybe'
nul :: PgTyped a => Koln a
nul = Koln O.null

-- | Convert a 'Kol' to a 'Koln'.
fromKol :: PgTyped a => Kol a -> Koln a
fromKol = Koln . O.toNullable . unKol

-- | Convert a 'Koln' to a 'Kol'.
--
-- This function behaves as 'Data.Maybe.fromMaybe'.
--
-- @
-- 'fromKoln' ka na == 'matchKoln' ka id na
-- @
fromKoln :: PgTyped a => Kol a -> Koln a -> Kol a
fromKoln ka0 = matchKoln ka0 id

--- | Case analysis for 'Koln'.
--
-- This function behaves as 'Prelude.maybe' for 'Maybe': If @'Koln' a@ is
-- @NULL@, then evaluate to the first argument, otherwise it applies the given
-- function to the @'Kol' a@ underlying the given @'Koln' a@.
matchKoln
  :: (PgTyped a, PgTyped b) => Kol b -> (Kol a -> Kol b) -> Koln a -> Kol b
matchKoln kb0 f na =
  Kol (O.matchNullable (unKol kb0) (unKol . f . Kol) (unKoln na))

-- | Like 'fmap' for 'Maybe'.
--
-- Apply the given function to the underlying @('Kol' a)@ only as long as the
-- given @('Koln' a)@ is not @NULL@, otherwise, evaluates to @NULL@.
mapKoln :: (PgTyped a, PgTyped b) => (Kol a -> Kol b) -> Koln a -> Koln b
mapKoln f kna = bindKoln kna (fromKol . f)

-- | 'mapKoln' with the arguments flipped.
forKoln :: (PgTyped a, PgTyped b) => Koln a -> (Kol a -> Kol b) -> Koln b
forKoln kna f = mapKoln f kna

liftKoln2
  :: (PgTyped a, PgTyped b, PgTyped c)
  => (Kol a -> Kol b -> Kol c)
  -> (Koln a -> Koln b -> Koln c)
liftKoln2 f = \na nb ->
  bindKoln na (\ka -> bindKoln nb (\kb -> fromKol (f ka kb)))

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

-- | Whether a 'Koln' is 'nul' (@NULL@).
isNull :: Koln a -> Kol O.PGBool
isNull = Kol . O.isNull . unKoln

--------------------------------------------------------------------------------

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

-- | OVERLAPPABLE. Orphan in "Opaleye.SOT.Internal.Koln".
instance {-# OVERLAPPABLE #-}
    ( O.QueryRunnerColumnDefault pg hs
    ) => O.QueryRunnerColumnDefault pg (Maybe hs) where
  queryRunnerColumnDefault = OI.QueryRunnerColumn u (fmap (fmap (fmap Just)) fp)
    where OI.QueryRunnerColumn u fp = O.queryRunnerColumnDefault

---

-- | Behaves as the 'Monoid' instance for 'Maybe'.
instance (PgTyped a, Monoid (Kol a)) => Monoid (Koln a) where
  mempty = fromKol mempty
  mappend = liftKoln2 mappend

---

instance {-# OVERLAPPING #-}
  GHC.TypeError ('GHC.Text "The Num instance for " 'GHC.:<>: 'GHC.ShowType (Koln a) 'GHC.:<>: 'GHC.Text " is explicitely disabled.")
    => Num (Koln a) where
  fromInteger = undefined
  (*) = undefined
  (+) = undefined
  (-) = undefined
  abs = undefined
  negate = undefined
  signum = undefined

---

instance forall a b. ToKol a b => ToKol [Maybe a] (PGArrayn b) where
  kol = kolArrayn . map (koln :: Maybe a -> Koln b)

-- | Build a @'Kol' ('PGArrayn' x)@ from any 'Foldable'.
--
-- The return type is not fixed to @'Kol' ('PGArrayn' x)@ so that you can
-- easily use 'kolArrayn' as part of the implementation for 'kol' (see instance
-- @'ToKol' ['Maybe' a] ('PGArrayn' p)@ as an example of this).
kolArrayn
 :: forall f a as
 .  (Foldable f, PgTyped as, PgType as ~ PGArrayn (PgType a))
 => f (Koln a) -> Kol as
kolArrayn xs = Kol $ O.unsafeCast
   (pgPrimTypeName (Proxy :: Proxy (PGArrayn (PgType a))))
   (OI.Column (HDB.ArrayExpr (map (OI.unColumn . unKoln) (toList xs))))

