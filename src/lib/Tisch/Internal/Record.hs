{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A @'Record' (xs :: [(k,'Type')])@ is analogous to a
-- @'Data.HList.HList' (ys :: ['Type'])@ where each of the elements in @ys@ is a
-- @'Tagged' (a :: k) (b :: Type)@.
--
-- We use 'Record' here is a rather ad-hoc implementation of a map carrying
-- information about the keys and the values at the type level. It is not super
-- efficient (i.e., it is a map built on top of asequential list, not a tree),
-- but we don't expect this to be a huge problem since we use this to represent
-- database rows, which usually don't have a huge number of tables.
--
-- Notice that none of the operations in this module ensure that the keys in a
-- 'Record' are unique, nor that operations such as mapping over the 'Record'
-- preserves its length. This is for performance reasons, and it is not a
-- problem insofar as "Tisch.Internal" is concerned.
--
-- Much of this code comes from the HList package (MIT licensed, by Oleg
-- Kiselyov, Ralf Laemmel, Keean Schupke). Thanks to them for their work on
-- HList.

module Tisch.Internal.Record where

import           Control.Lens (Lens)
import           Data.Kind
import           Data.Void
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as PP
import           Data.Proxy
import           Data.Tagged
import           GHC.TypeLits (Symbol)
import           Tisch.Internal.Profunctors as PP

--------------------------------------------------------------------------------
data Record :: [(k, Type)] -> Type where
  RNil :: Record '[]
  RCons :: Tagged a b -> Record abs -> Record ('(a, b) ': abs)

instance Show (Record '[]) where
  show RNil = "RNil"
instance (Show b, Show (Record abs)) => Show (Record ('(a, b) ': abs)) where
  show (RCons tab rabs) = "RCons (" ++ show tab ++ ") (" ++ show rabs ++ ")"

--------------------------------------------------------------------------------
type family RecordKeys (abs :: [(k, Type)]) :: [k] where
  RecordKeys ('(a,b) ': abs) = (a ': RecordKeys abs)
  RecordKeys '[] = '[]

--------------------------------------------------------------------------------
-- | Like 'Data.HList.ApplyAB' from "Data.HList".
class ApplyAB f a b where
  applyAB :: f -> a -> b

---
-- | @'applyAB' ('FnMap' f) xs === map f xs@
newtype FnMap f = FnMap f
instance (RMap f x y) => ApplyAB (FnMap f) (Record x) (Record y) where
  applyAB (FnMap f) rx = rMapAux f rx
  {-# INLINE applyAB #-}

rMap :: (RMap f x y) => f -> Record x -> Record y
rMap f rx = applyAB (FnMap f) rx
{-# INLINE rMap #-}

class (RecordKeys x ~ RecordKeys y) => RMap f x y where
  rMapAux :: f -> Record x -> Record y

instance RMap f '[] '[] where
  rMapAux _ _ = RNil
  {-# INLINE rMapAux #-}

instance
    ( ApplyAB f b b', RMap f abs abs'
    ) => RMap f ('(a, b) ': abs) ('(a, b') ': abs') where
  rMapAux f (RCons (Tagged x) l) = RCons (Tagged (applyAB f x)) (rMapAux f l)

--------------------------------------------------------------------------------
-- 'PP.Default' instances

instance PP.ProductProfunctor p => PP.Default p (Record '[]) (Record '[]) where
  def = P.dimap (const ()) (const RNil) PP.empty
  {-# INLINE def #-}

-- | Defaults to 'Just'.
instance PP.ProductProfunctor p => PP.Default p (Record '[]) (Maybe (Record '[]))
  where
    def = P.rmap Just PP.def
    {-# INLINE def #-}

instance
    forall p a b b' abs abs'.
    ( PP.ProductProfunctor p
    , PP.Default p b b'
    , PP.Default p (Record abs) (Record abs')
    ) => PP.Default p (Record ('(a,b) ': abs)) (Record ('(a,b') ': abs'))
  where
    def = P.dimap (\(RCons tb rabs) -> (tb, rabs))
                  (\(tb', rabs') -> RCons tb' rabs')
                  ((PP.***!) (PP.def :: p (Tagged a b) (Tagged a b'))
                             (PP.def :: p (Record abs) (Record abs')))

instance
    forall p a b b' abs abs'.
    ( PP.ProductProfunctor p
    , PP.Default p b (Maybe b')
    , PP.Default p (Record abs) (Maybe (Record abs'))
    ) => PP.Default p (Record ('(a, b) ': abs)) (Maybe (Record ('(a,b') ': abs')))
  where
    def = P.dimap (\(RCons b bs) -> (b, bs))
                  (\(mb', mrabs') -> RCons <$> mb' <*> mrabs')
                  ((PP.***!) (PP.def_Tagged_MaybeTagged
                                     :: p (Tagged a b) (Maybe (Tagged a b')))
                             (PP.def :: p (Record abs) (Maybe (Record abs'))))

--------------------------------------------------------------------------------
-- 'PP.ProductProfunctorAdaptor' instances

-- | Instance required by 'Tisch.rawTableRW', for read-write purposes.
instance
    ( PP.ProductProfunctor p
    ) => PP.ProductProfunctorAdaptor p (Record '[]) (Record '[]) (Record '[])
  where
    ppa = const (P.dimap (const ()) (const RNil) PP.empty)
    {-# INLINE ppa #-}

-- | Instance required by 'Tisch.rawTableRW', for read-write purposes.
instance
    forall p apbcs abs acs (a :: k) b c.
    ( PP.ProductProfunctor p
    , PP.ProductProfunctorAdaptor p (Record apbcs) (Record abs) (Record acs)
    ) => PP.ProductProfunctorAdaptor p
            (Record ('(a, p b c) ': apbcs))
            (Record ('(a,b) ': abs))
            (Record ('(a,c) ': acs))
  where
    ppa = \(RCons tpbc rapbcs) ->
      P.dimap
        (\(RCons tb rabs) -> (tb, rabs))
        (uncurry RCons)
        ((PP.***!) (PP.ppa tpbc   :: p (Tagged a b) (Tagged a c))
                   (PP.ppa rapbcs :: p (Record abs) (Record acs)))

-- | Instance required by 'Tisch.rawTableRO', for read-only purposes.
instance
    ( PP.ProductProfunctor p
    ) => PP.ProductProfunctorAdaptor p (Record '[]) Void (Record '[])
  where
    ppa = const (P.dimap absurd (const RNil) PP.empty)
    {-# INLINE ppa #-}

-- | Instance required by 'Tisch.rawTableRO', for read-only purposes.
instance
    forall p apbcs acs (a :: k) b c.
    ( PP.ProductProfunctor p
    , PP.ProductProfunctorAdaptor p (Record apbcs) Void (Record acs)
    ) => PP.ProductProfunctorAdaptor p
            (Record ('(a, p b c) ': apbcs))
            Void
            (Record ('(a,c) ': acs))
  where
    ppa = \(RCons tpbc rapbcs) ->
      P.dimap
        (absurd :: Void -> (Tagged a b, Void))
        (uncurry RCons)
        ((PP.***!) (PP.ppa tpbc   :: p (Tagged a b) (Tagged a c))
                   (PP.ppa rapbcs :: p Void (Record acs)))

--------------------------------------------------------------------------------

class RLens (a :: k) (axs :: [(k,Type)]) (ays :: [(k,Type)]) (x :: Type) (y :: Type)
  | axs a -> x, ays a -> y, axs a y -> ays, ays a x -> axs
 where
  -- | A lens into the 'Record' value indexed by the key @a@.
  rLens :: proxy a -> Lens (Record axs) (Record ays) x y

instance RLens a ('(a,x) ': axs) ('(a,y) ': axs) x y where
  rLens _ f = \(RCons (Tagged x) raxs) ->
     fmap (\y -> RCons (Tagged y) raxs) (f x)
  {-# INLINE rLens #-}

instance {-# OVERLAPPABLE #-}
  ( RLens a axs ays x y
  , s ~ (skip ': axs) -- Need to specify s and t this way, otherwise
  , t ~ (skip ': ays) -- we overlap with the other RLens instance.
  ) => RLens a s t x y
 where
  rLens prx f = \(RCons tx raxs) -> fmap (RCons tx) (rLens prx f raxs)
  {-# INLINE rLens #-}

--------------------------------------------------------------------------------
-- Stuff required to implement 'rBuild' follows. Quite possibly some of the
-- kind annotations are unneeded, but it was quite easy to add all of them in
-- order to have this code compile.

-- | Like 'Data.HList.hBuild', but for building a 'Record'. This takes kind @k@
-- explicitely as parameter.
rBuild
  :: forall k r
  .  RBuild' ('[] :: [(k,Type)]) (r :: Type)
  => Proxy ('Proxy :: Proxy k)
  -> r
rBuild _ = rBuild' (RNil :: Record ('[] :: [(k,Type)]))
{-# INLINE rBuild #-}

-- | Like 'rBuild', but fixing the 'Record' index kind to 'Symbol'.
rBuildSymbol :: RBuild' ('[] :: [(Symbol,Type)]) r => r
rBuildSymbol = rBuild (Proxy :: Proxy ('Proxy :: Proxy Symbol))
{-# INLINE rBuildSymbol #-}

-- | Like 'Data.HList.hEnd', but for building a 'Record'
rEnd :: Record axs -> Record (axs :: [(k,Type)])
rEnd = id
{-# INLINE rEnd #-}

---
class RBuild' (axs :: [(k,Type)]) (r :: Type) where
  rBuild' :: Record axs -> r

instance
  forall (axs :: [(k,Type)]) (axs' :: [(k,Type)]).
  (RReverse axs axs') => RBuild' axs (Record axs')
 where
  rBuild' raxs = rReverse raxs
  {-# INLINE rBuild' #-}

instance
  forall (a :: k) (x :: Type) (axs :: [(k,Type)]) (r :: Type).
  (RBuild' ('(a,x) ': axs) r) => RBuild' axs (Tagged a x -> r)
 where
  rBuild' raxs = \tx -> rBuild' (RCons tx raxs)
  {-# INLINE rBuild' #-}

---
class RReverse (xs :: [(k,Type)]) (sx :: [(k,Type)]) | xs -> sx, sx -> xs where
  rReverse :: Record xs -> Record sx

instance
  forall (xs :: [(k,Type)]) (sx :: [(k,Type)]).
  ( RRevApp xs ('[] :: [(k,Type)]) sx
  , RRevApp sx ('[] :: [(k,Type)]) xs
    -- GHC warns that this constraint is redundant, but it is not.
  ) => RReverse xs sx where
  rReverse l = rRevApp l (RNil :: Record ('[] :: [(k,Type)]))
  {-# INLINE rReverse #-}

---
type family RRevAppR (l1 :: [(k,Type)]) (l2 :: [(k,Type)]) :: [(k,Type)] where
  RRevAppR (e ': l) l' = RRevAppR l (e ': l')
  RRevAppR '[] l = l

---
class RRevApp (l1 :: [(k,Type)]) (l2 :: [(k,Type)]) (l3 :: [(k,Type)]) | l1 l2 -> l3 where
  rRevApp :: Record l1 -> Record l2 -> Record l3

instance RRevApp ('[] :: [(k,Type)]) (l2 :: [(k,Type)]) (l2 :: [(k,Type)]) where
  rRevApp _ l = l
  {-# INLINE rRevApp #-}

instance
  forall (x :: (k,Type)) (l :: [(k,Type)]) (l' :: [(k,Type)]) (z :: [(k,Type)]).
  (RRevApp l (x ': l') z) => RRevApp (x ': l) l' z
 where
  rRevApp (RCons x l) l' = rRevApp l (RCons x l')
  {-# INLINE rRevApp #-}

