{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Tisch.Internal.Profunctors
  ( ProductProfunctorAdaptor(ppa)
  , def_Tagged_MaybeTagged
  ) where

--------------------------------------------------------------------------------
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as PP
import           Data.Tagged

--------------------------------------------------------------------------------
-- | A (very general!) generalization of product profunctor adaptors such as
-- 'PP.p1', 'PP.p4', etc.
class P.Profunctor p => ProductProfunctorAdaptor p l ra rb where
  ppa :: l -> p ra rb

instance
    ( P.Profunctor p
    , ProductProfunctorAdaptor p (p a b) (Tagged ta a) (Tagged tb b)
    ) => ProductProfunctorAdaptor p (Tagged t (p a b)) (Tagged ta a) (Tagged tb b)
  where
    ppa = ppa . unTagged
    {-# INLINE ppa #-}

instance
    ( P.Profunctor p
    ) => ProductProfunctorAdaptor p (p a b) (Tagged ta a) (Tagged tb b)
  where
    ppa = P.dimap unTagged Tagged
    {-# INLINE ppa #-}

--------------------------------------------------------------------------------
-- | This could be an orphan 'PP.Default' instance, but we prefer it this way.
def_Tagged_MaybeTagged
  :: (PP.ProductProfunctor p, PP.Default p a (Maybe b))
  => p (Tagged ta a) (Maybe (Tagged tb b))
def_Tagged_MaybeTagged = P.dimap unTagged (fmap Tagged) PP.def
{-# INLINE def_Tagged_MaybeTagged #-}

