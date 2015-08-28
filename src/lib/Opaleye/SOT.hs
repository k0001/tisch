{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Opaleye.SOT where

import           Control.Monad.Catch as Cx
import           Data.Proxy (Proxy(..))
import           Data.Int
import           Data.HList (Tagged(Tagged, unTagged), HList(HCons, HNil))
import qualified Data.HList as HL
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as PP
import           Data.Singletons
import qualified Data.Promotion.Prelude.List as List (Map)
import           GHC.Exts (Constraint)
import qualified GHC.TypeLits as GHC
import qualified Opaleye as O

--------------------------------------------------------------------------------

-- | Whether to read a plain value or a possibly null value.
data RN = R  -- ^ Read plain value.
        | RN -- ^ Read possibly null value.

-- | Whether to write a plain value or a possibly null value.
data WN = W  -- ^ Write plain value.
        | WN -- ^ Write possibly null value.

--------------------------------------------------------------------------------

-- | Column description.
--
-- This is only used as a promoted datatype.
--
-- At the type level, 'Col' is expected to have kind
-- @'Col' 'GHC.Symbol' 'WN' 'RN' * *@. 
--
-- * @name@: Column name. Use 'Col_Name' at the type level.
--
-- * @wn@: See 'WN'. Use 'Col_WN' at the type level.
--
-- * @rn@: See 'RN'. Use 'Col_RN' at the type level.
--
-- * @pgType@: Type of the column value used in Opaleye queries
--   (e.g., @'PGText' :: *@, @'PGInt2' :: *@). Use 'Col_PgType' at
--   the type level.
--
-- * @hsType@: Type of the column value used in Haskell outside Opaleye queries.
--   Use 'Col_HsType' at the type level.
data Col name wn rn pgType hsType
   = Col name wn rn pgType hsType

--

type family Col_Name (col :: Col GHC.Symbol WN RN * *) :: GHC.Symbol where
  Col_Name ('Col n w r p h) = n
data Col_NameSym0 (col :: TyFun (Col GHC.Symbol WN RN * *) GHC.Symbol)
type instance Apply Col_NameSym0 col = Col_Name col

type family Col_WN (col :: Col GHC.Symbol WN RN * *) :: WN where
  Col_WN ('Col n w r p h) = w
data Col_WNSym0 (col :: TyFun (Col GHC.Symbol WN RN * *) WN)
type instance Apply Col_WNSym0 col = Col_WN col

type family Col_RN (col :: Col GHC.Symbol WN RN * *) :: RN where
  Col_RN ('Col n w r p h) = r
data Col_RNSym0 (col :: TyFun (Col GHC.Symbol WN RN * *) RN)
type instance Apply Col_RNSym0 col = Col_RN col

type family Col_PgType (col :: Col GHC.Symbol WN RN * *) :: * where
  Col_PgType ('Col n w r p h) = p
data Col_PgTypeSym0 (col :: TyFun (Col GHC.Symbol WN RN * *) *)
type instance Apply Col_PgTypeSym0 col = Col_PgType col

type family Col_HsType (col :: Col GHC.Symbol WN RN * *) :: * where
  Col_HsType ('Col n w 'R  p h) = h
  Col_HsType ('Col n w 'RN p h) = Maybe h
data Col_HsTypeSym0 (col :: TyFun (Col GHC.Symbol WN RN * *) *)
type instance Apply Col_HsTypeSym0 col = Col_HsType col

---

-- | Type of the 'HL.Record' columns in Haskell.
type Cols_Hs (a :: *) = List.Map Col_HsRecordFieldSym0 (Cols a)
type Col_HsRecordField (col :: Col GHC.Symbol WN RN * *)
  = Tagged (Col_Name col) (Col_HsType col)
data Col_HsRecordFieldSym0 (col :: TyFun (Col GHC.Symbol WN RN * *) *)
type instance Apply Col_HsRecordFieldSym0 col = Col_HsRecordField col

---

-- | Tag to be used with 'Tagged' that uniquely identifies a specific column
-- in a specific table in a specific schema.
data Tisch a => TC (a :: *) (column :: GHC.Symbol)

---

-- | Type of the 'HL.Record' columns when inserting or updating a row.
type Cols_PgWrite (a :: *) = List.Map (Col_PgWriteSym0 @@ a) (Cols a)
type family Col_PgWrite (a :: *) (col :: Col GHC.Symbol WN RN * *) :: * where
  Col_PgWrite a ('Col n 'W 'R p h) = Tagged (TC a n) (O.Column p)
  Col_PgWrite a ('Col n 'W 'RN p h) = Tagged (TC a n) (O.Column (O.Nullable p))
  Col_PgWrite a ('Col n 'WN 'R p h) = Tagged (TC a n) (Maybe (O.Column p))
  Col_PgWrite a ('Col n 'WN 'RN p h) = Tagged (TC a n) (Maybe (O.Column (O.Nullable p)))
data Col_PgWriteSym1 (a :: *) (col :: TyFun (Col GHC.Symbol WN RN * *) *)
type instance Apply (Col_PgWriteSym1 a) col = Col_PgWrite a col
data Col_PgWriteSym0 (col :: TyFun a (TyFun (Col GHC.Symbol WN RN * *) * -> *))
type instance Apply Col_PgWriteSym0 a = Col_PgWriteSym1 a

---

-- | Type of the 'HL.Record' columns (e.g., result of 'O.query')
type Cols_PgRead (a :: *) = List.Map (Col_PgReadSym0 @@ a) (Cols a)
type family Col_PgRead (a :: *) (col :: Col GHC.Symbol WN RN * *) :: * where
  Col_PgRead a ('Col n w 'R p h) = Tagged (TC a n) (O.Column p)
  Col_PgRead a ('Col n w 'RN p h) = Tagged (TC a n) (O.Column (O.Nullable p))
data Col_PgReadSym1 (a :: *) (col :: TyFun (Col GHC.Symbol WN RN * *) *)
type instance Apply (Col_PgReadSym1 a) col = Col_PgRead a col
data Col_PgReadSym0 (col :: TyFun a (TyFun (Col GHC.Symbol WN RN * *) * -> *))
type instance Apply Col_PgReadSym0 a = Col_PgReadSym1 a

---

-- | Type of the 'HL.Record' columns when they can all be nullable
-- (e.g., rhs on a 'O.leftJoin').
type Cols_PgReadNull (a :: *) = List.Map (Col_PgReadNullSym0 @@ a) (Cols a)
type family Col_PgReadNull (a :: *) (col :: Col GHC.Symbol WN RN * *) :: * where
  Col_PgReadNull a ('Col n w r p h) = Tagged (TC a n) (O.Column (O.Nullable p))
data Col_PgReadNullSym1 (a :: *) (col :: TyFun (Col GHC.Symbol WN RN * *) *)
type instance Apply (Col_PgReadNullSym1 a) col = Col_PgReadNull a col
data Col_PgReadNullSym0 (col :: TyFun a (TyFun (Col GHC.Symbol WN RN * *) * -> *))
type instance Apply Col_PgReadNullSym0 a = Col_PgReadNullSym1 a

--------------------------------------------------------------------------------

class PP.ProductProfunctor p => ProductProfunctorAdaptor p l ra rb | p l -> ra rb, p ra rb -> l where
  ppa :: l -> p ra rb

instance PP.ProductProfunctor p => ProductProfunctorAdaptor p (Tagged t (p a b)) (Tagged t a) (Tagged t b) where
  ppa = P.dimap unTagged Tagged . unTagged
  {-# INLINE ppa #-}

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
  {-# INLINABLE ppa #-}

instance
    ( ProductProfunctorAdaptor p (HList pabs) (HList as) (HList bs)
    ) => ProductProfunctorAdaptor p (HL.Record pabs) (HL.Record as) (HL.Record bs) where
  ppa = P.dimap (\(HL.Record x) -> x) HL.Record . ppa . (\(HL.Record x) -> x)
  {-# INLINE ppa #-}

--------------------------------------------------------------------------------
 
-- | Tisch means table in german.
class Tisch (a :: *) where
  type SchemaName a :: GHC.Symbol
  type TableName a :: GHC.Symbol
  type Cols a :: [Col GHC.Symbol WN RN * *]
  fromTisch :: Cx.MonadThrow m => HL.Record (Cols_Hs a) -> m a
  toTisch :: a -> HL.Record (Cols_Hs a)

--------------------------------------------------------------------------------

-- | 'O.TableProperties' for all the columns in 'Tisch' @a@.
type Cols_Props (a :: *) = List.Map (Col_PropsSym1 a) (Cols a)

-- | 'O.TableProperties' for a single column in 'Tisch' @a@.
type Col_Props (a :: *) (col :: Col GHC.Symbol WN RN * *)
  = O.TableProperties (Col_PgWrite a col) (Col_PgRead a col)
data Col_PropsSym1 (a :: *) (col :: TyFun (Col GHC.Symbol WN RN * *) *)
type instance Apply (Col_PropsSym1 a) col = Col_Props a col
data Col_PropsSym0 (col :: TyFun a (TyFun (Col GHC.Symbol WN RN * *) * -> *))
type instance Apply Col_PropsSym0 a = Col_PropsSym1 a

class ICol_Props (col :: Col GHC.Symbol WN RN * *) where
  colProps :: Tisch a => Proxy a -> Proxy col -> Col_Props a col

-- | 'colProps' is equivalent 'O.required'.
instance forall n p h. GHC.KnownSymbol n => ICol_Props ('Col n 'W 'R p h) where
  colProps _ = \_ -> P.dimap unTagged Tagged (O.required (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'O.required'.
instance forall n p h. GHC.KnownSymbol n => ICol_Props ('Col n 'W 'RN p h) where
  colProps _ = \_ -> P.dimap unTagged Tagged (O.required (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'O.optional'.
instance forall n p h. GHC.KnownSymbol n => ICol_Props ('Col n 'WN 'R p h) where
  colProps _ = \_ -> P.dimap unTagged Tagged (O.optional (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'O.optional'.
instance forall n p h. GHC.KnownSymbol n => ICol_Props ('Col n 'WN 'RN p h) where
  colProps _ = \_ -> P.dimap unTagged Tagged (O.optional (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}

-- | Use with 'HL.ApplyAB' to apply 'colProps' to each element of an 'HList'.
data HCol_Props (a :: *) = HCol_Props

instance forall a (col :: Col GHC.Symbol WN RN * *) pcol out n w r p h
  . ( Tisch a
    , GHC.KnownSymbol n
    , ICol_Props col
    , pcol ~ Proxy col
    , col ~ 'Col n w r p h
    , out ~ Col_Props a col
    ) => HL.ApplyAB (HCol_Props a) pcol out
    where
      applyAB _ = colProps (Proxy :: Proxy a)
      {-# INLINE applyAB #-}


-- | All this constraints are readily satisfied by instances of 'Tisch'.
type TischTable a
  = ( Tisch a
    , GHC.KnownSymbol (TableName a)
    , GHC.KnownSymbol (SchemaName a)
    , DistributeProxy (Cols a)
    , HL.SameLength (Cols_Props a) (List.Map ProxySym0 (Cols a))
    , HL.HMapAux HList (HCol_Props a) (List.Map ProxySym0 (Cols a)) (Cols_Props a)
    , ProductProfunctorAdaptor O.TableProperties (HL.Record (Cols_Props a)) (HL.Record (Cols_PgWrite a)) (HL.Record (Cols_PgRead a))
    )

-- | An Opaleye 'O.Table' for a 'Tisch'.
type Table (a :: *) = O.Table (HL.Record (Cols_PgWrite a)) (HL.Record (Cols_PgRead a))

-- | ^ Build the Opaleye 'O.Table' for a 'Tisch'.
table :: TischTable a => Proxy a -> Table a
table (_ :: Proxy a) =
  O.Table (GHC.symbolVal (Proxy :: Proxy (TableName a))) $ ppa $
     HL.Record (HL.hMapL (HCol_Props :: HCol_Props a)
                         (distributeProxy (Proxy :: Proxy (Cols a))))


retag' :: (HL.RecordValues r, HL.HMapTaggedFn (HL.RecordValuesR r) b) => HL.Record r -> HL.Record b
retag' = HL.hMapTaggedFn . HL.recordValues

--------------------------------------------------------------------------------

instance (PP.ProductProfunctor p, PP.Default p a b) => PP.Default p (Tagged t a) (Tagged t b) where
  def = ppa (Tagged PP.def)
  {-# INLINE def #-}

instance PP.ProductProfunctor p => PP.Default p (HList '[]) (HList '[]) where
  def = ppa HNil
  {-# INLINE def #-}

instance
    ( PP.ProductProfunctor p, PP.Default p a1 b1, PP.Default p (HList as) (HList bs)
    ) => PP.Default p (HList (a1 ': as)) (HList (b1 ': bs)) where
  def = P.dimap (\(HCons x xs) -> (x,xs)) (uncurry HCons) (PP.def PP.***! PP.def)
  {-# INLINE def #-}

instance
    ( PP.ProductProfunctor p, PP.Default p (HList as) (HList bs)
    ) => PP.Default p (HL.Record as) (HL.Record bs) where
  def = P.dimap (\(HL.Record x) -> x) HL.Record PP.def
  {-# INLINE def #-}

--------------------------------------------------------------------------------
-- Misc

-- | Apply a same constraint to all the types in the list.
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)


-- | Defunctionalized 'Proxy'. To be used with 'Apply'.
data ProxySym0 (a :: TyFun k *)
type instance Apply ProxySym0 a = Proxy a

class DistributeProxy (xs :: [k]) where
  distributeProxy :: Proxy xs -> HList (List.Map ProxySym0 xs)
instance DistributeProxy ('[] :: [k]) where
  distributeProxy _ = HNil
  {-# INLINE distributeProxy #-}
instance forall (x :: k) (xs :: [k]). DistributeProxy xs => DistributeProxy (x ': xs) where
  distributeProxy _ = HCons (Proxy :: Proxy x) (distributeProxy (Proxy :: Proxy xs))
  {-# INLINE distributeProxy #-}

--------------------------------------------------------------------------------
-- Test

data Test = Test Bool (Maybe String) Int32 (Maybe Int64)


instance Tisch Test where
  type SchemaName Test = "s"
  type TableName Test = "t"
  type Cols Test = [ 'Col "c1" 'W 'R O.PGBool Bool
                   , 'Col "c2" 'W 'RN O.PGText String 
                   , 'Col "c3" 'WN 'R O.PGInt4 Int32
                   , 'Col "c4" 'WN 'RN O.PGInt8 Int64 ]
  fromTisch :: MonadThrow m => HL.Record '[Tagged "c1" Bool, Tagged "c2" (Maybe String), Tagged "c3" Int32, Tagged "c4" (Maybe Int64)] -> m Test
  fromTisch r = return $ Test
     (HL.hLookupByLabel (HL.Label :: HL.Label "c1") r)
     (HL.hLookupByLabel (HL.Label :: HL.Label "c2") r)
     (HL.hLookupByLabel (HL.Label :: HL.Label "c3") r)
     (HL.hLookupByLabel (HL.Label :: HL.Label "c4") r)
  toTisch :: Test -> HL.Record '[Tagged "c1" Bool, Tagged "c2" (Maybe String), Tagged "c3" Int32, Tagged "c4" (Maybe Int64)]
  toTisch (Test c1 c2 c3 c4) = HL.hRearrange' $
     (HL.Label :: HL.Label "c2") HL..=. c2 HL..*.
     (HL.Label :: HL.Label "c1") HL..=. c1 HL..*.
     (HL.Label :: HL.Label "c4") HL..=. c4 HL..*.
     (HL.Label :: HL.Label "c3") HL..=. c3 HL..*.
     HL.emptyRecord
