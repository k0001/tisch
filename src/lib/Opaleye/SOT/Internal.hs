{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This is an internal module. You are not encouraged to use it directly.
module Opaleye.SOT.Internal where

import           Control.Lens
import           Control.Monad.Catch (MonadThrow)
import           Data.Proxy (Proxy(..))
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
-- This is only used as a promoted datatype expected to have kind
-- @'Col' 'GHC.Symbol' 'WN' 'RN' * *@. 
--
-- * @name@: Column name.
--
-- * @wn@: Whether @NULL@ can be written to this column ('WN') or not ('W'). 
--
-- * @rn@: Whether @NULL@ might be read from this column ('RN') or not ('R').
--
-- * @pgType@: Type of the column value used in Opaleye queries
--   (e.g., 'O.PGText', 'O.PGInt2').
--
-- * @hsType@: Type of the column value used in Haskell outside Opaleye
--   queries. Hint: don't use something like @'Maybe' 'Bool'@ here if you
--   want to indicate that this is an optional 'Bool' column. Instead, use
--   'Int' here and 'WN' and 'RN' in the @wn@ and @rn@ fields.
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

-- | Lookup column info by name
type Col_ByName (a :: *) (name :: GHC.Symbol) = Col_ByName' name (Cols a)
type family Col_ByName' (name :: GHC.Symbol) (cols :: [Col GHC.Symbol WN RN * *])
       :: Col GHC.Symbol WN RN * * where
  Col_ByName' n ('Col n  w r p h ': xs) = 'Col n w r p h
  Col_ByName' n ('Col n' w r p h ': xs) = Col_ByName' n xs
data Col_ByNameSym1 (a :: *) (name :: TyFun GHC.Symbol (Col GHC.Symbol WN RN * *))
type instance Apply (Col_ByNameSym1 a) name = Col_ByName a name
data Col_ByNameSym0 (name :: TyFun a (TyFun GHC.Symbol (Col GHC.Symbol WN RN * *) -> *))
type instance Apply Col_ByNameSym0 a = Col_ByNameSym1 a

---

-- | Type of the 'HL.Record' columns in Haskell.
type Cols_Hs (a :: *) = List.Map Col_HsRecordFieldSym0 (Cols a)
type Col_HsRecordField (col :: Col GHC.Symbol WN RN * *)
  = Tagged (Col_Name col) (Col_HsType col)
data Col_HsRecordFieldSym0 (col :: TyFun (Col GHC.Symbol WN RN * *) *)
type instance Apply Col_HsRecordFieldSym0 col = Col_HsRecordField col

---

-- | Tag to be used alone or with 'Tagged' for uniquely identifying a specific
-- table in a specific schema.
data Tisch t => T (t :: *) = T

-- | Tag to be used alone or with 'Tagged' for uniquely identifying a specific
-- column in a specific table in a specific schema.
data Tisch t => TC (t :: *) (c :: GHC.Symbol) = TC

-- | Tag to be used alone or with 'Tagged' for uniquely identifying a specific
-- column in an unknown table.
data C (c :: GHC.Symbol) = C

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

type TRecord (a :: *) xs = Tagged (T a) (HL.Record xs)

--------------------------------------------------------------------------------

-- | Tisch means table in german.
class Tisch (a :: *) where
  type SchemaName a :: GHC.Symbol
  type TableName a :: GHC.Symbol
  type Cols a :: [Col GHC.Symbol WN RN * *]
  fromTisch :: MonadThrow m => TRecord a (Cols_Hs a) -> m a
  toTisch :: a -> TRecord a (Cols_Hs a)

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

--------------------------------------------------------------------------------

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

-- | Build the Opaleye 'O.Table' for a 'Tisch'.
tisch ::  TischTable a => O.Table (TRecord a (Cols_PgWrite a)) (TRecord a (Cols_PgRead a))
tisch = tisch' T 
{-# INLINE tisch #-}

-- | Like 'tisch', but takes @a@ explicitly to help the compiler when it
-- can't infer @a@.
tisch' :: TischTable a
       => T a
       -> O.Table (TRecord a (Cols_PgWrite a)) (TRecord a (Cols_PgRead a))
tisch' (_ :: T a) = O.Table tableName (ppa (Tagged (ppa recProps)))
  where
    tableName = GHC.symbolVal (Proxy :: Proxy (TableName a))
    recProps = HL.Record (HL.hMapL (HCol_Props :: HCol_Props a)
                                   (distributeProxy (Proxy :: Proxy (Cols a))))

--------------------------------------------------------------------------------

-- | Provide 'Comparable' instances for every two columns that you want to be
-- able to compare (e.g., using 'eq').
class (Tisch t1, Tisch t2) => Comparable (t1 :: *) (c1 :: GHC.Symbol) (t2 :: *) (c2 :: GHC.Symbol) (a :: *) where
  _ComparableL :: Iso (Tagged (TC t1 c1) (O.Column a)) (Tagged (TC t2 c2) (O.Column a)) (O.Column a) (O.Column a)
  _ComparableL = _Wrapped
  _ComparableR :: Iso (Tagged (TC t2 c2) (O.Column a)) (Tagged (TC t1 c1) (O.Column a)) (O.Column a) (O.Column a)
  _ComparableR = _Wrapped

-- | Trivial. Same table, same column, same value.
instance Tisch t => Comparable t c t c a

--------------------------------------------------------------------------------

-- | Convert a Haskell value to a PostgreSQL 'O.Column' value.
-- Think of 'O.pgString', 'O.pgInt4', 'O.pgStrictText', etc.
class ToPgColumn (pg :: *) (hs :: *) where toPgColumn :: hs -> O.Column pg

-- | Trivial.
instance ToPgColumn pg (O.Column pg) where toPgColumn = id
  
wrappedToPgColumn :: (Wrapped w, ToPgColumn a (Unwrapped w)) => w -> O.Column a 
wrappedToPgColumn = toPgColumn . view _Wrapped'

-- TODO instance ToPgColumn a b => ToPgColumn (O.Nullable a) (Maybe b) where toPgColumn = undefined
instance ToPgColumn O.PGText [Char] where toPgColumn = O.pgString
instance ToPgColumn O.PGBool Bool where toPgColumn = O.pgBool

--------------------------------------------------------------------------------

-- | Lens to the value of a column.
col :: HL.HLensCxt (TC t c) HL.Record xs xs' a a'
    => C c
    -> Lens (TRecord t xs) (TRecord t xs') (Tagged (TC t c) a) (Tagged (TC t c) a')
col prx = cola prx . _Unwrapped
{-# INLINE col #-}

-- | Lens to the value of a column without the 'TC' tag.
-- 
-- Most of the time you'll want to use 'tc' instead.
cola :: forall t c xs xs' a a'
     .  HL.HLensCxt (TC t c) HL.Record xs xs' a a'
     => C c
     -> Lens (TRecord t xs) (TRecord t xs') a a'
cola (_ :: C c) = _Wrapped . HL.hLens (HL.Label :: HL.Label (TC t c)) 
{-# INLINE cola #-}

--------------------------------------------------------------------------------

-- | Like 'O..==', but restricted to 'Comparable' columns.
eq :: Comparable lt lc rt rc a
   => Tagged (TC lt lc) (O.Column a)
   -> Tagged (TC rt rc) (O.Column a)
   -> O.Column O.PGBool
eq l r = (O..==) (view _ComparableL l) (view _ComparableR r)
{-# INLINE eq #-}

-- | Like 'eq', but the first argument is a constant.
eqc :: (ToPgColumn a h)
    => h
    -> Tagged (TC rt rc) (O.Column a)
    -> O.Column O.PGBool
eqc la r = (O..==) (toPgColumn la) (unTagged r)
{-# INLINE eqc #-}

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

-- | Orphan. 'Opaleye.SOT.Internal'.
instance (PP.ProductProfunctor p, PP.Default p a b) => PP.Default p (Tagged t a) (Tagged t b) where
  def = ppa (Tagged PP.def)
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
  {-# INLINE def #-}

-- | Orphan. 'Opaleye.SOT.Internal'.
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

retag' :: (HL.RecordValues r, HL.HMapTaggedFn (HL.RecordValuesR r) b) => HL.Record r -> HL.Record b
retag' = HL.hMapTaggedFn . HL.recordValues

