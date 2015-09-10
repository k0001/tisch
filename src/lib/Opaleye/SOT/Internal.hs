{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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

import           Control.Arrow
import           Control.Lens
import qualified Control.Exception as Ex
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
import           GHC.Float (float2Double)
import qualified GHC.TypeLits as GHC
import qualified Opaleye as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as OI
import qualified Opaleye.Internal.PGTypes as OI

-------------------------------------------------------------------------------

-- | Whether to read a plain value or possibly @NULL@. 
data RN = R  -- ^ Read plain value.
        | RN -- ^ Possibly read @NULL@.

-- | Whether to write a specific value or possibly @DEFAULT@.
data WD = W  -- ^ Write a specific value.
        | WD -- ^ Possibly write @DEFAULT@.

-- | Whether to write @DEFAULT@ or a specific value when writing to a column.
data WDef a = WDef   -- ^ Write @DEFAULT@.
            | WVal a -- ^ Write the specified value.
  deriving (Eq, Ord, Show, Functor)

instance Applicative WDef where
  pure = WVal
  {-# INLINE pure #-}
  WVal f <*> WVal a = WVal (f a)
  _ <*> _ = WDef
  {-# INLINE (<*>) #-}

instance Monad WDef where
  return = pure
  {-# INLINE return #-} 
  WVal a >>= k = k a
  _ >>= _ = WDef
  {-# INLINE (>>=) #-} 

-- | Case analysis for 'WDef'. Evaluates to the first argument if 'WDef',
-- otherwise applies the given function to the @a@ in 'WVal'.
wdef :: b -> (a -> b) -> WDef a -> b
wdef b f = \w -> case w of { WDef -> b; WVal a -> f a }
{-# INLINE wdef #-}

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
-- * @pgType@: Type of the column value used in Opaleye queries
--   (e.g., 'O.PGText', 'O.PGInt2').
--
-- * @hsType@: Type of the column value used in Haskell outside Opaleye
--   queries. Hint: don't use something like @'Maybe' 'Bool'@ here if you
--   want to indicate that this is an optional 'Bool' column. Instead, use
--   'Int' here and 'RN' in the @rn@ field.
data Col name wd rn pgType hsType
   = Col name wd rn pgType hsType

--

type family Col_Name (col :: Col GHC.Symbol WD RN * *) :: GHC.Symbol where
  Col_Name ('Col n w r p h) = n
data Col_NameSym0 (col :: TyFun (Col GHC.Symbol WD RN * *) GHC.Symbol)
type instance Apply Col_NameSym0 col = Col_Name col

type family Col_PgRType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgRType ('Col n w 'R  p h) = O.Column p
  Col_PgRType ('Col n w 'RN p h) = O.Column (O.Nullable p)

type family Col_PgRNType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgRNType ('Col n w 'R  p h) = O.Column (O.Nullable p)
  Col_PgRNType ('Col n w 'RN p h) = O.Column (O.Nullable (O.Nullable p))

type family Col_PgWType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgWType ('Col n 'W  r p h) = Col_PgRType ('Col n 'W r p h)
  Col_PgWType ('Col n 'WD r p h) = Maybe (Col_PgRType ('Col n 'WD r p h))

type family Col_HsRType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_HsRType ('Col n w 'R  p h) = h
  Col_HsRType ('Col n w 'RN p h) = Maybe h

type family Col_HsIType (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_HsIType ('Col n 'W  r p h) = Col_HsRType ('Col n 'W r p h)
  Col_HsIType ('Col n 'WD r p h) = WDef (Col_HsRType ('Col n 'WD r p h))

---

-- | Lookup a column in @'Tisch' t@ by its name.
type Col_ByName (t :: *) (c :: GHC.Symbol) = Col_ByName' c (Cols t)
type family Col_ByName' (name :: GHC.Symbol) (cols :: [Col GHC.Symbol WD RN * *]) :: Col GHC.Symbol WD RN * * where
  Col_ByName' n ('Col n  w r p h ': xs) = 'Col n w r p h
  Col_ByName' n ('Col n' w r p h ': xs) = Col_ByName' n xs

type HasColName (t :: *) (c :: GHC.Symbol) =  HasColName' c (Cols t)
type family HasColName' (name :: GHC.Symbol) (cols :: [Col GHC.Symbol WD RN * *]) :: Constraint where
  HasColName' n ('Col n  w r p h ': xs) = ()
  HasColName' n ('Col n' w r p h ': xs) = HasColName' n xs

---

-- | Payload for @('HsR' t)@
type Cols_HsR (t :: *) = List.Map (Col_HsRFieldSym1 t) (Cols t)
type Col_HsRField (t :: *) (col :: Col GHC.Symbol WD RN * *)
  = Tagged (TC t (Col_Name col)) (Col_HsRType col)
data Col_HsRFieldSym1 (t :: *) (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_HsRFieldSym1 t) col = Col_HsRField t col

-- | Payload for @('HsI' t)@
type Cols_HsI (t :: *) = List.Map (Col_HsIFieldSym1 t) (Cols t)
type Col_HsIField (t :: *) (col :: Col GHC.Symbol WD RN * *)
  = Tagged (TC t (Col_Name col)) (Col_HsIType col)
data Col_HsIFieldSym1 (t :: *) (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_HsIFieldSym1 t) col = Col_HsIField t col

-- | Payload for @('PgR' t)@
type Cols_PgR (t :: *) = List.Map (Col_PgRSym1 t) (Cols t)
type family Col_PgR (t :: *) (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgR t ('Col n w r p h) = Tagged (TC t n) (Col_PgRType ('Col n w r p h))
data Col_PgRSym1 (t :: *) (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_PgRSym1 t) col = Col_PgR t col

-- | Payload for @('PgRN' t)@
type Cols_PgRN (t :: *) = List.Map (Col_PgRNSym1 t) (Cols t)
type family Col_PgRN (t :: *) (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgRN t ('Col n w r p h) = Tagged (TC t n) (Col_PgRNType ('Col n w r p h))
data Col_PgRNSym1 (t :: *) (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_PgRNSym1 t) col = Col_PgRN t col

-- | Type of the 'HL.Record' columns when inserting or updating a row. Also,
-- payload for @('PgI' t)@.
type Cols_PgW (t :: *) = List.Map (Col_PgWSym1 t) (Cols t)
type family Col_PgW (t :: *) (col :: Col GHC.Symbol WD RN * *) :: * where
  Col_PgW t ('Col n w r p h) = Tagged (TC t n) (Col_PgWType ('Col n w r p h))
data Col_PgWSym1 (t :: *) (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_PgWSym1 t) col = Col_PgW t col

--------------------------------------------------------------------------------

-- | Tag to be used alone or with 'Tagged' for uniquely identifying a specific
-- table in a specific schema.
data Tisch t => T (t :: *) = T

-- | Tag to be used alone or with 'Tagged' for uniquely identifying a specific
-- column in a specific table in a specific schema.
data Tisch t => TC (t :: *) (c :: GHC.Symbol) = TC

-- | Tag to be used alone or with 'Tagged' for uniquely identifying a specific
-- column in an unknown table.
data C (c :: GHC.Symbol) = C

--------------------------------------------------------------------------------

-- | All the representation of @t@ used within @opaleye-sot@ are @('Rec' t)@.
type Rec (t :: *) xs = Tagged (T t) (HL.Record xs)

-- | Expected output type for 'O.runQuery' on a @('PgR' t)@.
--
-- Important: If you are expecting a @('PgR' t)@ on the right side
-- of a 'O.leftJoin', you will need to use @('Maybe' ('PgR' t))@.
--
-- Mnemonic: Haskell Read.
type HsR (t :: *) = Rec t (Cols_HsR t)

-- | Output type of 'toHsI', used when inserting a new row to the table.
--
-- This type is used internally as an intermediate representation between
-- your own Haskell representation for a to-be-inserted @t@ and @('PgW' t)@.
--
-- Mnemonic: Haskell Insert.
type HsI (t :: *) = Rec t (Cols_HsI t)

-- | Output type of @'O.queryTable' ('tisch' t)@.
--
-- Mnemonic: PostGresql Read.
type PgR (t :: *) = Rec t (Cols_PgR t)

-- | Like @('PgRN' t)@ but every field is 'O.Nullable', as in the
-- output type of the right hand side of a 'O.leftJoin' with @'(tisch' t)@.
--
-- Mnemonic: PostGresql Read Nulls.
type PgRN (t :: *) = Rec t (Cols_PgRN t)

-- | Representation of @('ToHsI' t)@ as 'O.Columns'. To be used when
-- writing to the database.
--
-- Mnemonic: PostGresql Write.
type PgW (t :: *) = Rec t (Cols_PgW t)

--------------------------------------------------------------------------------

-- | All these constraints need to be satisfied by tools that work with 'Tisch'.
-- It's easier to just write all the constraints once here and make 'ITisch' a
-- superclass of 'Tisch'. Moreover, they enforce some sanity constraints on our
-- 'Tisch' so that we can get early compile time errors.
type ITisch t
  = ( GHC.KnownSymbol (SchemaName t)
    , GHC.KnownSymbol (TableName t)
    , HDistributeProxy (Cols t)
    , HL.HMapAux HList (HCol_Props t) (List.Map ProxySym0 (Cols t)) (Cols_Props t)
    , HL.HMapAux HList HL.TaggedFn (HL.RecordValuesR (Cols_HsR t)) (Cols_HsR t)
    , HL.HMapAux HList HL.TaggedFn (HL.RecordValuesR (Cols_PgW t)) (Cols_PgW t)
    , HL.HMapAux HList HToPgWField (HL.RecordValuesR (Cols_HsI t)) (HL.RecordValuesR (Cols_PgW t))
    , HL.HMapAux HList HToPgWField (HL.RecordValuesR (Cols_PgR t)) (HL.RecordValuesR (Cols_PgW t))
    , HL.HRLabelSet (Cols_HsR t)
    , HL.HRLabelSet (Cols_HsI t)
    , HL.HRLabelSet (Cols_PgR t)
    , HL.HRLabelSet (Cols_PgRN t)
    , HL.HRLabelSet (Cols_PgW t)
    , HL.RecordValues (Cols_HsR t)
    , HL.RecordValues (Cols_HsI t)
    , HL.RecordValues (Cols_PgR t)
    , HL.RecordValues (Cols_PgRN t)
    , HL.RecordValues (Cols_PgW t)
    , HL.SameLength (HL.RecordValuesR (Cols_HsI t)) (HL.RecordValuesR (Cols_PgW t))
    , HL.SameLength (HL.RecordValuesR (Cols_PgR t)) (HL.RecordValuesR (Cols_PgW t))
    , HL.SameLength (Cols_Props t) (List.Map ProxySym0 (Cols t))
    , ProductProfunctorAdaptor O.TableProperties (HL.Record (Cols_Props t)) (HL.Record (Cols_PgW t)) (HL.Record (Cols_PgR t))
    )

-- | Tisch means table in german.
--
-- An instance of this class can uniquely describe a PostgreSQL table and
-- how to convert back and forth between it and its Haskell representation.
--
-- The @t@ type is only used as a tag for the purposes of uniquely identifying
-- this 'Tisch'. We recommend that for each 'Tisch' you define a tag with a
-- single constructor like the following:
--
-- @
-- -- | Tag for the users table (just an example).
-- data TUser = TUser
-- @
--
-- Why? Because that way the 'TUser' type can be used as the 'Tisch' tag,
-- and the @TUser@ term constructor can be used as a type proxy for tools such
-- as 'tisch' or 'unHsR'. 
class ITisch t => Tisch (t :: *) where
  -- | PostgreSQL schema name where to find the table (defaults to @"public"@, 
  -- PostgreSQL's default schema name).
  type SchemaName t :: GHC.Symbol
  type SchemaName t = "public"

  -- | Table name.
  type TableName t :: GHC.Symbol

  -- | Columns in this table. See the documentation for 'Col'.
  type Cols t :: [Col GHC.Symbol WD RN * *]

--------------------------------------------------------------------------------

-- | Convert an Opaleye-compatible Haskell representation of @a@ to @a@ when
-- /reading/ from the database.
--
-- Notice that you are not required to provide instances of this class if working
-- with @'HsR' t@ is sufficient for you, or if you already have a function
-- @('HsR' t -> a)@ at hand.
class Tisch t => UnHsR t (a :: *) where
  -- | Convert an Opaleye-compatible Haskell representation of @a@ to @a@.
  --
  -- For your convenience, you are encouraged to use 'cola', but you may also use
  -- other tools from "Data.HList.Record" as you see fit:
  --
  -- @
  -- 'unHsR'' r = Person (r '^.' 'cola' ('C' :: 'C' "name"))
  --                   (r '^.' 'cola' ('C' :: 'C' "age"))
  -- @
  --
  -- Hint: If the type checker is having trouble inferring @('HsR' t)@,
  -- consider using 'unHsR' instead.
  unHsR' :: HsR t -> Either Ex.SomeException a

-- | Like 'unHsR'', except it takes @t@ explicitely for the times when
-- it can't be inferred.
unHsR :: UnHsR t a => t -> HsR t -> Either Ex.SomeException a
unHsR _ = unHsR'
{-# INLINE unHsR #-}

--------------------------------------------------------------------------------

-- | Build a @('HsR' t)@ representation for @a@ for /inserting/ it to the database.
--
-- Notice that you are not required to provide instances of this class if working
-- with @'HsI' t@ is sufficient for you, or if you already have a function
-- @(a -> 'HsI' t)@ at hand.
class Tisch t => ToHsI t (a :: *) where 
  -- | Convert an @a@ to an Opaleye-compatible Haskell representation
  -- to be used when inserting a new row to this table.
  --
  -- For your convenience, you may use 'mkHsI' together with 'HL.hBuild' to build
  -- 'toHsI':
  --
  -- @
  -- 'toHsI' (Person name age) = 'mkHsI' $ \\set_ -> 'HL.hBuild'
  --     (set_ ('C' :: 'C' "name") name)
  --     (set_ ('C' :: 'C' "age") age)
  -- @
  --
  -- You may also use other tools from "Data.HList.Record" as you see fit.
  --
  -- Hint: If the type checker is having trouble inferring @('HsI' t)@,
  -- consider using 'toHsI' instead. Nevertheless, it is more
  -- likely that you use 'toPgW' directly, which skips the 'HsI' intermediate
  -- representation altogether.
  toHsI' :: a -> HsI t

-- | Like 'toHsI'', except it takes @t@ explicitely for the times when
-- it can't be inferred.
toHsI :: ToHsI t a => t -> a -> HsI t
toHsI _ = toHsI'
{-# INLINE toHsI #-}

-- | Convenience intended to be used within 'toHsI'', together with 'HL.hBuild'.

-- TODO: see if it is posisble to pack 'hsi' and 'HL.hBuild' into
-- a single thing.
mkHsI
  :: forall t xs
  .  (Tisch t, HL.HRearrange (HL.LabelsOf (Cols_HsI t)) xs (Cols_HsI t))
  => ((forall c a. (C c -> a -> Tagged (TC t c) a)) -> HList xs)
  -> HsI t -- ^
mkHsI k = Tagged
      $ HL.Record
      $ HL.hRearrange2 (Proxy :: Proxy (HL.LabelsOf (Cols_HsI t)))
      $ k (const Tagged)
{-# INLINE mkHsI #-}

--------------------------------------------------------------------------------

-- | Use with 'HL.ApplyAB' to apply convert a field in a
-- @('HList' ('Cols_HsI' t)@) to a field in a @('HList' ('Cols_PgW' t))@.
data HToPgWField = HToPgWField
instance {-# OVERLAPPABLE #-} (ToPgColumn pg hs) => HL.ApplyAB HToPgWField hs (O.Column pg) where
  applyAB _ = toPgColumn
  {-# INLINE applyAB #-}
instance HL.ApplyAB HToPgWField (O.Column pg) (O.Column pg) where
  applyAB _ = id 
  {-# INLINE applyAB #-}
instance (ToPgColumn pg hs) => HL.ApplyAB HToPgWField (WDef hs) (Maybe (O.Column pg)) where
  applyAB _ = fmap toPgColumn . wdef Nothing Just 
  {-# INLINE applyAB #-}
instance HL.ApplyAB HToPgWField (O.Column pg) (Maybe (O.Column pg)) where
  applyAB _ = Just
  {-# INLINE applyAB #-}

-- | You'll need to use this function to convert a 'HsI' to a 'PgW' when using 'O.runInsert'.
toPgW_fromHsI' :: Tisch t => HsI t -> PgW t
toPgW_fromHsI' = Tagged . HL.hMapTaggedFn . HL.hMapL HToPgWField . HL.recordValues . unTagged
{-# INLINE toPgW_fromHsI' #-}

-- | Like 'toPgW_fromHsI'', but takes an explicit @t@.
toPgW_fromHsI :: Tisch t => t -> HsI t -> PgW t
toPgW_fromHsI _ = toPgW_fromHsI'
{-# INLINE toPgW_fromHsI #-}

--------------------------------------------------------------------------------

-- | Convert a custom Haskell type to a representation appropiate for /inserting/
-- it as a new row.
toPgW' :: ToHsI t a => a -> PgW t
toPgW' = toPgW_fromHsI' . toHsI'
{-# INLINE toPgW' #-}

-- | Like 'toPgW'', but takes an explicitl @t@.
toPgW :: ToHsI t a => t -> a -> PgW t
toPgW _ = toPgW'
{-# INLINE toPgW #-}

--------------------------------------------------------------------------------

-- | Convert a @('PgR' t)@ resulting from a 'O.queryTable'-like operation
-- to a @('PgW' t)@ that can be used in a 'O.runUpdate'-like operation.
update' :: Tisch t => PgR t -> PgW t
update' = Tagged . HL.hMapTaggedFn . HL.hMapL HToPgWField . HL.recordValues . unTagged
{-# INLINE update' #-}

-- | Like 'update'', but takes an explicit @t@ for when it can't be inferred.
update :: Tisch t => t -> PgR t -> PgW t
update _ = update'
{-# INLINE update #-}

--------------------------------------------------------------------------------

-- | 'O.TableProperties' for all the columns in 'Tisch' @t@.
type Cols_Props (t :: *) = List.Map (Col_PropsSym1 t) (Cols t)

-- | 'O.TableProperties' for a single column in 'Tisch' @t@.
type Col_Props (t :: *) (col :: Col GHC.Symbol WD RN * *)
  = O.TableProperties (Col_PgW t col) (Col_PgR t col)
data Col_PropsSym1 (t :: *) (col :: TyFun (Col GHC.Symbol WD RN * *) *)
type instance Apply (Col_PropsSym1 t) col = Col_Props t col
data Col_PropsSym0 (col :: TyFun t (TyFun (Col GHC.Symbol WD RN * *) * -> *))
type instance Apply Col_PropsSym0 t = Col_PropsSym1 t

class ICol_Props (col :: Col GHC.Symbol WD RN * *) where
  colProps :: Tisch t => Proxy t -> Proxy col -> Col_Props t col

-- | 'colProps' is equivalent 'O.required'.
instance forall n p h. GHC.KnownSymbol n => ICol_Props ('Col n 'W 'R p h) where
  colProps _ = \_ -> P.dimap unTagged Tagged (O.required (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'O.required'.
instance forall n p h. GHC.KnownSymbol n => ICol_Props ('Col n 'W 'RN p h) where
  colProps _ = \_ -> P.dimap unTagged Tagged (O.required (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'O.optional'.
instance forall n p h. GHC.KnownSymbol n => ICol_Props ('Col n 'WD 'R p h) where
  colProps _ = \_ -> P.dimap unTagged Tagged (O.optional (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'O.optional'.
instance forall n p h. GHC.KnownSymbol n => ICol_Props ('Col n 'WD 'RN p h) where
  colProps _ = \_ -> P.dimap unTagged Tagged (O.optional (GHC.symbolVal (Proxy :: Proxy n)))
  {-# INLINE colProps #-}

-- | Use with 'HL.ApplyAB' to apply 'colProps' to each element of an 'HList'.
data HCol_Props (t :: *) = HCol_Props

instance forall t (col :: Col GHC.Symbol WD RN * *) pcol out n w r p h
  . ( Tisch t
    , GHC.KnownSymbol n
    , ICol_Props col
    , pcol ~ Proxy col
    , col ~ 'Col n w r p h
    , out ~ Col_Props t col
    ) => HL.ApplyAB (HCol_Props t) pcol out
    where
      applyAB _ = colProps (Proxy :: Proxy t)
      {-# INLINE applyAB #-}

--------------------------------------------------------------------------------

-- | Opaleye 'O.Table' for a 'Tisch'.
type TischTable (t :: *) = O.Table (PgW t) (PgR t)

-- | Build the Opaleye 'O.Table' for a 'Tisch'.
tisch' :: Tisch t => TischTable t
tisch' = go where -- to hide the "forall" from the haddocks
   go :: forall t. Tisch t => TischTable t
   go = O.TableWithSchema
     (GHC.symbolVal (Proxy :: Proxy (SchemaName t)))
     (GHC.symbolVal (Proxy :: Proxy (TableName t)))
     (ppaUnTagged $ ppa $ HL.Record
        (HL.hMapL (HCol_Props :: HCol_Props t)
        (hDistributeProxy (Proxy :: Proxy (Cols t)))))
   {-# INLINE go #-}
{-# INLINE tisch' #-}

-- | Like 'tisch'', but takes @t@ explicitly to help the compiler when it
-- can't infer @t@.
tisch :: Tisch t => t -> TischTable t
tisch _ = tisch'
{-# INLINE tisch #-}

--------------------------------------------------------------------------------

-- | Provide 'Comparable' instances for every two columns that you want to be
-- able to compare (e.g., using 'eq').
class ( Tisch t1, Tisch t2, HasColName t1 c1, HasColName t2 c2
      ) => Comparable (t1 :: *) (c1 :: GHC.Symbol) (t2 :: *) (c2 :: GHC.Symbol) (a :: *) where
  _ComparableL :: Iso (Tagged (TC t1 c1) (O.Column a)) (Tagged (TC t2 c2) (O.Column a)) (O.Column a) (O.Column a)
  _ComparableL = _Wrapped
  _ComparableR :: Iso (Tagged (TC t2 c2) (O.Column a)) (Tagged (TC t1 c1) (O.Column a)) (O.Column a) (O.Column a)
  _ComparableR = _Wrapped

-- | Trivial. Same table, same column, same value.
instance (Tisch t, HasColName t c) => Comparable t c t c a 

--------------------------------------------------------------------------------

-- | Convert a Haskell value to a PostgreSQL 'O.Column' value.
-- Think of 'O.pgString', 'O.pgInt4', 'O.pgStrictText', etc.
--
-- You probably won't ever need to call 'toPgColumn' explicity, yet you need to
-- provide an instance for every Haskell type you plan to convert to its
-- PostgreSQL representation. Quite likely, you will be using 'toPgTC' though.
--
-- A a default implementation of 'toPgColumn' is available for 'Wrapped' types
class ToPgColumn (pg :: *) (hs :: *) where
  toPgColumn :: hs -> O.Column pg
  default toPgColumn :: (Wrapped hs, ToPgColumn pg (Unwrapped hs)) => hs -> O.Column pg
  toPgColumn = toPgColumn . view _Wrapped'
  {-# INLINE toPgColumn #-}

toPgColumnN :: ToPgColumn pg hs => hs -> O.Column (O.Nullable pg)
toPgColumnN = O.toNullable . toPgColumn
{-# INLINE toPgColumnN #-}

-- | OVERLAPPABLE. Any @pg@ can be made 'O.Nullable'. TODO: Do we need this instance?
instance {-# OVERLAPPABLE #-} ToPgColumn pg hs => ToPgColumn (O.Nullable pg) hs where
  toPgColumn = toPgColumnN
  {-# INLINE toPgColumn #-}
-- | OVERLAPPS @'ToPgColumn' ('O.Nullable' pg) hs@. 'Nothing' is @NULL@.
instance ToPgColumn pg hs => ToPgColumn (O.Nullable pg) (Maybe hs) where
  toPgColumn = maybe O.null toPgColumnN
  {-# INLINE toPgColumn #-}

instance ToPgColumn ph hs => ToPgColumn ph (Tagged t hs)
instance ToPgColumn O.PGText [Char] where toPgColumn = O.pgString
instance ToPgColumn O.PGText Char where toPgColumn = toPgColumn . (:[])
instance ToPgColumn O.PGBool Bool where toPgColumn = O.pgBool
-- | Note: Portability wise, it's a /terrible/ idea to have an 'Int' instance instead.
-- Use 'Int32', 'Int64', etc. explicitely.
instance ToPgColumn O.PGInt4 Int32 where toPgColumn = O.pgInt4 . fromIntegral
-- | Note: Portability wise, it's a /terrible/ idea to have an 'Int' instance instead.
-- Use 'Int32', 'Int64', etc. explicitely.
instance ToPgColumn O.PGInt8 Int64 where toPgColumn = O.pgInt8
instance ToPgColumn O.PGFloat4 Float where toPgColumn = pgFloat4
instance ToPgColumn O.PGFloat8 Float where toPgColumn = pgFloat8
instance ToPgColumn O.PGFloat8 Double where toPgColumn = O.pgDouble
instance ToPgColumn O.PGText Data.Text.Text where toPgColumn = O.pgStrictText
instance ToPgColumn O.PGText Data.Text.Lazy.Text where toPgColumn = O.pgLazyText
instance ToPgColumn O.PGBytea Data.ByteString.ByteString where toPgColumn = O.pgStrictByteString
instance ToPgColumn O.PGBytea Data.ByteString.Lazy.ByteString where toPgColumn = O.pgLazyByteString
instance ToPgColumn O.PGTimestamptz Data.Time.UTCTime where toPgColumn = O.pgUTCTime
instance ToPgColumn O.PGTimestamp Data.Time.LocalTime where toPgColumn = O.pgLocalTime
instance ToPgColumn O.PGTime Data.Time.TimeOfDay where toPgColumn = O.pgTimeOfDay
instance ToPgColumn O.PGDate Data.Time.Day where toPgColumn = O.pgDay
instance ToPgColumn O.PGUuid Data.UUID.UUID where toPgColumn = O.pgUUID
instance ToPgColumn O.PGCitext (Data.CaseInsensitive.CI Data.Text.Text) where toPgColumn = O.pgCiStrictText
instance ToPgColumn O.PGCitext (Data.CaseInsensitive.CI Data.Text.Lazy.Text) where toPgColumn = O.pgCiLazyText
instance Data.Aeson.ToJSON hs => ToPgColumn O.PGJson hs where toPgColumn = O.pgLazyJSON . Data.Aeson.encode
instance Data.Aeson.ToJSON hs => ToPgColumn O.PGJsonb hs where toPgColumn = O.pgLazyJSONB . Data.Aeson.encode

--------------------------------------------------------------------------------

-- | Like 'toPgColumn', but wraps the resulting 'O.Column' in a 'TC'.
toPgTC :: ToPgColumn pg hs => t -> C c -> hs -> Tagged (TC t c) (O.Column pg)
toPgTC _ _ = Tagged . toPgColumn
{-# INLINE toPgTC #-}

-- | Like 'toPgColumnN', but wraps the resulting 'O.Column' in a 'TC'.
toPgTCN :: ToPgColumn pg hs => t -> C c -> hs -> Tagged (TC t c) (O.Column (O.Nullable pg))
toPgTCN _ _ = Tagged . toPgColumnN
{-# INLINE toPgTCN #-}

--------------------------------------------------------------------------------

-- | Lens to the value of a column.
-- 
-- Mnemonic: the COLumn.
col :: HL.HLensCxt (TC t c) HL.Record xs xs' a a'
    => C c
    -> Lens (Rec t xs) (Rec t xs') (Tagged (TC t c) a) (Tagged (TC t c) a')
col prx = cola prx . _Unwrapped
{-# INLINE col #-}

-- | Lens to the value of a column without the 'TC' tag.
--
-- Most of the time you'll want to use 'col' instead, but this might be more useful
-- when trying to change the type of @a@ during an update, or when implementing
-- 'unHsR'.
--
-- Mnemonic: The COLumn's A.
cola :: HL.HLensCxt (TC t c) HL.Record xs xs' a a'
     => C c
     -> Lens (Rec t xs) (Rec t xs') a a'
cola = go where -- just to hide the "forall" from the haddocks
  go
    :: forall t c xs xs' a a'. HL.HLensCxt (TC t c) HL.Record xs xs' a a'
    => C c -> Lens (Rec t xs) (Rec t xs') a a'
  go = \_ -> _Wrapped . HL.hLens (HL.Label :: HL.Label (TC t c))
  {-# INLINE go #-}
{-# INLINE cola #-}


-- | Like 'cola', but just a 'Setter' that takes constant 'ToPgColumn' values.
-- 
-- Mnemonic: the COLumn's A constant Value.
colav
  :: (HL.HLensCxt (TC t c) HL.Record xs xs' (O.Column a) (O.Column a'), ToPgColumn a' hs)
  => C c -> Setter (Rec t xs) (Rec t xs') (O.Column a) hs
colav c = cola c . sets (\f ca -> toPgColumn (f ca))
{-# INLINE colav #-}

--------------------------------------------------------------------------------
-- Binary operations on columns
--
-- We need to make a difference between nullable and not nullable columns due to
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/97

class (NotNullable ua, NotNullable ub) => PgCompatOp2 (ua :: *) (ub :: *) (la :: *) (lb :: *) | la -> ua, lb -> ub where
  -- | Wrap the given function so that it supports input types other than 'O.Column' as well.
  pgCompatOp2 :: (O.Column ua -> O.Column ub -> O.Column r) -> la -> lb -> O.Column r
instance (NotNullable a, NotNullable b) => PgCompatOp2 a b (O.Column a) (O.Column b) where
  pgCompatOp2 f = f
instance (NotNullable x, Comparable ta ca tb cb x) => PgCompatOp2 x x (Tagged (TC ta ca) (O.Column x)) (Tagged (TC tb cb) (O.Column x)) where
  pgCompatOp2 f (Tagged a) (Tagged b) = f a b
instance (NotNullable a, NotNullable b) => PgCompatOp2 a b (Tagged ta (O.Column a)) (O.Column b) where
  pgCompatOp2 f (Tagged a) b = f a b
instance (NotNullable a, NotNullable b) => PgCompatOp2 a b (O.Column a) (Tagged tb (O.Column b)) where
  pgCompatOp2 f a (Tagged b) = f a b

class (NotNullable ua, NotNullable ub) => PgCompatOp2N (ua :: *) (ub :: *) (la :: *) (lb :: *) | la -> ua, lb -> ub where
  -- | PostgreSQL version of @(liftA2 :: (Maybe a -> b -> c) -> Maybe a -> Maybe b -> Maybe c)@,
  -- but it wraps the given function so that it supports input types other than 'O.Column' as well.
  pgCompatOp2N :: (O.Column ua -> O.Column ub -> O.Column r) -> la -> lb -> O.Column (O.Nullable r)
instance (NotNullable a, NotNullable b) => PgCompatOp2N a b (O.Column (O.Nullable a)) (O.Column (O.Nullable b)) where
  pgCompatOp2N f a b = O.ifThenElse (isNull a O..|| isNull b) O.null
      (O.toNullable (unsafeUnNullable a `f` unsafeUnNullable b))
instance (NotNullable x, Comparable ta ca tb cb x) => PgCompatOp2N x x (Tagged (TC ta ca) (O.Column (O.Nullable x))) (Tagged (TC tb cb) (O.Column (O.Nullable x))) where
  pgCompatOp2N f (Tagged a) (Tagged b) = pgCompatOp2N f a b
instance (NotNullable a, NotNullable b) => PgCompatOp2N a b (Tagged ta (O.Column (O.Nullable a))) (O.Column (O.Nullable b)) where
  pgCompatOp2N f (Tagged a) b = pgCompatOp2N f a b
instance (NotNullable a, NotNullable b) => PgCompatOp2N a b (O.Column (O.Nullable a)) (Tagged tb (O.Column (O.Nullable b))) where
  pgCompatOp2N f a (Tagged b) = pgCompatOp2N f a b

---

eq :: PgCompatOp2 x x a b => a -> b -> O.Column O.PGBool
eq = pgCompatOp2 (O..==)

eqn :: PgCompatOp2N x x a b => a -> b -> O.Column (O.Nullable O.PGBool)
eqn = pgCompatOp2N (O..==)

lt :: (O.PGOrd x, PgCompatOp2 x x a b) => a -> b -> O.Column O.PGBool
lt = pgCompatOp2 (O..<)

ltn :: (O.PGOrd x, PgCompatOp2N x x a b) => a -> b -> O.Column (O.Nullable O.PGBool)
ltn = pgCompatOp2N (O..<)

or_ :: PgCompatOp2 O.PGBool O.PGBool a b => a -> b -> O.Column O.PGBool
or_ = pgCompatOp2 (O..||)

orn :: PgCompatOp2N O.PGBool O.PGBool a b => a -> b -> O.Column (O.Nullable O.PGBool)
orn = pgCompatOp2N (O..||)

--------------------------------------------------------------------------------
-- Ordering

-- | Ascending order, no @NULL@s involved.
asc :: (WrappedCol w b, NotNullable b, O.PGOrd b) => (a -> w) -> O.Order a
asc f = O.asc (view _UnwrappedCol . f)

-- | Ascending order, @NULL@s last.
ascnl :: (WrappedNullableCol w b, O.PGOrd b) => (a -> w) -> O.Order a
ascnl f = O.asc (unsafeUnNullable . view _UnwrappedNullableCol . f)

-- | Ascending order, @NULL@s first.
ascnf :: (WrappedNullableCol w b, O.PGOrd b) => (a -> w) -> O.Order a
ascnf f = O.ascNullsFirst (unsafeUnNullable . view _UnwrappedNullableCol . f)

-- | Descending order, no @NULL@s involved.
desc :: (WrappedCol w b, NotNullable b, O.PGOrd b) => (a -> w) -> O.Order a
desc f = O.desc (view _UnwrappedCol . f)

-- | Descending order, @NULL@s first.
descnf :: (WrappedNullableCol w b, O.PGOrd b) => (a -> w) -> O.Order a
descnf f = O.desc (unsafeUnNullable . view _UnwrappedNullableCol . f)

-- | Descending order, @NULL@s last.
descnl :: (WrappedNullableCol w b, O.PGOrd b) => (a -> w) -> O.Order a
descnl f = O.descNullsLast (unsafeUnNullable . view _UnwrappedNullableCol . f)

--------------------------------------------------------------------------------

-- | Like 'Control.Lens.Wrapped', but the "unwrapped" type is expected to be
-- @('O.Column' a))@.
class WrappedCol (w :: *) (a :: *) | w -> a where
  _UnwrappedCol :: Iso' w (O.Column a)
instance WrappedCol (O.Column a) a where
  _UnwrappedCol = id
instance WrappedCol w a => WrappedCol (Tagged (TC t c) w) a where
  _UnwrappedCol = _Wrapped . _UnwrappedCol

--------------------------------------------------------------------------------

-- | Horrible hack to workaround the current represenation for nullable columns.
-- See https://github.com/tomjaguarpaw/haskell-opaleye/issues/97
type family NotNullable (x :: *) :: Constraint where
  NotNullable (O.Nullable x) = "NotNullable" ~ "NotNullable: expected `x` but got `Nullable x`"
  NotNullable x = ()

-- | Like 'Control.Lens.Wrapped', but the "unwrapped" type is expected to be
-- @('O.Column' ('O.Nullable' a))@.
class WrappedNullableCol (w :: *) (a :: *) | w -> a where
  _UnwrappedNullableCol :: Iso' w (O.Column (O.Nullable a))
instance WrappedNullableCol (O.Column (O.Nullable a)) a where
  _UnwrappedNullableCol = id
instance WrappedNullableCol w a => WrappedNullableCol (Tagged (TC t c) w) a where
  _UnwrappedNullableCol = _Wrapped . _UnwrappedNullableCol

-- | Like 'O.isNull', but also works with the return type of
-- @('view' ('col' ('C' :: 'C' "foo"))@
isNull :: WrappedNullableCol w a => w -> O.Column O.PGBool
isNull = O.isNull . view _UnwrappedNullableCol

-- | Flatten @('O.Column' ('O.Nullable' 'O.PGBool'))@ or compatible
-- (see 'WrappedNullableCol') to @('O.Column' 'O.PGBool')@. An outer @NULL@ is
-- converted to @TRUE@.
nullTrue :: WrappedNullableCol w O.PGBool => O.QueryArr w (O.Column O.PGBool)
nullTrue = arr (\w -> let w' = view _UnwrappedNullableCol w
                      in O.ors [O.isNull w', unsafeUnNullable w'])

-- | Flatten @('O.Column' ('O.Nullable' 'O.PGBool'))@
-- to @('O.Column' 'O.PGBool')@. An outer @NULL@ is converted to @FALSE@.
nullFalse :: O.QueryArr (O.Column (O.Nullable O.PGBool)) (O.Column O.PGBool)
nullFalse = arr (\w -> O.not (O.isNull w) O..|| unsafeUnNullable w)

--------------------------------------------------------------------------------

-- | A generalization of product profunctor adaptors such as 'PP.p1', 'PP.p4', etc.
--
-- The functional dependencies make type inference easier, but also forbid some
-- otherwise acceptable instances. See the instance for 'Tagged' for example.
class P.Profunctor p => ProductProfunctorAdaptor p l ra rb | p l -> ra rb, p ra rb -> l where
  ppa :: l -> p ra rb

ppaUnTagged :: P.Profunctor p => p a b -> p (Tagged ta a) (Tagged tb b)
ppaUnTagged = P.dimap unTagged Tagged
{-# INLINE ppaUnTagged #-}

ppaTagged :: P.Profunctor p => Tagged tpab (p a b) -> p (Tagged ta a) (Tagged tb b)
ppaTagged = ppaUnTagged . unTagged
{-# INLINE ppaTagged #-}

-- | Due to the functional dependencies in 'ProductProfunctorAdaptor', this instance is not as
-- polymorphic as it could be in @t@. Use 'ppaTagged' instead for a fully polymorphic version.
instance P.Profunctor p => ProductProfunctorAdaptor p (Tagged t (p a b)) (Tagged t a) (Tagged t b) where
  ppa = ppaTagged
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
  ppa = P.dimap unRecord HL.Record . ppa . unRecord
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
  {-# INLINABLE def #-}

-- | Orphan. 'Opaleye.SOT.Internal'.
instance
    ( PP.ProductProfunctor p, PP.Default p (HList as) (HList bs)
    ) => PP.Default p (HL.Record as) (HL.Record bs) where
  def = P.dimap unRecord HL.Record PP.def
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
  {-# INLINABLE def #-}

-- | Orphan. 'Opaleye.SOT.Internal'.
instance
    ( PP.ProductProfunctor p, PP.Default p (HList as) (Maybe (HList bs))
    ) => PP.Default p (HL.Record as) (Maybe (HL.Record bs)) where
  def = P.dimap unRecord (fmap HL.Record) PP.def
  {-# INLINE def #-}

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
  {-# INLINE hDistributeProxy #-}

--------------------------------------------------------------------------------

unRecord :: HL.Record xs -> HList xs
unRecord = \(HL.Record x) -> x
{-# INLINE unRecord #-}

--------------------------------------------------------------------------------
-- Belongs in Opaleye

pgFloat4 :: Float -> O.Column O.PGFloat4
pgFloat4 = OI.literalColumn . OI.DoubleLit . float2Double

pgFloat8 :: Float -> O.Column O.PGFloat8
pgFloat8 = OI.literalColumn . OI.DoubleLit . float2Double

unsafeUnNullable :: O.Column (O.Nullable a) -> O.Column a
unsafeUnNullable = O.unsafeCoerceColumn
{-# INLINE unsafeUnNullable #-}
