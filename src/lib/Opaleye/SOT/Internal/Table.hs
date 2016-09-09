{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
--
-- | This is an internal module. You are very discouraged from using it directly.
module Opaleye.SOT.Internal.Table
 ( RCap(..)
 , WCap(..)
 , WDef(..)
 , wdef
 , Column(..)
 , Column_Name
 , Column_NameSym0
 , Column_PgType
 , Column_PgTypeSym0
 , Column_PgR
 , Column_PgRSym0
 , Column_PgRN
 , Column_PgRNSym0
 , Column_PgW
 , Column_PgWSym0
 , Column_HsR
 , Column_HsRSym0
 , Column_HsI
 , Column_HsISym0
 , HsR(..)
 , Columns_NamedHsR
 , HsI(..)
 , Columns_NamedHsI
 , PgR(..)
 , Columns_NamedPgR
 , PgRN(..)
 , Columns_NamedPgRN
 , PgW(..)
 , Columns_NamedPgW
 , TableR
 , TableRW
 , Table
 , Database
 , SchemaName
 , TableName
 , Columns
 , mkHsI
 , MkHsI
 , C(..)
 , hsi
 , Columns_CNamedFunArgs
 , FnPgWfromHsIField(..)
 , pgWfromHsI
 , FnPgWfromPgRField(..)
 , pgWfromPgR
 , colProps_ro
 , colProps_ron
 , colProps_wr
 , colProps_wrn
 , colProps_wdr
 , colProps_wdrn
 , Column_Props
 , Column_PropsSym0
 , IColumn_Props(..)
 , RDistributeColProps(..)
 , ColLens(..)
 , Column_ByName
 , RawTable(..)
 , rawTableRW
 , rawTableRO
 , All
 ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Fix (MonadFix(..))
import qualified Data.Aeson as Aeson
import           Data.Data (Data)
import           Data.Kind
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as PP
import qualified Data.Promotion.Prelude.List as List (Map)
import           Data.Promotion.Prelude.Bool (If)
import           Data.Proxy (Proxy(..))
import           Data.Singletons
import           Data.Tagged
import           Data.Type.Equality
import           Data.Typeable (Typeable)
import           Data.Void
import           GHC.Exts (Constraint)
import qualified GHC.OverloadedLabels as GHC
import           GHC.Generics (Generic)
import qualified GHC.TypeLits as GHC
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Opaleye as O
import qualified Opaleye.Internal.PackMap as OI
import qualified Opaleye.Internal.Table as OI
import qualified Opaleye.Internal.TableMaker as OI

import qualified Opaleye.SOT.Internal.Profunctors as PP
import           Opaleye.SOT.Internal.Record (Record(RNil, RCons))
import qualified Opaleye.SOT.Internal.Record as Record
import           Opaleye.SOT.Internal.Singletons ((:&&&$$$))
import           Opaleye.SOT.Internal.Kol
import           Opaleye.SOT.Internal.Koln

--------------------------------------------------------------------------------

-- | Reading capabilities for a column.
data RCap
  = R  -- ^ Only non @NULL@ values can be read from this column.
  | RN -- ^ Possibly read @NULL@ from this column.

-- | Writing capabilities for a column.
data WCap
  = W  -- ^ Can write a specific value to this column.
  | WD -- ^ Can write a specific value as well as @DEFAULT@. See 'WDef'.
  | RO -- ^ Can't write to the column, it is read-only.
       --   This is particularly useful for read-only views.

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

-- | Like @'Maybe' a@.
instance Aeson.FromJSON a => Aeson.FromJSON (WDef a) where
  parseJSON v = fmap (maybe WDef WVal) (Aeson.parseJSON v)
  {-# INLINE parseJSON #-}

-- | Like @'Maybe' a@.
instance Aeson.ToJSON a => Aeson.ToJSON (WDef a) where
  toJSON = Aeson.toJSON . wdef Nothing Just
  {-# INLINE toJSON #-}
  toEncoding = Aeson.toEncoding . wdef Nothing Just
  {-# INLINE toEncoding #-}

--------------------------------------------------------------------------------

-- | Column description.
--
-- This is only used as a promoted datatype expected to have kind
-- @'Col' 'Symbol' 'WCap' 'RCap' 'Type' 'Type'@.
--
-- * @name@: Column name.
--
-- * @wcap@: Writing capabilities for the column. See 'W'.
--
-- * @rcap@: Reading capabilities for the column. See 'R'.
--
-- * @pgType@: Type of the column value used in Opaleye queries as index to
--   'Kol' or 'Koln'. This must be an instance of 'PgTyped'.
--
-- * @hsType@: Type of the column value used in Haskell outside Opaleye
--   queries. Hint: don't use something like @'Maybe' 'Bool'@ here if you
--   want to indicate that this is an optional 'Bool' column. Instead, use
--   'Bool' here and 'RN' in the @r@ field.
--
-- /Notice that 'Col' is very different from 'Kol' and 'Koln': 'Kol' and 'Koln'/
-- /are used at runtime for manipulating values stored in columns, 'Col' is used/
-- /to describe the properties of a column at compile time./
data Column name wcap rcap pgType hsType
   = Column name wcap rcap pgType hsType

--

type family Column_Name (col :: Column Symbol WCap RCap Type Type) :: Symbol where
  Column_Name ('Column n w r p h) = n
data Column_NameSym0 (col :: TyFun (Column Symbol WCap RCap Type Type) Symbol)
type instance Apply Column_NameSym0 col = Column_Name col

type family Column_PgType (col :: Column Symbol WCap RCap Type Type) :: Type where
  Column_PgType ('Column n w r p h) = p
data Column_PgTypeSym0 (col :: TyFun (Column Symbol WCap RCap Type Type) Type)
type instance Apply Column_PgTypeSym0 col = Column_PgType col

type family Column_PgR (col :: Column Symbol WCap RCap Type Type) :: Type where
  Column_PgR ('Column n w 'R  p h) = Kol p
  Column_PgR ('Column n w 'RN p h) = Koln p
data Column_PgRSym0 (col :: TyFun (Column Symbol WCap RCap Type Type) Type)
type instance Apply Column_PgRSym0 col = Column_PgR col

type family Column_PgRN (col :: Column Symbol WCap RCap Type Type) :: Type where
  Column_PgRN ('Column n w r p h) = Koln p
data Column_PgRNSym0 (col :: TyFun (Column Symbol WCap RCap Type Type) Type)
type instance Apply Column_PgRNSym0 col = Column_PgRN col

type family Column_PgW (col :: Column Symbol WCap RCap Type Type) :: Type where
  Column_PgW ('Column n 'W  r p h) = Column_PgR ('Column n 'W r p h)
  Column_PgW ('Column n 'WD r p h) = WDef (Column_PgR ('Column n 'WD r p h))
  Column_PgW ('Column n 'RO r p h) = GHC.TypeError
    ('GHC.Text "The column " 'GHC.:<>: 'GHC.ShowType n 'GHC.:<>:
     'GHC.Text " is not a writeable.")
data Column_PgWSym0 (col :: TyFun (Column Symbol WCap RCap Type Type) Type)
type instance Apply Column_PgWSym0 col = Column_PgW col

type family Column_HsR (col :: Column Symbol WCap RCap Type Type) :: Type where
  Column_HsR ('Column n w 'R  p h) = h
  Column_HsR ('Column n w 'RN p h) = Maybe h
data Column_HsRSym0 (col :: TyFun (Column Symbol WCap RCap Type Type) Type)
type instance Apply Column_HsRSym0 col = Column_HsR col

type family Column_HsI (col :: Column Symbol WCap RCap Type Type) :: Type where
  Column_HsI ('Column n 'W  r p h) = Column_HsR ('Column n 'W r p h)
  Column_HsI ('Column n 'WD r p h) = WDef (Column_HsR ('Column n 'WD r p h))
  Column_HsI ('Column n 'RO r p h) = GHC.TypeError
    ('GHC.Text "The column " 'GHC.:<>: 'GHC.ShowType n 'GHC.:<>:
     'GHC.Text " is not a writeable.")
data Column_HsISym0 (col :: TyFun (Column Symbol WCap RCap Type Type) Type)
type instance Apply Column_HsISym0 col = Column_HsI col

--------------------------------------------------------------------------------
-- Note: By using newtype wrappers, instead of just type synonyms for 'Record',
-- we can provide nicer error messages to the users at the small cost of a bit
-- more complicated implementation for us (e.g., the implementation of `col`
-- could be generalized otherwise).

-- | Expected output type for 'O.runQuery' on a @'PgR' t@.
--
-- Important: If you are expecting a @'PgR' t@ on the right side
-- of a 'O.leftJoin', you will need to use @'Maybe' ('PgR' t)@.
--
-- Mnemonic: Haskell Read.
newtype HsR t = HsR { unHsR :: Record (Columns_NamedHsR t) }
type Columns_NamedHsR t = List.Map (Column_NameSym0 :&&&$$$ Column_HsRSym0) (Columns t)

deriving instance Eq (Record (Columns_NamedHsR t)) => Eq (HsR t)
deriving instance Ord (Record (Columns_NamedHsR t)) => Ord (HsR t)
deriving instance Show (Record (Columns_NamedHsR t)) => Show (HsR t)
deriving instance Generic (Record (Columns_NamedHsR t)) => Generic (HsR t)
instance (Aeson.FromJSON (Record (Columns_NamedHsR t)), Generic (HsR t)) => Aeson.FromJSON (HsR t)
instance (Aeson.ToJSON (Record (Columns_NamedHsR t)), Generic (HsR t)) => Aeson.ToJSON (HsR t)

instance
  ( Profunctor p
  , PP.Default p (Record (Columns_NamedHsR t)) (Record (Columns_NamedHsR t))
  ) => PP.Default p (HsR t) (HsR t) where
    def = P.dimap unHsR HsR PP.def
    {-# INLINE def #-}

---
-- | @'HsI' t@ is the Haskell representation of Haskell values to be inserted to
-- the database, as taken by "Opaleye.SOT.Run.runInsert".
--
-- Mnemonic: Haskell Insert.
newtype HsI t = HsI { unHsI :: Record (Columns_NamedHsI t) }
type Columns_NamedHsI t = List.Map (Column_NameSym0 :&&&$$$ Column_HsISym0) (Columns t)

deriving instance Eq (Record (Columns_NamedHsI t)) => Eq (HsI t)
deriving instance Ord (Record (Columns_NamedHsI t)) => Ord (HsI t)
deriving instance Show (Record (Columns_NamedHsI t)) => Show (HsI t)
deriving instance Generic (Record (Columns_NamedHsI t)) => Generic (HsI t)
instance (Aeson.FromJSON (Record (Columns_NamedHsI t)), Generic (HsI t)) => Aeson.FromJSON (HsI t)
instance (Aeson.ToJSON (Record (Columns_NamedHsI t)), Generic (HsI t)) => Aeson.ToJSON (HsI t)

instance
  ( Profunctor p
  , PP.Default p (Record (Columns_NamedHsI t)) (Record (Columns_NamedHsI t))
  ) => PP.Default p (HsI t) (HsI t) where
    def = P.dimap unHsI HsI PP.def
    {-# INLINE def #-}

---
-- | Output type of @'Opaleye.SOT.query' (... :: 'Table' t)@.
--
-- Mnemonic: PostGresql Read.
newtype PgR t = PgR { unPgR :: Record (Columns_NamedPgR t) }
type Columns_NamedPgR t = List.Map (Column_NameSym0 :&&&$$$ Column_PgRSym0) (Columns t)

instance
  ( Profunctor p
  , PP.Default p (Record (Columns_NamedPgR t)) (Record (Columns_NamedPgR t))
  ) => PP.Default p (PgR t) (PgR t) where
    def = P.dimap unPgR PgR PP.def
    {-# INLINE def #-}

instance
  ( Profunctor p
  , PP.Default p (Record (Columns_NamedPgR t)) (Record (Columns_NamedHsR t))
  ) => PP.Default p (PgR t) (HsR t) where
  def = P.dimap unPgR HsR PP.def
  {-# INLINE def #-}

instance
  ( Profunctor p
  , PP.Default p (Record (Columns_NamedPgR t)) (Record (Columns_NamedPgRN t))
  ) => PP.Default p (PgR t) (PgRN t) where
  def = P.dimap unPgR PgRN PP.def
  {-# INLINE def #-}

---
-- | Like @'PgRN' t@ but every field is 'Koln', as in the output type of the
-- right hand side of a 'leftJoin' with @'Table' t@.
--
-- Mnemonic: PostGresql Read Nulls.
newtype PgRN t = PgRN { unPgRN :: Record (Columns_NamedPgRN t) }
type Columns_NamedPgRN t = List.Map (Column_NameSym0 :&&&$$$ Column_PgRNSym0) (Columns t)

instance
  ( Profunctor p
  , PP.Default p (Record (Columns_NamedPgRN t)) (Record (Columns_NamedPgRN t))
  ) => PP.Default p (PgRN t) (PgRN t) where
    def = P.dimap unPgRN PgRN PP.def
    {-# INLINE def #-}

instance
  ( Profunctor p
  , PP.Default p (Record (Columns_NamedPgRN t)) (Maybe (Record (Columns_NamedHsR t)))
  ) => PP.Default p (PgRN t) (Maybe (HsR t)) where
  def = P.dimap unPgRN (fmap HsR) PP.def
  {-# INLINE def #-}

---
-- | Representation of PostgreSQL values to be written to the database. This
-- type can be used as input for "Opaleye.SOT.Run.runInsertRaw" and friends.
--
-- An @'HsI' t@ can always be converted to a @'PgW' t@ using 'pgWfromHsI'.
--
-- Mnemonic: PostGresql Write.
newtype PgW t = PgW { unPgW :: Record (Columns_NamedPgW t) }
type Columns_NamedPgW t = List.Map (Column_NameSym0 :&&&$$$ Column_PgWSym0) (Columns t)

instance
  ( Profunctor p
  , PP.Default p (Record (Columns_NamedPgW t)) (Record (Columns_NamedPgW t))
  ) => PP.Default p (PgW t) (PgW t) where
    def = P.dimap unPgW PgW PP.def
    {-# INLINE def #-}

--------------------------------------------------------------------------------

-- | A @t@ in @'Table' t@ that satisfies these constraints can be used for read
-- purposes (e.g., with 'query').

-- Note: All these constraints need to be satisfied by tools that work with
-- 'Table' in a read-only manner. It's easier to just write all the constraints
-- once here and make 'TableR' a superclass of 'Table'. Moreover, they enforce
-- some sanity constraints on our 'Table' so that we can get early compile time
-- errors.
type TableR t =
  ( All PgTyped (List.Map (Column_PgTypeSym0) (Columns t))
  , KnownSymbol (SchemaName t)
  , KnownSymbol (TableName t)
  , RDistributeColProps (Columns t)
  , PP.Default OI.ColumnMaker (PgR t) (PgR t)
  , PP.ProductProfunctorAdaptor
       O.TableProperties
       (Record (List.Map (Column_NameSym0 :&&&$$$ Column_PropsSym0) (Columns t)))
       Void
       (Record (Columns_NamedPgR t))
  )

-- | A @t@ in @'Table' t@ that satisfies these constraints can be used for
-- read-write purposes (e.g., with 'runInsert').
type TableRW t =
  ( TableR t
  , Record.RMap FnPgWfromPgRField (Columns_NamedPgR t) (Columns_NamedPgW t)
  , Record.RMap FnPgWfromHsIField (Columns_NamedHsI t) (Columns_NamedPgW t)
  , PP.ProductProfunctorAdaptor
       O.TableProperties
       (Record (List.Map (Column_NameSym0 :&&&$$$ Column_PropsSym0) (Columns t)))
       (Record (Columns_NamedPgW t))
       (Record (Columns_NamedPgR t))
  )

-- | A proxy type used to identify a PostgreSQL table at the type level.
--
-- You will usually write your `Table` instances this way:
--
-- @
-- data Users
-- data instance Table Users = TUsers
-- @
--
-- 'TUsers' is the term level representation for our users table, identified as
-- @'Table' 'Users'@ at the type level.
--
-- Now, functions such as 'query' expect a 'Table' as an argument. You will be
-- able to use 'TUsers' as that argument.
--
-- Appart from your @'Table' 'Users'@, your tables will need to define other
-- instances, all indexed by 'Users', namely: 'Database', 'SchemaName',
-- 'TableName', and 'Columns'.
--
-- Note: Compared to using other proxy types such as 'Proxy', 'Table' leads to
-- much better type inference, because in 'Table' the type depends on the
-- constructor, whereas in 'Proxy' it doesn't.
data family Table (t :: k) :: Type

-- | @'Database' t@ specifies the database where @'Table' t@ exists. This should
-- be an unique identifier per database so that accidental operations across
-- different databases can be forbidden. All tables belonging to the same
-- database should specify the same 'Database'.
--
-- The unique identifier can be any type. An interesting idea to keep code
-- organized is to have an ADT per database with one constructor per table in
-- that database. That way, using 'DataKinds', you can write 'Database' and
-- 'Table' instances this way:
--
-- @
-- data CoreDatabase
--   = Users
--   | Articles
--
-- data instance 'Table' 'Users = TUsers
-- type instance 'Database' 'Users = CoreDatabase
-- ...
--
-- data instance 'Table' 'Articles = TArticles
-- type instance 'Database' 'Articles = CoreDatabase
-- ...
-- @
--
-- And similarly for 'SchemaName', 'TableName' and 'Columns'.
type family Database (t :: k) :: k'

-- | @'SchemaName' t@ specifies the literal name of the PostgreSQL schema where
-- @'Table' t@ exists. PostgreSQL uses a schema named @"public"@ by default, so
-- you might use that value here unless you've explicitely chosen a different
-- schema for your table.
type family SchemaName (t :: k) :: Symbol

-- | @'SchemaName' t@ specifies the literal name of the PostgreSQL table for
-- @'Table' t@.
type family TableName (t :: k) :: Symbol

-- | @'Columns' t@ specifies the columns for @'Table' t@. This a type-level list
-- of type-level 'Column' values. Please see the documentation for 'Column' to
-- learn more.
type family Columns (t :: k) :: [Column Symbol WCap RCap Type Type]

--------------------------------------------------------------------------------

-- | Helper function to safely build an @'HsI' t@.
--
-- The type of this function isn't easy to understand, but an example should
-- clarify its usage. We will asume we have a @TPerson@ which is an instance of
-- 'Table', and @Person@ datatype as follows:
--
-- @
-- data TPerson
--
-- instance 'Table' TPerson where
--   data 'T' TPerson = TPerson
--   type 'Database' TPerson = ... not important ...
--   type 'SchemaName' TPerson = ... not important ...
--   type 'TableName' TPerson = ... not important ...
--   type 'Columns' TPerson
--     = '[ ''Col' "name" 'W' 'R' 'O.PGText' 'Data.Text.Text'
--        , ''Col' "age" 'W' 'R' 'O.PGInt4' 'Int32'
--        ]
--
-- data Person = Person
--   { _personName :: 'Text'
--   , _personAge :: 'Int'
--   }
-- @
--
-- With that in place, and with the 'OverloadedLabels' GHC extension enabled,
-- we can use 'mkHsI' as follows:
--
-- @
-- personToHsI :: Person -> 'HsI' TPerson
-- personToHsI person =
--   'mkHsI' TPerson
--     ('hsi' #name (_personName person))
--     ('hsi' #age (_personAge age))
-- @
--
-- The column names must appear in the same order as they do in @'Columns'
-- TPerson@, otherwise you will get a compiler error.
--
-- As long as your column name is also a valid Haskell name, you can use the
-- 'OverloadedLabels' syntax as above to specify the name of the column.
-- However, if your column is name is more esoteric, you can just replace the
-- call to 'hsi' with 'Tagged', specifying the column name as 'Symbol' between
-- quotes:
--
-- @
-- personToHsI :: Person -> 'HsI' TPerson
-- personToHsI person =
--   'mkHsI' TPerson
--     ('Tagged' \@ "name" (_personName person))
--     ('Tagged' \@ "age" (_personAge age))
-- @
--
-- Note: Technically, you can use the 'Tagged' without specifying the column
-- name and it will work just fine. However, for maintenance purposes (i.e., in
-- case you change the order of your columns in the future), we highly recommend
-- being explicit about the column name.
--
-- Note: You are not required to use this function to build an @'HsI' t@ if
-- working with 'HsI', 'Record.RCons' and 'Record.RNil' (not exported, from
-- "Opaleye.SOT.Internal.Record") are sufficient to you, this is
-- just a convenience.
mkHsI :: Table t -> MkHsI t
mkHsI _ = Record.rBuildSymbol
{-# INLINE mkHsI #-}

-- | See 'mkHsI'.
type MkHsI t = Record.RBuild' ('[] :: [(Symbol,Type)])
                              (Columns_CNamedFunArgs Column_HsISym0 (HsI t) (Columns t))
                           => (Columns_CNamedFunArgs Column_HsISym0 (HsI t) (Columns t))

-- | Just like @'Proxy' c@, but with the different name to prevent clashing the
-- 'GHC.IsLabel' instance with third party code. Used by 'hsi'.
data C (c :: Symbol) = C

-- | Used by 'hsi'.
instance GHC.IsLabel (c :: Symbol) (C c) where
  fromLabel _ = C
  {-# INLINE fromLabel #-}

-- | Helper function for building an 'HsI'. To construct a 'C' use the GHC's
-- @OverloadedLabels@ extension (e.g., @#foo == ('C' :: 'C' "foo"))@.
--
-- See 'mkHsI' for a full example.
hsi :: C (c :: Symbol) -> x -> Tagged c x
hsi _ = Tagged
{-# INLINE hsi #-}

instance Record.RBuild' axs (Record (Columns_NamedHsI t)) => Record.RBuild' axs (HsI t) where
  rBuild' raxs = HsI (Record.rBuild' raxs)
  {-# INLINE rBuild' #-}

-- | Used by 'MkHsI'.
type family Columns_CNamedFunArgs
    (f :: TyFun (Column Symbol WCap RCap Type Type) Type -> Type)
    (z :: Type) (cols :: [Column Symbol WCap RCap Type Type]) :: Type
 where
  Columns_CNamedFunArgs f z '[] = z
  Columns_CNamedFunArgs f z (x ': xs) =
    Tagged (Column_Name x) (Apply f x) -> Columns_CNamedFunArgs f z xs

--------------------------------------------------------------------------------

-- | To be used with 'Record.ApplyAB'.
data FnPgWfromHsIField = FnPgWfromHsIField
instance Record.ApplyAB FnPgWfromHsIField x x where
  applyAB _ = id
instance (ToKol a b) => Record.ApplyAB FnPgWfromHsIField a (Kol b) where
  applyAB _ = kol
instance (ToKol a b) => Record.ApplyAB FnPgWfromHsIField (WDef a) (WDef (Kol b)) where
  applyAB _ = fmap kol
instance (ToKol a b) => Record.ApplyAB FnPgWfromHsIField (Maybe a) (Koln b) where
  applyAB _ = maybe nul koln
instance (ToKol a b) => Record.ApplyAB FnPgWfromHsIField (WDef (Maybe a)) (WDef (Koln b)) where
  applyAB _ = fmap (maybe nul koln)

-- | Convert a custom Haskell type to a representation appropiate for /inserting/
-- it as a new row using 'Opaleye.SOT.Run.runInsert'.
pgWfromHsI :: TableRW t => HsI t -> PgW t
pgWfromHsI = PgW . Record.rMap FnPgWfromHsIField . unHsI
{-# INLINE pgWfromHsI #-}

--------------------------------------------------------------------------------

-- | To be used with 'Record.ApplyAB'.
data FnPgWfromPgRField = FnPgWfromPgRField
instance Record.ApplyAB FnPgWfromPgRField x x where
  applyAB _ = id
instance Record.ApplyAB FnPgWfromPgRField (Kol a) (WDef (Kol a)) where
  applyAB _ = WVal
instance Record.ApplyAB FnPgWfromPgRField (Koln a) (WDef (Koln a)) where
  applyAB _ = WVal

-- | Convert a @('PgR' t)@ resulting from a 'O.queryTable'-like operation
-- to a @('PgW' t)@ that can be used in a 'Opaleye.SOT.runUpdate'-like
-- operation.
pgWfromPgR :: TableRW t => PgR t -> PgW t
pgWfromPgR = PgW . Record.rMap FnPgWfromPgRField . unPgR
{-# INLINE pgWfromPgR #-}

--------------------------------------------------------------------------------

-- | Column properties: Read only (not nullable).
colProps_ro :: PgTyped a => String -> O.TableProperties Void (Kol a)
colProps_ro n = P.rmap Kol $ OI.TableProperties
  (OI.Writer (OI.PackMap (\f ws -> f (fmap absurd ws, n)))) -- <- dead code
  (OI.View (OI.runViewColumnMaker OI.tableColumn n))

-- | Column properties: Read only (nullable).
colProps_ron :: PgTyped a => String -> O.TableProperties Void (Koln a)
colProps_ron n = P.rmap Koln $ OI.TableProperties
  (OI.Writer (OI.PackMap (\f ws -> f (fmap absurd ws, n)))) -- <- dead code
  (OI.View (OI.runViewColumnMaker OI.tableColumn n))

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

-- | 'O.TableProperties' for a single column in @'Columns' t@.
type family Column_Props (col :: Column Symbol WCap RCap Type Type) :: Type where
  Column_Props ('Column n 'RO r p h) =
    O.TableProperties Void (Column_PgR ('Column n 'RO r p h))
  Column_Props col = O.TableProperties (Column_PgW col) (Column_PgR col)
data Column_PropsSym0 (col :: TyFun (Column Symbol WCap RCap Type Type) Type)
type instance Apply Column_PropsSym0 t = Column_Props t

class IColumn_Props (col :: Column Symbol WCap RCap Type Type) where
  colProps :: proxy col -> Column_Props col

-- | 'colProps' is equivalent 'colProps_ro'.
instance forall n p h. (KnownSymbol n, PgTyped p) => IColumn_Props ('Column n 'RO 'R p h) where
  colProps _ = colProps_ro (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_ron'.
instance forall n p h. (KnownSymbol n, PgTyped p) => IColumn_Props ('Column n 'RO 'RN p h) where
  colProps _ = colProps_ron (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wr'.
instance forall n p h. (KnownSymbol n, PgTyped p) => IColumn_Props ('Column n 'W 'R p h) where
  colProps _ = colProps_wr (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wrn'.
instance forall n p h. (KnownSymbol n, PgTyped p) => IColumn_Props ('Column n 'W 'RN p h) where
  colProps _ = colProps_wrn (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wdr'.
instance forall n p h. (KnownSymbol n, PgTyped p) => IColumn_Props ('Column n 'WD 'R p h) where
  colProps _ = colProps_wdr (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}
-- | 'colProps' is equivalent 'colProps_wdrn'.
instance forall n p h. (KnownSymbol n, PgTyped p) => IColumn_Props ('Column n 'WD 'RN p h) where
  colProps _ = colProps_wdrn (symbolVal (Proxy :: Proxy n))
  {-# INLINE colProps #-}

class RDistributeColProps (cols :: [Column Symbol WCap RCap Type Type]) where
  rDistributeColProps
    :: Proxy cols
    -> Record (List.Map (Column_NameSym0 :&&&$$$ Column_PropsSym0) cols)
instance RDistributeColProps '[] where
  rDistributeColProps _ = RNil
instance (RDistributeColProps cols, IColumn_Props ('Column n w r p h))
  => RDistributeColProps ('Column n w r p h ': cols) where
  rDistributeColProps (_ :: Proxy ('Column n w r p h ': cols)) =
     RCons (Tagged @n (colProps (Proxy @('Column n w r p h))))
           (rDistributeColProps (Proxy @cols))

--------------------------------------------------------------------------------

class ColLens n x a b | x n -> a b where
  -- | 'Lens'' into the value in a column.
  --
  -- Mnemonic: the COLumn value.
  --
  -- See 'GHC.IsLabel' for alternative APIs for this.
  --
  -- Notice that this lens is more polymorphic than it needs to be, as
  -- @'Lens' x x a a@ would usually suffice for our needs. However, we need to make this fully
  -- polymorphic over @a@ because otherwise `Control.Lens.set` won't
  -- readily pick our 'GHC.IsLabel' implementation for 'col' when required.
  col :: proxy (n :: Symbol) -> Lens x x a b

instance (TableR t, x ~ Columns_NamedHsR t, Record.RLens n x x a b) => ColLens n (HsR t) a b where
  col prx = iso unHsR HsR . Record.rLens prx
  {-# INLINE col #-}

instance (TableRW t, x ~ Columns_NamedHsI t, Record.RLens n x x a b) => ColLens n (HsI t) a b where
  col prx = iso unHsI HsI . Record.rLens prx
  {-# INLINE col #-}

instance (TableR t, x ~ Columns_NamedPgR t, Record.RLens n x x a b) => ColLens n (PgR t) a b where
  col prx = iso unPgR PgR . Record.rLens prx
  {-# INLINE col #-}

instance (TableR t, x ~ Columns_NamedPgRN t, Record.RLens n x x a b) => ColLens n (PgRN t) a b where
  col prx = iso unPgRN PgRN . Record.rLens prx
  {-# INLINE col #-}

instance (TableRW t, x ~ Columns_NamedPgW t, Record.RLens n x x a b) => ColLens n (PgW t) a b where
  col prx = iso unPgW PgW . Record.rLens prx
  {-# INLINE col #-}

--------------------------------------------------------------------------------
-- Lenses into column values through OverloadedLabels:

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (HsR t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((HsR t) -> f (HsR t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (HsI t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((HsI t) -> f (HsI t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (PgR t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((PgR t) -> f (PgR t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (PgRN t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((PgRN t) -> f (PgRN t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'col' ('Proxy' :: 'Proxy' "foo")@ in places where a lens-like
-- value is expected. Notice @f@ is rigid.
instance forall n f t a b.
  ( ColLens n (PgW t) a b, Functor f
  ) => GHC.IsLabel n ((a -> f b) -> ((PgW t) -> f (PgW t)))
 where
  fromLabel _ = col (Proxy :: Proxy n)
  {-# INLINE fromLabel #-}

--------------------------------------------------------------------------------
-- Projection of column values through OverloadedLabels:

type family Column_ByName (n :: Symbol) (cols :: [Column Symbol WCap RCap Type Type]) :: Column Symbol WCap RCap Type Type where
  Column_ByName n (c ': cs) = If (Column_Name c == n) c (Column_ByName n cs)
  Column_ByName n '[] = GHC.TypeError
    ('GHC.Text "Columns_ByName: No column named " 'GHC.:<>: 'GHC.ShowType n)


-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (HsR t) a a) => GHC.IsLabel n (HsR t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (HsI t) a a) => GHC.IsLabel n (HsI t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (PgR t) a a) => GHC.IsLabel n (PgR t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (PgRN t) a a) => GHC.IsLabel n (PgRN t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

-- | @#foo@ works like @'view' ('col' ('Proxy' :: 'Proxy' "foo"))@.
instance forall n t a. (ColLens n (PgW t) a a) => GHC.IsLabel n (PgW t -> a) where
  fromLabel _ = view (col (Proxy :: Proxy n))
  {-# INLINE fromLabel #-}

--------------------------------------------------------------------------------

-- | A wrapper around @opaleye@'s 'O.Table' adding a placeholder @t@, which
-- shall mention the 'Database' associated with the query.
newtype RawTable (d :: k) w r = RawTable { unRawTable :: O.Table w r }
  deriving (Functor)

-- | Obtain the read-write 'RawTable' for a 'Table', required by
-- 'Opaleye.SOT.Run.runInsertRaw' and friends.
rawTableRW :: TableRW t => Table t -> RawTable (Database t) (PgW t) (PgR t)
rawTableRW (_ :: Table t) = RawTable $ O.TableWithSchema
  (symbolVal (Proxy :: Proxy (SchemaName t)))
  (symbolVal (Proxy :: Proxy (TableName t)))
  (P.dimap unPgW PgR (PP.ppa (rDistributeColProps (Proxy :: Proxy (Columns t)))))
{-# INLINABLE rawTableRW #-}

-- | Obtain the read-only 'RawTable' for a 'Table'.
--
-- Hint: You probably won't need to use this function unless you are trying to
-- interact with the underlying Opaleye library.
rawTableRO :: TableR t => Table t -> RawTable (Database t) Void (PgR t)
rawTableRO (_ :: Table t) = RawTable $ O.TableWithSchema
  (symbolVal (Proxy :: Proxy (SchemaName t)))
  (symbolVal (Proxy :: Proxy (TableName t)))
  (P.rmap PgR (PP.ppa (rDistributeColProps (Proxy :: Proxy (Columns t)))))
-- {-# INLINABLE rawTableRO #-}

--------------------------------------------------------------------------------
-- Misc

-- | Apply a same constraint to all the types in the list.
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

