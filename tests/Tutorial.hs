-- This module is kept here so that it can be typechecked when
-- running the test suite.

{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | @tisch@ is a different API for the core @opaleye@
infrastructure with the following goals in mind:

* Close to @opaleye@.

  @tisch@ is fully compatible with @opalaye@ and doesn't aim
  to replace it, just to complement it. @tisch@ is designed in
  such a way that we are expected to work with tools both from
  "Opaleye" and "Tisch" at the same time. If anything, perhaps
  some of the ideas on @tisch@ can eventually be ported
  to the core @opaleye@ project.

* Type safety everywhere.

  While it is true that @opaleye@, by relying in the type system,
  makes it mostly impossible to write malformed queries, it doesn't
  keep us for accidentaly referring to a wrong column, comparing two
  columns we are not supposed to compare, confusing two tables
  that happen to have the same shape, accidentally mixing nullable
  and not nullable columns, or similar scenarios. @tisch@, by
  making a heavy use of the GHC type system, provides an additional
  layer of safety that can prevent these undesired scenarios.

* Boilerplate removal.

  Working with @opaleye@ can get a bit boilerplatey. @tisch@
  never requires us to say the same thing more than once, and it
  provides us with generic machinery so that we can skip altogether
  the provision of the types that @opaleye@ requires. Also,
  @tisch@ does not rely on Template Haskell to achieve this,
  but on the type system instead.

* Maintenance.

  As a consequence of the extended type safety and small amount of
  boilerplate that @tisch@ requires, maintaining code that uses
  the @tisch@ API is easy. For example, when writing queries,
  columns are identified by their PostgreSQL name, so, if we ever
  change the name of a column in the table description, then our
  querying code using the old name will not compile.

* THIS IS A WORK IN PROCESS! I'M NOT DONE WRITING WHAT'S NICE ABOUT
  THIS. Meanwhile, read the source code here as an example. These
  examples come from the LearningSQLExample.sql file in this project,
  which is maybe not such great example SQL. We'll see if we can come up
  with a better example SQL. Also, this example code doesn't belong in
  this module, probably.
-}
module Tutorial
  ( Department
  , DepartmentId(..)
  , Branch
  , BranchId(..)
  , Employee
  , EmployeeId(..)
  , ProductType
  , ProductTypeId(..)
  , Product
  , ProductId(..)
  , ProductCode(..)
  , Customer
  , CustomerId(..)
  , CustomerType(..)
  , Individual
  , Business
  , Officer
  , OfficerId(..)
  , Account
  , AccountId(..)
  , AccountStatus(..)
  , Transaction
  , TransactionId(..)
  , TransactionType(..)
  , Table(..)
  , q_Account_desc
  , q_Account_asc_multi
  , q_Account_in_static
  , q_Account_agg_subq
  , q_Employee_literals
  , q_Employee_Department_join1
  , q_Employee_Department_join2
  , q_Account_Individual_leftJoin
  , exampleRun
  ) where

import           Control.Category (id)
import           Control.Lens
import           Data.Proxy
import qualified Data.Time as Time
import           Data.Time (Day, LocalTime)
import           Data.Int
import           Tisch
import           Prelude hiding (id)

--------------------------------------------------------------------------------
-- Defining tables, types, etc.
--------------------------------------------------------------------------------
-- Let's explore how to teach @tisch@ about a table.

-- | Type-level unique identifier for our “department” table.
data Department

-- | Proxy type for talking about the 'Department' table at the term-level.
--
-- For convenience, we reuse the same name 'Department' for the constructor.
-- Pay attention: The 'Department' in @'Table' 'Department'@ refers to the
-- 'Department' type we defined before, which has no constructor. The
-- @'Department' :: 'Table' 'Department'@ constructor, on the other hand, is
-- just a constructor that happens to share the same name with 'Department' type.
data instance Table Department = Department

-- | The 'Db1' type will be used as a unique type-level identifier for the
-- database where our tables exists. This will prevent accidental
-- inter-database queries.
data Db1

-- | Associate the 'Department' table identifier with the 'Db1' database.
type instance Database Department = Db1

-- | Name of the PostgreSQL schema where the 'Department' table table exists.
-- @"public"@ is the default schema name used by PostgreSQL.
type instance SchemaName Department = "public"

-- | Name of the 'Department' table.
type instance TableName Department = "department"

-- | Columns in the 'Department' table.
--
-- 1. A column named "department_id", into which either a concrete value or
--    @DEFAULT@ can be written (see 'WD'), and that cannot be @NULL@ (see 'R').
--    On the Haskell side, the value in this column is represented as
--   'DepartmentId'. On the PostgreSQL side, this column is also represented by
--   'DepartmentId', but only nominally, in concrete terms the column contains
--   a 'PGInt4' value (see the 'PgTyped' instance for 'DepartmentId').
--
-- 2. A column named "name", into which a concrete value can be written
--    (@DEFAULT@ is not supported for this column, see 'W'), and that cannot be
--    @NULL@ (see 'R'). On the Haskell side, the value in this column is
--    represented as 'String'. On the PostgreSQL side, this column is
--    represented by 'PGText'.
--
-- To better understand what's going on here, read the documentation for
-- 'Column'.
type instance Columns Department =
 '[ 'Column "department_id" 'WD 'R DepartmentId DepartmentId
  , 'Column "name" 'W 'R PGText String
  ]

-- | As specified in @'Columns' 'Department'@, instead of just using 'Int32'
-- for our department identifiers, we will use a newtype around it.
newtype DepartmentId = DepartmentId { unDepartmentId :: Int32 }
  deriving (Eq, Show)

-- | Since 'DepartmentId' is just a newtype wrapper, we can create a 'Wrapped'
-- instance for it, which will come in handy later. It is not really necessary
-- to provide this instance, but if you do provide it, you will get a default
-- implementation for 'ToKol' later on.
instance Wrapped DepartmentId where
  type Unwrapped DepartmentId = Int32
  _Wrapped' = iso unDepartmentId DepartmentId

-- | We also want to use 'DepartmentId' instead of 'PGInt4' in the PostgreSQL
-- side of things. Notice that in this case 'DepartmentId' will only ever be
-- used nominally, as a type, and never as a term, so the 'Int32' mentioned in
-- the 'DepartmentId' constructor doesn't matter at all. @'PgType' DepartmentId
-- ~ 'PGInt4'@ says that the underlying representation for 'DepartmentId' is
-- 'PGInt4', and that it is safe to upcast @'Kol' 'DepartmentId'@ to
-- @'Kol' 'PGInt4'@, and downcast @'Kol' 'PGInt4'@ to @'Kol' 'DepartmentId'
-- internally if needed (see the Law 1 of 'PgTyped').
instance PgTyped DepartmentId where
  type PgType DepartmentId = PGInt4

-- The most practical way of creating a @'Kol' 'DepartmentId'@, which is the
-- expected type for the value contained in the @"department_id"@ column of our
-- 'Department' table, is to introduce a @'ToKol' 'DepartmentId' 'DeparmentId'@.
--
-- @'ToKol' 'DepartmentId' 'DepartmentId'@ says that, given a 'DepartmentId'
-- value (first argument to 'ToKol'), we can obtain a @'Kol' 'DepartmentId'@
-- (second argument to 'ToKol') with 'kol':
--
-- @
-- 'kol' :: 'DepartmentId' -> 'Kol' 'DepartmentId'
-- @
--
-- Since we have a 'Wrapped' instance around 'Int32' for the first parameter
-- 'DepartmentId', and @'PgType' 'Department' ~ 'PGInt4'@, and there exists
-- already a @'ToKol' 'Int32' 'O.PGInt4'@, the implementation of the 'kol' method
-- within the 'ToKol' instance comes for free. Otherwise, we would have needed
-- to implement it ourselves. In fact, manually definining this instance for
-- 'Wrapped' types is completely unnecessary, since there exists a “catch all”
-- instance already, so let's leave 'ToKol' out.
--
--    instance ToKol DepartmentId DepartmentId



-- | We will want to be able to compare this field for equality, so let's
-- provide a 'PgEq' instance for 'DepartmentId'.
instance PgEq DepartmentId

-- | Last, we need to be able to parse the values coming from the database into
-- Haskell terms. For this, we create a 'QueryRunnerColumnDefault' instance.
-- In our case, since 'DepartmentId' is just a wrapper around 'Int32' which
-- already has a @'QueryRunnerColumnDefault' 'PGInt4' 'Int32'@ instance, we can
-- just use 'qrcWrapped'.
instance QueryRunnerColumnDefault PGInt4 DepartmentId where
  queryRunnerColumnDefault = qrcWrapped

---

newtype BranchId = BranchId { unBranchId :: Int32 }
  deriving (Eq, Show)
instance Wrapped BranchId where
  type Unwrapped BranchId = Int32
  _Wrapped' = iso unBranchId BranchId
instance PgTyped BranchId where
  type PgType BranchId = PGInt4
instance ToKol BranchId BranchId
instance QueryRunnerColumnDefault PGInt4 BranchId where
  queryRunnerColumnDefault = qrcWrapped

data Branch
data instance Table Branch = Branch
type instance Database Branch = Db1
type instance SchemaName Branch = "public"
type instance TableName Branch = "branch"
type instance Columns Branch =
    [ 'Column "branch_id" 'WD 'R BranchId BranchId
    , 'Column "name" 'W 'R PGText String
    , 'Column "address" 'W 'RN PGText String
    , 'Column "city" 'W 'RN PGText String
    , 'Column "state" 'W 'RN PGText String
    , 'Column "zip" 'W 'RN PGText String
    ]

---

newtype EmployeeId = EmployeeId { unEmployeeId :: Int32 }
  deriving (Eq, Show)
instance Wrapped EmployeeId where
  type Unwrapped EmployeeId = Int32
  _Wrapped' = iso unEmployeeId EmployeeId
instance PgTyped EmployeeId where
  type PgType EmployeeId = PGInt4
instance ToKol EmployeeId EmployeeId
instance QueryRunnerColumnDefault PGInt4 EmployeeId where
  queryRunnerColumnDefault = qrcWrapped


data Employee
data instance Table Employee = Employee
type instance Database Employee = Db1
type instance SchemaName Employee = "public"
type instance TableName Employee = "employee"
type instance Columns Employee =
    [ 'Column "employee_id" 'WD 'R EmployeeId EmployeeId
    , 'Column "fname" 'W 'R PGText String
    , 'Column "lname" 'W 'R PGText String
    , 'Column "start_date" 'W 'R PGDate Day
    , 'Column "end_date" 'W 'RN PGDate Day
    , 'Column "superior_employee_id" 'W 'RN EmployeeId EmployeeId
    , 'Column "department_id" 'W 'RN DepartmentId DepartmentId
    , 'Column "title" 'W 'RN PGText String
    , 'Column "assigned_branch_id" 'W 'RN BranchId BranchId
    ]

---

newtype ProductTypeId = ProductTypeId { unProductTypeId :: Int32 }
  deriving (Eq, Show)
instance Wrapped ProductTypeId where
  type Unwrapped ProductTypeId = Int32
  _Wrapped' = iso unProductTypeId ProductTypeId
instance PgTyped ProductTypeId where
  type PgType ProductTypeId = PGInt4
instance ToKol ProductTypeId ProductTypeId
instance QueryRunnerColumnDefault PGInt4 ProductTypeId where
  queryRunnerColumnDefault = qrcWrapped

data ProductType
data instance Table ProductType = ProductType
type instance Database ProductType = Db1
type instance SchemaName ProductType = "public"
type instance TableName ProductType = "product_type"
type instance Columns ProductType =
    [ 'Column "product_type_cd" 'W 'R ProductTypeId ProductTypeId
    , 'Column "name" 'W 'R PGText String
    ]

---

newtype ProductId = ProductId { unProductId :: Int32 }
  deriving (Eq, Show)
instance Wrapped ProductId where
  type Unwrapped ProductId = Int32
  _Wrapped' = iso unProductId ProductId
instance PgTyped ProductId where
  type PgType ProductId = PGInt4
instance ToKol ProductId ProductId
instance QueryRunnerColumnDefault PGInt4 ProductId where
  queryRunnerColumnDefault = qrcWrapped

newtype ProductCode = ProductCode { unProductCode :: String }
  deriving (Eq, Show)
instance Wrapped ProductCode where
  type Unwrapped ProductCode = String
  _Wrapped' = iso unProductCode ProductCode
instance PgTyped ProductCode where
  type PgType ProductCode = PGText
instance PgEq ProductCode
instance ToKol ProductCode ProductCode
instance QueryRunnerColumnDefault PGText ProductCode where
  queryRunnerColumnDefault = qrcWrapped

data Product
data instance Table Product = Product
type instance Database Product = Db1
type instance SchemaName Product = "public"
type instance TableName Product = "product"
type instance Columns Product =
    [ 'Column "product_cd" 'W 'R ProductCode ProductCode
    , 'Column "name" 'W 'R PGText String
    , 'Column "product_type_cd" 'W 'R ProductTypeId ProductTypeId
    , 'Column "date_offered" 'W 'RN PGDate Day
    , 'Column "date_retired" 'W 'RN PGDate Day
    ]

---

newtype CustomerId = CustomerId { unCustomerId :: Int32 }
  deriving (Eq, Show)
instance Wrapped CustomerId where
  type Unwrapped CustomerId = Int32
  _Wrapped' = iso unCustomerId CustomerId
instance PgTyped CustomerId where
  type PgType CustomerId = PGInt4
instance PgEq CustomerId
instance ToKol CustomerId CustomerId
instance QueryRunnerColumnDefault PGInt4 CustomerId where
  queryRunnerColumnDefault = qrcWrapped

data CustomerType
   = CustomerType_I
   | CustomerType_B -- whatever I and B mean.
   deriving (Show)

_CustomerType_Char :: Prism' Char CustomerType
_CustomerType_Char = prism'
  (\case { CustomerType_I -> 'I'; CustomerType_B -> 'B' })
  (\case { 'I' -> Just CustomerType_I; 'B' -> Just CustomerType_B; _ -> Nothing })

instance PgTyped CustomerType where
  type PgType CustomerType = PGText
instance ToKol CustomerType CustomerType where
  kol = unsafeDowncastKol . kol . review _CustomerType_Char

data Customer
data instance Table Customer = Customer
type instance Database Customer = Db1
type instance SchemaName Customer = "public"
type instance TableName Customer = "customer"
type instance Columns Customer =
    [ 'Column "customer_id" 'WD 'R CustomerId CustomerId
    , 'Column "fed_id" 'W 'R PGText String -- I have no idea what "fed" is supposed to mean.
    , 'Column "cust_type_cd" 'W 'R CustomerType CustomerType
    , 'Column "address" 'W 'RN PGText String
    , 'Column "city" 'W 'RN PGText String
    , 'Column "state" 'W 'RN PGText String
    , 'Column "postal_code" 'W 'RN PGText String
    ]

---

data Individual
data instance Table Individual = Individual
type instance Database Individual = Db1
type instance SchemaName Individual = "public"
type instance TableName Individual = "individual"
type instance Columns Individual =
    [ 'Column "customer_id" 'W 'R CustomerId CustomerId
    , 'Column "fname" 'W 'R PGText String
    , 'Column "lname" 'W 'R PGText String
    , 'Column "birth_date" 'W 'RN PGDate Day
    ]

---

newtype BizStateId = BizStateId { unBizStateId :: Int32 }
  deriving (Eq, Show)
instance Wrapped BizStateId where
  type Unwrapped BizStateId = Int32
  _Wrapped' = iso unBizStateId BizStateId
instance PgTyped BizStateId where
  type PgType BizStateId = PGInt4
instance ToKol BizStateId BizStateId
instance QueryRunnerColumnDefault PGInt4 BizStateId where
  queryRunnerColumnDefault = qrcWrapped

data Business
data instance Table Business = Business
type instance Database Business = Db1
type instance SchemaName Business = "public"
type instance TableName Business = "business"
type instance Columns Business =
    [ 'Column "customer_id" 'W 'R CustomerId CustomerId
    , 'Column "name" 'W 'R PGText String
    , 'Column "state_id" 'W 'R BizStateId BizStateId
    , 'Column "incorp_date" 'W 'RN PGDate Day
    ]

---

newtype OfficerId = OfficerId { unOfficerId :: Int32 }
  deriving (Eq, Show)
instance Wrapped OfficerId where
  type Unwrapped OfficerId = Int32
  _Wrapped' = iso unOfficerId OfficerId
instance PgTyped OfficerId where
  type PgType OfficerId = PGInt4
instance ToKol OfficerId OfficerId
instance QueryRunnerColumnDefault PGInt4 OfficerId where
  queryRunnerColumnDefault = qrcWrapped

data Officer
data instance Table Officer = Officer
type instance Database Officer = Db1
type instance SchemaName Officer = "public"
type instance TableName Officer = "officer"
type instance Columns Officer =
    [ 'Column "officer_id" 'WD 'R OfficerId OfficerId
    , 'Column "customer_id" 'W 'R CustomerId CustomerId
    , 'Column "fname" 'W 'R PGText String
    , 'Column "lname" 'W 'R PGText String
    , 'Column "title" 'W 'RN PGText String
    , 'Column "start_date" 'W 'R PGDate Day
    , 'Column "end_date" 'W 'RN PGDate Day
    ]

---

newtype AccountId = AccountId { unAccountId :: Int32 }
  deriving (Eq, Show)
instance Wrapped AccountId where
  type Unwrapped AccountId = Int32
  _Wrapped' = iso unAccountId AccountId
instance PgTyped AccountId where
  type PgType AccountId = PGInt4
instance PgEq AccountId
instance ToKol AccountId AccountId
instance QueryRunnerColumnDefault PGInt4 AccountId where
  queryRunnerColumnDefault = qrcWrapped

data AccountStatus
   = AccountStatus_Active
   | AccountStatus_Closed
   | AccountStatus_Frozen
   deriving (Show)

_AccountStatus_String :: Prism' String AccountStatus
_AccountStatus_String = prism'
  (\case AccountStatus_Active -> "ACTIVE"
         AccountStatus_Closed -> "CLOSED"
         AccountStatus_Frozen -> "FROZEN")
  (\case "ACTIVE" -> Just AccountStatus_Active
         "CLOSED" -> Just AccountStatus_Closed
         "FROZEN" -> Just AccountStatus_Frozen
         _ -> Nothing)

instance PgTyped AccountStatus where
  type PgType AccountStatus = PGText
instance ToKol AccountStatus AccountStatus where
  kol = unsafeDowncastKol . kol . review _AccountStatus_String
instance QueryRunnerColumnDefault PGText AccountStatus where
  queryRunnerColumnDefault = qrcPrism _AccountStatus_String

data Account
data instance Table Account = Account
type instance Database Account = Db1
type instance SchemaName Account = "public"
type instance TableName Account = "account"
type instance Columns Account =
    [ 'Column "account_id" 'WD 'R AccountId AccountId
    , 'Column "product_cd" 'W 'R ProductCode ProductCode
    , 'Column "customer_id" 'W 'R CustomerId CustomerId
    , 'Column "open_date" 'W 'R PGDate Day
    , 'Column "close_date" 'W 'RN PGDate Day
    , 'Column "last_activity_date" 'W 'RN PGDate Day
    , 'Column "status" 'W 'R AccountStatus AccountStatus
    , 'Column "open_branch_id" 'W 'RN PGInt4 BranchId
    , 'Column "open_employee_id" 'W 'RN PGInt4 EmployeeId
    , 'Column "avail_balance" 'W 'RN PGFloat4 Float
    , 'Column "pending_balance" 'W 'RN PGFloat4 Float
    ]

---

newtype TransactionId = TransactionId { unTransactionId :: Int32 }
  deriving (Eq, Show)
instance Wrapped TransactionId where
  type Unwrapped TransactionId = Int32
  _Wrapped' = iso unTransactionId TransactionId
instance PgTyped TransactionId where
  type PgType TransactionId = PGInt4
-- instance ToKol TransactionId PGInt4
instance ToKol TransactionId TransactionId
instance QueryRunnerColumnDefault PGInt4 TransactionId where
  queryRunnerColumnDefault = qrcWrapped

data TransactionType
   = TransactionType_Debit
   | TransactionType_Credit
   deriving (Show)

_TransactionType_String :: Prism' String TransactionType
_TransactionType_String = prism'
  (\case TransactionType_Debit -> "DBT"
         TransactionType_Credit -> "CDT")
  (\case "DBT" -> Just TransactionType_Debit
         "CDT" -> Just TransactionType_Credit
         _ -> Nothing)

instance PgTyped TransactionType where
  type PgType TransactionType = PGText
instance ToKol TransactionType TransactionType where
  kol = unsafeDowncastKol . kol . review _TransactionType_String

data Transaction
data instance Table Transaction = Transaction
type instance Database Transaction = Db1
type instance SchemaName Transaction = "public"
type instance TableName Transaction = "transaction"
type instance Columns Transaction =
   '[ 'Column "txn_id" 'WD 'R TransactionId TransactionId
    , 'Column "txn_date" 'W 'R PGTimestamp LocalTime
    , 'Column "account_id" 'W 'R AccountId AccountId
    , 'Column "txn_type_cd" 'W 'RN TransactionType TransactionType
    , 'Column "amount" 'W 'R PGFloat4 Float
    , 'Column "teller_employee_id" 'W 'RN EmployeeId EmployeeId
    , 'Column "execution_branch_id" 'W 'RN BranchId BranchId
    , 'Column "funds_avail_date" 'W 'RN PGTimestamp LocalTime
    ]

--------------------------------------------------------------------------------
-- Querying.

-- We somewhat offer tisch counterparts for the HRR examples at
-- https://khibino.github.io/haskell-relational-record/examples.html
--------------------------------------------------------------------------------

-- | Order by field, desc.
q_Account_desc :: Query Db1 () (PgR Account)
q_Account_desc = orderBy (descNullsFirst #avail_balance) (query Account)

-- | Order by multiple fields, asc.
q_Account_asc_multi :: Query Db1 () (PgR Account)
q_Account_asc_multi = orderBy
    (mappend (ascNullsFirst #open_employee_id)   -- labels behave as projections.
             (asc (view #product_cd))) -- labels behave as lenses too.
    (query Account)

-- | Literals, using possibly nullable columns.
q_Employee_literals :: Query Db1 () (PgR Employee)
q_Employee_literals = proc () -> do
  e <- query Employee -< ()
  restrict -< isNull (#end_date e) -- labels behave as projections.
  restrict -< fromKoln (kol False) $
     forKoln (#title e) $ \eTitle ->
        lor (lt (kol (Time.fromGregorian 2003 1 1))
                (view #start_date e)) -- labels behave as lenses too.
            (eq (kol ("Teller" :: String)) eTitle)
  id -< e

-- | This is the same as 'q_Employee_Department_join2'
q_Employee_Department_join1 :: Query Db1 () (PgR Employee, PgR Department)
q_Employee_Department_join1 = proc () -> do
  e <- query Employee -< ()
  d <- query Department -< ()
  restrict -< fromKoln (kol False) $
     mapKoln (eq (#department_id d)) (#department_id e)
  id -< (e,d)

-- | This is the same as 'q_Employee_Department_join1'
q_Employee_Department_join2 :: Query Db1 () (PgR Employee, PgR Department)
q_Employee_Department_join2 =
  innerJoin (query Employee) (query Department) $ \e d ->
     fromKoln (kol False) $
        mapKoln (eq (#department_id d)) (#department_id e)

q_Account_Individual_leftJoin :: Query Db1 () (PgR Account, PgRN Individual)
q_Account_Individual_leftJoin =
  leftJoin (query Account) (query Individual) $ \a i ->
     eq (#customer_id a)
        (view (col (Proxy @"customer_id")) i) -- a different way of referring to
                                              -- the customer_id column, useful
                                              -- if the column name is not a
                                              -- valid Haskell name

-- | Checking for membership using 'IN' and a static list.
q_Account_in_static :: Query Db1 () (PgR Account)
q_Account_in_static = proc () -> do
  a <- query Account -< ()
  restrict -< member (#product_cd a)
     (map (kol . ProductCode) ["CHK", "SAV", "CD", "MM"])
  id -< a

-- | Aggregation subquery.
q_Account_agg_subq :: Query Db1 () (PgR Account)
q_Account_agg_subq = proc () -> do
  a <- query Account -< ()
  max_accId <- aggregate maxgg (fmap #account_id (query Account)) -< ()
  restrict -< eq (#account_id a) max_accId
  id -< a

--------------------------------------------------------------------------------

exampleRun
  :: Allow 'Fetch ps
  => Conn Db1 ps
  -> IO ( [HsR Account]
        , [HsR Account]
        , [HsR Account]
        , [HsR Account]
        , [HsR Employee]
        , [(HsR Employee, HsR Department)]
        , [(HsR Employee, HsR Department)]
        , [(HsR Account, Maybe (HsR Individual))] )
exampleRun = \conn -> (,,,,,,,)
  <$> runQuery conn q_Account_desc
  <*> runQuery conn q_Account_asc_multi
  <*> runQuery conn q_Account_in_static
  <*> runQuery conn q_Account_agg_subq
  <*> runQuery conn q_Employee_literals
  <*> runQuery conn q_Employee_Department_join1
  <*> runQuery conn q_Employee_Department_join2
  <*> runQuery conn q_Account_Individual_leftJoin

