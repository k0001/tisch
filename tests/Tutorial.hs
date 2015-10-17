-- This module is kept here so that it can be typechecked when
-- running the test suite.

{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{- | @opaleye-sot@ is a different API for the core @opaleye@
infraestructure with the following goals in mind:

* Close to @opaleye@.

  @opaleye-sot@ is fully compatible with @opalaye@ and doesn't aim
  to replace it, just to complement it. @opaleye-sot@ is designed in
  such a way that we are expected to work with tools both from
  "Opaleye" and "Opaleye.SOT" at the same time. If anything, perhaps
  some of the ideas on @opaleye-sot@ can eventually be ported
  to the core @opaleye@ project.

* Type safety everywhere.

  While it is true that @opaleye@, by relying in the type system,
  makes it mostly impossible to write malformed queries, it doesn't
  keep us for accidentaly referring to a wrong column, comparing two
  columns we are not supposed to compare, confusing two tables
  that happen to have the same shape, accidentally mixing nullable
  and not nullable columns, or similar scenarios. @opaleye-sot@, by
  making a heavy use of the GHC type system, provides an additional
  layer of safety that can prevent these undesired scenarios.

* Boilerplate removal.

  Working with @opaleye@ can get a bit boilerplatey. @opaleye-sot@
  never requires us to say the same thing more than once, and it
  provides us with generic machinery so that we can skip altogether
  the provision of the types that @opaleye@ requires. Also,
  @opaleye-sot@ does not rely on Template Haskell to achieve this,
  but on the type system instead.

* Maintenance.

  As a consequence of the extended type safety and small amount of
  boilerplate that @opaleye-sot@ requires, maintaining code that uses
  the @opaleye-sot@ API is easy. For example, when writing queries,
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
  ( TDepartment
  , TBranch
  , TEmployee
  , TProductType
  , TProduct
  , TCustomer
  , TIndividual
  , TBusiness
  , TOfficer
  , TAccount
  , TTransaction
  , q_TAccount_desc
  , q_TAccount_asc_multi
  , q_TEmployee_1
  , q_TEmployee_TDepartment_join
  , q_TAccount_TIndividual_leftJoin
  ) where

import           Control.Arrow
import           Control.Category (id)
import           Control.Lens
import qualified Data.Time as Time
import           Data.Time (Day, LocalTime)
import           Data.Int
import qualified Opaleye as O
import           Opaleye.SOT
import           Prelude hiding (id)

--------------------------------------------------------------------------------
-- Defining tables, types, etc.
--------------------------------------------------------------------------------

-- | Unique type-level identifier for our database
data Db1


newtype DepartmentId = DepartmentId { unDepartmentId :: Int32 }
instance Wrapped DepartmentId where { type Unwrapped DepartmentId = Int32; _Wrapped' = iso unDepartmentId DepartmentId }
instance ToKol DepartmentId O.PGInt4

data TDepartment
instance Tisch TDepartment where
  type Database TDepartment = Db1
  type SchemaName TDepartment = "public"
  type TableName TDepartment = "department"
  type Cols TDepartment =
    [ 'Col "department_id" 'WD 'R O.PGInt4 DepartmentId
    , 'Col "name" 'W 'R O.PGText String
    ]

---

newtype BranchId = BranchId { unBranchId :: Int32 }
instance Wrapped BranchId where { type Unwrapped BranchId = Int32; _Wrapped' = iso unBranchId BranchId }
instance ToKol BranchId O.PGInt4

data TBranch
instance Tisch TBranch where
  type Database TBranch = Db1
  type SchemaName TBranch = "public"
  type TableName TBranch = "branch"
  type Cols TBranch =
    [ 'Col "branch_id" 'WD 'R O.PGInt4 BranchId
    , 'Col "name" 'W 'R O.PGText String
    , 'Col "address" 'W 'RN O.PGText String
    , 'Col "city" 'W 'RN O.PGText String
    , 'Col "state" 'W 'RN O.PGText String
    , 'Col "zip" 'W 'RN O.PGText String
    ]

---

newtype EmployeeId = EmployeeId { unEmployeeId :: Int32 }
instance Wrapped EmployeeId where { type Unwrapped EmployeeId = Int32; _Wrapped' = iso unEmployeeId EmployeeId }
instance ToKol EmployeeId O.PGInt4

data TEmployee
instance Tisch TEmployee where
  type Database TEmployee = Db1
  type SchemaName TEmployee = "public"
  type TableName TEmployee = "employee"
  type Cols TEmployee =
    [ 'Col "employee_id" 'WD 'R O.PGInt4 EmployeeId
    , 'Col "fname" 'W 'R O.PGText String
    , 'Col "lname" 'W 'R O.PGText String
    , 'Col "start_date" 'W 'R O.PGDate Day
    , 'Col "end_date" 'W 'RN O.PGDate Day
    , 'Col "superior_employee_id" 'W 'RN O.PGInt4 EmployeeId
    , 'Col "department_id" 'W 'RN O.PGInt4 DepartmentId
    , 'Col "title" 'W 'RN O.PGText String
    , 'Col "assigned_branch_id" 'W 'RN O.PGInt4 BranchId
    ]

---

newtype ProductTypeId = ProductTypeId { unProductTypeId :: String }
instance Wrapped ProductTypeId where { type Unwrapped ProductTypeId = String; _Wrapped' = iso unProductTypeId ProductTypeId }
instance ToKol ProductTypeId O.PGText

data TProductType
instance Tisch TProductType where
  type Database TProductType = Db1
  type SchemaName TProductType = "public"
  type TableName TProductType = "product_type"
  type Cols TProductType =
    [ 'Col "product_type_cd" 'W 'R O.PGText ProductTypeId
    , 'Col "name" 'W 'R O.PGText String
    ]

---

newtype ProductId = ProductId { unProductId :: String }
instance Wrapped ProductId where { type Unwrapped ProductId = String; _Wrapped' = iso unProductId ProductId }
instance ToKol ProductId O.PGText

data TProduct
instance Tisch TProduct where
  type Database TProduct = Db1
  type SchemaName TProduct = "public"
  type TableName TProduct = "product"
  type Cols TProduct =
    [ 'Col "product_cd" 'W 'R O.PGText ProductId
    , 'Col "name" 'W 'R O.PGText String
    , 'Col "product_type_cd" 'W 'R O.PGText ProductTypeId
    , 'Col "date_offered" 'W 'RN O.PGDate Day
    , 'Col "date_retired" 'W 'RN O.PGDate Day
    ]

---

newtype CustomerId = CustomerId { unCustomerId :: Int32 }
instance Wrapped CustomerId where { type Unwrapped CustomerId = Int32; _Wrapped' = iso unCustomerId CustomerId }
instance ToKol CustomerId O.PGInt4

data CustomerType
   = CustomerType_I
   | CustomerType_B -- whatever I and B mean.
   deriving (Show)

_CustomerType_Char :: Prism' Char CustomerType
_CustomerType_Char = prism'
  (\case { CustomerType_I -> 'I'; CustomerType_B -> 'B' })
  (\case { 'I' -> Just CustomerType_I; 'B' -> Just CustomerType_B; _ -> Nothing })

instance ToKol CustomerType O.PGText where kol = kol . review _CustomerType_Char

data TCustomer
instance Tisch TCustomer where
  type Database TCustomer = Db1
  type SchemaName TCustomer = "public"
  type TableName TCustomer = "customer"
  type Cols TCustomer =
    [ 'Col "customer_id" 'WD 'R O.PGInt4 CustomerId
    , 'Col "fed_id" 'W 'R O.PGText String -- I have no idea what "fed" is supposed to mean.
    , 'Col "cust_type_cd" 'W 'R O.PGText CustomerType
    , 'Col "address" 'W 'RN O.PGText String
    , 'Col "city" 'W 'RN O.PGText String
    , 'Col "state" 'W 'RN O.PGText String
    , 'Col "postal_code" 'W 'RN O.PGText String
    ]

---

data TIndividual
instance Tisch TIndividual where
  type Database TIndividual = Db1
  type SchemaName TIndividual = "public"
  type TableName TIndividual = "individual"
  type Cols TIndividual =
    [ 'Col "customer_id" 'W 'R O.PGInt4 CustomerId
    , 'Col "fname" 'W 'R O.PGText String
    , 'Col "lname" 'W 'R O.PGText String
    , 'Col "birth_date" 'W 'RN O.PGDate Day
    ]

---

newtype BizStateId = BizStateId { unBizStateId :: String }
instance Wrapped BizStateId where { type Unwrapped BizStateId = String; _Wrapped' = iso unBizStateId BizStateId }
instance ToKol BizStateId O.PGText

data TBusiness
instance Tisch TBusiness where
  type Database TBusiness = Db1
  type SchemaName TBusiness = "public"
  type TableName TBusiness = "business"
  type Cols TBusiness =
    [ 'Col "customer_id" 'W 'R O.PGInt4 CustomerId
    , 'Col "name" 'W 'R O.PGText String
    , 'Col "state_id" 'W 'R O.PGText BizStateId
    , 'Col "incorp_date" 'W 'RN O.PGDate Day
    ]

---

newtype OfficerId = OfficerId { unOfficerId :: Int32 }
instance Wrapped OfficerId where { type Unwrapped OfficerId = Int32; _Wrapped' = iso unOfficerId OfficerId }
instance ToKol OfficerId O.PGInt4

data TOfficer
instance Tisch TOfficer where
  type Database TOfficer = Db1
  type SchemaName TOfficer = "public"
  type TableName TOfficer = "officer"
  type Cols TOfficer =
    [ 'Col "officer_id" 'WD 'R O.PGInt4 OfficerId
    , 'Col "customer_id" 'W 'R O.PGInt4 CustomerId
    , 'Col "fname" 'W 'R O.PGText String
    , 'Col "lname" 'W 'R O.PGText String
    , 'Col "title" 'W 'RN O.PGText String
    , 'Col "start_date" 'W 'R O.PGDate Day
    , 'Col "end_date" 'W 'RN O.PGDate Day
    ]

---

newtype AccountId = AccountId { unAccountId :: Int32 }
instance Wrapped AccountId where { type Unwrapped AccountId = Int32; _Wrapped' = iso unAccountId AccountId }
instance ToKol AccountId O.PGInt4

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

instance ToKol AccountStatus O.PGText where kol = kol . review _AccountStatus_String

data TAccount
instance Tisch TAccount where
  type Database TAccount = Db1
  type SchemaName TAccount = "public"
  type TableName TAccount = "account"
  type Cols TAccount =
    [ 'Col "account_id" 'WD 'R O.PGInt4 AccountId
    , 'Col "product_cd" 'W 'R O.PGText ProductId
    , 'Col "customer_id" 'W 'R O.PGInt4 CustomerId
    , 'Col "open_date" 'W 'R O.PGDate Day
    , 'Col "close_date" 'W 'RN O.PGDate Day
    , 'Col "last_activity_date" 'W 'RN O.PGDate Day
    , 'Col "status" 'W 'R O.PGText AccountStatus
    , 'Col "open_branch_id" 'W 'RN O.PGInt4 BranchId
    , 'Col "open_employee_id" 'W 'RN O.PGInt4 EmployeeId
    , 'Col "avail_balance" 'W 'RN O.PGFloat4 Float
    , 'Col "pending_balance" 'W 'RN O.PGFloat4 Float
    ]

---

newtype TransactionId = TransactionId { unTransactionId :: Int32 }
instance Wrapped TransactionId where { type Unwrapped TransactionId = Int32; _Wrapped' = iso unTransactionId TransactionId }
instance ToKol TransactionId O.PGInt4

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

instance ToKol TransactionType O.PGText where kol = kol . review _TransactionType_String

data TTransaction
instance Tisch TTransaction where
  type Database TTransaction = Db1
  type SchemaName TTransaction = "public"
  type TableName TTransaction = "transaction"
  type Cols TTransaction =
   '[ 'Col "txn_id" 'WD 'R O.PGInt4 TransactionId
    , 'Col "txn_date" 'W 'R O.PGTimestamp LocalTime
    , 'Col "account_id" 'W 'R O.PGInt4 AccountId
    , 'Col "txn_type_cd" 'W 'RN O.PGText TransactionType
    , 'Col "amount" 'W 'R O.PGFloat4 Float
    , 'Col "teller_employee_id" 'W 'RN O.PGInt4 EmployeeId
    , 'Col "execution_branch_id" 'W 'RN O.PGInt4 BranchId
    , 'Col "funds_avail_date" 'W 'RN O.PGTimestamp LocalTime
    ]

--------------------------------------------------------------------------------
-- Querying.

-- We somewhat offer opaleye-sot counterparts for the HRR examples at
-- https://khibino.github.io/haskell-relational-record/examples.html
--------------------------------------------------------------------------------

-- | Order by field, desc.
q_TAccount_desc :: O.Query (PgR TAccount)
q_TAccount_desc =
  O.orderBy (descnf (view (col (C::C "avail_balance"))))
            (O.queryTable table') -- Here: table' == table TAccount, inferred.

-- | Order by multiple fields, asc.
q_TAccount_asc_multi :: O.Query (PgR TAccount)
q_TAccount_asc_multi =
  O.orderBy (mappend (ascnl (view (col (C::C "open_employee_id"))))
                     (asc   (view (col (C::C "product_cd")))))
            (queryTisch') -- Here: table' == table TAccount, inferred.
q_TEmployee_1 :: O.Query (PgR TEmployee)
q_TEmployee_1 = proc () -> do
  e <- queryTisch' -< () -- Here: table' == table TEmployee, inferred.
  restrict -< isNull (e ^. col (C::C "end_date"))
  restrict <<< nullFalse -< ou
     (lt (koln (Time.fromGregorian 2003 1 1))
         (e ^. col (C::C "start_date")))
     (eq (koln "Teller")
         (e ^. col (C::C "title")))
  id -< e

q_TEmployee_TDepartment_join :: O.Query (PgR TEmployee, PgR TDepartment)
q_TEmployee_TDepartment_join = proc () -> do
  e <- queryTisch' -< () -- inferred
  d <- queryTisch' -< () -- inferred
  restrict <<< nullFalse -< eq
     (e ^. col (C::C "department_id")) -- tnc
     (d ^. col (C::C "department_id")) -- tc
     -- Comparing the two columns above doesn't compile without the
     -- 'Comparable' instance below. Yay for not allowing
     -- comparissons/joins between unrelated columns!
  id -< (e,d)

instance Comparable TEmployee "department_id" TDepartment "department_id"

q_TAccount_TIndividual_leftJoin :: O.Query (PgR TAccount, PgRN TIndividual)
q_TAccount_TIndividual_leftJoin =
  leftJoin
   (queryTisch (T::T TAccount))    -- Can't be inferred.
   (queryTisch (T::T TIndividual)) -- Can't be inferred.
   (\(a,i) -> eq (a ^. col (C::C "customer_id"))
                 (i ^. col (C::C "customer_id")))

instance Comparable TAccount "customer_id" TIndividual "customer_id"
