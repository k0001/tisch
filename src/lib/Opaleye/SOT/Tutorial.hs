{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- | @opaleye-sot@ is a different API for the core @opaleye@
infraestructure with the following goals in mind:

* Type safety everywhere.

  While it is true that @opaleye@, by relying in the type system,
  makes it impossible to write malformed queries, it doesn't keep
  us for accidentaly referring to a wrong column, comparing two
  columns we are not supposed to compare, confusing two tables
  that happen to have the same shape, or similar scenarios.
  @opaleye-sot@, by making a heavy use of the GHC type system,
  provides an additional layer of safety that can prevent these
  undesired scenarios.

* Boilerplate removal.

  Working with @opaleye@ can get a bit boilerplatey. @opaleye-sot@
  never requires us to say the same thing more than once, and it
  provides us with generic machinery so that we can skip altogether
  the provision of the types that @opaleye@ requires.

* Maintenance.

  As a consequence of the extended type safety and small amount of
  boilerplate that @opaleye-sot@ requires, maintaining code that uses
  the @opaleye-sot@ API is easy. For example, when writing queries,
  columns are identified by their PostgreSQL name, so, if we ever
  change the name of a column in the table description, then our
  querying code using the old name will not compile.

* THIS IS A WORK IN PROCESS! I'M NOT DONE WRITING WHAT'S NICE ABOUT
  THIS. Meanwhile, read the source code here as an example.

-}
module Opaleye.SOT.Tutorial
  ( TDepartment(..)
  , TBranch(..)
  , TEmployee(..)
  , TProductType(..)
  , TProduct(..)
  , TCustomer(..)
  , TIndividual(..)
  , TBusiness(..)
  , TOfficer(..)
  , TAccount(..)
  , TTransaction(..)
  ) where

import           Data.Time (Day, LocalTime)
import           Data.Int
import qualified Opaleye as O
import           Opaleye.SOT

--------------------------------------------------------------------------------

-- NOTE: In the examples below we use unprecise types such as 'String' or tuples
-- where more precise and descriptive types would be usually better. This is just
-- to keep the examples simple for the time being. We will fix this before
-- relasing these examples to the wild.
--
-- Also, to be honest, the example SQL from LearningSQLExample.sql is quite poor,
-- not exactly great. We should improve that.

---

type Department_Id = Int32
type Department_Name = String

data TDepartment = TDepartment
instance Tisch TDepartment where
  type SchemaName TDepartment = "public"
  type TableName TDepartment = "department"
  type Cols TDepartment =
    [ 'Col "department_id" 'WD 'R O.PGInt4 Department_Id
    , 'Col "name" 'W 'R O.PGText Department_Name
    ]

---

type Branch_Id = Int32
type Branch_Name = String
type Branch_Address = String
type Branch_City = String
type Branch_State = String
type Branch_Zip = String

data TBranch = TBranch
instance Tisch TBranch where
  type SchemaName TBranch = "public"
  type TableName TBranch = "branch"
  type Cols TBranch =
    [ 'Col "branch_id" 'WD 'R O.PGInt4 Branch_Id
    , 'Col "name" 'W 'R O.PGText Branch_Name
    , 'Col "address" 'W 'RN O.PGText Branch_Address
    , 'Col "city" 'W 'RN O.PGText Branch_City
    , 'Col "state" 'W 'RN O.PGText Branch_State
    , 'Col "zip" 'W 'RN O.PGText Branch_Zip
    ]

---

type Employee_Id = Int32
type Employee_FirstName = String
type Employee_LastName = String
type Employee_StartDate = Day
type Employee_EndDate = Day
type Employee_SuperiorEmployeeId = Employee_Id
type Employee_DepartmentId = Department_Id
type Employee_Title = String
type Employee_AssignedBranchId = Branch_Id

data TEmployee = TEmployee
instance Tisch TEmployee where
  type SchemaName TEmployee = "public"
  type TableName TEmployee = "employee"
  type Cols TEmployee =
    [ 'Col "employee_id" 'WD 'R O.PGInt4 Employee_Id
    , 'Col "fname" 'W 'R O.PGText Employee_FirstName
    , 'Col "lname" 'W 'R O.PGText Employee_LastName
    , 'Col "start_date" 'W 'R O.PGDate Employee_StartDate
    , 'Col "end_date" 'W 'RN O.PGDate Employee_EndDate
    , 'Col "superior_employee_id" 'W 'RN O.PGInt4 Employee_SuperiorEmployeeId
    , 'Col "department_id" 'W 'RN O.PGInt4 Employee_DepartmentId
    , 'Col "title" 'W 'RN O.PGText Employee_Title
    , 'Col "assigned_branch_id" 'W 'RN O.PGInt4 Employee_AssignedBranchId
    ]

---

type ProductType_Cd = String
type ProductType_Name = String

data TProductType = TProductType
instance Tisch TProductType where
  type SchemaName TProductType = "public"
  type TableName TProductType = "product_type"
  type Cols TProductType =
    [ 'Col "product_type_cd" 'W 'R O.PGText ProductType_Cd
    , 'Col "name" 'W 'R O.PGText ProductType_Name
    ]

---

type Product_Cd = String
type Product_Name = String
type Product_TypeCd = String
type Product_DateOffered = Day
type Product_DateRetired = Day

data TProduct = TProduct
instance Tisch TProduct where
  type SchemaName TProduct = "public"
  type TableName TProduct = "department"
  type Cols TProduct =
    [ 'Col "product_cd" 'W 'R O.PGText Product_Cd
    , 'Col "name" 'W 'R O.PGText Product_Name
    , 'Col "product_type_cd" 'W 'R O.PGText Product_TypeCd
    , 'Col "date_offered" 'W 'RN O.PGDate Product_DateOffered
    , 'Col "date_retired" 'W 'RN O.PGDate Product_DateRetired
    ]

---

type Customer_Id = Int32
type Customer_FedId = String
type Customer_TypeId = Char
type Customer_Address = String
type Customer_City = String
type Customer_State = String
type Customer_PostalCode = String

data TCustomer = TCustomer
instance Tisch TCustomer where
  type SchemaName TCustomer = "public"
  type TableName TCustomer = "customer"
  type Cols TCustomer =
    [ 'Col "customer_id" 'WD 'R O.PGInt4 Customer_Id
    , 'Col "fed_id" 'W 'R O.PGText Customer_FedId
    , 'Col "cust_type_id" 'W 'R O.PGText Customer_TypeId
    , 'Col "address" 'W 'RN O.PGText Customer_Address
    , 'Col "city" 'W 'RN O.PGText Customer_City
    , 'Col "state" 'W 'RN O.PGText Customer_State
    , 'Col "postal_code" 'W 'RN O.PGText Customer_PostalCode
    ]

---

type Individual_CustomerId = Customer_Id
type Individual_FirstName = String
type Individual_LastName = String
type Individual_BirthDate = Day

data TIndividual = TIndividual
instance Tisch TIndividual where
  type SchemaName TIndividual = "public"
  type TableName TIndividual = "individual"
  type Cols TIndividual =
    [ 'Col "customer_id" 'W 'R O.PGInt4 Individual_CustomerId
    , 'Col "fname" 'W 'R O.PGText Individual_FirstName
    , 'Col "lname" 'W 'R O.PGText Individual_LastName
    , 'Col "birth_date" 'W 'RN O.PGDate Individual_BirthDate
    ]

---

type Business_CustomerId = Customer_Id
type Business_Name = String
type Business_StateId = String
type Business_IncorpDate = Day

data TBusiness = TBusiness
instance Tisch TBusiness where
  type SchemaName TBusiness = "public"
  type TableName TBusiness = "business"
  type Cols TBusiness =
    [ 'Col "customer_id" 'W 'R O.PGInt4 Business_CustomerId
    , 'Col "name" 'W 'R O.PGText Business_Name
    , 'Col "state_id" 'W 'R O.PGText Business_StateId
    , 'Col "incorp_date" 'W 'RN O.PGDate Business_IncorpDate
    ]

---

type Officer_Id = Int32
type Officer_CustomerId = Customer_Id
type Officer_FirstName = String
type Officer_LastName = String
type Officer_Title = String
type Officer_StartDate = Day
type Officer_EndDate = Day

data TOfficer = TOfficer
instance Tisch TOfficer where
  type SchemaName TOfficer = "public"
  type TableName TOfficer = "officer"
  type Cols TOfficer =
    [ 'Col "officer_id" 'WD 'R O.PGInt4 Officer_Id
    , 'Col "customer_id" 'W 'R O.PGInt4 Officer_CustomerId
    , 'Col "fname" 'W 'R O.PGText Officer_FirstName
    , 'Col "lname" 'W 'R O.PGText Officer_LastName
    , 'Col "title" 'W 'RN O.PGText Officer_Title
    , 'Col "start_date" 'W 'R O.PGDate Officer_StartDate
    , 'Col "end_date" 'W 'RN O.PGDate Officer_EndDate
    ]

---

type Account_Id = Int32
type Account_ProductCd = Product_Cd
type Account_CustomerId = Customer_Id
type Account_OpenDate = Day
type Account_CloseDate = Day
type Account_LastActivityDate = Day
type Account_Status = String
type Account_OpenBranchId = Branch_Id
type Account_OpenEmployeeId = Employee_Id
type Account_AvailableBalance = Float
type Account_PendingBalance = Float

data TAccount = TAccount
instance Tisch TAccount where
  type SchemaName TAccount = "public"
  type TableName TAccount = "account"
  type Cols TAccount =
    [ 'Col "account_id" 'WD 'R O.PGInt4 Account_Id
    , 'Col "product_cd" 'W 'R O.PGText Account_ProductCd
    , 'Col "customer_id" 'W 'R O.PGInt4 Account_CustomerId
    , 'Col "open_date" 'W 'R O.PGDate Account_OpenDate
    , 'Col "close_date" 'W 'RN O.PGDate Account_CloseDate
    , 'Col "last_activity_date" 'W 'RN O.PGDate Account_LastActivityDate
    , 'Col "status" 'W 'R O.PGText Account_Status
    , 'Col "open_branch_id" 'W 'RN O.PGInt4 Account_OpenBranchId
    , 'Col "open_employee_id" 'W 'RN O.PGInt4 Account_OpenEmployeeId
    , 'Col "avail_balance" 'W 'RN O.PGFloat4 Account_AvailableBalance
    , 'Col "pending_balance" 'W 'RN O.PGFloat4 Account_PendingBalance
    ]

---

type Transaction_Id = Int32
type Transaction_Date = LocalTime
type Transaction_AccountId = Account_Id
type Transaction_TypeCd = String
type Transaction_Amount = Float
type Transaction_TellerEmployeeId = Employee_Id
type Transaction_ExecutionBranchId = Branch_Id
type Transaction_FundsAvailableDate = LocalTime

data TTransaction = TTransaction
instance Tisch TTransaction where
  type SchemaName TTransaction = "public"
  type TableName TTransaction = "transaction"
  type Cols TTransaction =
    [ 'Col "txn_id" 'WD 'R O.PGInt4 Transaction_Id
    , 'Col "txn_date" 'W 'R O.PGTimestamp Transaction_Date
    , 'Col "account_id" 'W 'R O.PGInt4 Transaction_AccountId
    , 'Col "txn_type_cd" 'W 'RN O.PGText Transaction_TypeCd
    , 'Col "amount" 'W 'R O.PGFloat4 Transaction_Amount
    , 'Col "teller_employee_id" 'W 'RN O.PGInt4 Transaction_TellerEmployeeId
    , 'Col "execution_branch_id" 'W 'RN O.PGInt4 Transaction_ExecutionBranchId
    , 'Col "funds_avail_date" 'W 'RN O.PGTimestamp Transaction_FundsAvailableDate
    ]
