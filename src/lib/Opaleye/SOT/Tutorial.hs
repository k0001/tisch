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
  ) where

import           Control.Lens
import qualified Data.HList as HL
import           Data.Time (Day)
import           Data.Int
import qualified Opaleye as O
import           Opaleye.SOT

--------------------------------------------------------------------------------

-- NOTE: In the examples below we use unprecise types such as 'String' or tuples
-- where more precise and descriptive types would be usually better. This is just
-- to keep the examples simple for the time being. We will fix this before
-- relasing these examples to the wild.

---

type Department_Id = Int32
type Department_Name = String

data TDepartment = TDepartment
instance Tisch TDepartment where
  type SchemaName TDepartment = "public"
  type TableName TDepartment = "department"
  type Cols TDepartment =
    [ 'Col "department_id" 'WD 'R O.PGInt4 Department_Id
    , 'Col "name" 'W 'R O.PGText Department_Name ]
  type UnHsR TDepartment = (Department_Id, Department_Name)
  unHsR' = \r -> return
    ( r ^. cola (C::C "department_id")
    , r ^. cola (C::C "name") )
  type UnHsI TDepartment = Department_Name
  toHsI' = \n -> mkHsI $ \set_ -> HL.hBuild
    (set_ (C::C "department_id") WDef)
    (set_ (C::C "name") n)

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
    , 'Col "zip" 'W 'RN O.PGText Branch_Zip ]
  type UnHsR TBranch = (Branch_Id, Branch_Name, Maybe Branch_Address,
                        Maybe Branch_City, Maybe Branch_State, Maybe Branch_Zip)
  unHsR' = \r -> return
    ( r ^. cola (C::C "branch_id")
    , r ^. cola (C::C "name")
    , r ^. cola (C::C "address")
    , r ^. cola (C::C "city")
    , r ^. cola (C::C "state")
    , r ^. cola (C::C "zip") )
  type UnHsI TBranch = (Branch_Name, Maybe Branch_Address, Maybe Branch_City,
                        Maybe Branch_State, Maybe Branch_Zip)
  toHsI' = \(n,ma,mc,ms,mz) -> mkHsI $ \set_ -> HL.hBuild
    (set_ (C::C "branch_id") WDef)
    (set_ (C::C "address") ma)
    (set_ (C::C "name") n) -- the order of the fields doesn't matter.
    (set_ (C::C "state") ms)
    (set_ (C::C "city") mc)
    (set_ (C::C "zip") mz)


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
    , 'Col "assigned_branch_id" 'W 'RN O.PGInt4 Employee_AssignedBranchId ]
  type UnHsR TEmployee =
    ( Employee_Id
    , Employee_FirstName
    , Employee_LastName
    , Employee_StartDate
    , Maybe Employee_EndDate
    , Maybe Employee_SuperiorEmployeeId
    , Maybe Employee_DepartmentId
    , Maybe Employee_Title
    , Maybe Employee_AssignedBranchId )
  unHsR' = \r -> return
    ( r ^. cola (C::C "employee_id")
    , r ^. cola (C::C "fname")
    , r ^. cola (C::C "lname")
    , r ^. cola (C::C "start_date")
    , r ^. cola (C::C "end_date")
    , r ^. cola (C::C "superior_employee_id")
    , r ^. cola (C::C "department_id")
    , r ^. cola (C::C "title")
    , r ^. cola (C::C "assigned_branch_id") )
  type UnHsI TEmployee =
    ( Employee_FirstName
    , Employee_LastName
    , Employee_StartDate
    , Maybe Employee_EndDate
    , Maybe Employee_SuperiorEmployeeId
    , Maybe Employee_DepartmentId
    , Maybe Employee_Title
    , Maybe Employee_AssignedBranchId )
  toHsI' = \(fn, ln, sd, med, mseId, mdId, mt, mabId) -> mkHsI $ \set_ -> HL.hBuild
    (set_ (C::C "employee_id") WDef)
    (set_ (C::C "fname") fn)
    (set_ (C::C "lname") ln)
    (set_ (C::C "start_date") sd)
    (set_ (C::C "end_date") med)
    (set_ (C::C "superior_employee_id") mseId)
    (set_ (C::C "department_id") mdId)
    (set_ (C::C "title") mt)
    (set_ (C::C "assigned_branch_id") mabId)


---

type ProductType_Cd = String
type ProductType_Name = String

data TProductType = TProductType
instance Tisch TProductType where
  type SchemaName TProductType = "public"
  type TableName TProductType = "product_type"
  type Cols TProductType =
    [ 'Col "product_type_cd" 'W 'R O.PGText ProductType_Cd
    , 'Col "name" 'W 'R O.PGText ProductType_Name ]
  type UnHsR TProductType = (ProductType_Cd, ProductType_Name)
  unHsR' = \r -> return
    ( r ^. cola (C::C "product_type_cd")
    , r ^. cola (C::C "name") )
  toHsI' = \(cd,n) -> mkHsI $ \set_ -> HL.hBuild
    (set_ (C::C "product_type_cd") cd)
    (set_ (C::C "name") n)


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
    , 'Col "date_retired" 'W 'RN O.PGDate Product_DateRetired ]
  type UnHsR TProduct = (Product_Cd, Product_Name, Product_TypeCd, Maybe Product_DateOffered, Maybe Product_DateRetired)
  unHsR' = \r -> return
    ( r ^. cola (C::C "product_cd")
    , r ^. cola (C::C "name")
    , r ^. cola (C::C "product_type_cd")
    , r ^. cola (C::C "date_offered")
    , r ^. cola (C::C "date_retired") )
  toHsI' = \(cd,n,tcd,mo,mr) -> mkHsI $ \set_ -> HL.hBuild
    (set_ (C::C "product_cd") cd)
    (set_ (C::C "name") n)
    (set_ (C::C "product_type_cd") tcd)
    (set_ (C::C "date_offered") mo)
    (set_ (C::C "date_retired") mr)
