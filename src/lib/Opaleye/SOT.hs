-- | For a better experience, it is recommended that you import this module
-- unqualified as follows:
--
-- @
-- import Opaleye.SOT
-- @
--
-- Note that "Opaleye.SOT" re-exports all of "Opaleye.SOT.Run", you might want
-- to refer to that module for documentation.
--
-- Both "Opaleye.SOT" and "Opaleye.SOT.Run" override some of the names exported
-- by the "Opaleye", so it is recommended that you import Opaleye, if needed,
-- qualified as:
--
-- @
-- import qualified Opaleye as O
-- @
--
-- This module doesn't export any infix operator.
module Opaleye.SOT
 ( -- * Executing queries
   module Opaleye.SOT.Run

   -- * Defining a 'Tabla'
 , Tabla(..)
 , UnHsR(..)
 , unHsR
 , unHsR_
 , ToHsI(..)
 , toHsI
 , mkHsI
 , toPgW'
 , toPgW

   -- * Working with 'Tabla'
 , table'
 , table
 , queryTabla'
 , queryTabla
 , HsR
 , HsI
 , PgR
 , PgRN
 , PgW
 , update'
 , update

   -- * Kol
 , Kol
 , unKol
 , ToKol(..)
 , liftKol1
 , liftKol2

   -- * Koln
 , NotNullable
 , Koln
 , unKoln
 , koln
 , matchKoln
 , isNull
 , mapKoln
 , bindKoln
 , altKoln
 , liftKoln1
 , liftKoln2

   -- * Querying
 , leftJoin
 , leftJoinExplicit
 , restrict
 , nullTrue
 , nullFalse
 , lnot, Op_lnot
 , lor, Op_lor
 , land, Op_land
 , eq, Op_eq
 , eqs, Op_eqs
 , lt, Op_lt
 , lte, Op_lte
 , gt, Op_gt
 , gte, Op_gte
   -- * Selecting
 , col
 , cola
   -- * Ordering
 , asc
 , ascnf
 , ascnl
 , desc
 , descnf
 , descnl

   -- * WDef
 , WDef(WDef, WVal)
 , wdef

   -- * Types
 , Comparable
 , PGType
 , Col(..)
 , C(..)
 , T(..)
 , TC(..)
 , RN(..)
 , WD(..)

   -- ** Individual columns
 , Col_ByName
 , Col_Name
 , Col_PgRType
 , Col_PgRNType
 , Col_PgWType
 , Col_HsRType
 , Col_HsIType

   -- ** Set of columns
 , Rec
 , Cols_HsR
 , Cols_HsI
 , Cols_PgR
 , Cols_PgRN
 , Cols_PgW

   -- * Miscellaneous
 , op1
 , op2
 ) where

import           Opaleye.SOT.Internal
import           Opaleye.SOT.Run
