-- | For a better experience, it is recommended that you import this module
-- unqualified as follows:
--
-- @
-- import           Opaleye.SOT
-- import qualified Opaleye as O
-- @
--
-- This module overrides some of the names exported by the "Opaleye".
--
-- This module doesn't export any infix operator.
module Opaleye.SOT
 ( -- * Defining a 'Tabla'
   Tabla(..)
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

   -- * Executing
   --
   -- Please see the "Opaleye.SOT.Run" module for documentation.
 , module Opaleye.SOT.Run

   -- * Querying
 , leftJoin
 , leftJoinExplicit
 , restrict
 , nullTrue
 , nullFalse
 , no, Op_no
 , eq, Op_eq
 , lt, Op_lt
 , ou, Op_ou
 , et, Op_et
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
