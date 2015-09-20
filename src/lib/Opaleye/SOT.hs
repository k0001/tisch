-- | For a better experience, it is recommended that you import this module
-- unqualified. It overrides some exports of "Opaleye" module. Import as follows:
--
-- @
-- import           Opaleye.SOT
-- import qualified Opaleye as O
-- @
module Opaleye.SOT
 ( -- * Defining a 'Tisch'
   Tisch(..)
 , UnHsR(..)
 , unHsR
 , unHsR_
 , ToHsI(..)
 , toHsI
 , mkHsI
 , toPgW'
 , toPgW

   -- * Working with 'Tisch'
 , TischTable
 , table'
 , table
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
 , liftKol
 , liftKol2

   -- * Koln
 , NotNullable
 , Koln
 , unKoln
 , koln
 , matchKoln
 , nul
 , isNull
 , altKoln
 , bindKoln
 , liftKoln
 , liftKoln2

   -- * Executing
 , runUpdate

   -- * Querying
 , leftJoin
 , leftJoinExplicit
 , restrict
 , nullFalse
 , nullTrue
 , no, Ino
 , eq, Ieq
 , lt, Ilt
 , ou, Iou
 , et, Iet
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

   -- * Types
 , Comparable
 , Col(..)
 , C(..)
 , T(..)
 , TC(..)
 , RN(..)
 , WD(..)

   -- * Column type details
 , Col_ByName
 , Col_Name
 , Col_PgRType
 , Col_PgRNType
 , Col_PgWType
 , Col_HsRType
 , Col_HsIType
 ) where

import           Opaleye.SOT.Internal
