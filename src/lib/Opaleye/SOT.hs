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
 , ToHsI(..)
 , toHsI
 , mkHsI
 , toPgW'
 , toPgW

   -- * Working with 'Tisch'
 , TischTable
 , tisch'
 , tisch
 , HsR
 , HsI
 , PgR
 , PgRN
 , PgW
 , update'
 , update

   -- * Columns
 , toPgTC
 , toPgTCN
   -- ** Querying
 , eq'
 , eq
 , eqn
 , lt'
 , lt
 , ltn
 , ou'
 , ou
 , oun
   -- ** Selecting
 , col
 , cola
 , coln
   -- ** Modifying
 , colav
   -- ** Ordering
 , asc
 , ascnf
 , ascnl
 , desc
 , descnf
 , descnl

   -- * Nullable columns
 , NullableColumn
 , NotNullable
 , toNullableColumn
 , fromNullableColumn
 , bindNullableColumn
 , nullc
 , toPgColumnN
 , isNull
 , nullTrue
 , nullTrue'
 , nullFalse
 , nullFalse'

   -- * Types
 , Comparable
 , ToPgColumn(..)
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
