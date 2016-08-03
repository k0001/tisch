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

   -- * Working with 'Tabla'
 , table
 , queryTabla
 , HsR(..)
 , HsI(..)
 , mkHsI
 , hsi
 , PgR(..)
 , PgRN(..)
 , PgW(..)
 , pgWfromHsI
 , pgWfromPgR

   -- * Kol
 , Kol(..)
 , kolCast
 , unsafeCoerceKol
 , ToKol(..)
 , liftKol1
 , liftKol2

   -- * Koln
 , Koln(..)
 , koln
 , nul
 , FromKol(..)
 , fromKol
 , matchKoln
 , isNull
 , mapKoln
 , forKoln
 , bindKoln
 , altKoln

   -- * Querying
 , O.Query
 , O.QueryArr
 , leftJoin
 , leftJoinExplicit
 , restrict
 , nullTrue
 , nullFalse
   -- ** Booleans
 , lnot
 , lor
 , land
 , matchBool
   -- ** Equality
 , eq
 , member
   -- ** Comparisons
 , lt
 , lte
 , gt
 , gte
   -- * Selecting
 , col
   -- * Ordering
 , O.orderBy
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
 , Col(..)
 , RN(..)
 , WD(..)

   -- ** Column types
 , PgTyped(..)
 , KolCast
 , KolLike
 , PgNum
 , PgFractional
 , O.PGOrd
 , O.PGBool
 , O.PGBytea
 , O.PGCitext
 , O.PGDate
 , O.PGFloat4
 , O.PGFloat8
 , O.PGInt2
 , O.PGInt4
 , O.PGInt8
 , O.PGJsonb
 , O.PGJson
 , O.PGNumeric
 , O.PGText
 , O.PGTimestamptz
 , O.PGTimestamp
 , O.PGTime
 , O.PGUuid

   -- * Miscellaneous
 , op1
 , op2
 ) where

import           Opaleye.SOT.Internal
import           Opaleye.SOT.Run
import qualified Opaleye as O
