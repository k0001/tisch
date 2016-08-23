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
 , ToKol(..)
 , liftKol1
 , liftKol2

   -- * Koln
 , Koln(..)
 , koln
 , nul
 , fromKol
 , fromKoln
 , matchKoln
 , isNull
 , mapKoln
 , forKoln
 , bindKoln
 , altKoln

   -- * Querying
 , O.Query
 , O.QueryArr
 , queryTabla
 , leftJoin
 , restrict
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

   -- * Operators
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
   -- ** Various numeric
 , modulo
 , euler's
 , itruncate
 , iround
 , iceil
 , ifloor
   -- ** Bit-wise
 , bwand
 , bwor
 , bwxor
 , bwnot
 , bwsl
 , bwsr
   -- ** Time
 , toTimestamptz
 , toTimestamp
 , timestamptzEpoch
 , timestampCentury
 , timestampDay
 , timestampDayOfTheWeek
 , timestampDayOfTheWeekISO8601
 , timestampDayOfTheYear
 , timestampDecade
 , timestampHour
 , timestampMicroseconds
 , timestampMillenium
 , timestampMilliseconds
 , timestampMinute
 , timestampMonth
 , timestampQuarter
 , timestampSecond
 , timestampWeekISO8601
 , timestampYear
 , timestampYearISO8601

   -- * WDef
 , WDef(WDef, WVal)
 , wdef

   -- * Types
 , Col(..)
 , RN(..)
 , WD(..)

   -- ** Column types
 , PgTyped(..)
 , PgNum
 , PgIntegral
 , PgFractional
 , PgFloating
 , PgEq
 , PgOrd
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

   -- ** Coercing / type casting
 , CastKol
 , castKol
 , upcastKol
 , unsafeDowncastKol
 , unsafeCoerceKol
 , unsaferCoerceKol
 , unsaferCoerceExplicitKol
 ) where

import           Opaleye.SOT.Internal
import           Opaleye.SOT.Run
import qualified Opaleye as O
