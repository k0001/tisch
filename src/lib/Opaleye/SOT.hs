-- This is the entry point for the @opaleye-sot@ library.
--
-- This module re-exports much, but not all of the @Opaleye.SOT.Run@ and
-- @Opaleye.SOT.Internal.*@ modules. If you are looking for some tool that you
-- can't find here, please refer to those modules instead.
--
-- This module doesn't export any infix operator, and likely never will.
module Opaleye.SOT
 ( -- * Running queries
   --
   -- $runningQueries
   runQuery
 , runQuery1
 , runInsert
 , runInsert1
 , runUpdate
 , runDelete
 , runInsertReturning
 , runInsertReturning1
   -- * Connection management
 , Perm(..)
 , Allow
 , Forbid
 , Conn
 , Conn'
 , connect
 , connect'
 , close
 , withReadOnlyTransaction
 , withReadWriteTransaction
 , withSavepoint
   -- * Defining a 'Table'
 , Table
 , TableR
 , TableRW
 , Database
 , SchemaName
 , TableName
 , Columns
 , Column(..)
 , RCap(..)
 , WCap(..)
   -- * Working with 'Table'
 , HsR
 , HsI
 , mkHsI
 , hsi
 , WDef(..)
 , wdef
 , PgR
 , PgRN
 , PgW
   -- * Kol
 , Kol
 , ToKol(..)
 , liftKol1
 , liftKol2
   -- * Koln
 , Koln
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
 , Query
 , query
 , innerJoin
 , leftJoin
 , restrict
 , limit
 , offset
   -- * Selecting
 , col
   -- * Ordering
 , O.Order
 , orderBy
 , asc
 , ascNullsFirst
 , ascNullsLast
 , desc
 , descNullsFirst
 , descNullsLast
   -- * Operators
 , lnot
 , lor
 , land
 , matchBool
   -- ** Equality
 , PgEq
 , eq
 , member
   -- ** Comparisons
 , PgOrd
 , lt
 , lte
 , gt
 , gte
   -- ** Various numeric
 , PgNum
 , PgIntegral
 , PgFractional
 , PgFloating
 , modulo
 , euler's
 , itruncate
 , iround
 , iceil
 , ifloor
   -- ** Bit-wise
 , PgBitwise
 , bwand
 , bwor
 , bwxor
 , bwnot
 , bwsl
 , bwsr
   -- ** Time
 , now
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
   -- * Aggregation
   --
   -- $aggregation
 , Aggregator
 , orderAggregator
 , aggregate
 , groupBy
 , AggSum
 , sumgg
 , countgg
 , countngg
 , countRows
 , AggAvg
 , avggg
 , bwandgg
 , bworgg
 , landgg
 , lorgg
 , maxgg
 , mingg
 , arraygg
 , arrayngg
 , jsonarraygg
 , jsonbarraygg
 , textgg
 , byteagg
 , AggStdDev
 , stddevgg
 , stddevpopgg
 , variancegg
 , variancepopgg
   -- * Column types
 , PgTyped(..)
 , O.PGArray
 , PGArrayn
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
 , PGNumeric
 , O.PGText
 , O.PGTimestamptz
 , O.PGTimestamp
 , O.PGTime
 , O.PGUuid
   -- ** Parsing
 , QueryRunnerColumnDefault(..)
 , qrcFromField
 , qrcFieldParser
 , qrcFieldParserMap
 , qrcMap
 , qrcMapMay
 , qrcPrism
 , qrcWrapped
   -- ** Coercing / type casting
 , CastKol
 , castKol
 , upcastKol
 , unsafeDowncastKol
 , unsafeCoerceKol
 , unsaferCoerceKol
 , unsaferCastKol
 ) where

import qualified Opaleye as O

import Opaleye.SOT.Internal.Aggregation
import Opaleye.SOT.Internal.Compat
import Opaleye.SOT.Internal.Kol
import Opaleye.SOT.Internal.Koln
import Opaleye.SOT.Internal.Table
import Opaleye.SOT.Internal.Query
import Opaleye.SOT.Run


-- $runningQueries
--
-- The "Opaleye.SOT.Run" module exports lower-level variants of these @runXxx@
-- functions, in case you need those.


-- $aggregation
--
-- Except for 'groupBy', all aggregation functions working on 'Kol's are
-- suffixed with @gg@ (for a/gg/regate). Aggregation functions working on
-- 'Koln's are suffixed with @ngg@.
