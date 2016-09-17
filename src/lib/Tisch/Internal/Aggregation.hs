{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Tisch.Internal.Aggregation
  ( O.Aggregator
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
  ) where

import qualified Data.Profunctor as P
import GHC.TypeLits (KnownNat, CmpNat, type (+))
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as OI
import qualified Opaleye.Internal.Aggregate as OI
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HDB

import Tisch.Internal.Compat (PGNumeric)
import Tisch.Internal.Fun (PgOrd, PgEq, PgNum, PgIntegral)
import Tisch.Internal.Kol (Kol(..), PgTyped(..), PGArrayn)
import Tisch.Internal.Koln (Koln(..))
import Tisch.Internal.Query (Query(..))

--------------------------------------------------------------------------------

-- | Order the values within each aggregation in `Aggregator` using the given
-- ordering. This is only relevant for aggregations that depend on the order
-- they get their elements, like 'arraygg' or 'textgg'.
--
-- You can either apply it to an aggregation of multiple columns, in which case
-- it will apply to all aggregation functions in there, or you can apply it to a
-- single column, and then compose the aggregations afterwards. Examples:
--
-- @
-- x :: 'O.Aggregator' ('Kol' a, 'Kol' b) ('Kol' ('O.PGArray' a), 'Kol' ('O.PGArray' a))
-- x = (,) <$> 'orderAggregator' ('Tisch.asc' 'snd')  ('P.lmap' 'fst' 'arraygg')
--         <*> 'orderAggregator' ('Tisch.descl 'snd') ('P.lmap' 'fst' 'arraygg')
-- @
--
-- This will generate:
--
--
-- > SELECT array_agg(a ORDER BY b ASC),
-- >        array_agg(a ORDER BY b DESC)
-- > FROM (SELECT a, b FROM ...)
--
--
-- Or:
--
-- @
-- x :: 'O.Aggregator' ('Kol' a, 'Kol' b) ('Kol' ('O.PGArray' a), 'Kol' ('O.PGArray' a))
-- x = 'orderAggregator' ('Tisch.asc' 'snd') $ 'Tisch.Internal.Profunctors.ppa' ('arraygg', 'arraygg')
-- @
--
-- This will generate:
--
-- > SELECT array_agg(a ORDER BY b ASC),
-- >        array_agg(b ORDER BY b ASC)
-- > FROM (SELECT a, b FROM ...)
orderAggregator :: PgOrd a => O.Order a -> O.Aggregator a b -> O.Aggregator a b
orderAggregator = OI.orderAggregate

--------------------------------------------------------------------------------

-- | Given a 'Query' producing rows of type @a@ and an 'Aggregator' accepting
-- rows of type @a@, apply the aggregator to the results of the query.
--
-- Please note that when aggregating an empty query with no @GROUP BY@ clause,
-- @tisch@'s behaviour differs from Postgres's behaviour. PostgreSQL
-- returns a single row whereas @tisch@ returns zero rows.
-- @tisch@'s behaviour is consistent with the meaning of aggregating over
-- groups of rows, while PostgreSQL's behaviour is. When a query has zero rows
-- it has zero groups, and thus zero rows in the result of an aggregation.
--
-- If you simply want to count the number of rows in a query you might find the
-- 'countRows' function more convenient.
--
-- By design there is no aggregation function of type @Aggregator a b ->
-- Query d x a -> QueryArr d x b@, as such a function would allow violation of
-- SQL's scoping rules and lead to invalid queries.
aggregate :: O.Aggregator a b -> Query d () a -> Query d () b
aggregate f = Query . O.aggregate f . unQuery

--------------------------------------------------------------------------------

-- | Group the aggregation by equality.
groupBy :: PgEq a => O.Aggregator (Kol a) (Kol a)
groupBy = P.dimap unKol Kol O.groupBy

-- | Instances of 'AggSum' can be used with 'sumgg'.
--
-- TODO: Support all possible inputs and ouputs. See PostgreSQL docs.
class (PgNum a, PgNum b) => AggSum a b
instance {-# OVERLAPPABLE #-} PgNum a => AggSum a a
instance AggSum O.PGInt2 O.PGInt8
instance AggSum O.PGInt4 O.PGInt8
instance KnownNat s => AggSum O.PGInt8 (PGNumeric s)
instance AggSum O.PGFloat4 O.PGFloat8
instance AggSum O.PGInt8 (PGNumeric 0)

-- | Add the values in input columns.
sumgg :: AggSum a b => O.Aggregator (Kol a) (Kol b)
sumgg = unsafeMakeAggr HDB.AggrSum

-- | Count the number of non-@NULL@ input values.
--
-- See also: 'countRows', 'countngg'.
countgg :: O.Aggregator (Kol a) (Kol O.PGInt8)
countgg = P.dimap unKol Kol O.count

-- | Count the number of input values, whether they are @NULL@ or not.
--
-- See also: 'countRows', 'countgg'.
countngg :: O.Aggregator (Koln a) (Kol O.PGInt8)
countngg = P.rmap Kol O.countStar

-- | Count the number of rows in a 'Query'.
--
-- This is different from @'aggregate' 'countngg'@ because this always
-- returns exactly one row, even when the input 'Query' is empty.
--
-- See also: 'countgg', 'countngg'.
countRows :: Query d () a -> Query d () (Kol O.PGInt8)
countRows = Query . fmap Kol . O.countRows . unQuery

-- | Instances of 'AggAvg' can be used with 'aggAvg'.
--
-- TODO: Support all possible inputs and ouputs. See PostgreSQL docs.
class (PgNum a, PgNum b) => AggAvg a b
instance {-# OVERLAPPABLE #-} PgNum a => AggAvg a a
instance AggAvg O.PGFloat4 O.PGFloat8
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggAvg O.PGInt2 (PGNumeric s)
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggAvg O.PGInt4 (PGNumeric s)
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggAvg O.PGInt8 (PGNumeric s)
-- | Warning: Depending on your choice of @s'@, you might be getting less
-- resolution than expected.
instance (KnownNat s, KnownNat s', CmpNat s (s' + 1) ~ 'GT) => AggAvg (PGNumeric s) (PGNumeric s')

-- | The average (arithmetic mean) of all input values
avggg :: AggAvg a b => O.Aggregator (Kol a) (Kol b)
avggg = unsafeMakeAggr HDB.AggrAvg

-- | Bitwise AND of all input values.
bwandgg :: PgIntegral a => O.Aggregator (Kol a) (Kol a)
bwandgg = unsafeMakeAggr (HDB.AggrOther "bit_and")

-- | Bitwise OR of all input values.
bworgg :: PgIntegral a => O.Aggregator (Kol a) (Kol a)
bworgg = unsafeMakeAggr (HDB.AggrOther "bit_or")

-- | Logical AND of all input values.
landgg :: O.Aggregator (Kol O.PGBool) (Kol O.PGBool)
landgg = unsafeMakeAggr HDB.AggrBoolAnd

-- | Logical OR of all input values.
lorgg :: O.Aggregator (Kol O.PGBool) (Kol O.PGBool)
lorgg = unsafeMakeAggr HDB.AggrBoolOr

-- | Maximum value of all input values.
maxgg :: PgOrd a => O.Aggregator (Kol a) (Kol a)
maxgg = unsafeMakeAggr HDB.AggrMax

-- | Minimum value of all input values.
mingg :: PgOrd a => O.Aggregator (Kol a) (Kol a)
mingg = unsafeMakeAggr HDB.AggrMin

-- | Collect all non-@NULL@ input values into a 'O.PGArray'.
arraygg :: PgTyped a => O.Aggregator (Kol a) (Kol (O.PGArray a))
arraygg = unsafeMakeAggr HDB.AggrArr

-- | Collect all nullable input values into a 'O.PGArrayn'.
arrayngg :: PgTyped a => O.Aggregator (Koln a) (Kol (PGArrayn a))
arrayngg = P.dimap unKoln Kol (OI.makeAggr HDB.AggrArr)

-- | Aggregates values as a 'O.PGJson' array.
jsonarraygg :: O.Aggregator (Kol a) (Kol O.PGJson)
jsonarraygg = unsafeMakeAggr (HDB.AggrOther "json_agg")

-- | Aggregates values as a 'O.PGJsonb' array.
jsonbarraygg :: O.Aggregator (Kol a) (Kol O.PGJsonb)
jsonbarraygg = unsafeMakeAggr (HDB.AggrOther "jsonb_agg")

-- | Aggregates 'O.PGText' values by concatenating them using the given
-- separator.
textgg :: Kol O.PGText -> O.Aggregator (Kol O.PGText) (Kol O.PGText)
textgg = unsafeMakeAggr . HDB.AggrStringAggr . OI.unColumn . unKol

-- | Aggregates 'O.PGBytea' values by concatenating them using the given
-- separator.
byteagg :: Kol O.PGBytea -> O.Aggregator (Kol O.PGBytea) (Kol O.PGBytea)
byteagg = unsafeMakeAggr . HDB.AggrStringAggr . OI.unColumn . unKol

-- | Instances of 'AggStdDev' can be used with 'stddevgg',
-- 'stddevpopgg', 'variance' and 'variancepopgg'.
class (PgNum a, PgNum b) => AggStdDev a b
instance AggStdDev O.PGFloat4 O.PGFloat8
instance AggStdDev O.PGFloat8 O.PGFloat8
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggStdDev O.PGInt2 (PGNumeric s)
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggStdDev O.PGInt4 (PGNumeric s)
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggStdDev O.PGInt8 (PGNumeric s)
-- | Warning: Depending on your choice of @s'@, you might be getting less
-- resolution than expected.
instance (KnownNat s, KnownNat s', CmpNat s (s' + 1) ~ 'GT) => AggStdDev (PGNumeric s) (PGNumeric s')

-- | Sample standard deviation of the input values.
stddevgg :: AggStdDev a b => O.Aggregator (Kol a) (Kol b)
stddevgg = unsafeMakeAggr HDB.AggrStdDev

-- | Population standard deviation of the input values.
stddevpopgg :: AggStdDev a b => O.Aggregator (Kol a) (Kol b)
stddevpopgg = unsafeMakeAggr HDB.AggrStdDevP

-- | Sample variance of the input values (square of the sample standard
-- deviation 'stdevgg').
variancegg :: AggStdDev a b => O.Aggregator (Kol a) (Kol b)
variancegg = unsafeMakeAggr HDB.AggrVar

-- | Population variance of the input values (square of the population standard
-- deviation 'stdevoppgg').
variancepopgg :: AggStdDev a b => O.Aggregator (Kol a) (Kol b)
variancepopgg = unsafeMakeAggr HDB.AggrVarP

--------------------------------------------------------------------------------

unsafeMakeAggr :: PgTyped b => HDB.AggrOp -> O.Aggregator (Kol a) (Kol b)
unsafeMakeAggr x = P.dimap unKol Kol (OI.makeAggr x)
{-# INLINE unsafeMakeAggr #-}
