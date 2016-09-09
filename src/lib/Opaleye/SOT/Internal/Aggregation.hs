{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Opaleye.SOT.Internal.Aggregation
  ( O.Aggregator
  , aggregatorOrder
  , aggregate
  , groupBy
  ) where

import qualified Data.Profunctor as P
import qualified Opaleye.Order as O
import qualified Opaleye.Aggregate as O
import qualified Opaleye.Internal.Aggregate as O

import Opaleye.SOT.Internal.Kol (Kol(..), PgOrd, PgEq)
import Opaleye.SOT.Internal.Query (Query(..))

--------------------------------------------------------------------------------

-- | TODO: update these docs.
--
-- Order the values within each aggregation in `Aggregator` using
-- the given ordering. This is only relevant for aggregations that
-- depend on the order they get their elements, like
-- `Opaleye.Aggregate.arrayAgg` and `Opaleye.Aggregate.stringAgg`.
--
-- You can either apply it to an aggregation of multiple columns, in
-- which case it will apply to all aggregation functions in there, or you
-- can apply it to a single column, and then compose the aggregations
-- afterwards. Examples:
--
-- > x :: Aggregator (Column a, Column b) (Column (PGArray a), Column (PGArray a))
-- > x = (,) <$> orderAggregate (asc snd) (lmap fst arrayAggGrouped)
-- >         <*> orderAggregate (desc snd) (lmap fst arrayAggGrouped)
--
-- This will generate:
--
-- @
-- SELECT array_agg(a ORDER BY b ASC), array_agg(a ORDER BY b DESC)
-- FROM (SELECT a, b FROM ...)
-- @
--
-- Or:
--
-- > x :: Aggregator (Column a, Column b) (Column (PGArray a), Column (PGArray b))
-- > x = orderAggregate (asc snd) $ p2 (arrayAggGrouped, arrayAggGrouped)
--
-- This will generate:
--
-- @
-- SELECT array_agg(a ORDER BY b ASC), array_agg(b ORDER BY b ASC)
-- FROM (SELECT a, b FROM ...)
-- @
aggregatorOrder :: PgOrd a => O.Order a -> O.Aggregator a b -> O.Aggregator a b
aggregatorOrder = O.orderAggregate

--------------------------------------------------------------------------------

-- | Given a 'Query' producing rows of type @a@ and an 'Aggregator' accepting
-- rows of type @a@, apply the aggregator to the results of the query.
--
-- Please note that when aggregating an empty query with no @GROUP BY@ clause,
-- @opaleye-sot@'s behaviour differs from Postgres's behaviour. PostgreSQL
-- returns a single row whereas @opaleye-sot@ returns zero rows.
-- @opaleye-sot@'s behaviour is consistent with the meaning of aggregating over
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
--
-- TODO: rename this?
groupBy :: PgEq a => O.Aggregator (Kol a) (Kol a)
groupBy = P.dimap unKol Kol O.groupBy

