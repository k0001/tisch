-- | TODO: This is a work in progress.
module Tisch.Internal.Window
 ( winRowNum
 , winRank
 , winRankDense
 , winRankPercent
 , winRankCumDist
 , winBucket
 , winLag
 , winLead
 , winFirst
 , winLast
 , winNth
 ) where

import qualified Opaleye as O
import Tisch.Internal.Kol (PgTyped, Kol(..))
import Tisch.Internal.Koln (Koln(..))
import Tisch.Internal.Compat (AnyColumn(..), unsafeFunExpr)

--------------------------------------------------------------------------------

-- | Window function. Number of the current row within its partition, counting
-- from 1.
--
-- Sql function name: @row_number()@
winRowNum :: Kol O.PGInt8
winRowNum = Kol (unsafeFunExpr "row_number" [])

-- | Window function. Rank of the current row with gaps. Same as `winRowNum` of
-- its first peer.
--
-- Sql function name: @rank()@
winRank :: Kol O.PGInt8
winRank = Kol (unsafeFunExpr "rank" [])

-- | Window function. Rank of the current row without gaps. This function counts
-- peer groups.
--
-- Sql function name: @dense_rank()@
winRankDense :: Kol O.PGInt8
winRankDense = Kol (unsafeFunExpr "dense_rank" [])

-- | Window function. Relative rank of the current row: Percentage.
--
-- ('winRank' - 1) / (total rows - 1)
--
-- Sql function name: @percent_rank()@
winRankPercent :: Kol O.PGFloat8
winRankPercent = Kol (unsafeFunExpr "percent_rank" [])

-- | Window function. Relative rank of the current row: Cumulative distribution.
--
-- (number of rows preceding or peer with the current row) / (total rows)
--
-- Sql function name: @cume_dist()@
winRankCumDist :: Kol O.PGFloat8
winRankCumDist = Kol (unsafeFunExpr "cume_dist" [])

-- | Window function. Integer rangingfrom 1 to the argument value, dividing the
-- partition as equally as possible.
--
-- Sql function name: @ntile()@
winBucket
  :: Kol O.PGInt4  -- ^ Total number of buckets.
  -> Kol O.PGInt4  -- ^ Bucket for the current row.
winBucket (Kol x) = Kol (unsafeFunExpr "ntile" [AnyColumn x])

-- | Window function. 'winLag' value offset@ returns @value@ evaluated
-- at the row that is @offset@ rows /after/ the current row within the partition.
-- If there is no such row, returns @Tisch.nul@ instead. @offset@ is evaluated
-- with respect to the current row.
--
-- Sql function name: @lag()@.
winLag
  :: PgTyped a
  => Kol a
  -- ^ Value to evaluate at the row that is /offset/ rows /before/ the current
  -- row within the partition.
  -> Kol O.PGInt4 -- ^ Offset.
  -> Koln a
winLag (Kol v) (Kol o) = Koln (unsafeFunExpr "lag" [AnyColumn v, AnyColumn o])

-- | Window function. 'winLead' value offset@ returns @value@ evaluated
-- at the row that is @offset@ rows /after/ the current row within the partition.
-- If there is no such row, returns @Tisch.nul@ instead. @offset@ is evaluated
-- with respect to the current row.
--
-- Sql function name: @lead()@.
winLead
  :: PgTyped a
  => Kol a
  -- ^ Value to evaluate at the row that is /offset/ rows /after/ the current
  -- row within the partition.
  -> Kol O.PGInt4 -- ^ Offset.
  -> Koln a
winLead (Kol v) (Kol o) = Koln (unsafeFunExpr "lead" [AnyColumn v, AnyColumn o])

-- | Window function. @'winFirst' x@ returns @x@ evaluated at the row that is
-- the first row of the window frame.
--
-- Sql function name: @first_value@.
winFirst :: Kol a -> Kol a
winFirst (Kol a) = Kol (unsafeFunExpr "first_value" [AnyColumn a])

-- | Window function. @'winLast' x@ returns @x@ evaluated at the row that is
-- the last row of the window frame.
--
-- Sql function name: @last_value@.
winLast :: Kol a -> Kol a
winLast (Kol a) = Kol (unsafeFunExpr "last_value" [AnyColumn a])

-- | Window function. @'winNth' x nth@ returns @x@ evaluated at the row that is
-- the @nth@ row of the window frame (counting from 1). If no such row exists,
-- returns @nul@.
--
-- Sql function name: @last_value@.
winNth :: Kol a -> Kol O.PGInt4 -> Koln a
winNth (Kol a) (Kol nth) =
  Koln (unsafeFunExpr "last_value" [AnyColumn a, AnyColumn nth])

