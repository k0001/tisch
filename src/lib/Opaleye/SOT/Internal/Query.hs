{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

-- | This is an internal module. You are very discouraged from using it directly.
--
-- This module exports stuff that doesn't really fit into the other internal
-- modules.
module Opaleye.SOT.Internal.Query
 ( Query(..)
 , query
 , restrict
 , innerJoin
 , leftJoin
 , limit
 , offset
 , orderBy
 , asc
 , ascNullsFirst
 , ascNullsLast
 , desc
 , descNullsFirst
 , descNullsLast
 ) where

import           Control.Arrow
import           Control.Category (Category)
import           Data.Profunctor (Profunctor)
import           Data.Profunctor.Product (ProductProfunctor)
import qualified Data.Profunctor.Product.Default as PP
import qualified Opaleye as O
import qualified Opaleye.Internal.Join as OI

import Opaleye.SOT.Internal.Koln
import Opaleye.SOT.Internal.Kol
import Opaleye.SOT.Internal.Table
  (Table, TableR, Database, PgR, RawTable(..), rawTableRO)
import Opaleye.SOT.Internal.Compat (unsafeUnNullableColumn)

--------------------------------------------------------------------------------

-- | A wrapper around @opaleye@'s 'O.QuerryArr' adding a placeholder @t@, which
-- shall mention the 'Database' associated with the query.
--
-- Note that, contrary to @opaleye@, we don't make a distinction between
-- 'O.QueryArr' and 'O.Query', as we think that hurts comprehension. We always
-- use our 'Query' type instead, which behaves as @opaleye@'s 'O.QueryArr', not
-- as @opaleye@'s 'O.Query'.
newtype Query (d :: k) a b = Query { unQuery :: O.QueryArr a b }
 deriving (Functor, Applicative, Category, Arrow, Profunctor, ProductProfunctor)

-- | Query all of the rows in a 'Table.
--
-- This is @SELECT * FROM t@, basically.
query :: TableR t => Table t -> Query (Database t) () (PgR t)
query = Query . O.queryTable . unRawTable . rawTableRO
{-# INLINE query #-}

-------------------------------------------------------------------------------

-- | Restrict query results to a particular condition.
--
-- This is analogous to 'Control.Monad.guard' for 'MonadPlus'.
--
-- Hint: Many times you will have a @'Koln' 'O.PGBool'@ instead of a @'Kol'
-- 'O.PGBool'@. In order to use that with 'restrict' you will have to convert it
-- to a @'Kol' 'O.PGBool'@ first, deciding whether you want 'nul' (@NULL@) to
-- mean “true” or “false”.
--
-- To treat @NULL@ as true:
--
-- @
-- 'fromKoln' ('kol' 'True') :: 'Koln' 'O.PGBool' -> 'Kol' 'O.PGBool'
-- @
--
-- To treat @NULL@ as false:
--
-- @
-- 'fromKoln' ('kol' 'False') :: 'Koln' 'O.PGBool' -> 'Kol' 'O.PGBool'
-- @
restrict :: Query t (Kol O.PGBool) ()
restrict = Query O.restrict <<^ unKol

-- | Perform an SQL @INNER JOIN@.
--
-- @'innerJoin' t1 t2 f@ returns all of the rows from @t1@ paired with the rows
-- from @f2@ when @f@ is true.
innerJoin
  :: Query t () a
  -> Query t () b
  -> (a -> b -> Kol O.PGBool)
  -> Query t () (a, b) -- ^
innerJoin qa qb f = proc () -> do
  a <- qa -< ()
  b <- qb -< ()
  restrict -< f a b
  returnA -< (a, b)

-- | Perform an SQL @LEFT JOIN@.
--
-- @'leftJoin' t1 t2 f@ returns all of the rows from @t1@ (the left table),
-- possibly paired with the rows from @f2@ (the right table) in case @f@ is
-- true.
leftJoin
  :: ( PP.Default O.Unpackspec a a
     , PP.Default O.Unpackspec b b
     , PP.Default OI.NullMaker b nb )
  => Query t () a -- ^ Left table.
  -> Query t () b -- ^ Right table.
  -> (a -> b -> Kol O.PGBool)
  -> Query t () (a, nb) -- ^
leftJoin (Query qa) (Query qb) f =
   Query (O.leftJoinExplicit PP.def PP.def PP.def qa qb (unKol . uncurry f))

--------------------------------------------------------------------------------

-- | Limit the maximum number of resulting rows in a 'Query'.
limit :: Int -> Query d () a -> Query d () a
limit n = Query. O.limit n . unQuery

-- | Offset the results of a query by the given amount, skipping that many
-- resuling rows.
offset :: Int -> Query d () a -> Query d () a
offset n = Query. O.offset n . unQuery

--------------------------------------------------------------------------------
-- Ordering

-- | Order a 'Query' (see 'asc', 'desc' and friends for possible orderings).
--
-- This corresponds to the SQL clause @ORDER BY@
orderBy :: O.Order a -> Query d () a -> Query d () a
orderBy o = Query . O.orderBy o . unQuery

-- | Ascending order, no @NULL@s involved.
asc :: PgOrd b => (a -> Kol b) -> O.Order a
asc f = O.asc (unKol . f)

-- | Ascending order, @NULL@s last.
ascNullsLast :: PgOrd b => (a -> Koln b) -> O.Order a
ascNullsLast f = O.asc (unsafeUnNullableColumn . unKoln . f)

-- | Ascending order, @NULL@s first.
ascNullsFirst :: PgOrd b => (a -> Koln b) -> O.Order a
ascNullsFirst f = O.ascNullsFirst (unsafeUnNullableColumn . unKoln . f)

-- | Descending order, no @NULL@s involved.
desc :: PgOrd b => (a -> Kol b) -> O.Order a
desc f = O.desc (unKol . f)

-- | Descending order, @NULL@s first.
descNullsFirst :: PgOrd b => (a -> Koln b) -> O.Order a
descNullsFirst f = O.desc (unsafeUnNullableColumn . unKoln . f)

-- | Descending order, @NULL@s last.
descNullsLast :: PgOrd b => (a -> Koln b) -> O.Order a
descNullsLast f = O.descNullsLast (unsafeUnNullableColumn . unKoln . f)


