{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is an internal module. You are very discouraged from using it directly.
--
-- This module exports stuff that doesn't really fit into the other internal
-- modules.
module Tisch.Internal.Query
 ( Query(..)
 , query
 , restrict
 , restrictf
 , distinct
 , memberq
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
import qualified Opaleye.Internal.Distinct as OI
import qualified Opaleye.Internal.Join as OI

import Tisch.Internal.Koln (Koln(unKoln), isNull)
import Tisch.Internal.Kol (Kol(unKol))
import Tisch.Internal.Table
  (Table, TableR, Database, PgR, RawTable(..), rawTableRO)
import Tisch.Internal.Compat (unsafeUnNullableColumn)
import Tisch.Internal.Fun (PgEq, PgOrd, eq, lnot)

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
restrict :: Query d (Kol O.PGBool) ()
restrict = Query O.restrict <<^ unKol

-- | This is a 'filter'-like version of 'restrict': Only values that satisfy the
-- predicate are kept.
restrictf :: (a -> Kol O.PGBool) -> Query d a a
restrictf p = proc a -> do
  restrict -< p a
  returnA -< a

-- | Perform an SQL @INNER JOIN@.
--
-- @'innerJoin' t1 t2 f@ returns all of the rows from @t1@ paired with the rows
-- from @f2@ when @f@ is true.
innerJoin
  :: Query d () a
  -> Query d () b
  -> (a -> b -> Kol O.PGBool)
  -> Query d () (a, b) -- ^
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
  :: forall a b nb d
  .  ( PP.Default O.Unpackspec a a
     , PP.Default O.Unpackspec b b
     , PP.Default OI.NullMaker b nb )
  => Query d () a -- ^ Left table.
  -> Query d () b -- ^ Right table.
  -> (a -> b -> Kol O.PGBool)
  -> Query d () (a, nb) -- ^
leftJoin (Query qa) (Query qb) f =
   Query (O.leftJoinExplicit PP.def PP.def PP.def qa qb (unKol . uncurry f))

-- | Whether the given 'Kol' is present in the given 'Query'.
--
-- Sql operator: @IN@.
memberq
  :: forall a d
  .  PgEq a
  => Kol a -- ^ Needle.
  -> Query d () (Kol a)  -- ^ Haystack.
  -> Query d () (Kol O.PGBool)
memberq ks q0 = arr (lnot . isNull . snd) <<< q2
 where
   q1 = arr (const 1) <<< restrictf (eq ks) <<< q0
   q2 :: Query d () (Kol O.PGInt4, Koln O.PGInt4)
   q2 = leftJoin (arr (const 1)) (distinct q1) eq

-- | Remove duplicate rows from the 'Query'.

-- TODO: Can we somehow have a 'PgEq' constraint here?
distinct :: PP.Default OI.Distinctspec a a => Query t () a -> Query t () a
distinct = Query . O.distinctExplicit PP.def . unQuery

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


