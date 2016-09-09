{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.SOT.Internal.Debug
 ( renderSqlQuery
 , renderSqlQuery'
 ) where

import qualified Data.Profunctor.Product.Default as PP
import qualified Opaleye.Sql as O
import qualified Opaleye.Internal.Unpackspec as OI

import Opaleye.SOT.Internal.Query (Query(..))

--------------------------------------------------------------------------------

renderSqlQuery
  :: forall d v. (PP.Default OI.Unpackspec v v) => Query d () v -> Maybe String
renderSqlQuery = renderSqlQuery' (PP.def :: OI.Unpackspec v v)

renderSqlQuery' :: OI.Unpackspec v v' -> Query d () v -> Maybe String
renderSqlQuery' u = O.showSqlForPostgresExplicit u . unQuery




