module Opaleye.SOT
 ( -- * Defining a 'Tisch'
   Tisch(..)
 , recHs
 , viewC
 , setC
 
   -- * Working with 'Tisch'
 , tisch
 , tisch'
 , RecHs
 , RecHsMay
 , mayTRecHs
 , RecPgRead
 , RecPgReadNull
 , RecPgWrite

   -- * Querying
 , eq
 , eqc
 , eqn
 , eqnc
   -- ** Selecting columns
 , col
 , cola


   -- * Types
 , Comparable
 , ToPgColumn(..)
 , Col(..)
 , WN(..)
 , RN(..)
 , TC(..)
 , T(..)
 , C(..)
 ) where

import           Opaleye.SOT.Internal
