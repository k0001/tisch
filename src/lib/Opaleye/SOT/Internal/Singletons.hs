{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Opaleye.SOT.Internal.Singletons
 ( First
 , FirstSym0
 , FirstSym1
 , FirstSym2

 , Second
 , SecondSym0
 , SecondSym1
 , SecondSym2

 , (:&&&)
 , (:&&&$)
 , (:&&&$$)
 , (:&&&$$$)
 , (:&&&$$$$)
 ) where

import Data.Singletons.TH

--------------------------------------------------------------------------------

$(promoteOnly [d|
  -- | Like 'Control.Arrow.first' for tuples.
  first :: (x -> x') -> (x, y) -> (x', y)
  first f (x, y) = (f x, y)

  -- | Like 'Control.Arrow.second' for tuples.
  second :: (y -> y') -> (x, y) -> (x, y')
  second f (x, y) = (x, f y)

  infixr 3 &&&
  -- | Like '(Control.Arrow.&&&)' for tuples.
  (&&&) :: (x -> y) -> (x -> z) -> x -> (y, z)
  (&&&) f g = \x -> (f x, g x)
  |])

