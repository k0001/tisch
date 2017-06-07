{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains test and example code. The funny thing is that,
-- as most of this library happens at the type level, these tests run
-- while compiling the library.
--
-- You might learn a thing or two reading the source code.
module Main where

import           Control.Arrow
import           Control.Lens
import           Data.Int
import           Data.Proxy
import qualified Opaleye as O

import           Tisch

import           Tutorial () -- Just for typechecking

--------------------------------------------------------------------------------

main :: IO ()
main = pure () -- nothing to do here, the tests run in the type checker.

--------------------------------------------------------------------------------
-- Table1

data Db1
data Table1
data instance Table Table1 = Table1

type instance Database Table1 = Db1
type instance SchemaName Table1 = "s"
type instance TableName Table1 = "t"
type instance Columns Table1
  = [ 'Column "c1" 'W 'R O.PGBool Bool
    , 'Column "c2" 'W 'RN O.PGBool Bool
    , 'Column "c3" 'WD 'R O.PGBool Bool
    , 'Column "c4" 'WD 'RN O.PGInt8 Int64
    ]

data Table1R = Table1R Bool (Maybe Bool) Bool (Maybe Int64)

testR_fromHsR :: HsR Table1 -> Table1R
testR_fromHsR = \r -> Table1R
   (view (col (Proxy :: Proxy "c1")) r) -- == #c1 r
   (view (col (Proxy @"c2")) r)         -- == #c2 r
   (view #c3 r)                         -- == #c3 r
   (#c4 r)                              -- == #c4 r

data Table1W = Table1W Bool (Maybe Bool) (WDef Bool) (WDef (Maybe Int64))

testW_toHsI :: Table1W -> HsI Table1
testW_toHsI (Table1W c1 c2 c3 c4) =
  mkHsI Table1
   (hsi #c1 c1)
   (hsi #c2 c2)
   (hsi #c3 c3)
   (hsi #c4 c4)

query1 :: Query Db1 () (PgR Table1, PgR Table1, PgR Table1, PgRN Table1)
query1 = proc () -> do
   t1 <- query Table1 -< ()
   t2 <- query Table1 -< ()
   restrict -< eq (#c1 t1) (#c1 t2)
   (t3, t4n) <- leftJoin
      (query Table1) (query Table1)
      (\t3 t4 -> eq (#c1 t3) (#c3 t4)) -< ()
   returnA -< (t1,t2,t3,t4n)

query2 :: Query Db1 () (PgR Table1)
query2 = proc () -> do
  (t,_,_,_) <- query1 -< ()
  returnA -< t

outQuery2 :: Allow 'Fetch ps => Conn Db1 ps -> IO [HsR Table1]
outQuery2 conn = runQuery conn query2

query3 :: Query Db1 () (PgRN Table1)
query3 = proc () -> do
  (_,_,_,t) <- query1 -< ()
  returnA -< t

outQuery3 :: Allow 'Fetch ps => Conn Db1 ps -> IO [Maybe (HsR Table1)]
outQuery3 conn = runQuery conn query3

update1 :: Allow 'Update ps => Conn Db1 ps -> IO Int64
update1 conn = runUpdate conn Table1 upd fil
  where
    fil :: PgR Table1 -> Kol PGBool
    fil = eq (kol True) . #c1
    upd :: PgW Table1 -> PgW Table1
    upd = set #c1 (kol True)

outQuery1
  :: Allow 'Fetch ps
  => Conn Db1 ps
  -> IO [(HsR Table1, HsR Table1, HsR Table1, Maybe (HsR Table1))]
outQuery1 conn = runQuery conn query1
