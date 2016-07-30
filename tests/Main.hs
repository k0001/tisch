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
import qualified Database.PostgreSQL.Simple as Pg
import qualified Opaleye as O

import           Opaleye.SOT
import           Opaleye.SOT.Internal

import           Tutorial () -- Just for typechecking

--------------------------------------------------------------------------------

main :: IO ()
main = pure () -- nothing to do here, the tests run in the type checker.

--------------------------------------------------------------------------------
-- TTest

data DbTest

data TTest
instance Tabla TTest where
  type Database TTest = DbTest
  type SchemaName TTest = "s"
  type TableName TTest = "t"
  type Cols TTest
    = [ 'Col "c1" 'W 'R O.PGBool Bool
      , 'Col "c2" 'W 'RN O.PGBool Bool
      , 'Col "c3" 'WD 'R O.PGBool Bool
      , 'Col "c4" 'WD 'RN O.PGInt8 Int64
      ]

data TestR = TestR Bool (Maybe Bool) Bool (Maybe Int64)

testR_fromHsR :: HsR TTest -> TestR
testR_fromHsR = \r -> TestR
   (view (col' (C::C "c1")) r) -- == #c1 r
   (view (col @"c2") r)        -- == #c2 r
   (view #c3 r)                -- == #c3 r
   (#c4 r)                     -- == #c4 r

data TestW = TestW Bool (Maybe Bool) (WDef Bool) (WDef (Maybe Int64))

testW_toHsI :: TestW -> HsI TTest
testW_toHsI (TestW c1 c2 c3 c4) =
  mkHsI @TTest
   (hsi #c1 c1)
   (hsi #c2 c2)
   (hsi #c3 c3)
   (hsi #c4 c4)

query1 :: O.Query (PgR TTest, PgR TTest, PgR TTest, PgRN TTest)
query1 = proc () -> do
   t1 <- queryTabla -< () -- inferred
   t2 <- queryTabla -< () -- inferred
   restrict -< eq (#c1 t1) (#c1 t2)
   (t3, t4n) <- leftJoin
      (queryTabla @TTest) -- can't be inferred
      (queryTabla @TTest) -- can't be inferred
      (\(t3,t4) -> eq (#c1 t3) (#c3 t4)) -< ()
   returnA -< (t1,t2,t3,t4n)

query2 :: O.Query (PgR TTest)
query2 = proc () -> do
  (t,_,_,_) <- query1 -< ()
  returnA -< t

outQuery2 :: Pg.Connection -> IO [HsR TTest]
outQuery2 conn = O.runQuery conn query2

query3 :: O.Query (PgRN TTest)
query3 = proc () -> do
  (_,_,_,t) <- query1 -< ()
  returnA -< t

outQuery3 :: Pg.Connection -> IO [Maybe (HsR TTest)]
outQuery3 conn = O.runQuery conn query3

update1 :: Allow 'Update ps => Conn ps -> IO Int64
update1 c = runUpdateTabla c (T :: T TTest) upd fil
  where
    -- inferred: fil :: PgR TTest -> Kol PGBool
    fil = eq (kol True) . #c1
    -- inferred: upd :: PgW TTest -> PgW TTest
    upd = set #c1 (kol True)

outQuery1 :: Pg.Connection
          -> IO [(HsR TTest, HsR TTest, HsR TTest, Maybe (HsR TTest))]
outQuery1 conn = O.runQuery conn query1
