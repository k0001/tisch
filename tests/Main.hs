{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains test and example code. The funny thing is that,
-- as most of this library happens at the type level, these tests run
-- while compiling the library.
--
-- You might learn a thing or two reading the source code.
module Main where

import           Control.Arrow
import           Control.Lens
import qualified Data.HList as HL
import           Data.Int
import qualified Database.PostgreSQL.Simple as Pg
import qualified Opaleye as O

import           Opaleye.SOT

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
  type Cols TTest = [ 'Col "c1" 'W 'R O.PGBool Bool
                    , 'Col "c2" 'W 'RN O.PGBool Bool
                    , 'Col "c3" 'WD 'R O.PGBool Bool
                    , 'Col "c4" 'WD 'RN O.PGInt8 Int64 ]

data TestR = TestR Bool (Maybe Bool) Bool (Maybe Int64)

testR_fromHsR :: HsR TTest -> TestR
testR_fromHsR = \r -> TestR
   (r ^. col (C::C "c1"))
   (r ^. col (C::C "c2"))
   (r ^. col (C::C "c3"))
   (r ^. col (C::C "c4"))

data TestW = TestW Bool (Maybe Bool) (WDef Bool) (WDef (Maybe Int64))

testW_toHsI :: TestW -> HsI TTest
testW_toHsI (TestW c1 c2 c3 c4)
  = mkHsI T $ \set_ -> HL.hBuild
     (set_ (C::C "c1") c1)
     (set_ (C::C "c2") c2)
     (set_ (C::C "c3") c3)
     (set_ (C::C "c4") c4)

types :: ()
types = seq x () where
  x :: ( Rec TTest '[]
           ~ HL.Tagged (T TTest) (HL.Record '[])
       , HsR TTest
           ~ Rec TTest (Cols_HsR TTest)
       , Cols_HsR TTest
           ~ '[HL.Tagged (TC TTest "c1") Bool,
               HL.Tagged (TC TTest "c2") (Maybe Bool),
               HL.Tagged (TC TTest "c3") Bool,
               HL.Tagged (TC TTest "c4") (Maybe Int64)]
       ) => ()
  x = ()

query1 :: O.Query (PgR TTest, PgR TTest, PgR TTest, PgRN TTest)
query1 = proc () -> do
   t1 <- queryTabla T -< ()
   t2 <- queryTabla T -< ()
   restrict -< eq
      (view (col (C::C "c1")) t1)
      (view (col (C::C "c1")) t2)
   (t3, t4n) <- leftJoin
      (queryTabla (T::T TTest))
      (queryTabla (T::T TTest))
      (\(t3, t4) -> eq
         (view (col (C::C "c1")) t3)
         (view (col (C::C "c3")) t4)) -< ()
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
update1 c = runUpdateTabla c (T::T TTest) id fil
  where fil = \v -> eq (kol True) (view (col (C::C "c1")) v)

outQuery1 :: Pg.Connection
          -> IO [(HsR TTest, HsR TTest, HsR TTest, Maybe (HsR TTest))]
outQuery1 conn = O.runQuery conn query1
