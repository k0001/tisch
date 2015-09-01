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
module Opaleye.SOT.Internal.Test where

import           Control.Arrow
import           Control.Lens
import qualified Data.HList as HL
import           Data.Int
import qualified Database.PostgreSQL.Simple as Pg
import qualified Opaleye as O

import           Opaleye.SOT.Internal

--------------------------------------------------------------------------------
-- TTest

data TTest = TTest

data Test = Test Bool (Maybe Bool) Bool (Maybe Int64)

-- | Internal. See "Opaleye.SOT.Internal.TTest".
instance Tisch TTest where
  type UnTisch TTest = Test
  type SchemaName TTest = "s"
  type TableName TTest = "t"
  type Cols TTest = [ 'Col "c1" 'W 'R O.PGBool Bool
                    , 'Col "c2" 'W 'RN O.PGBool Bool
                    , 'Col "c3" 'WN 'R O.PGBool Bool
                    , 'Col "c4" 'WN 'RN O.PGInt8 Int64 ]
  fromRecHs' = \r -> return $ Test
     (r ^. cola (C::C "c1"))
     (r ^. cola (C::C "c2"))
     (r ^. cola (C::C "c3"))
     (r ^. cola (C::C "c4"))
  toRecHs (Test c1 c2 c3 c4) = mkRecHs $ \set_ -> HL.hBuild
     (set_ (C::C "c1") c1)
     (set_ (C::C "c3") c3)
     (set_ (C::C "c2") c2)
     (set_ (C::C "c4") c4)

types :: ()
types = seq x () where
  x :: ( Rec TTest '[]
           ~ HL.Tagged (T TTest) (HL.Record '[])
       , RecHs TTest
           ~ Rec TTest (Cols_Hs TTest)
       , Cols_Hs TTest
           ~ '[HL.Tagged (TC TTest "c1") Bool,
               HL.Tagged (TC TTest "c2") (Maybe Bool),
               HL.Tagged (TC TTest "c3") Bool,
               HL.Tagged (TC TTest "c4") (Maybe Int64)]
       , RecHsMay TTest
           ~ Rec TTest (Cols_HsMay TTest)
       , Cols_HsMay TTest
           ~ '[HL.Tagged (TC TTest "c1") (Maybe Bool),
               HL.Tagged (TC TTest "c2") (Maybe (Maybe Bool)),
               HL.Tagged (TC TTest "c3") (Maybe Bool),
               HL.Tagged (TC TTest "c4") (Maybe (Maybe Int64))]
       ) => ()
  x = ()

-- | Internal. See "Opaleye.SOT.Internal.TTest".
instance Comparable TTest "c1" TTest "c3" O.PGBool 

query1 :: O.Query (RecPgRead TTest, RecPgRead TTest, RecPgRead TTest, RecPgReadNull TTest)
query1 = proc () -> do
   t1 <- O.queryTable tisch' -< ()
   t2 <- O.queryTable tisch' -< ()
   O.restrict -< eq
      (view (col (C::C "c1")) t1)
      (view (col (C::C "c1")) t2)
   (t3, t4n) <- O.leftJoin 
      (O.queryTable (tisch TTest))
      (O.queryTable (tisch TTest))
      (\(t3, t4) -> eq -- requires instance Comparable TTest "c1" TTest "c3" O.PGBool 
         (view (col (C::C "c1")) t3)
         (view (col (C::C "c3")) t4)) -< ()
   returnA -< (t1,t2,t3,t4n)

query2 :: O.Query (RecPgRead TTest)
query2 = proc () -> do
  (t,_,_,_) <- query1 -< ()
  returnA -< t

outQuery2 :: Pg.Connection -> IO [RecHs TTest]
outQuery2 conn = O.runQuery conn query2

query3 :: O.Query (RecPgReadNull TTest)
query3 = proc () -> do
  (_,_,_,t) <- query1 -< ()
  returnA -< t

outQuery3 :: Pg.Connection -> IO [Maybe (RecHs TTest)]
outQuery3 conn = fmap mayRecHs <$> O.runQuery conn query3

update1 :: Pg.Connection -> IO Int64
update1 conn = O.runUpdate conn tisch' upd fil
  where upd :: RecPgRead TTest -> RecPgWrite TTest
        upd = over (cola (C::C "c3")) Just
            . over (cola (C::C "c4")) Just
        fil :: Rec TTest (Cols_PgRead TTest) -> O.Column O.PGBool
        fil = \v -> eqc True (view (col (C::C "c1")) v)

outQuery1 :: Pg.Connection
          -> IO [(RecHs TTest, RecHs TTest, RecHs TTest, Maybe (RecHs TTest))]
outQuery1 conn = do
  xs :: [(RecHs TTest, RecHs TTest, RecHs TTest, RecHsMay TTest)]
     <- O.runQuery conn query1
  return $ xs <&> \(a,b,c,d) -> (a,b,c, mayRecHs d)
