{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Miscellaneous compatibility stuff.
module Opaleye.SOT.Internal.Compat
  ( AnyColumn(..)
  , unsafeFunExpr
  , unsafeUnNullableColumn
  , pgFloat4
  , pgFloat8
  , pgInt2
  ) where

import           Data.Int
import           GHC.Float (float2Double)
import qualified GHC.TypeLits as GHC
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as OI
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HDB
import qualified Opaleye.Internal.PGTypes as OI
import qualified Opaleye.Internal.RunQuery as OI

--------------------------------------------------------------------------------

data AnyColumn = forall a. AnyColumn (O.Column a)

-- | 'unsafeFunExpr "f" xs' calls a function called @"f"@ with arguments @xs@.
-- The return type must correctly be set by the caller.
unsafeFunExpr :: HDB.Name -> [AnyColumn] -> O.Column b
unsafeFunExpr fname =
  OI.Column . HDB.FunExpr fname . map (\(AnyColumn (OI.Column x)) -> x)

unsafeUnNullableColumn :: O.Column (O.Nullable a) -> O.Column a
unsafeUnNullableColumn = O.unsafeCoerceColumn

pgFloat4 :: Float -> O.Column O.PGFloat4
pgFloat4 = OI.literalColumn . HDB.DoubleLit . float2Double

pgFloat8 :: Float -> O.Column O.PGFloat8
pgFloat8 = OI.literalColumn . HDB.DoubleLit . float2Double

pgInt2 :: Int16 -> O.Column O.PGInt2
pgInt2 = OI.literalColumn . HDB.IntegerLit . fromIntegral

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.QueryRunnerColumnDefault O.PGFloat4 Float where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn

-- | Conversions to 'Int' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( GHC.TypeError
      ('GHC.Text "QueryRunnerColumnDefault conversions to Int are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Int is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one Int8, Int16, Int32, Int64 from Data.Int.")
  ) => OI.QueryRunnerColumnDefault a Int
  where queryRunnerColumnDefault = undefined

-- | Conversions to 'Word' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( GHC.TypeError
      ('GHC.Text "QueryRunnerColumnDefault conversions to Word are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Word is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one of Word8, Word16, Word32 from Data.Word.")
  ) => OI.QueryRunnerColumnDefault a Word
  where queryRunnerColumnDefault = undefined

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.PGFractional O.PGFloat4 where
  pgFromRational = pgFloat4 . fromRational

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.PGNum O.PGFloat4 where
  pgFromInteger = pgFloat4 . fromInteger

-- | Orphan. "Opaleye.SOT.Internal".
instance OI.PGNum O.PGInt2 where
  pgFromInteger = pgInt2 . fromInteger

