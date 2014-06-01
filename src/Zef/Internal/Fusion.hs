module Zef.Internal.Fusion where

import Data.List

import Zef.Internal.Types
import Zef.Image

data (Image a) => Cascade a = NascentCascade  { cscSource :: a }
                            | BufferedCascade { cscSource :: a }

cascade :: Image a => a -> Cascade a
cascade = NascentCascade
{-# INLINE [0] cascade #-}

uncascade :: Image a => Cascade a -> a
uncascade = cscSource
{-# INLINE [0] uncascade #-}
{-# RULES "cascade/uncascade fusion" forall c. cascade (uncascade c) = c #-}

isNascent :: Image a => Cascade a -> Bool
isNascent c = case c of
  NascentCascade _ -> True
  _                -> False

cascadeBuffer :: Image a => Cascade a -> IO a
cascadeBuffer c = if isNascent c
  then mkSimilarImage $ cscSource c
  else return $ cscSource c

coalesceCascades :: Image a => [Cascade a] -> Cascade a
coalesceCascades cs = foldl1' selectCascade cs
    where selectCascade c1 c2 = if isNascent c2 then c1 else c2
