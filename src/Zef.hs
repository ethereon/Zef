-- |
-- Module       : Zef
-- Copyright    : (c) 2014 Saumitro Dasgupta
-- License      : BSD
-- Maintainer   : Saumitro Dasgupta <saumitro@cs.stanford.edu>
-- Stability    : experimental
-- Portability  : unknown
--
-- Functional Computer Vision
--

module Zef
    ( RGBImage
    , GrayImage
    , imageSize
    , module Zef.Image
    ) where

import Zef.Internal.Types (RGBImage, GrayImage)
import Zef.Internal.Image (imageSize)
import Zef.Image
