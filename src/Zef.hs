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
    ( CvMat 
    , RGBImage
    , GrayImage
    , ImageSize(..)
    , imageSize
    , module Zef.Image
    , module Zef.Primitives
    ) where

import Zef.Internal.Types (CvMat, RGBImage, GrayImage, ImageSize(..))
import Zef.Internal.Image (imageSize)
import Zef.Image
import Zef.Primitives
