{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Zef.Image where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr

-- |Opaque types corresponding to OpenCV's CvMat type.
data CvMat
type PCvMat = Ptr CvMat
type ImageData = ForeignPtr CvMat

-- |A matrix containing 3 channel RGB image (stored as B G R).
newtype RGBImage = RGBImage { unRGBImage :: ImageData } deriving (Eq, Show)

-- |A matrix containing a single channel gray scale image.
newtype GrayImage = GrayImage { unGrayImage :: ImageData } deriving (Eq, Show)

class Image a where
    imageData   :: a -> ImageData

instance Image RGBImage where
    imageData   = unRGBImage

instance Image GrayImage where
    imageData   = unGrayImage


