{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Zef.Internal.Types where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

#include <zef_interop.h>
#include <opencv2/core/core_c.h>

-- |Opaque type corresponding to OpenCV's CvMat type.
data CvMat

type PCvMat = Ptr CvMat

newtype ImageData = ImageData { unImageData :: ForeignPtr CvMat } deriving (Eq, Show)

data ImageSize = ImageSize { imageWidth  :: CInt
                           , imageHeight :: CInt
                           }
                           deriving (Eq, Show)

class Image a where
    getImageData    :: a -> ImageData
    wrapImageData   :: ImageData -> a

-- |A matrix containing 3 channel RGB image (stored as B G R).
newtype RGBImage = RGBImage { unRGBImage :: ImageData } deriving (Eq, Show)

instance Image ImageData where
    getImageData    = id
    wrapImageData   = id

instance Image RGBImage where
    getImageData    = unRGBImage
    wrapImageData   = RGBImage

-- |A matrix containing a single channel gray scale image.
newtype GrayImage = GrayImage { unGrayImage :: ImageData } deriving (Eq, Show)

instance Image GrayImage where
    getImageData    = unGrayImage
    wrapImageData   = GrayImage
