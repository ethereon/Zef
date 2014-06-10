{-# LANGUAGE CPP                        #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zef.Internal.Types where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe

#include <zef_core.h>
#include <opencv2/core/core_c.h>

-- |Opaque type corresponding to OpenCV's CvMat type.
data CvMat

type PCvMat = Ptr CvMat

newtype ImageData = ImageData { unImageData :: ForeignPtr CvMat } deriving (Show)

data ImageSize = ImageSize { imageWidth  :: CInt
                           , imageHeight :: CInt
                           }
                           deriving (Eq, Show)

class Image a where
    toImageData     :: a -> ImageData
    fromImageData   :: ImageData -> a

foreign import ccall unsafe "zef_core.h.h zef_mat_eq"
    c_mat_eq :: PCvMat -> PCvMat -> IO CInt

instance Eq ImageData where
  x == y  = unsafePerformIO $ do
    withForeignPtr (unImageData x) $ \pX -> do
      withForeignPtr (unImageData y) $ \pY -> do
        isEq <- (c_mat_eq pX pY)
        return $ isEq==1

-- |A matrix containing 3 channel RGB image (stored as B G R).
newtype RGBImage = RGBImage { unRGBImage :: ImageData } deriving (Eq, Show)

instance Image ImageData where
    toImageData     = id
    fromImageData   = id

instance Image RGBImage where
    toImageData     = unRGBImage
    fromImageData   = RGBImage

-- |A matrix containing a single channel gray scale image.
newtype GrayImage = GrayImage { unGrayImage :: ImageData } deriving (Eq, Show)

instance Image GrayImage where
    toImageData     = unGrayImage
    fromImageData   = GrayImage

type UnaryImageOp = PCvMat -> PCvMat -> IO ()

type BinaryImageOp = PCvMat -> PCvMat -> PCvMat -> IO ()
