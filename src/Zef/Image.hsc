{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Zef.Image where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import System.IO.Unsafe
import Control.Applicative

#include <zef_interop.h>
#include <opencv2/highgui/highgui_c.h>

---- Types for C counterparts

-- |Opaque types corresponding to OpenCV's CvMat type.
data CvMat

type PCvMat = Ptr CvMat

type ImageData = ForeignPtr CvMat

---- Image Types

class Image a where
    getImageData    :: a -> ImageData
    wrapImageData   :: ImageData -> a

-- |A matrix containing 3 channel RGB image (stored as B G R).
newtype RGBImage = RGBImage { unRGBImage :: ImageData } deriving (Eq, Show)

instance Image RGBImage where
    getImageData    = unRGBImage
    wrapImageData   = RGBImage

-- |A matrix containing a single channel gray scale image.
newtype GrayImage = GrayImage { unGrayImage :: ImageData } deriving (Eq, Show)

instance Image GrayImage where
    getImageData    = unGrayImage
    wrapImageData   = GrayImage

---- Interop Utility

unsafeImageOp :: Image a => a -> (PCvMat -> IO b) -> b
unsafeImageOp img f = unsafePerformIO $ withForeignPtr (getImageData img) f

---- Reading Images

foreign import ccall unsafe "highui_c.h cvLoadImageM"
    c_cvLoadImageM :: CString -> CInt -> IO PCvMat

-- |Read an image from the given path using the given mode.
-- The mode parameter is one of the valid OpenCV constants.
loadImageWithMode :: CInt -> String -> IO ImageData
loadImageWithMode mode imgPath = do
    withCString imgPath $ \cstr -> do
        pMat <- c_cvLoadImageM cstr mode
        newImageData pMat

loadImageCast :: Image a => (ImageData -> a) -> CInt -> String -> IO a
loadImageCast f mode imgPath = f <$> loadImageWithMode mode imgPath

loadImage :: String -> IO RGBImage
loadImage = loadImageCast RGBImage (#const CV_LOAD_IMAGE_COLOR)

loadGrayscaleImage :: String -> IO GrayImage
loadGrayscaleImage = loadImageCast GrayImage (#const CV_LOAD_IMAGE_GRAYSCALE)

---- Memory Management

foreign import ccall unsafe "zefUtil.h &zef_free_mat"
    c_zef_free_mat :: FunPtr (PCvMat -> IO ())

newImageData :: PCvMat -> IO ImageData
newImageData p = newForeignPtr c_zef_free_mat p

---- Color Conversion

foreign import ccall unsafe "ZefUtil.h zef_rgb_to_gray"
    c_zef_rgb_to_gray :: PCvMat -> IO PCvMat

rgbToGray :: RGBImage -> GrayImage
rgbToGray img = GrayImage $ unsafeImageOp img $ \pImg -> do
    pOut <- c_zef_rgb_to_gray pImg
    newImageData pOut

---- Type Conversion
foreign import ccall unsafe "zefUtil.h zef_convert_scale"
    c_zef_convert_scale :: PCvMat -> CInt -> CDouble -> IO PCvMat

imageConvertScale :: Image a => CInt -> CDouble -> a -> a
imageConvertScale destDepth scale img = wrapImageData $ unsafeImageOp img $ \pImg -> do
    pOut <- c_zef_convert_scale pImg destDepth scale
    newImageData pOut

byteToFloat :: Image a => a -> a
byteToFloat = imageConvertScale (#const CV_32F) (1/255)

floatToByte :: Image a => a -> a
floatToByte = imageConvertScale (#const CV_8U) 255



