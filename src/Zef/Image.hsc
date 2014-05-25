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
#include <opencv2/core/core_c.h>
#include <opencv2/highgui/highgui_c.h>

---- Types for C counterparts

-- |Opaque type corresponding to OpenCV's CvMat type.
data CvMat

type PCvMat = Ptr CvMat

type ImageData = ForeignPtr CvMat

data ImageSize = ImageSize { imageWidth  :: CInt
                           , imageHeight :: CInt
                           }
                           deriving (Eq, Show)

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

---- Core Utility

withImagePtr :: Image a => a -> (PCvMat -> IO b) -> IO b
withImagePtr img f = withForeignPtr (getImageData img) f

unsafeImageOp :: Image a => a -> (PCvMat -> IO b) -> b
unsafeImageOp img f = unsafePerformIO $ withImagePtr img f

foreign import ccall unsafe "zef_interop.h.h &zef_free_mat"
    c_zef_free_mat :: FunPtr (PCvMat -> IO ())

newImageData :: PCvMat -> IO ImageData
newImageData p = newForeignPtr c_zef_free_mat p

foreign import ccall unsafe "core_c.h cvGetElemType"
    c_cvGetElemType :: PCvMat -> IO CInt

imageType :: Image a => a -> CInt
imageType img = unsafeImageOp img $ \pImg -> c_cvGetElemType pImg

foreign import ccall unsafe "zef_interop.h zef_get_mat_depth"
    c_zef_get_mat_depth :: PCvMat -> IO CInt

imageDepth :: Image a => a -> CInt
imageDepth img = unsafeImageOp img $ \pImg -> c_zef_get_mat_depth pImg

foreign import ccall unsafe "zef_interop.h zef_get_mat_channel_count"
    c_zef_get_mat_channel_count :: PCvMat -> IO CInt

imageChannelCount :: Image a => a -> CInt
imageChannelCount img = unsafeImageOp img $ \pImg -> c_zef_get_mat_channel_count pImg

foreign import ccall unsafe "zef_interop.h zef_make_mat_type"
    c_zef_make_mat_type :: CInt -> CInt -> CInt

mkImageType :: CInt -> CInt -> CInt
mkImageType = c_zef_make_mat_type

foreign import ccall unsafe "zef_interop.h zef_get_width"
    c_zef_get_width :: PCvMat -> IO CInt

foreign import ccall unsafe "zef_interop.h zef_get_height"
    c_zef_get_height :: PCvMat -> IO CInt

imageSize :: Image a => a -> ImageSize
imageSize img = unsafeImageOp img $ \pImg -> do
    w <- c_zef_get_width pImg
    h <- c_zef_get_height pImg
    return $ ImageSize w h

---- Creating Images

foreign import ccall unsafe "core_c.h cvCreateMat"
    c_cvCreateMat :: CInt -> CInt -> CInt -> IO PCvMat

createMatrix :: CInt -> CInt -> CInt -> IO ImageData
createMatrix rows cols mType = do
    pMat <- c_cvCreateMat rows cols mType
    newImageData pMat

createImage :: ImageSize -> CInt -> IO ImageData
createImage imgSize mType = createMatrix (imageHeight imgSize) (imageWidth imgSize) mType

mkSingleChan :: Image a => a -> IO GrayImage
mkSingleChan img = GrayImage <$> createImage (imageSize img) destType
    where destType = mkImageType (imageDepth img) 1

mkSimilarImage :: Image a => a -> IO a
mkSimilarImage img = wrapImageData <$> createImage (imageSize img) (imageType img)

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

---- Color + Channel Conversion

foreign import ccall unsafe "zef_interop.h.h zef_rgb_to_gray"
    c_zef_rgb_to_gray :: PCvMat -> IO PCvMat

rgbToGray :: RGBImage -> GrayImage
rgbToGray img = GrayImage $ unsafeImageOp img $ \pImg -> do
    pOut <- c_zef_rgb_to_gray pImg
    newImageData pOut

foreign import ccall unsafe "core_c.h cvSplit"
    c_cvSplit :: PCvMat -> PCvMat -> PCvMat -> PCvMat -> IO ()

splitRGB :: RGBImage -> [GrayImage]
splitRGB img = unsafeImageOp img $ \pImg -> do
    r <- mkSingleChan img
    g <- mkSingleChan img
    b <- mkSingleChan img
    withImagePtr r $ \pR -> do
        withImagePtr g $ \pG -> do
            withImagePtr b $ \pB -> do
                c_cvSplit pImg pR pG pB
                return [r, g, b]

---- Type Conversion

foreign import ccall unsafe "zef_interop.h.h zef_convert_scale"
    c_zef_convert_scale :: PCvMat -> CInt -> CDouble -> IO PCvMat

imageConvertScale :: Image a => CInt -> CDouble -> a -> a
imageConvertScale destDepth scale img = wrapImageData $ unsafeImageOp img $ \pImg -> do
    pOut <- c_zef_convert_scale pImg destDepth scale
    newImageData pOut

byteToFloat :: Image a => a -> a
byteToFloat = imageConvertScale (#const CV_32F) (1/255)

floatToByte :: Image a => a -> a
floatToByte = imageConvertScale (#const CV_8U) 255