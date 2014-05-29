{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Zef.Internal.Image where

import Control.Applicative
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe

import Zef.Internal.Types

#include <zef_interop.h>
#include <opencv2/core/core_c.h>
#include <opencv2/highgui/highgui_c.h>

---- Core Utility

imagePtr :: Image a => a -> ForeignPtr CvMat
imagePtr = unImageData . getImageData

withImagePtr :: Image a => a -> (PCvMat -> IO b) -> IO b
withImagePtr img f = withForeignPtr (imagePtr img) f

unsafeImageOp :: Image a => a -> (PCvMat -> IO b) -> b
unsafeImageOp img f = unsafePerformIO $ withImagePtr img f

foreign import ccall unsafe "zef_interop.h.h &zef_release_mat"
    c_zef_release_mat :: FunPtr (PCvMat -> IO ())

newImageData :: PCvMat -> IO ImageData
newImageData p = ImageData <$> newForeignPtr c_zef_release_mat p

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

foreign import ccall unsafe "zef_interop.h zef_create_mat"
    c_zef_create_mat :: CInt -> CInt -> CInt -> IO PCvMat

createMatrix :: CInt -> CInt -> CInt -> IO ImageData
createMatrix rows cols mType = do
    pMat <- c_zef_create_mat rows cols mType
    newImageData pMat

createImage :: ImageSize -> CInt -> IO ImageData
createImage imgSize mType = createMatrix (imageHeight imgSize) (imageWidth imgSize) mType

foreign import ccall unsafe "zef_interop.h zef_set"
    c_zef_set :: PCvMat -> CDouble -> IO ()

setImage :: Image a => a -> CDouble -> IO ()
setImage img v = withImagePtr img $ \pImg -> c_zef_set pImg v

mkSimilarChan :: Image a => a -> CInt -> IO ImageData
mkSimilarChan img n = createImage (imageSize img) destType
    where destType = mkImageType (imageDepth img) n

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

---- Type Conversion

foreign import ccall unsafe "zef_interop.h.h zef_convert_scale"
    c_zef_convert_scale :: PCvMat -> CInt -> CDouble -> IO PCvMat

scaleConvertImage :: Image a => CInt -> CDouble -> a -> a
scaleConvertImage destDepth scale img = wrapImageData $ unsafeImageOp img $ \pImg -> do
    pOut <- c_zef_convert_scale pImg destDepth scale
    newImageData pOut
