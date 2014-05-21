{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Zef.UI where

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Zef.Image
import Control.Applicative

#include <opencv2/highgui/highgui_c.h>
#include <ZefUtil.h>

---- FFI Imports : OpenCV

foreign import ccall unsafe "highui_c.h cvWaitKey"
    c_cvWaitKey :: CInt -> IO CInt

foreign import ccall unsafe "highui_c.h cvLoadImageM"
    c_cvLoadImageM :: CString -> CInt -> IO PCvMat

foreign import ccall unsafe "highui_c.h cvShowImage"
    c_cvShowImage :: CString -> PCvMat -> IO ()

---- FFI Imports : Zef Utility Wrapper

foreign import ccall unsafe "zefUtil.h &zef_free_mat"
    c_zef_free_mat :: FunPtr (PCvMat -> IO ())

---- Wrappers

loadImageWithMode :: CInt -> String -> IO ImageData
loadImageWithMode mode imgPath = do
    withCString imgPath $ \cstr -> do
        pMat <- c_cvLoadImageM cstr mode
        newForeignPtr c_zef_free_mat pMat

loadImageCast :: Image a => (ImageData -> a) -> CInt -> String -> IO a
loadImageCast f mode imgPath = f <$> loadImageWithMode mode imgPath

loadImage :: String -> IO RGBImage
loadImage = loadImageCast RGBImage (#const CV_LOAD_IMAGE_COLOR)

loadGrayscaleImage :: String -> IO GrayImage
loadGrayscaleImage = loadImageCast GrayImage (#const CV_LOAD_IMAGE_GRAYSCALE)

showImageNamed :: Image a => String -> a -> IO ()
showImageNamed name img = do
    withCString name $ \cstr -> do
        withForeignPtr (imageData img) $ \pImg -> do
            c_cvShowImage cstr pImg

showImage :: Image a => a -> IO ()
showImage = showImageNamed "Preview"

waitForKeyPressUntil :: Double -> IO Int
waitForKeyPressUntil secs = do
    keyCode <- c_cvWaitKey $ fromInteger $ round $ secs*1000
    return $ fromIntegral keyCode

waitForKeyPress :: IO Int
waitForKeyPress = waitForKeyPressUntil 0