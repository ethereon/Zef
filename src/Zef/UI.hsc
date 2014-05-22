{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Zef.UI where

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Zef.Image

#include <opencv2/highgui/highgui_c.h>

---- FFI Imports : OpenCV

foreign import ccall unsafe "highui_c.h cvWaitKey"
    c_cvWaitKey :: CInt -> IO CInt

foreign import ccall unsafe "highui_c.h cvShowImage"
    c_cvShowImage :: CString -> PCvMat -> IO ()

---- Wrappers

showImageNamed :: Image a => String -> a -> IO ()
showImageNamed name img = do
    withCString name $ \cstr -> do
        withForeignPtr (getImageData img) $ \pImg -> do
            c_cvShowImage cstr pImg

showImage :: Image a => a -> IO ()
showImage = showImageNamed "Preview"

waitForKeyPressUntil :: Double -> IO Int
waitForKeyPressUntil secs = do
    keyCode <- c_cvWaitKey $ fromInteger $ round $ secs*1000
    return $ fromIntegral keyCode

waitForKeyPress :: IO Int
waitForKeyPress = waitForKeyPressUntil 0