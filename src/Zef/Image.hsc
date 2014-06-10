{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Zef.Image where

import Control.Applicative
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr.Safe
import System.IO.Unsafe

import Zef.Internal.Image
import Zef.Internal.Types
import Zef.Primitives

#include <zef_core.h>
#include <opencv2/core/core_c.h>
#include <opencv2/highgui/highgui_c.h>

---- Constructing Images

uniformImage :: Image a => CInt -> ImageSize -> CDouble -> a
uniformImage imgType imgSize v = unsafePerformIO $ do
    img <- createImage imgSize imgType
    setImage img v
    return $ fromImageData img

uniformRGBImage :: ImageSize -> CDouble -> RGBImage
uniformRGBImage = uniformImage (#const CV_32FC3)

uniformGrayImage :: ImageSize -> CDouble -> GrayImage
uniformGrayImage = uniformImage (#const CV_32FC1)

mkSingleChan :: Image a => a -> IO GrayImage
mkSingleChan img = GrayImage <$> mkSimilarChan img 1

mkSimilarImage :: Image a => a -> IO a
mkSimilarImage img = fromImageData <$> createImage (imageSize img) (imageType img)

foreign import ccall unsafe "zef_core.h zef_create_roi"
    c_zef_create_roi :: PCvMat -> Ptr Rect -> IO PCvMat

getROI :: Image a => a -> Rect -> a
getROI img rect = unsafePerformIO $ withImagePtr img $ \pImg -> do
    alloca $ \pRect -> do
        poke pRect rect
        pRoi <- c_zef_create_roi pImg pRect
        roi <- newImageData pRoi
        return $ fromImageData roi

foreignRGBImage :: PCvMat -> IO RGBImage
foreignRGBImage pMat = do
    fpMat <- newForeignPtr_ pMat
    return $ fromImageData $ ImageData fpMat

---- Reading Images

loadImage :: String -> IO RGBImage
loadImage = loadImageCast RGBImage (#const CV_LOAD_IMAGE_COLOR)

loadGrayscaleImage :: String -> IO GrayImage
loadGrayscaleImage = loadImageCast GrayImage (#const CV_LOAD_IMAGE_GRAYSCALE)

---- Writing Images

foreign import ccall unsafe "highui_c.h cvSaveImage"
    c_cvSaveImage :: CString -> PCvMat -> Ptr CInt -> IO ()

saveImage :: Image a => a -> FilePath -> IO ()
saveImage img path = withImagePtr img $ \pImg ->
    withCString path $ \pPath ->
        c_cvSaveImage pPath pImg nullPtr

---- Color + Channel Conversion

foreign import ccall unsafe "zef_core.h.h zef_rgb_to_gray"
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

foreign import ccall unsafe "core_c.h cvMerge"
    c_cvMerge :: PCvMat -> PCvMat -> PCvMat -> PCvMat -> PCvMat -> IO ()

mergeRGB :: [GrayImage] -> RGBImage
mergeRGB chans = unsafePerformIO $ do
    merged <- RGBImage <$> mkSimilarChan (chans!!0) 3
    withImagePtr merged $ \pMerged -> do
        withImagePtr (chans!!0) $ \pR -> do
            withImagePtr (chans!!1) $ \pG -> do
                withImagePtr (chans!!2) $ \pB -> do
                    c_cvMerge pR pG pB nullPtr pMerged
                    return merged

---- Type Conversion

byteToFloat :: Image a => a -> a
byteToFloat = scaleConvertImage (#const CV_32F) (1/255)

floatToByte :: Image a => a -> a
floatToByte = scaleConvertImage (#const CV_8U) 255

---- Transformation Utility

transformImage :: Image a => a -> UnaryImageOp -> a
transformImage src f = unsafeImageOp src $ \pSrc -> do
    dst <- mkSimilarImage src
    withImagePtr dst $ \pDst -> do
        f pSrc pDst
    return dst

transformImageBinary :: Image a => a -> a -> BinaryImageOp -> a
transformImageBinary srcA srcB f = transformImage srcA $ \pSrcA pDst -> do
    withImagePtr srcB $ \pSrcB -> do
        f pSrcA pSrcB pDst

performUnaryOp :: Image a => UnaryImageOp -> a -> a
performUnaryOp c_f src = transformImage src c_f

performBinaryOp :: Image a => BinaryImageOp -> a -> a -> a
performBinaryOp c_f srcA srcB = transformImageBinary srcA srcB c_f
