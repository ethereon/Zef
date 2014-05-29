{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Zef.Pyramid where

import Foreign.C.Types
import Control.Monad
import System.IO.Unsafe

import Zef.Image
import Zef.Math as Im
import Zef.Internal.Types
import Zef.Internal.Image

#include <zef_interop.h>
#include <opencv2/core/core_c.h>

foreign import ccall unsafe "core_c.h cvPyrUp"
    c_cvPyrUp' :: PCvMat -> PCvMat -> CInt -> IO ()

c_cvPyrUp :: PCvMat -> PCvMat -> IO ()
c_cvPyrUp a b = c_cvPyrUp' a b (#const CV_GAUSSIAN_5x5)

pyrUp' :: Image a => a -> ImageSize -> a
pyrUp' src dstSize = unsafeImageOp src $ \pSrc -> do
    dst <- createImage dstSize (imageType src)
    withImagePtr dst $ \pDst -> do
        c_cvPyrUp pSrc pDst
        return $ wrapImageData dst

pyrUp :: Image a => a -> a
pyrUp src = pyrUp' src $ ImageSize dstW dstH
    where dstW    = (imageWidth srcSize)*2
          dstH    = (imageHeight srcSize)*2
          srcSize = (imageSize src)

foreign import ccall unsafe "core_c.h cvPyrDown"
    c_cvPyrDown' :: PCvMat -> PCvMat -> CInt -> IO ()

c_cvPyrDown :: PCvMat -> PCvMat -> IO ()
c_cvPyrDown a b = c_cvPyrDown' a b (#const CV_GAUSSIAN_5x5)

pyrDown :: Image a => a -> a
pyrDown src = unsafeImageOp src $ \pSrc -> do
    let srcSize = (imageSize src)
        dstW    = floor $ ((fromIntegral (imageWidth srcSize) :: Float)+1)/2
        dstH    = floor $ ((fromIntegral (imageHeight srcSize) :: Float)+1)/2
    dst <- createImage (ImageSize dstW dstH) (imageType src)
    withImagePtr dst $ \pDst -> do
        c_cvPyrDown pSrc pDst
        return $ wrapImageData dst

-- | Constructs a Gaussian pyramid for the given image.
-- The returned list contains the given image at the head,
-- followed by n descending levels.
buildPyramid :: Image a => a -> Int -> [a]
buildPyramid img n = scanl (\x _ -> pyrDown x) img [1..n]

-- | Constructs a Laplacian pyramid by taking the difference
-- of Gaussians. Returns n Laplacian levels + lowest Gaussian level.
buildLaplacianPyramid :: Image a => a -> Int -> [a]
buildLaplacianPyramid img n = unsafePerformIO $ do
    let pyrs = buildPyramid img n
    forM_ [1..n] $ \i -> do
        let high   = pyrs!!(i-1)
            low    = pyrs!!i
        lowUp <- mkSimilarImage high
        withImagePtr low $ \pLow -> do
            withImagePtr lowUp $ \pLowUp -> do
                -- Upsample lower level.
                c_cvPyrUp pLow pLowUp
                withImagePtr high $ \pHigh -> do
                    -- Subtract from higher level to get the Laplacian level.
                    c_cvSub pHigh pLowUp pHigh
    -- We have overwritten the top n levels of the Gaussian pyramid.
    -- Include the last (n+1-th) Gaussian level for reconstruction.
    return pyrs

collapsePyramid :: Image a => [a] -> a
collapsePyramid pyr = sumUp lowest rest
    where lowest:rest    = reverse pyr
          sumUp acc lvls = case lvls of
            x:xs -> sumUp ((pyrUp' acc $ imageSize x).+x) xs
            []    -> acc

maxPyrLevels :: ImageSize -> Int
maxPyrLevels imgSize = floor $ (logS :: Float)/(log 2)
    where logS = log $ fromIntegral $ min (imageWidth imgSize) (imageHeight imgSize)
