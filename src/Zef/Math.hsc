{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Zef.Math where

import Foreign.C.Types
import System.IO.Unsafe
import Control.Monad
import Zef.Image
import Prelude  hiding (div)

#include <zef_interop.h>
#include <opencv2/core/core_c.h>

type UnaryImageOp = PCvMat -> PCvMat -> IO ()

type BinaryImageOp = PCvMat -> PCvMat -> PCvMat -> IO ()

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

foreign import ccall unsafe "core_c.h cvAdd"
    c_cvAdd :: BinaryImageOp

add :: Image a => a -> a -> a
add = performBinaryOp c_cvAdd

(.+) :: Image a => a -> a -> a
(.+) = add

foreign import ccall unsafe "core_c.h cvSub"
    c_cvSub :: BinaryImageOp

sub :: Image a => a -> a -> a
sub = performBinaryOp c_cvSub

(.-) :: Image a => a -> a -> a
(.-) = sub

foreign import ccall unsafe "core_c.h cvMul"
    c_cvMul :: PCvMat -> PCvMat -> PCvMat -> CDouble -> IO ()

mul :: Image a => a -> a -> a
mul = performBinaryOp $ \pSrcA pSrcB pDst -> c_cvMul pSrcA pSrcB pDst 1.0

(.*) :: Image a => a -> a -> a
(.*) = mul

foreign import ccall unsafe "core_c.h cvDiv"
    c_cvDiv :: PCvMat -> PCvMat -> PCvMat -> CDouble -> IO ()

div :: Image a => a -> a -> a
div = performBinaryOp $ \pSrcA pSrcB pDst -> c_cvDiv pSrcA pSrcB pDst 1.0

(./) :: Image a => a -> a -> a
(./) = div

foreign import ccall unsafe "core_c.h cvLaplace"
    c_cvLaplace :: PCvMat -> PCvMat -> CInt -> IO ()

laplacian :: Image a => a -> a
laplacian = performUnaryOp (\pSrc pDst -> c_cvLaplace pSrc pDst 3)

foreign import ccall unsafe "zef_interop.h zef_abs"
    c_zef_abs :: UnaryImageOp

abs :: Image a => a -> a
abs = performUnaryOp c_zef_abs

foreign import ccall unsafe "core_c.h cvPow"
    c_cvPow :: PCvMat -> PCvMat -> CDouble -> IO ()

pow' :: Image a => CDouble -> a -> a
pow' e = performUnaryOp $ \pSrc pDst -> c_cvPow pSrc pDst e

pow :: Image a => a -> CDouble -> a
pow img e = pow' e img

(.^) :: Image a => a -> CDouble -> a
(.^) = pow

sqrt :: Image a => a -> a
sqrt = pow' 0.5

scale :: Image a => a -> CDouble -> a
scale img s = scaleConvertImage (imageDepth img) s img

(~*) :: Image a => a -> CDouble -> a
(~*) = scale

sum :: Image a => [a] -> a
sum images = unsafePerformIO $ do
    acc <- mkSimilarImage (images!!0)
    setImage acc 0
    withImagePtr acc $ \pAcc ->
        forM_ images $ \img ->
            withImagePtr img $ \pImg ->
                c_cvAdd pAcc pImg pAcc
    return acc

