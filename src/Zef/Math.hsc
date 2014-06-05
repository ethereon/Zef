{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Zef.Math where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Control.Monad
import Prelude hiding (div)

import qualified Zef.Internal.FusionOps as F
import Zef.Internal.Fusion
import Zef.Internal.Types
import Zef.Internal.Image
import Zef.Image

#include <zef_core.h>
#include <opencv2/core/core_c.h>
#include <opencv2/imgproc/types_c.h>
#include <opencv2/imgproc/imgproc_c.h>

foreign import ccall unsafe "core_c.h cvAdd"
    c_cvAdd' :: PCvMat -> PCvMat -> PCvMat -> PCvMat -> IO ()

c_cvAdd :: BinaryImageOp
c_cvAdd srcA srcB dst = c_cvAdd' srcA srcB dst nullPtr

add :: Image a => a -> a -> a
add = F.performBinaryOp c_cvAdd
{-# INLINE add #-}

(.+) :: Image a => a -> a -> a
(.+) = add

foreign import ccall unsafe "core_c.h cvSub"
    c_cvSub' :: PCvMat -> PCvMat -> PCvMat -> PCvMat -> IO ()

c_cvSub :: BinaryImageOp
c_cvSub srcA srcB dst = c_cvSub' srcA srcB dst nullPtr

sub :: Image a => a -> a -> a
sub = F.performBinaryOp c_cvSub
{-# INLINE sub #-}

(.-) :: Image a => a -> a -> a
(.-) = sub

foreign import ccall unsafe "core_c.h cvMul"
    c_cvMul :: PCvMat -> PCvMat -> PCvMat -> CDouble -> IO ()

mul :: Image a => a -> a -> a
mul = F.performBinaryOp $ \pSrcA pSrcB pDst -> c_cvMul pSrcA pSrcB pDst 1.0
{-# INLINE mul #-}

(.*) :: Image a => a -> a -> a
(.*) = mul

foreign import ccall unsafe "core_c.h cvDiv"
    c_cvDiv :: PCvMat -> PCvMat -> PCvMat -> CDouble -> IO ()

div :: Image a => a -> a -> a
div = F.performBinaryOp $ \pSrcA pSrcB pDst -> c_cvDiv pSrcA pSrcB pDst 1.0
{-# INLINE div #-}

(./) :: Image a => a -> a -> a
(./) = div

foreign import ccall unsafe "core_c.h cvLaplace"
    c_cvLaplace :: PCvMat -> PCvMat -> CInt -> IO ()

laplacian :: Image a => a -> a
laplacian = F.performUnaryOp (\pSrc pDst -> c_cvLaplace pSrc pDst 3)
{-# INLINE laplacian #-}

foreign import ccall unsafe "zef_core.h zef_abs"
    c_zef_abs :: UnaryImageOp

abs :: Image a => a -> a
abs = F.performUnaryOp c_zef_abs
{-# INLINE abs #-}

foreign import ccall unsafe "core_c.h cvPow"
    c_cvPow :: PCvMat -> PCvMat -> CDouble -> IO ()

pow' :: Image a => CDouble -> a -> a
pow' e = F.performUnaryOp $ \pSrc pDst -> c_cvPow pSrc pDst e
{-# INLINE pow' #-}

pow :: Image a => a -> CDouble -> a
pow img e = pow' e img
{-# INLINE pow #-}

(.^) :: Image a => a -> CDouble -> a
(.^) = pow

sqrt :: Image a => a -> a
sqrt = pow' 0.5

foreign import ccall unsafe "core_c.h cvConvertScale"
    c_cvConvertScale :: PCvMat -> PCvMat -> CDouble -> CDouble -> IO ()

scale :: Image a => a -> CDouble -> a
scale img s = F.performUnaryOp (\pSrc pDst -> c_cvConvertScale pSrc pDst s 0) img
{-# INLINE scale #-}

(~*) :: Image a => a -> CDouble -> a
(~*) = scale

sum :: Image a => [a] -> a
sum images = uncascade $ unsafePerformIO $ do
    acc <- mkSimilarImage (images!!0)
    setImage acc 0
    withImagePtr acc $ \pAcc ->
        forM_ images $ \img ->
            withImagePtr img $ \pImg ->
                c_cvAdd pAcc pImg pAcc
    return $ BufferedCascade { cscSource  = acc }
{-# INLINE sum #-}

sumStacked :: Image a => [[a]] -> [a]
sumStacked stacks = unsafePerformIO $ do
    stackOut <- mapM mkSimilarImage (stacks!!0)
    forM_ stackOut $ \img -> setImage img 0
    forM_ stacks $ \aStack ->
        forM_ (zip stackOut aStack) $ \(levelAcc, anImg) ->
            withImagePtr levelAcc $ \pLevelAcc ->
                withImagePtr anImg $ \pImg ->
                    c_cvAdd pLevelAcc pImg pLevelAcc
    return stackOut

---- Moments

data Moments = Moments { -- Spatial Moments
                         m00  :: CDouble
                       , m10  :: CDouble
                       , m01  :: CDouble
                       , m20  :: CDouble
                       , m11  :: CDouble
                       , m02  :: CDouble
                       , m30  :: CDouble
                       , m21  :: CDouble
                       , m12  :: CDouble
                       , m03  :: CDouble
                         -- Central Moments
                       , mu20 :: CDouble
                       , mu11 :: CDouble
                       , mu02 :: CDouble
                       , mu30 :: CDouble
                       , mu21 :: CDouble
                       , mu12 :: CDouble
                       , mu03 :: CDouble
                       }

instance Storable Moments where    
    sizeOf    _ = (#size CvMoments)
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        t00  <- (#peek CvMoments, m00) ptr
        t10  <- (#peek CvMoments, m10) ptr
        t01  <- (#peek CvMoments, m01) ptr
        t20  <- (#peek CvMoments, m20) ptr
        t11  <- (#peek CvMoments, m11) ptr
        t02  <- (#peek CvMoments, m02) ptr
        t30  <- (#peek CvMoments, m30) ptr
        t21  <- (#peek CvMoments, m21) ptr
        t12  <- (#peek CvMoments, m12) ptr
        t03  <- (#peek CvMoments, m03) ptr
        tu20 <- (#peek CvMoments, mu20) ptr
        tu11 <- (#peek CvMoments, mu11) ptr
        tu02 <- (#peek CvMoments, mu02) ptr
        tu30 <- (#peek CvMoments, mu30) ptr
        tu21 <- (#peek CvMoments, mu21) ptr
        tu12 <- (#peek CvMoments, mu12) ptr
        tu03 <- (#peek CvMoments, mu03) ptr
        return  Moments { m00  = t00
                        , m10  = t10
                        , m01  = t01
                        , m20  = t20
                        , m11  = t11
                        , m02  = t02
                        , m30  = t30
                        , m21  = t21
                        , m12  = t12
                        , m03  = t03
                        , mu20 = tu20
                        , mu11 = tu11
                        , mu02 = tu02
                        , mu30 = tu30
                        , mu21 = tu21
                        , mu12 = tu12
                        , mu03 = tu03
                        }
    poke = undefined

foreign import ccall unsafe "imgproc_c.h cvMoments"
    c_cvMoments :: PCvMat -> Ptr Moments -> CInt -> IO ()

moments :: Image a => a -> Moments
moments img = unsafePerformIO $ withImagePtr img $ \pImg -> do
    alloca $ \pMoments -> do
        c_cvMoments pImg pMoments 0
        peek pMoments
