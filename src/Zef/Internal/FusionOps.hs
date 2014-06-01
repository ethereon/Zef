module Zef.Internal.FusionOps where

import System.IO.Unsafe

import Zef.Internal.Fusion
import Zef.Internal.Types
import Zef.Internal.Image

transformImage :: Image a => Cascade a -> UnaryImageOp -> Cascade a
transformImage c f = unsafePerformIO $ withImagePtr (cscSource c) $ \pSrc -> do
    buf <- cascadeBuffer c
    withImagePtr buf $ \pBuf -> do
        f pSrc pBuf
        return $ BufferedCascade { cscSource  = buf }

transformImageBinary :: Image a => Cascade a -> Cascade a -> BinaryImageOp -> Cascade a
transformImageBinary c1 c2 f = unsafePerformIO $ withImagePtr (cscSource c1) $ \pSrc1 ->
    withImagePtr (cscSource c2) $ \pSrc2 -> do
        buf <- cascadeBuffer $ coalesceCascades [c1, c2]
        withImagePtr buf $ \pBuf -> do
            f pSrc1 pSrc2 pBuf
            return $ BufferedCascade { cscSource  = buf }

performUnaryOp :: Image a => UnaryImageOp -> a -> a
performUnaryOp f src = uncascade $ transformImage (cascade src) f
{-# INLINE performUnaryOp #-}

performBinaryOp :: Image a => BinaryImageOp -> a -> a -> a
performBinaryOp f src1 src2 = uncascade $ transformImageBinary c1 c2 f
    where c1 = cascade src1
          c2 = cascade src2
{-# INLINE performBinaryOp #-}
