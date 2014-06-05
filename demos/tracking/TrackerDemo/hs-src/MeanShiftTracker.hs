module MeanShiftTracker where

import Zef
import Zef.Math as Img
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

maxIters :: CInt
maxIters = 10

epsilon :: CInt
epsilon = 1

clamp :: CInt -> CInt -> CInt -> CInt
clamp v lower upper = min upper $ max lower v

halfDim :: CInt -> CDouble
halfDim v = 0.5*(fromIntegral v)

adjustWindow :: Rect -> ImageSize -> Rect
adjustWindow win imgSize = adjWin
    where r         = win `intersectRect` imgRect
          imgW      = (imageWidth imgSize)
          imgH      = (imageHeight imgSize)
          imgRect   = Rect 0 0 imgW imgH
          (x, y)    = if imgRect==zeroRect
                        then (floor $ halfDim imgW, floor $ halfDim imgH)
                        else (rectX r, rectY r)
          adjWin    = Rect { rectX = x
                           , rectY = y
                           , rectW = max (rectW r) 1
                           , rectH = max (rectH r) 1
                           }

performMeanShift :: CInt -> RGBImage -> Rect -> Rect -> Rect
performMeanShift iter probImage currWin win
    | terminate = currWin
    | otherwise = performMeanShift (succ iter) probImage nextWin win
    where currRect  = adjustWindow currWin imgSize
          imgSize   = imageSize probImage
          roi       = getROI probImage currRect
          mmts      = Img.moments roi
          dx        = round $ (m10 mmts)/(m00 mmts) - (halfDim (rectW win))
          dy        = round $ (m01 mmts)/(m00 mmts) - (halfDim (rectH win))
          nx        = clamp ((rectX currRect)+dx) 0 ((imageWidth imgSize)-(rectW currRect))
          ny        = clamp ((rectY currRect)+dy) 0 ((imageHeight imgSize)-(rectH currRect))
          tx        = nx - (rectX currRect)
          ty        = ny - (rectY currRect)
          nextWin   = currRect { rectX = nx, rectY = ny }
          terminate = (iter==maxIters) || ((tx*tx + ty*ty)<epsilon)

foreign export ccall meanShift :: Ptr CvMat -> Ptr Rect -> IO ()
meanShift :: Ptr CvMat -> Ptr Rect -> IO ()
meanShift pImg pWin = do
    win  <- peek pWin
    img <- foreignRGBImage pImg
    poke pWin $ performMeanShift 0 img win win
