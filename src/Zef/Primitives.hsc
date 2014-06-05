{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Zef.Primitives where

import Foreign
import Foreign.C.Types

#include <opencv2/core/core_c.h>

data Rect = Rect { rectX :: CInt
                 , rectY :: CInt
                 , rectW :: CInt                     
                 , rectH :: CInt
                 }
                 deriving (Eq, Show)

instance Storable Rect where
    sizeOf    _ = (#size CvSize)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        x <- (#peek CvRect, x) ptr
        y <- (#peek CvRect, y) ptr
        w <- (#peek CvRect, width) ptr
        h <- (#peek CvRect, height) ptr
        return  Rect { rectX = x
                     , rectY = y
                     , rectW = w
                     , rectH = h
                     }
    poke ptr (Rect x y w h) = do
        (#poke CvRect, x) ptr x
        (#poke CvRect, y) ptr y
        (#poke CvRect, width) ptr w
        (#poke CvRect, height) ptr h

zeroRect :: Rect
zeroRect = Rect { rectX = 0
                , rectY = 0
                , rectW = 0
                , rectH = 0
                }

rectMaxX :: Rect -> CInt
rectMaxX r = (rectX r) + (rectW r)

rectMaxY :: Rect -> CInt
rectMaxY r = (rectY r) + (rectH r)

intersectRect :: Rect -> Rect -> Rect
intersectRect a b = r
    where x = max (rectX a) (rectX b)
          y = max (rectY a) (rectY b)
          w = (min (rectMaxX a) (rectMaxX b)) - x
          h = (min (rectMaxY a) (rectMaxY b)) - y
          r = if (w<=0) || (h<=0)
                then zeroRect
                else Rect { rectX = x
                          , rectY = y
                          , rectW = w
                          , rectH = h
                          }
