module Main where

import Zef.Image
import Zef.UI

main :: IO ()
main = do
    img <- loadImage "/Users/ethereon/Dropbox/Photos/Madeline.jpg"
    showImage $ rgbToGray $ byteToFloat img
    _ <- waitForKeyPress
    return ()

