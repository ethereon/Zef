module Main where

import Zef.UI

main :: IO ()
main = do
    img <- loadImage "/Users/ethereon/Dropbox/Photos/Madeline.jpg"
    showImage img
    key <- waitForKeyPress
    return ()

