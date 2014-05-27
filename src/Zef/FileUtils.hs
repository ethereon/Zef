module Zef.FileUtils where

import Zef.Image

import System.Directory
import Control.Applicative
import System.FilePath
import Data.List

imageExtensions :: [String]
imageExtensions = [".png", ".jpg", ".jpeg", ".bmp", ".tif"]

isImagePath :: FilePath -> Bool
isImagePath imgPath = any (\ext -> ext `isSuffixOf` imgPath) imageExtensions

getFileList :: FilePath -> IO [FilePath]
getFileList srcDir = do
    allFiles <- getDirectoryContents srcDir
    return $ map (\x -> joinPath [srcDir, x]) allFiles

getImagePaths :: FilePath -> IO [FilePath]
getImagePaths path = filter isImagePath <$> getFileList path

imagesAtPath :: FilePath -> IO [RGBImage]
imagesAtPath path = do
    paths <- getImagePaths path
    mapM loadImage paths