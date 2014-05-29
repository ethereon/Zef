module Zef.FileUtils where

import Control.Applicative
import Data.List
import System.Directory
import System.FilePath

import Zef.Image
import Zef.Internal.Types

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
