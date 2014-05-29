--  HDR Exposure Fusion using Zef

--  Implements the algorithm as described in Merten's paper:
--  http://research.edm.uhasselt.be/~tmertens/papers/exposure_fusion_reduced.pdf

module Main where

import Zef
import Zef.Math as Img
import Zef.Pyramid
import Zef.UI
import Zef.FileUtils
import System.Environment
import System.IO

contrast :: GrayImage -> GrayImage
contrast = Img.abs . Img.laplacian

saturation :: RGBImage -> GrayImage
saturation img = Img.sqrt variance
    where chans     = splitRGB img
          mean      = (Img.sum chans)~*(1/3)
          variance  = Img.sum $ map (\c -> (c.-mean).^2) chans

weightMap :: RGBImage -> GrayImage
weightMap src = (contrast gs).*(saturation src)
    where gs  = rgbToGray src

blendImagePyr :: RGBImage -> GrayImage -> [RGBImage]
blendImagePyr img weights = map blendLevel $ zip imgPyrs wtPyrs
  where imgPyrs       = buildLaplacianPyramid img numPyrLevels
        wtPyrs        = buildPyramid weights numPyrLevels
        blendLevel    = \(lvl, w) -> mergeRGB (map (\c -> c.*w) (splitRGB lvl))
        numPyrLevels  = maxPyrLevels $ imageSize img

exposureFusion :: [RGBImage] -> RGBImage
exposureFusion images = floatToByte $ collapsePyramid pyrBlended
  where floatImgs   = map byteToFloat images
        weights     = map weightMap floatImgs
        normWeights = map (\w -> w./(Img.sum weights)) weights
        blend       = \(img, w) -> blendImagePyr img w
        pyrBlended  = Img.sumStacked $ map blend $ zip floatImgs normWeights

main :: IO ()
main = do
    args <- getArgs
    if not $ (length args) `elem` [1, 2]
      then
        hPutStrLn stderr "usage: hdr IMAGE_SET_DIR [OUTPUT_FILE_NAME]"
      else do
        let srcDir:rest = args
        images <- imagesAtPath srcDir
        let result = exposureFusion images
        case rest of
          outputFilename:_ ->
            saveImage result outputFilename
          _ -> do
            showImage result
            waitForKeyPress
