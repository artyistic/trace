module Main where

import qualified Codec.Picture as P
import Graphics.Vec3
import Graphics.Point
import Graphics.Ray
import Data.Function (on)

main :: IO ()
main = createImage

createImage :: IO ()
createImage = P.writePng "output/test.png" $ P.generateImage pixelRenderer imageWidth imageHeight

pixelRenderer :: (Integral a, Integral b) => a -> b -> P.PixelRGB8
pixelRenderer x y = rayColor $ Ray cameraCenter (pixelCenter (fromIntegral x) (fromIntegral y) <-> pt cameraCenter)
  where pixelCenter x' y' = pt pixel00Loc <+> (pixelDu .^ x') <+> (pixelDv .^ y')

-- all temp constants of the eye and viewport

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth :: Int
imageWidth = 400

imageHeight :: Int
imageHeight = max (floor $ fromIntegral imageWidth / aspectRatio) 1

focalLength :: Double
focalLength = 1.0

viewportHeight :: Double
viewportHeight = 2.0

viewportWidth :: Double
viewportWidth = viewportHeight * on (/) fromIntegral imageWidth imageHeight

cameraCenter :: Point
cameraCenter = Point $ V3 0 0 0

viewportU :: V3
viewportU = V3 viewportWidth 0 0

viewportV :: V3
viewportV = V3 0 (negate viewportHeight) 0

pixelDu :: V3
pixelDu = viewportU .^ (1 / fromIntegral imageWidth)
pixelDv :: V3
pixelDv = viewportV .^ (1 / fromIntegral imageHeight)

viewportUpperLeft :: Point
viewportUpperLeft =
  Point $
    pt cameraCenter
      <-> V3 0 0 focalLength
      <-> (viewportU
      .^ 0.5)
      <-> (viewportV
      .^ 0.5)

pixel00Loc :: Point
pixel00Loc = Point $ pt viewportUpperLeft <+> ((pixelDu <+> pixelDv) .^ 0.5)