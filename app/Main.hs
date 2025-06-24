module Main where

import qualified Codec.Picture as P
import Data.Function (on)
import Graphics.Pixel
import Graphics.Point
import Graphics.Ray
import Graphics.Vec3
import Hittable
import Shapes.Sphere

main :: IO ()
main = createImage

createImage :: IO ()
createImage = P.writePng "output/test.png" $ P.generateImage pixelRenderer imageWidth imageHeight

pixelRenderer :: (Integral a, Integral b) => a -> b -> P.PixelRGB8
pixelRenderer x y = rayColor $ Ray cameraCenter (toV3 $ pixelCenter (fromIntegral x) (fromIntegral y) <-> cameraCenter)
  where
    pixelCenter x' y' = evalPoint pixel00Loc (\p -> p <+> pixelDu .^ x' <+> pixelDv .^ y')

-- all temp constants of the eye and viewport, ie the scene

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
cameraCenter = fromCoord 0 0 0

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
  evalPoint
    cameraCenter
    ( \x ->
        x
          <-> V3 0 0 focalLength
          <-> viewportU
          .^ 0.5
          <-> viewportV
          .^ 0.5
    )

pixel00Loc :: Point
pixel00Loc = evalPoint viewportUpperLeft (<+> (pixelDu <+> pixelDv) .^ 0.5)

-- | TEMPORARILY MOVED raycolor here due to cyclic dependency of ray and sphere
-- rayColor returns a rgb given a ray
-- hardcoding to consider a red sphere in the scene
-- later maybe pass a object list or sth?
rayColor :: Ray -> P.PixelRGB8
rayColor r@(Ray _ direction) =
  toRGB8 $ maybe (p1 <+> p2) (renderSurfaceNormal redSphere r) t
  where
    p1 = white .^ (1.0 - a)
    p2 = lightBlue .^ a
    a = 0.5 * (toY (normalize direction) + 1)
    t = hitT <$> hit redSphere r 0 (1/0)

renderSurfaceNormal :: Sphere -> Ray -> Double -> Color
renderSurfaceNormal (Sphere center _) r t =
  colorFromV3 (V3 (toX n + 1) (toY n + 1) (toZ n + 1) .^ 0.5)
  where
    n = normalize $ at r t <-> center

-- temporarily we will define objects in the scene here
-- later move to a data type of list maybe containing objects?
redSphere :: Sphere
redSphere = Sphere (fromCoord 0 0 (-1)) 0.5

-- colors for background
white :: Color
white = color 1 1 1

lightBlue :: Color
lightBlue = color 0.5 0.7 1.0