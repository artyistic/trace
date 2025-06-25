module Camera (camera, render) where

import Codec.Picture as P
import qualified Codec.Picture as P
import Data.Function
import Graphics.Pixel
import Graphics.Point
import Graphics.Ray
import Graphics.Vec3
import Hittable
import Hittables
import Shapes.Sphere

-- camera is just defined by aspectRatio and imageWidth
data Camera = Camera
  { aspectRatio :: Double,
    focalLength :: Double,
    imageWidth :: Int,
    -- convention will set it to be (0, 0, 0)
    center :: Point,
    -- derived from above
    imageHeight :: Int,
    pixelDu :: V3,
    pixelDv :: V3,
    pixel00Loc :: Point
  }

-- init a camera
camera :: Double -> Double -> Int -> Camera
camera aspectRatio focalLength imageWidth =
  Camera
    aspectRatio
    focalLength
    imageWidth
    cameraCenter
    imageHeight
    pixelDu
    pixelDv
    pixel00Loc
  where
    imageHeight = max (floor $ fromIntegral imageWidth / aspectRatio) 1
    viewportHeight = 2.0
    viewportWidth = viewportHeight * on (/) fromIntegral imageWidth imageHeight
    cameraCenter = fromCoord 0 0 0

    viewportU = V3 viewportWidth 0 0
    viewportV = V3 0 (negate viewportHeight) 0

    pixelDu = viewportU .^ (1 / fromIntegral imageWidth)
    pixelDv = viewportV .^ (1 / fromIntegral imageHeight)

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
    pixel00Loc = evalPoint viewportUpperLeft (<+> (pixelDu <+> pixelDv) .^ 0.5)

render :: FilePath -> HittableList -> Camera -> IO ()
render
  fpath
  world
  (Camera
      _
      _
      imageWidth
      cameraCenter
      imageHeight
      pixelDu
      pixelDv
      pixel00Loc
    ) =
    P.writePng "output/test.png" $ P.generateImage pixelRenderer imageWidth imageHeight
    where
      pixelRenderer x y =
        rayColor
          (Ray cameraCenter (toV3 $ pixelCenter (fromIntegral x) (fromIntegral y) <-> cameraCenter))
          world
      pixelCenter x' y' = evalPoint pixel00Loc (\p -> p <+> pixelDu .^ x' <+> pixelDv .^ y')

rayColor :: Ray -> HittableList -> P.PixelRGB8
rayColor r@(Ray _ direction) world =
  toRGB8 $ maybe (p1 <+> p2) renderSurfaceNormal normal
  where
    p1 = white .^ (1.0 - a)
    p2 = lightBlue .^ a
    a = 0.5 * (toY (normalize direction) + 1)
    hitRecord = hit world r (1 / 0) 0
    normal = hitNormal <$> hitRecord
    renderSurfaceNormal n = colorFromV3 (V3 (toX n + 1) (toY n + 1) (toZ n + 1) .^ 0.5)

-- colors for background
white :: Color
white = color 1 1 1

lightBlue :: Color
lightBlue = color 0.5 0.7 1.0

-- redSphere :: Sphere
-- redSphere = Sphere (fromCoord 0 0 (-1)) 0.5