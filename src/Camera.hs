module Camera (camera, render, getSampleSquare) where

import Codec.Picture as P
import qualified Codec.Picture as P
import Control.Monad.Random
import Data.Function
import Graphics.Pixel
import Graphics.Point
import Graphics.Ray
import Graphics.Vec3
import Hittable
import Hittables
import qualified Interval as I
import Shapes.Sphere
import System.Random 


-- camera is just defined by aspectRatio and imageWidth
data Camera = Camera
  { aspectRatio :: Double,
    focalLength :: Double,
    imageWidth :: Int,
    samplesPerPixel :: Int,

    -- convention will set it to be (0, 0, 0)
    center :: Point,
    -- derived from above
    imageHeight :: Int,
    pixelDu :: V3,
    pixelDv :: V3,
    pixel00Loc :: Point
  }

-- init a camera
camera :: Double -> Double -> Int -> Int -> Camera
camera aspectRatio focalLength imageWidth samplesPerPixel =
  Camera
    aspectRatio
    focalLength
    imageWidth
    samplesPerPixel
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

render :: FilePath -> HittableList -> Camera -> StdGen -> IO ()
render
  fpath
  world
  ( Camera
      _
      _
      imageWidth
      samplesPerPixel
      cameraCenter
      imageHeight
      pixelDu
      pixelDv
      pixel00Loc
    )
    gen
     = P.writePng "output/test.png" $ P.generateImage t imageWidth imageHeight

          where
            -- for each x y position on viewPort, randomly sample pixels to construct a smooth edge
            -- obviously this does 10x work for each every pixel, super slow
            t x y = evalRand (pixelRenderer x y) gen

            -- replicateM makes a random array of position offsets,
            -- then we map rayColors, average it out and transform it into a PixelRgb8 type
            pixelRenderer x y =
              toRGB8 . averageColor . ( ( \(V3 offsetX offsetY _) ->
                    rayColor
                      (Ray cameraCenter (toV3 $ pixelCenter (fromIntegral x + offsetX) (fromIntegral y + offsetY) <-> cameraCenter))
                      world
                )
                  <$>
              ) <$> replicateM samplesPerPixel getSampleSquare
            pixelCenter x' y' = evalPoint pixel00Loc (\p -> p <+> pixelDu .^ x' <+> pixelDv .^ y')

rayColor :: Ray -> HittableList -> Color
rayColor r@(Ray _ direction) world =
  maybe (p1 <+> p2) renderSurfaceNormal normal
  where
    p1 = white .^ (1.0 - a)
    p2 = lightBlue .^ a
    a = 0.5 * (toY (normalize direction) + 1)
    hitRecord = hit world r (I.Interval 0 (1 / 0))
    normal = hitNormal <$> hitRecord
    renderSurfaceNormal n = colorFromV3 (V3 (toX n + 1) (toY n + 1) (toZ n + 1) .^ 0.5)

-- colors for background
white :: Color
white = color 1 1 1

lightBlue :: Color
lightBlue = color 0.5 0.7 1.0

-- Returns a vector to a random point in the [-.5,-.5]-[+.5,+.5] unit square on viewport
-- per documentation, the range of random of Doubles lies in [0, 1)
getSampleSquare :: Rand StdGen V3
getSampleSquare = (.^ 0.5) <$> liftM3 V3 getRandom getRandom (pure 0)