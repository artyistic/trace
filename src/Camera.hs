module Camera where

import Codec.Picture as P
import qualified Codec.Picture as P
import Control.Monad.Random
import Data.Function
import Graphics
import Hittable
import Hittables
import qualified Interval as I
import Shapes.Sphere
import System.Random
import Control.Monad.Loops
import Data.Bool (bool)
import Control.Monad.ST
import Codec.Picture.Types

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
     = do
        let pixels = [pixelRenderer x y | y <- [0 .. imageHeight - 1], x <- [0 .. imageWidth - 1]]
            t = evalRand (sequenceA pixels) gen
        writeFile fpath $ "P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255\n" ++
          foldr (\x y -> colorToRGBString x ++ "\n" ++ y) "" t
          where
            -- for each x y position on viewPort, randomly sample pixels to construct a smooth edge
            -- obviously this does 10x work for each every pixel, super slow

            -- replicateM makes a random array of position offsets,
            -- then we map rayColors, average it out and transform it into a PixelRgb8 type
            pixelRenderer :: Int -> Int -> Rand StdGen Color
            pixelRenderer x y = do
              samples <- sampleSquares
              let colors = traverse (\(V3 offsetX offsetY _) -> rayColor
                      (Ray cameraCenter (toV3 $ pixelCenter (fromIntegral x + offsetX) (fromIntegral y + offsetY) <-> cameraCenter))
                      world
                      50) samples
              averageColor <$> colors
            sampleSquares = replicateM samplesPerPixel getSampleSquare
            pixelCenter x' y' = evalPoint pixel00Loc (\p -> p <+> pixelDu .^ x' <+> pixelDv .^ y')

rayColor :: Ray -> HittableList -> Int -> Rand StdGen Color
rayColor r@(Ray _ direction) world depth =
  if depth <= 0 then pure $ color 0 0 0 else
  maybe (pure (p1 <+> p2)) diffuse hitRecord
  where
    p1 = white .^ (1.0 - a)
    p2 = lightBlue .^ a
    a = 0.5 * (toY (normalize direction) + 1)
    hitRecord = hit world r (I.Interval 0.001 (1 / 0))
    -- normal = hitNormal <$> hitRecord
    -- renderSurfaceNormal hr = let n = hitNormal hr in pure $ colorFromV3 (V3 (toX n + 1) (toY n + 1) (toZ n + 1) .^ 0.5)
    diffuse hR = do
                  let
                    normal = hitNormal hR
                    hitPt = hitP hR
                  d <- getRandomOnHemisphere normal
                  (.^ 0.5) <$> rayColor (Ray hitPt (d <+> normal)) world (depth - 1)
                  -- d + normal for lambertian sphere

-- colors for background
white :: Color
white = color 1 1 1

lightBlue :: Color
lightBlue = color 0.5 0.7 1.0

pink :: Color
pink = color 1 0 0.906

-- Returns a vector to a random point in the [-.5,-.5]-[+.5,+.5] unit square on viewport
-- per documentation, the range of random of Doubles lies in [0, 1)
getSampleSquare :: Rand StdGen V3
getSampleSquare = liftM3 V3 (getRandomR (-0.5, 0.5)) (getRandomR (-0.5, 0.5)) (pure 0)

-- returns a unit cube vector
getRandomVec :: Double -> Double -> Rand StdGen V3
getRandomVec min max = liftM3 V3 (getRandomR (min, max)) (getRandomR (min, max)) (getRandomR (min, max))

-- return a unit sphere vector, by rejection method
-- ie keep finding until the normalized vector is within sphere
getRandomUnitBallVec :: Rand StdGen V3
getRandomUnitBallVec = (\x -> x ./ sqrt (lengthSquared x)) <$> iterateUntil inUnitBall (getRandomVec (-1) 1)
  where inUnitBall = I.contains (I.Interval 1e-160 1) . lengthSquared
        -- ensure the vector has length at most one, and 1e-160 is a safe lower bound
        -- to prevent funny underflowing which leads to catastrophic divide by zero

getRandomOnHemisphere :: V3 -> Rand StdGen V3
getRandomOnHemisphere normal = fmap orient getRandomUnitBallVec
  where orient v = if (v .* normal) > 0 then v else invert v