module Main where

import Camera
import Control.Monad.Random
import Graphics.Pixel
import Graphics.Point
import Graphics.Vec3
import Hittable
import Hittables
import Shapes.Sphere
import Data.Maybe (catMaybes)
import Render


aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth :: Int
imageWidth = 400

samplesPerPixel :: Int
samplesPerPixel = 100

vfov :: Double
vfov = 20

lookFrom :: Point
lookFrom = fromCoord (-2) 2 1

lookAt :: Point
lookAt = fromCoord 0 0 (-1)

vup :: V3
vup = V3 0 1 0

defocusAngle :: Double
defocusAngle = 10.0

focusDistance :: Double
focusDistance = 3.4

maxDepth :: Int
maxDepth = 50

-- aspectRatio :: Double
-- aspectRatio = 16.0 / 9.0

-- imageWidth :: Int
-- imageWidth = 400

-- samplesPerPixel :: Int
-- samplesPerPixel = 100

-- vfov :: Double
-- vfov = 20

-- lookFrom :: Point
-- lookFrom = fromCoord 13 2 3

-- lookAt :: Point
-- lookAt = fromCoord 0 0 0

-- vup :: V3
-- vup = V3 0 1 0

-- defocusAngle :: Double
-- defocusAngle = 0.6

-- focusDistance :: Double
-- focusDistance = 10.0

-- maxDepth :: Int
-- maxDepth = 10

main :: IO ()
main = do
  let
    -- gen = mkStdGen 564128
    gen = mkStdGen 489713215
    -- world = evalRand bigWorld gen
    world = dielectricTestWorld
    cam =
      camera
      aspectRatio
      imageWidth
      samplesPerPixel
      vfov
      lookFrom
      lookAt
      vup
      defocusAngle
      focusDistance
  render "./output/test.ppm" world cam gen maxDepth

dielectricTestWorld :: HittableList
dielectricTestWorld =
  let materialGround = mkLambertian (color 0.8 0.8 0.0)
      materialCenter = mkLambertian (color 0.1 0.2 0.5)
      materialLeft = mkDielectric 1.5
      materialRight = mkMetal (color 0.8 0.6 0.2) 1.5
      materialBubble = mkDielectric (1.00 / 1.50)

      ground = Sphere (fromCoord 0.0 (-100.5) (-1.0)) 100.0 materialGround
      center = Sphere (fromCoord 0.0 0.0 (-1.2)) 0.5 materialCenter
      right = Sphere (fromCoord 1.0 0.0 (-1.0)) 0.5 materialRight

      -- left and the bubble represents a hollow glass sphere
      left = Sphere (fromCoord (-1.0) 0.0 (-1.0)) 0.5 materialLeft
      leftBubble = Sphere (fromCoord (-1.0) 0.0 (-1.0)) 0.4 materialBubble
   in makeHittableList [ground, center, left, leftBubble, right]

vfovTestWorld :: HittableList
vfovTestWorld =
  let r = cos $ pi / 4
      matLeft = mkLambertian $ color 1 0 0
      matRight = mkLambertian $ color 0 0 1

      left = Sphere (fromCoord (-r) 0 (-1)) r matLeft
      right = Sphere (fromCoord r 0 (-1)) r matRight
   in makeHittableList [left, right]

bigWorld :: Rand StdGen HittableList
bigWorld = do
  let
    matGround = mkLambertian $ color 0.5 0.5 0.5
    matbS1 = mkDielectric 1.5
    matbS2 = mkLambertian $ color 0.4 0.2 0.1
    matbS3 = mkMetal (color 0.7 0.6 0.5) 0.0

    ground = Sphere (fromCoord 0 (-1000) 0) 1000 matGround
    bigSphere1 = Sphere (fromCoord 0 1 0) 1.0 matbS1
    bigSphere2 = Sphere (fromCoord (-4) 1 0) 1.0 matbS2
    bigSphere3 = Sphere (fromCoord 4 1 0) 1.0 matbS3
  smallSpheres <- randomSpheres
  pure $ makeHittableList ([ground, bigSphere1, bigSphere2, bigSphere3] ++ smallSpheres)

randomSpheres :: Rand StdGen [Sphere]
randomSpheres =
  let rSphere = [genSpheres a b | a <- [-11..11], b <- [-11..11]]
      t = sequenceA rSphere
  in catMaybes <$> t

genSpheres :: Int -> Int -> Rand StdGen (Maybe Sphere)
genSpheres a b = do
  offsetX <- getRandomR (0, 1) :: Rand StdGen Double
  offsetZ <- getRandomR (0, 1) :: Rand StdGen Double
  chooseMat <- getRandomR (0, 1) :: Rand StdGen Double

  let center = V3 (fromIntegral a + 0.9 * offsetX) 0.2 (fromIntegral b + 0.9 * offsetZ)
  if distance center (V3 4 0.2 0) <= 0.9
    then pure Nothing
    else Just <$> t chooseMat (fromV3 center)
  where t :: Double -> Point -> Rand StdGen Sphere
        t chooseMat center
          | chooseMat < 0.8 = do
            c <- colorFromV3 <$> getRandomVec 0 1
            return $ Sphere center 0.2 (mkLambertian c)
          | chooseMat < 0.95 = do
            c <- colorFromV3 <$> getRandomVec 0.5 1
            f <- getRandomR (0, 0.5) :: Rand StdGen Double
            return $ Sphere center 0.2 (mkMetal c f)
          | otherwise = do
            return $ Sphere center 0.2 (mkDielectric 1.5)


