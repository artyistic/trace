module Scenes where
import Control.Monad.Random
import Shapes.Sphere
import Random
import Data.Maybe
import Graphics
import Hittable

dielectricTestWorld :: [Hittable]
dielectricTestWorld =
  let materialGround = mkLambertian (color 0.8 0.8 0.0)
      materialCenter = mkLambertian (color 0.1 0.2 0.5)
      materialLeft = mkDielectric 1.5
      materialRight = mkMetal (color 0.8 0.6 0.2) 1.5
      materialBubble = mkDielectric (1.00 / 1.50)

      ground = stationarySphere (V3 0.0 (-100.5) (-1.0)) 100.0 materialGround
      center = stationarySphere (V3 0.0 0.0 (-1.2)) 0.5 materialCenter
      right = stationarySphere (V3 1.0 0.0 (-1.0)) 0.5 materialRight

      -- left and the bubble represents a hollow glass sphere
      left = stationarySphere (V3 (-1.0) 0.0 (-1.0)) 0.5 materialLeft
      leftBubble = stationarySphere (V3 (-1.0) 0.0 (-1.0)) 0.4 materialBubble
   in map extractHittable [ground, center, left, leftBubble, right]

vfovTestWorld :: [Hittable]
vfovTestWorld =
  let r = cos $ pi / 4
      matLeft = mkLambertian $ color 1 0 0
      matRight = mkLambertian $ color 0 0 1

      left = stationarySphere (V3 (-r) 0 (-1)) r matLeft
      right = stationarySphere (V3 r 0 (-1)) r matRight
   in map extractHittable [left, right]

bigWorld :: Rand StdGen [Hittable]
bigWorld = do
  let
    matGround = mkLambertian $ color 0.5 0.5 0.5
    matbS1 = mkDielectric 1.5
    matbS2 = mkLambertian $ color 0.4 0.2 0.1
    matbS3 = mkMetal (color 0.7 0.6 0.5) 0.0

    ground = stationarySphere (V3 0 (-1000) 0) 1000 matGround
    bigSphere1 = stationarySphere (V3 0 1 0) 1.0 matbS1
    bigSphere2 = stationarySphere (V3 (-4) 1 0) 1.0 matbS2
    bigSphere3 = stationarySphere (V3 4 1 0) 1.0 matbS3
  smallSpheres <- randomSpheres
  pure $ map extractHittable ([ground, bigSphere1, bigSphere2, bigSphere3] ++ smallSpheres)

randomSpheres :: Rand StdGen [Sphere]
randomSpheres =
  let rSphere = [genSpheres a b | a <- [-11..11], b <- [-11..11]]
      t = sequenceA rSphere
  in catMaybes <$> t

genSpheres :: Int -> Int -> Rand StdGen (Maybe Sphere)
genSpheres a b = do
  offsetX <- getRandomDouble
  offsetZ <- getRandomDouble
  chooseMat <- getRandomDouble

  let center = V3 (fromIntegral a + 0.9 * offsetX) 0.2 (fromIntegral b + 0.9 * offsetZ)
  if distance center (V3 4 0.2 0) <= 0.9
    then pure Nothing
    else Just <$> t chooseMat center
  where t :: Double -> V3 -> Rand StdGen Sphere
        t chooseMat center
          | chooseMat < 0.8 = do
            c <- colorFromV3 <$> getRandomVec 0 1
            center2Dir <- flip (V3 0) 0 <$> getRandomR (0, 0.5)
            return $ movingSphere center (center <+> center2Dir) 0.2 (mkLambertian c)
          | chooseMat < 0.95 = do
            c <- colorFromV3 <$> getRandomVec 0.5 1
            f <- getRandomR (0, 0.5) :: Rand StdGen Double
            return $ stationarySphere center 0.2 (mkMetal c f)
          | otherwise = do
            return $ stationarySphere center 0.2 (mkDielectric 1.5)


