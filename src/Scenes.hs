module Scenes where

import Camera
import Control.Monad.Identity
import Control.Monad.Morph (generalize, hoist)
import Control.Monad.Random
import Data.Maybe (catMaybes)
import Graphics
import Hittable
import Material
import Random
import Shapes.Quad
import Shapes.Sphere
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Texture (checkerTex, checkerTexFromColor, imageTexture)
import Volumes.ConstantMedium (constantMedium)

data Scene = Scene
  { name :: String,
    description :: String,
    world :: RandT StdGen IO [Hittable],
    camera :: CameraConfig
  }

scenes :: [Scene]
scenes =
  [ Scene
      "bouncing-spheres"
      "Random spheres with motion blur and depth of field"
      bigWorld
      defaultCameraConfig
        { aspectRatio = 16.0 / 9.0,
          imageWidth = 400,
          samplesPerPixel = 100,
          vfov = 20,
          lookFrom = V3 13 2 3,
          lookAt = V3 0 0 0,
          vup = V3 0 1 0,
          defocusAngle = 0.6,
          focusDistance = 10.0
        },
    Scene
      "dielectric"
      "Glass bubble and metal sphere test"
      (pure dielectricTestWorld)
      defaultCameraConfig
        { aspectRatio = 16.0 / 9.0,
          imageWidth = 400,
          samplesPerPixel = 100,
          vfov = 20,
          lookFrom = V3 (-2) 2 1,
          lookAt = V3 0 0 (-1),
          vup = V3 0 1 0,
          defocusAngle = 10.0,
          focusDistance = 3.4
        },
    Scene
      "vfov-test"
      "Wide angle FOV test with two colored spheres"
      (pure vfovTestWorld)
      defaultCameraConfig
        { aspectRatio = 16.0 / 9.0,
          imageWidth = 400,
          samplesPerPixel = 100,
          vfov = 90,
          lookFrom = V3 0 0 0,
          lookAt = V3 0 0 (-1),
          vup = V3 0 1 0,
          defocusAngle = 0,
          focusDistance = 10.0
        },
    Scene
      "quads-test"
      "five quads forming a boxish figure"
      (pure quadsWorld)
      defaultCameraConfig
        { aspectRatio = 1.0,
          imageWidth = 400,
          samplesPerPixel = 100,
          vfov = 80,
          lookFrom = V3 0 0 9,
          lookAt = V3 0 0 0,
          vup = V3 0 1 0,
          defocusAngle = 0
        },
    Scene
      "simple-light test"
      "quad light on the right of a ball"
      (pure simpleLightsWorld)
      defaultCameraConfig
        { aspectRatio = 16.0 / 9.0,
          imageWidth = 400,
          samplesPerPixel = 100,
          vfov = 20,
          lookFrom = V3 26 3 6,
          lookAt = V3 0 2 0,
          vup = V3 0 1 0,
          defocusAngle = 0,
          background = const $ color 0 0 0
        },
    Scene
      "cornell-box"
      "cornell box without objects inside box"
      (pure cornellBoxWorld)
      defaultCameraConfig
        { aspectRatio = 1.0,
          imageWidth = 600,
          samplesPerPixel = 200,
          vfov = 40,
          lookFrom = V3 278 278 (-800),
          lookAt = V3 278 278 0,
          vup = V3 0 1 0,
          defocusAngle = 0,
          background = const $ color 0 0 0
        }
  ]

selectScene :: IO (Maybe Scene)
selectScene = do
  putStrLn "Available scenes:"
  mapM_
    ( \(i, scene) ->
        putStrLn $
          "  "
            ++ show i
            ++ ": "
            ++ scene.name
            ++ " — "
            ++ scene.description
    )
    (zip [(0 :: Int) ..] scenes)
  putStrLn "  q: quit"
  putStr "Select scene: "
  hFlush stdout
  input <- getLine
  case input of
    "q" -> return Nothing
    _ -> case readMaybe input of
      Just i | i >= 0 && i < length scenes -> return (Just (scenes !! i))
      _ -> putStrLn "Invalid selection, try again." >> selectScene

-- scene definitions

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
      left = stationarySphere (V3 (-1.0) 0.0 (-1.0)) 0.5 materialLeft
      leftBubble = stationarySphere (V3 (-1.0) 0.0 (-1.0)) 0.4 materialBubble
   in [ground, center, left, leftBubble, right]

vfovTestWorld :: [Hittable]
vfovTestWorld =
  let r = cos $ pi / 4
      matLeft = mkLambertian $ color 1 0 0
      matRight = mkLambertian $ color 0 0 1
      left = stationarySphere (V3 (-r) 0 (-1)) r matLeft
      right = stationarySphere (V3 r 0 (-1)) r matRight
   in [left, right]

bigWorld :: RandT StdGen IO [Hittable]
bigWorld = do
  earthTex <- liftIO $ imageTexture "./texImages/earthmap.jpg"
  let checker = checkerTexFromColor 0.32 (color 0.2 0.3 0.1) (color 0.9 0.9 0.9)
      matGround = mkLambertianWithTex checker
      matbS1 = mkDielectric 1.5
      matbS2 = mkLambertian $ color 0.4 0.2 0.1
      -- matbS3     = mkMetal (color 0.7 0.6 0.5) 0.0
      matbS3 = mkLambertianWithTex earthTex
      ground = stationarySphere (V3 0 (-1000) 0) 1000 matGround
      bigSphere1 = constantMedium (stationarySphere (V3 0 1 0) 1.0 matbS1) 1 (color 1 0 0) -- red sphere fog
      bigSphere2 = stationarySphere (V3 (-4) 1 0) 1.0 matbS2
      bigSphere3 = stationarySphere (V3 4 1 0) 1.0 matbS3
  smallSpheres <- mapRandT (return . runIdentity) randomSpheres
  pure $ [ground, bigSphere1, bigSphere2, bigSphere3] ++ smallSpheres

quadsWorld :: [Hittable]
quadsWorld =
  let red = mkLambertian $ color 1.0 0.2 0.2
      green = mkLambertian $ color 0.2 1.0 0.2
      blue = mkLambertian $ color 0.2 0.2 1.0
      orange = mkLambertian $ color 1.0 0.5 0.0
      teal = mkLambertian $ color 0.2 0.8 0.8

      left = quad (V3 (-3) (-2) 5) (V3 0 0 (-4)) (V3 0 4 0) red
      back = quad (V3 (-2) (-2) 0) (V3 4 0 0) (V3 0 4 0) green
      right = quad (V3 3 (-2) 1) (V3 0 0 4) (V3 0 4 0) blue
      upper = quad (V3 (-2) 3 1) (V3 4 0 0) (V3 0 0 4) orange
      lower = quad (V3 (-2) (-3) 5) (V3 4 0 0) (V3 0 0 (-4)) teal
   in [left, back, right, upper, lower]

simpleLightsWorld :: [Hittable]
simpleLightsWorld =
  let diffLight = mkDiffuseLight $ color 4 4 4
      white = mkLambertian $ color 1 1 1
      ground = stationarySphere (V3 0 (-1000) 0) 1000 white
      ball = stationarySphere (V3 0 2 0) 2 white
      light = quad (V3 3 1 (-2)) (V3 2 0 0) (V3 0 2 0) diffLight
   in [light, ball, ground]

cornellBoxWorld :: [Hittable]
cornellBoxWorld =
  let red = mkLambertian $ color 0.65 0.05 0.05
      white = mkLambertian $ color 0.73 0.73 0.73
      green = mkLambertian $ color 0.12 0.45 0.15
      diffLight = mkDiffuseLight $ color 15 15 15

      left = quad (V3 555 0 0) (V3 0 555 0) (V3 0 0 555) green
      right = quad (V3 0 0 0) (V3 0 555 0) (V3 0 0 555) red
      light = quad (V3 343 554 332) (V3 (-130) 0 0) (V3 0 0 (-105)) diffLight
      a = quad (V3 0 0 0) (V3 555 0 0) (V3 0 0 555) white
      b = quad (V3 555 555 555) (V3 (-555) 0 0) (V3 0 0 (-555)) white
      c = quad (V3 0 0 555) (V3 555 0 0) (V3 0 555 0) white
   in [left, right, light, a, b, c]

randomSpheres :: Rand StdGen [Hittable]
randomSpheres =
  catMaybes <$> sequenceA [genSpheres a b | a <- [-11 .. 11], b <- [-11 .. 11]]

genSpheres :: Int -> Int -> Rand StdGen (Maybe Hittable)
genSpheres a b = do
  offsetX <- getRandomDouble
  offsetZ <- getRandomDouble
  chooseMat <- getRandomDouble
  let center = V3 (fromIntegral a + 0.9 * offsetX) 0.2 (fromIntegral b + 0.9 * offsetZ)
  if distance center (V3 4 0.2 0) <= 0.9
    then pure Nothing
    else Just <$> mkSphere chooseMat center
  where
    mkSphere :: Double -> V3 -> Rand StdGen Hittable
    mkSphere chooseMat center
      | chooseMat < 0.8 = do
          c <- colorFromV3 <$> getRandomVec 0 1
          center2Dir <- flip (V3 0) 0 <$> getRandomR (0, 0.5)
          return $ movingSphere center (center <+> center2Dir) 0.2 (mkLambertian c)
      | chooseMat < 0.95 = do
          c <- colorFromV3 <$> getRandomVec 0.5 1
          f <- getRandomR (0, 0.5) :: Rand StdGen Double
          return $ stationarySphere center 0.2 (mkMetal c f)
      | otherwise =
          return $ stationarySphere center 0.2 (mkDielectric 1.5)