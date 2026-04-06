{-# LANGUAGE BangPatterns #-}
module Scenes
  ( Scene (..),
    scenes,
    selectScene
  )
where

import Control.Monad.Morph (hoist, generalize)
import Camera
import Control.Monad.Random
import Data.Maybe (catMaybes)
import Graphics
import Hittable
import Random
import Shapes.Sphere
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Material
import Volumes.ConstantMedium (constantMedium)
import Texture (checkerTex, checkerTexFromColor, imageTexture)
import Control.Monad.Identity

data Scene = Scene
  { sceneName        :: String,
    sceneDescription :: String,
    sceneWorld       :: RandT StdGen IO [Hittable],
    sceneCamera      :: CameraConfig
  }

scenes :: [Scene]
scenes =
  [ Scene
      "bouncing-spheres"
      "Random spheres with motion blur and depth of field"
      bigWorld
      defaultCameraConfig
        { cfgAspectRatio     = 16.0 / 9.0,
          cfgImageWidth      = 400,
          cfgSamplesPerPixel = 100,
          cfgVfov            = 20,
          cfgLookFrom        = V3 13 2 3,
          cfgLookAt          = V3 0 0 0,
          cfgVup             = V3 0 1 0,
          cfgDefocusAngle    = 0.6,
          cfgFocusDistance   = 10.0
        },
    Scene
      "dielectric"
      "Glass bubble and metal sphere test"
      (pure dielectricTestWorld)
      defaultCameraConfig
        { cfgAspectRatio     = 16.0 / 9.0,
          cfgImageWidth      = 400,
          cfgSamplesPerPixel = 100,
          cfgVfov            = 20,
          cfgLookFrom        = V3 (-2) 2 1,
          cfgLookAt          = V3 0 0 (-1),
          cfgVup             = V3 0 1 0,
          cfgDefocusAngle    = 10.0,
          cfgFocusDistance   = 3.4
        },
    Scene
      "vfov-test"
      "Wide angle FOV test with two colored spheres"
      (pure vfovTestWorld)
      defaultCameraConfig
        { cfgAspectRatio     = 16.0 / 9.0,
          cfgImageWidth      = 400,
          cfgSamplesPerPixel = 100,
          cfgVfov            = 90,
          cfgLookFrom        = V3 0 0 0,
          cfgLookAt          = V3 0 0 (-1),
          cfgVup             = V3 0 1 0,
          cfgDefocusAngle    = 0,
          cfgFocusDistance   = 10.0
        }
  ]

selectScene :: IO (Maybe Scene)
selectScene = do
  putStrLn "Available scenes:"
  mapM_
    (\(i, s) ->
        putStrLn $
          "  " ++ show i ++ ": " ++ sceneName s
            ++ " — "
            ++ sceneDescription s
    )
    (zip [(0 :: Int) ..] scenes)
  putStrLn "  q: quit"
  putStr "Select scene: "
  hFlush stdout
  input <- getLine
  case input of
    "q" -> return Nothing
    _   -> case readMaybe input of
      Just i | i >= 0 && i < length scenes -> return (Just (scenes !! i))
      _ -> putStrLn "Invalid selection, try again." >> selectScene

-- scene definitions

dielectricTestWorld :: [Hittable]
dielectricTestWorld =
  let materialGround = mkLambertian (color 0.8 0.8 0.0)
      materialCenter = mkLambertian (color 0.1 0.2 0.5)
      materialLeft   = mkDielectric 1.5
      materialRight  = mkMetal (color 0.8 0.6 0.2) 1.5
      materialBubble = mkDielectric (1.00 / 1.50)
      ground         = stationarySphere (V3 0.0 (-100.5) (-1.0)) 100.0 materialGround
      center         = stationarySphere (V3 0.0 0.0 (-1.2)) 0.5 materialCenter
      right          = stationarySphere (V3 1.0 0.0 (-1.0)) 0.5 materialRight
      left           = stationarySphere (V3 (-1.0) 0.0 (-1.0)) 0.5 materialLeft
      leftBubble     = stationarySphere (V3 (-1.0) 0.0 (-1.0)) 0.4 materialBubble
  in [ground, center, left, leftBubble, right]

vfovTestWorld :: [Hittable]
vfovTestWorld =
  let r        = cos $ pi / 4
      matLeft  = mkLambertian $ color 1 0 0
      matRight = mkLambertian $ color 0 0 1
      left     = stationarySphere (V3 (-r) 0 (-1)) r matLeft
      right    = stationarySphere (V3 r 0 (-1)) r matRight
  in [left, right]

bigWorld :: RandT StdGen IO [Hittable]
bigWorld = do
  earthTex <- liftIO $ imageTexture "./texImages/earthmap.jpg"
  let checker = checkerTexFromColor 0.32 (color 0.2 0.3  0.1) (color 0.9 0.9 0.9)
      matGround  = mkLambertianWithTex checker
      matbS1     = mkDielectric 1.5
      matbS2     = mkLambertian $ color 0.4 0.2 0.1
      -- matbS3     = mkMetal (color 0.7 0.6 0.5) 0.0
      matbS3 = mkLambertianWithTex earthTex
      ground     = stationarySphere (V3 0 (-1000) 0) 1000 matGround
      bigSphere1 = constantMedium (stationarySphere (V3 0 1 0) 1.0 matbS1) 1 (color 1 0 0) -- red sphere fog
      bigSphere2 = stationarySphere (V3 (-4) 1 0) 1.0 matbS2
      bigSphere3 = stationarySphere (V3 4 1 0) 1.0 matbS3
  smallSpheres <- mapRandT (return . runIdentity) randomSpheres
  pure $ [ground, bigSphere1, bigSphere2, bigSphere3] ++ smallSpheres

randomSpheres :: Rand StdGen [Hittable]
randomSpheres =
  catMaybes <$> sequenceA [genSpheres a b | a <- [-11 .. 11], b <- [-11 .. 11]]

genSpheres :: Int -> Int -> Rand StdGen (Maybe Hittable)
genSpheres a b = do
  offsetX   <- getRandomDouble
  offsetZ   <- getRandomDouble
  chooseMat <- getRandomDouble
  let center = V3 (fromIntegral a + 0.9 * offsetX) 0.2 (fromIntegral b + 0.9 * offsetZ)
  if distance center (V3 4 0.2 0) <= 0.9
    then pure Nothing
    else Just <$> mkSphere chooseMat center
  where
    mkSphere :: Double -> V3 -> Rand StdGen Hittable
    mkSphere chooseMat center
      | chooseMat < 0.8 = do
          c          <- colorFromV3 <$> getRandomVec 0 1
          center2Dir <- flip (V3 0) 0 <$> getRandomR (0, 0.5)
          return $ movingSphere center (center <+> center2Dir) 0.2 (mkLambertian c)
      | chooseMat < 0.95 = do
          c <- colorFromV3 <$> getRandomVec 0.5 1
          f <- getRandomR (0, 0.5) :: Rand StdGen Double
          return $ stationarySphere center 0.2 (mkMetal c f)
      | otherwise =
          return $ stationarySphere center 0.2 (mkDielectric 1.5)