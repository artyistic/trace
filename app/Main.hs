module Main where

import Camera
import Scenes
import Graphics
import Render
import Control.Monad.Random

dielectricCamera :: Camera
dielectricCamera = makeCamera defaultCameraConfig
  { cfgAspectRatio     = 16.0 / 9.0,
    cfgVfov            = 20,
    cfgLookFrom        = V3 (-2) 2 1,
    cfgLookAt          = V3 0 0 (-1),
    cfgVup             = V3 0 1 0,
    cfgDefocusAngle    = 10.0,
    cfgFocusDistance   = 3.4,
    cfgImageWidth      = 400,
    cfgSamplesPerPixel = 100
  }

bigWorldCamera :: Camera
bigWorldCamera = makeCamera defaultCameraConfig
  { cfgAspectRatio     = 16.0 / 9.0,
    cfgVfov            = 20,
    cfgLookFrom        = V3 13 2 3,
    cfgLookAt          = V3 0 0 0,
    cfgVup             = V3 0 1 0,
    cfgDefocusAngle    = 0.6,
    cfgFocusDistance   = 10.0,
    cfgImageWidth      = 400,
    cfgSamplesPerPixel = 100
  }

main :: IO ()
main = do
  gen <- getStdGen
  let
    world = evalRand bigWorld gen
    cam   = bigWorldCamera
    -- world = evalRand dielectricTestWorld gen
    -- cam   = dielectricCamera

  render "./output/test50.ppm" world cam 50