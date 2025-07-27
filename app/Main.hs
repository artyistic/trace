module Main where

import Camera
import Scenes
import Graphics
import Render
import Control.Monad.Random


dielectricCamera :: Camera
dielectricCamera = camera
      aspectRatio
      imageWidth
      samplesPerPixel
      vfov
      lookFrom
      lookAt
      vup
      defocusAngle
      focusDistance
  where
    aspectRatio = 16.0 / 9.0

    vfov = 20

    lookFrom = fromCoord (-2) 2 1
    lookAt = fromCoord 0 0 (-1)

    vup = V3 0 1 0

    defocusAngle = 10.0
    focusDistance = 3.4

    imageWidth = 400
    samplesPerPixel = 100

bigWorldCamera :: Camera
bigWorldCamera = camera
      aspectRatio
      imageWidth
      samplesPerPixel
      vfov
      lookFrom
      lookAt
      vup
      defocusAngle
      focusDistance
  where
    aspectRatio = 16.0 / 9.0

    vfov = 20

    lookFrom = fromCoord 13 2 3
    lookAt = fromCoord 0 0 0

    vup = V3 0 1 0

    defocusAngle = 0.6
    focusDistance = 10.0

    imageWidth = 400
    samplesPerPixel = 100


main :: IO ()
main = do
  gen <- getStdGen
  let
    world = evalRand bigWorld gen
    cam = bigWorldCamera
    -- world = dielectricTestWorld
    -- cam = dielectricCamera

  render "./output/test.ppm" world cam gen 10

