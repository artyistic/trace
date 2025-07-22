module Main where

import Camera
import Control.Monad.Random
import Graphics.Pixel
import Graphics.Point
import Graphics.Vec3
import Hittable
import Hittables
import Shapes.Sphere

main :: IO ()
main =
  let world = dielectricTestWorld
      cam =
        camera
          (16.0 / 9.0)
          400
          100
          20
          (fromCoord (-2) 2 1)
          (fromCoord 0 0 (-1))
          (V3 0 1 0)
          10.0
          3.4
   in render "./output/test.ppm" world cam (mkStdGen 564128) 50

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
