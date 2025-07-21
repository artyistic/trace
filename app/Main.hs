module Main where

import Camera
import Graphics.Pixel
import Hittables
import Hittable
import Shapes.Sphere
import Graphics.Point
import Control.Monad.Random

main :: IO ()
main = do
  -- let redSphere = Sphere (fromCoord 0 0 (-1)) 0.5 (mkLambertian grey)
  --     groundSphere = Sphere (fromCoord 0 (-100.5) (-1)) 100 (mkLambertian grey)
  let ground = Sphere (fromCoord 0.0 (-100.5) (-1.0)) 100.0 materialGround
      center = Sphere (fromCoord 0.0 0.0 (-1.2)) 0.5 materialCenter

      -- left and the bubble represents a hollow glass sphere
      left = Sphere (fromCoord (-1.0) 0.0 (-1.0)) 0.5 materialLeft
      leftBubble = Sphere (fromCoord (-1.0) 0.0 (-1.0)) 0.4 materialBubble

      right = Sphere (fromCoord 1.0 0.0 (-1.0)) 0.5 materialRight
      world = makeHittableList [ground, center, left, leftBubble, right]
      cam = camera (16.0 / 9.0) 1.0 400 100
  render "./output/test.ppm" world cam (mkStdGen 564128)

-- colors for background
white :: Color
white = color 1 1 1

lightBlue :: Color
lightBlue = color 0.5 0.7 1.0

grey :: Color
grey = color 0.5 0.5 0.5

pink :: Color
pink = color 1 0 0.906

materialGround :: Material
materialGround = mkLambertian (color 0.8 0.8 0.0)

materialCenter :: Material
materialCenter = mkLambertian (color 0.1 0.2 0.5)

materialLeft :: Material
materialLeft = mkDielectric 1.5

materialRight :: Material
materialRight = mkMetal (color 0.8 0.6 0.2) 1.5

materialBubble :: Material
materialBubble = mkDielectric (1.00 / 1.50)