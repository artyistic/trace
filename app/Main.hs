module Main where

import Camera
import Hittables
import Shapes.Sphere
import Graphics.Point
import Control.Monad.Random

main :: IO ()
main = do
  let redSphere = Sphere (fromCoord 0 0 (-1)) 0.5
      groundSphere = Sphere (fromCoord 0 (-100.5) (-1)) 100
      world = makeHittableList [redSphere, groundSphere]
      cam = camera (16.0 / 9.0) 1.0 400 10
  render "./output/test.png" world cam (mkStdGen 1564651)

-- createImage :: IO ()

-- pixelRenderer :: (Integral a, Integral b) => a -> b -> P.PixelRGB8


-- all temp constants of the eye and viewport, ie the scene


-- | TEMPORARILY MOVED raycolor here due to cyclic dependency of ray and sphere
-- rayColor returns a rgb given a ray
-- hardcoding to consider a red sphere in the scene
-- later maybe pass a object list or sth?

-- temporarily we will define objects in the scene here
-- later move to a data type of list maybe containing objects?
