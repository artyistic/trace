module Graphics.Ray where

import Graphics.Vec3
import Graphics.Point

data Ray = Ray {
  rayOrigin :: !Point,
  rayDirection :: !V3,
  rayTime :: !Double}

at :: Ray -> Double -> Point
at (Ray a r _) t = evalPoint a (<+> r .^ t)


