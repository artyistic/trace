module Graphics.Ray where

import Graphics.Vec3
import Graphics.Point

data Ray = Ray !Point !V3

at :: Ray -> Double -> Point
at (Ray a r) t = evalPoint a (<+> r .^ t)


