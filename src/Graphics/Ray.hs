module Graphics.Ray where

import Graphics.Vec3

data Ray = Ray {
  orig :: !V3,
  dir :: !V3,
  time :: !Double
}

at :: Ray -> Double -> V3
at (Ray a r _) t = a <+> r .^ t