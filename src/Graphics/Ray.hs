module Graphics.Ray where

import Graphics.Vec3

data Ray = Ray {
  rayOrigin :: !V3,
  rayDirection :: !V3,
  rayTime :: !Double
}

at :: Ray -> Double -> V3
at (Ray a r _) t = a <+> r .^ t