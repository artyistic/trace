module Graphics.Ray where

import Graphics.Vec3

data Ray = Ray {
  orig :: {-# UNPACK #-} !V3,
  dir :: {-# UNPACK #-} !V3,
  time :: {-# UNPACK #-} !Double
}

at :: Ray -> Double -> V3
at (Ray a r _) t = a <+> r .^ t