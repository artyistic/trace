module Volumes.ConstantMedium where

import Hittable
import Interval
import HitRecord
import Control.Monad (guard)
import Graphics
import Random (hashToFloat01)
import Material
import Control.Monad.Random

constantMedium :: Hittable -> Double -> Color -> Hittable
constantMedium boundary density c =
  Hittable {
    hit = volumeHit boundary density c,
    bbox = boundary.bbox
  }

volumeHit :: Hittable -> Double -> Color -> HitFun
volumeHit boundary density c r@Ray{..} (Interval tMin tMax) = do
  hr1        <- fst <$> boundary.hit r universe
  hr2        <- fst <$> boundary.hit r (Interval (hr1.t + 0.001) (1/0))
  let t1      = max tMin hr1.t
      t2      = min tMax hr2.t
  guard (t1 < t2)
  let t1'     = max t1 0
      xi          = hashToFloat01 orig dir time
  let hitDist = negInvDensity * log xi
  guard (hitDist <= (t2 - t1') * rayLength)
  let t       = t1' + hitDist / rayLength
  return
    (HitRecord (at r t) (V3 1 0 0) t True 0 0, mkIsotropic c)
    -- u v is arbitrary, as well as hitFrontFacing and hitNormal since the hit is INSIDE the volume
  where
    negInvDensity = -(1 / density)
    rayLength     = norm dir