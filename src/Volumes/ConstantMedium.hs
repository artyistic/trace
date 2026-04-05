module Volumes.ConstantMedium where

import Hittable
import Interval
import HitRecord
import Control.Monad (guard)
import Graphics
import Random (getRandomDouble, hashToFloat01)
import Material
import Control.Monad.Random

constantMedium :: Hittable -> Double -> Color -> Hittable
constantMedium boundary density c =
  Hittable {
    hit = volumeHit boundary density c,
    bbox = bbox boundary
  }

volumeHit :: Hittable -> Double -> Color -> HitFun
volumeHit boundary density c r@(Ray rO rD rT) (Interval tMin tMax) = do
  hr1        <- fst <$> hit boundary r universe
  hr2        <- fst <$> hit boundary r (Interval (hitT hr1 + 0.001) (1/0))
  let t1      = max tMin (hitT hr1)
      t2      = min tMax (hitT hr2)
  guard (t1 < t2)
  let t1'     = max t1 0
      xi          = hashToFloat01 rO rD rT 
  let hitDist = negInvDensity * log xi
  guard (hitDist <= (t2 - t1') * rayLength)
  let t       = t1' + hitDist / rayLength
  return 
    (HitRecord (at r t) (V3 1 0 0) t True 0 0, mkIsotropic c)
    -- u v is arbitrary, as well as hitFrontFacing and hitNormal since the hit is INSIDE the volume
  where
    negInvDensity = -(1 / density)
    rayLength     = norm (rayDirection r)