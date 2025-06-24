module Hittable
  ( HitRecord,
    hitP,
    hitNormal,
    hitT,
    hitFrontFacing,
    generateHitRecord,
    Hittable,
    hit,
  )
where

import Graphics.Point
import Graphics.Ray
import Graphics.Vec3

data HitRecord = HitRecord
  { hitP :: Point,
    hitNormal :: V3,
    hitT :: Double,
    hitFrontFacing :: Bool
  }

generateHitRecord :: Point -> Double -> Ray -> V3 -> HitRecord
generateHitRecord p t (Ray _ direction) outwardNormal =
  HitRecord p normal t frontFacing
  where
    frontFacing = (direction .* outwardNormal) < 0
    normal = if frontFacing then outwardNormal else invert outwardNormal

class Hittable a where
  {-
    hit takes
      a: parametrized type
      ray that might hit the objects
      tMax and tMin for an interval that matters
    returns
      Just HitRecord if the ray did hit object
      Nothing if no hit
  -}
  hit :: a -> Ray -> Double -> Double -> Maybe HitRecord
