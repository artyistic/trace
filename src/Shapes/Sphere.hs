module Shapes.Sphere (Sphere, movingSphere, stationarySphere, extractHittable) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Graphics.Ray
import Graphics.Vec3
import Hittable
import qualified Interval as I
import AABB (aabbFromPoints, aabbFromBoxes)

-- A sphere is defined by
--  a ray, which represents the spheres center point w/wo movement
--  a radius
--  a Material, which determines it's scattering properties
--  a Hittable data type, which contains the neccessary functions to be deemed hittable
data Sphere = Sphere !Ray !Double !Material !Hittable

newtype MovingSphere = MovingSphere Sphere
newtype StationarySphere = StationarySphere Sphere

extractHittable :: Sphere -> Hittable
extractHittable (Sphere _ _ _ h) = h

-- smart constructor for a moving sphere
movingSphere :: V3 -> V3 -> Double -> Material -> Sphere
movingSphere centerFrom centerTo radius mat =
  Sphere
    center
    radius
    mat
    Hittable
      { hit = sphereHit center radius mat,
        bounding_box =
          let rvec = V3 radius radius radius
              boxA = aabbFromPoints (at center 0 <-> rvec) (at center 0 <+> rvec)
              boxB = aabbFromPoints (at center 1 <-> rvec) (at center 1 <+> rvec)
          in aabbFromBoxes boxA boxB
      }
  where
    center = Ray centerFrom (centerTo <-> centerFrom) 0

-- smart constructor for a stationary sphere
stationarySphere :: V3 -> Double -> Material -> Sphere
stationarySphere staticCenter radius mat =
  Sphere
    center
    radius
    mat
    Hittable
      { 
        hit = sphereHit center radius mat,
        bounding_box = 
          let rvec = V3 radius radius radius
          in aabbFromPoints (staticCenter <-> rvec) (staticCenter <+> rvec)
      }
  where
    center = Ray staticCenter (V3 0 0 0) 0

-- a helper to generate the hit function for both stationary and moving spheres
-- takes in
--  Ray, the center
--  Double, the radius
--  Material
-- generates
-- (Ray -> I.Interval -> Maybe (HitRecord, Material)), the hit function
sphereHit :: Ray -> Double -> Material -> (Ray -> I.Interval -> Maybe (HitRecord, Material))
sphereHit center radius mat r@(Ray inOrigin inDirection inTime) tInterval = do
  let currCenter = at center inTime
      oc = currCenter <-> inOrigin -- from origin to center of sphere
      a = lengthSquared inDirection
      h = oc .* inDirection
      c = lengthSquared oc - radius * radius
      discriminant = h * h - a * c
      sqrtDiscriminant = sqrt discriminant
      root1 = (h - sqrtDiscriminant) / a
      root2 = (h + sqrtDiscriminant) / a
      checkRootBound r = guard (I.surrounds tInterval r) >> Just r
  -- no hit
  -- evaluates to nothing
  guard (discriminant >= 0)

  -- one or more roots => hit, check if root is within tMin and tMax
  -- alternative fails will evaluate to nothing
  t <- checkRootBound root1 <|> checkRootBound root2

  -- generate hit record
  let p = at r t
      outwardNormal = (p <-> currCenter) .^ (1 / radius)
  return (generateHitRecord r p t outwardNormal, mat)
