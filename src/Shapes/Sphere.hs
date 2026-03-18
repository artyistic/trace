module Shapes.Sphere (movingSphere, stationarySphere) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Graphics.Ray
import Graphics.Vec3
import Hittable
import qualified Interval as I
import AABB (aabbFromPoints, aabbFromBoxes)

-- | Smart constructor for a moving sphere.
-- The sphere center interpolates from centerFrom to centerTo over time [0,1].
movingSphere :: V3 -> V3 -> Double -> Material -> Hittable
movingSphere centerFrom centerTo radius mat =
  Hittable
    { hit         = sphereHit center radius mat,
      bounding_box =
        let rvec = V3 radius radius radius
            boxA = aabbFromPoints (at center 0 <-> rvec) (at center 0 <+> rvec)
            boxB = aabbFromPoints (at center 1 <-> rvec) (at center 1 <+> rvec)
        in aabbFromBoxes boxA boxB
    }
  where
    center = Ray centerFrom (centerTo <-> centerFrom) 0

-- | Smart constructor for a stationary sphere.
stationarySphere :: V3 -> Double -> Material -> Hittable
stationarySphere staticCenter radius mat =
  Hittable
    { hit         = sphereHit center radius mat,
      bounding_box =
        let rvec = V3 radius radius radius
        in aabbFromPoints (staticCenter <-> rvec) (staticCenter <+> rvec)
    }
  where
    center = Ray staticCenter (V3 0 0 0) 0

-- | Generate the hit function for a sphere given its center ray, radius, and material.
sphereHit :: Ray -> Double -> Material -> HitFun
sphereHit center radius mat r@(Ray inOrigin inDirection inTime) tInterval = do
  let currCenter     = at center inTime
      oc             = currCenter <-> inOrigin
      a              = lengthSquared inDirection
      h              = oc .* inDirection
      c              = lengthSquared oc - radius * radius
      discriminant   = h * h - a * c
      sqrtDisc       = sqrt discriminant
      root1          = (h - sqrtDisc) / a
      root2          = (h + sqrtDisc) / a
      checkRoot root = guard (I.surrounds tInterval root) >> Just root

  guard (discriminant >= 0)

  t <- checkRoot root1 <|> checkRoot root2

  let p             = at r t
      outwardNormal = (p <-> currCenter) .^ (1 / radius)
  return (generateHitRecord r p t outwardNormal, mat)