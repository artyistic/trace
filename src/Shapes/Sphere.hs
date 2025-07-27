module Shapes.Sphere (Sphere(..), mkSphereHittable, movingSphere, stationarySphere) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Graphics.Point
import Graphics.Ray
import Graphics.Vec3
import Hittable
import qualified Interval as I

-- A sphere is defined by its center point and a radius
-- rn no color attribute, later maybe we could have a single color
-- or even a color map, giving a color for any x, y points on the sphere
data Sphere = Sphere !Ray !Double !Material

movingSphere :: V3 -> V3 -> Double -> Material -> Sphere
movingSphere centerFrom centerTo =
  Sphere (Ray (fromV3 centerFrom) (centerTo <-> centerFrom) 0)

stationarySphere :: Point -> Double -> Material -> Sphere
stationarySphere staticCenter =
  Sphere (Ray  staticCenter (V3 0 0 0) 0)

{-# INLINE mkSphereHittable #-}
mkSphereHittable :: Sphere -> Hittable
mkSphereHittable (Sphere center radius mat) = Hittable {
  hit = \r@(Ray inOrigin inDirection inTime) tInterval ->
    do
      let
        currCenter = at center inTime
        oc = currCenter <-> inOrigin -- from origin to center of sphere
        a = lengthSquared inDirection
        h = toV3 oc .* inDirection
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
          outwardNormal = toV3 ((p <-> currCenter) .^ (1 / radius))
      return (generateHitRecord r p t outwardNormal, mat)
}
