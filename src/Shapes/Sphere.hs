module Shapes.Sphere (Sphere(..), mkSphereHittable) where

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
data Sphere = Sphere !Point !Double !Material 

{-# INLINE mkSphereHittable #-}
mkSphereHittable :: Sphere -> Hittable
mkSphereHittable (Sphere center radius mat) = Hittable {
  hit = \r@(Ray ptOrigin ptdirection) tInterval ->
    do
      let
        oc = center <-> ptOrigin -- from origin to center of sphere
        a = lengthSquared ptdirection
        h = toV3 oc .* ptdirection
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
          outwardNormal = toV3 ((p <-> center) .^ (1 / radius))
      return (generateHitRecord r p t outwardNormal, mat)
}
