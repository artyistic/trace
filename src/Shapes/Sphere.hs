module Shapes.Sphere where

import Graphics.Point
import Graphics.Ray
import Graphics.Vec3
import Hittable

-- A sphere is defined by its center point and a radius
-- rn no color attribute, later maybe we could have a single color
-- or even a color map, giving a color for any x, y points on the sphere
data Sphere = Sphere Point Double

instance Hittable Sphere where
  hit (Sphere center radius) (Ray ptOrigin ptdirection) =
    discriminant >= 0
    where
      oc = center <-> ptOrigin -- from origin to center of sphere
      a = ptdirection .* ptdirection
      b = -(2.0 * (toV3 oc .* ptdirection))
      c = (oc .* oc) - radius * radius
      discriminant = b * b - 4 * a * c