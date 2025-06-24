module Shapes.Sphere where

import Graphics.Point
import Graphics.Ray
import Graphics.Vec3
import Hittable
import Control.Monad (guard)
import Control.Applicative ((<|>))

-- A sphere is defined by its center point and a radius
-- rn no color attribute, later maybe we could have a single color
-- or even a color map, giving a color for any x, y points on the sphere
data Sphere = Sphere Point Double

instance Hittable Sphere where
  hit (Sphere center radius) r@(Ray ptOrigin ptdirection) tMin tMax = do
    guard (discriminant >= 0)
    t <- tryRoot root1 <|> tryRoot root2
    let p = at r t in Just $ generateHitRecord p t r (toV3 $ (p <-> center) .^ (1 / radius))
    where
      oc = center <-> ptOrigin -- from origin to center of sphere
      a = lengthSquared ptdirection
      h = toV3 oc .* ptdirection
      c = lengthSquared oc - radius * radius
      discriminant = h*h - a*c
      sqrtDiscriminant = sqrt discriminant
      root1 = (h - sqrtDiscriminant) / a
      root2 = (h + sqrtDiscriminant) / a
      tryRoot r = if tMin < r && r < tMax then Just r else Nothing
