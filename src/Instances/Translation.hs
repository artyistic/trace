module Instances.Translation where
import Hittable ( Hittable(..) )
import Graphics.Vec3
import Graphics.Ray
import Data.Bifunctor (Bifunctor(first))
import HitRecord
import qualified AABB

translate :: Hittable -> V3 -> Hittable
translate primitive v = Hittable
  { 
    -- all this is doing is moving ray by offset, then using the hit
    -- function by the primitive and correcting the intersection point by offset
    hit = \(Ray orig dir t) i ->
      first (\a -> a { p = a.p <+> v })
        <$> primitive.hit (Ray (orig <-> v) dir t) i
  , bbox = AABB.offset primitive.bbox v
  }