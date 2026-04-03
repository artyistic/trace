module Hittable where

import Graphics.Ray
import Graphics.Vec3
import Interval (Interval)
import Graphics
import Control.Monad.Random
import Random
import AABB
import HitRecord
import Material
import Control.Monad.Trans.Maybe


-- -- | A hit function takes an incident ray and a valid interval [tMin, tMax]
-- -- and returns a hit record and material if the ray intersects the object,
-- -- or Nothing if there is no intersection.
-- type HitFun = Ray -> Interval -> Maybe (HitRecord, Material)

-- -- volumes -- genuinely needs both effects  
-- type RandHitFun = Ray -> Interval -> Rand StdGen (Maybe (HitRecord, Material))

type HitFun = Ray -> Interval -> Maybe (HitRecord, Material)

{- | 
type Hittable where
@hit@: hit funtion for generating a hit

@bounding_box@: bounding box to determine collision
-}
data Hittable = Hittable {
  hit :: HitFun,
  bounding_box :: AABB
}