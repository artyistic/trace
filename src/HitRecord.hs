module HitRecord where
import Graphics

-- | Records the geometry of a ray-surface intersection.
-- Produced by a 'HitFun' and consumed by 'scatter'.
data HitRecord = HitRecord
  { p           :: !V3,     -- ^ Point of intersection in world space
    normal      :: !V3,     -- ^ Surface normal at the intersection point,
                               --   always points against the incident ray
    t           :: !Double, -- ^ Parameter t along the ray where the hit occurred
    frontFacing :: !Bool,    -- ^ True if the ray hit the front face of the surface
    u           :: !Double,
    v           :: !Double
  }

genHitRecord :: Ray -> V3 -> Double -> V3 -> Double -> Double -> HitRecord
genHitRecord (Ray _ direction _) p t outwardNormal = 
  HitRecord p normal t frontFacing
  where
    frontFacing = (direction .* outwardNormal) < 0
    normal = if frontFacing then outwardNormal else invert outwardNormal