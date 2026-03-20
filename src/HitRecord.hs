module HitRecord where
import Graphics
  
-- | Records the geometry of a ray-surface intersection.
-- Produced by a 'HitFun' and consumed by 'scatter'.
data HitRecord = HitRecord
  { hitP           :: !V3,     -- ^ Point of intersection in world space
    hitNormal      :: !V3,     -- ^ Surface normal at the intersection point,
                               --   always points against the incident ray
    hitT           :: !Double, -- ^ Parameter t along the ray where the hit occurred
    hitFrontFacing :: !Bool    -- ^ True if the ray hit the front face of the surface
  }

generateHitRecord :: Ray -> V3 -> Double -> V3 -> HitRecord
generateHitRecord (Ray _ direction _) p t outwardNormal =
  HitRecord p normal t frontFacing
  where
    frontFacing = (direction .* outwardNormal) < 0
    normal = if frontFacing then outwardNormal else invert outwardNormal