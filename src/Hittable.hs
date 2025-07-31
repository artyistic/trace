module Hittable
  ( HitRecord,
    hitP,
    hitNormal,
    hitT,
    hitFrontFacing,
    -- hitMaterial,
    generateHitRecord,
    Hittable(..),
    Material,
    scatter,
    mkLambertian,
    mkMetal,
    mkDielectric,
    HitFun
  )
where

import Graphics.Ray
import Graphics.Vec3
import Interval (Interval)
import Graphics
import Control.Monad.Random
import Random
import AABB

data HitRecord = HitRecord
  { hitP :: !V3,
    hitNormal :: !V3,
    hitT :: !Double,
    hitFrontFacing :: !Bool
  }

newtype Material = Material
  {
  {-
    scatter takes:
    a : parameterized type
    ray that hits
    hitrecord
    returns
    rand stdgen of maybe of tuple of
      color known as attenuation
      scattered ray
  -}
  scatter :: Ray -> HitRecord -> Rand StdGen (Maybe (Color, Ray))
  }

mkLambertian :: Color -> Material
mkLambertian c = Material {
  scatter = \(Ray _ _ inTime) hR -> do
    let normal = hitNormal hR
        hitPt = hitP hR
    d <- (\a -> if nearZero a then normal else a ) <$> getRandomUnitBallVec
    return $ pure (c, Ray hitPt (d <+> normal) inTime)
}

mkMetal :: Color -> Double -> Material
mkMetal c fuzz = Material {
  scatter = \r@(Ray _ inDirection inTime) hR -> do
    randomVec <- getRandomUnitBallVec
    let normal = hitNormal hR
        hitPt = hitP hR
        reflectedRay = reflect inDirection normal
        fuzzedReflected = normalize reflectedRay <+> randomVec .^ fuzz
        scattered@(Ray _ scatteredDir _adobeDctVersion) = Ray hitPt fuzzedReflected inTime

    if scatteredDir .* normal > 0 then return $ pure (c, scattered) else pure Nothing
}

mkDielectric :: Double -> Material
mkDielectric refractiveIndex = Material {
  scatter = \r@(Ray _ inDirection inTime) hR -> do
    let normal = hitNormal hR
        attenuation = color 1.0 1.0 1.0
        hitPt = hitP hR
        ri = if hitFrontFacing hR
                then 1.0 / refractiveIndex
                else refractiveIndex
        unitInDirection = normalize inDirection
        refractedRay = refract unitInDirection normal ri
        reflectedRay = reflect inDirection normal

        cosTheta = min (invert unitInDirection .* normal) 1.0 -- min 1.0 small angle floating pt errors
        sinTheta = sqrt (1 - cosTheta * cosTheta)
        cannotRefract = ri * sinTheta > 1.0
        schlickReflectance cosine r =
          let r0 = (1 -  r) / (1 + r) * (1 -  r) / (1 + r)
          in r0 + (1 - r0) * ((1 - cosine) ** 5)
    randomDouble <- getRandom :: Rand StdGen Double
    if cannotRefract || schlickReflectance cosTheta ri > randomDouble -- logic for total internal refraction
      then pure $ Just (attenuation, Ray hitPt reflectedRay inTime)
      else pure $ Just (attenuation, Ray hitPt refractedRay inTime)

}

generateHitRecord :: Ray -> V3 -> Double -> V3 -> HitRecord
generateHitRecord (Ray _ direction _) p t outwardNormal =
  HitRecord p normal t frontFacing
  where
    frontFacing = (direction .* outwardNormal) < 0
    normal = if frontFacing then outwardNormal else invert outwardNormal

-- class Hittable a where
--   {-
--     hit takes
--       a: parametrized type
--       ray that might hit the objects
--       tMax and tMin for an interval that matters
--     returns
--       Just HitRecord, and the corresponding Material if the ray did hit object
--       Nothing if no hit
--   -}
--   hit :: a -> Ray -> Interval -> Maybe HitRecord

type HitFun = (Ray -> Interval -> Maybe (HitRecord, Material))

data Hittable = Hittable {
  hit :: HitFun,
  bounding_box :: AABB
}