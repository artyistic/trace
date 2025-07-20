module Hittable
  ( HitRecord,
    hitP,
    hitNormal,
    hitT,
    hitFrontFacing,
    hitMaterial,
    generateHitRecord,
    Hittable,
    hit,
    Material,
    matScatter,
    mkLambertian,
    mkMetal
  )
where

import Graphics.Point
import Graphics.Ray
import Graphics.Vec3
import Interval (Interval)
import Graphics
import Control.Monad.Random

data HitRecord = HitRecord
  { hitP :: Point,
    hitNormal :: V3,
    hitT :: Double,
    hitFrontFacing :: Bool,
    hitMaterial :: Material
  }

data Material = Material
  {
  {-
    scatter takes:
    a : parameterized type
    ray that hits
    hitrecord
    returns
    maybe of tuple of
      color known as attenuation
      scattered ray
  -}
  matScatter :: Material -> Ray -> HitRecord -> Rand StdGen (Maybe (Color, Ray)),
  matColor :: Color
  }

mkLambertian :: Color -> Material
mkLambertian c = Material {
  matColor = c,
  matScatter = \(Material _ thisColor) r hR -> do
    let normal = hitNormal hR
        hitPt = hitP hR
    d <- (\a -> if nearZero a then normal else a ) <$> getRandomUnitBallVec
    return $ pure (thisColor, Ray hitPt (d <+> normal))
}

mkMetal :: Color -> Material
mkMetal c = Material {
  matColor = c,
  matScatter = \(Material _ thisColor) r@(Ray _ inDirection) hR -> do
    let normal = hitNormal hR
        hitPt = hitP hR
        reflectedRay = reflect inDirection normal
    return $ pure (thisColor, Ray hitPt reflectedRay)
}
generateHitRecord :: Ray -> Point -> Double -> V3 -> Material -> HitRecord
generateHitRecord (Ray _ direction) p t outwardNormal =
  HitRecord p normal t frontFacing
  where
    frontFacing = (direction .* outwardNormal) < 0
    normal = if frontFacing then outwardNormal else invert outwardNormal

class Hittable a where
  {-
    hit takes
      a: parametrized type
      ray that might hit the objects
      tMax and tMin for an interval that matters
    returns
      Just HitRecord if the ray did hit object
      Nothing if no hit
  -}
  hit :: a -> Ray -> Interval -> Maybe HitRecord
