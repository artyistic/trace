{-# LANGUAGE RecordWildCards #-}

module Material where

import Control.Monad.Random
import Graphics
import HitRecord
import Random
import Texture

data Material = Material
  { {-
      scatter takes:
      ray that hits
      hitrecord
      returns
      rand stdgen of maybe of tuple of
        color known as attenuation
        scattered ray
    -}
    scatter :: Ray -> HitRecord -> Rand StdGen (Maybe (Color, Ray)),
    emit :: Double -> Double -> V3 -> Color
  }

-- | helper function, emit black means no emittance
emitBlack :: Double -> Double -> V3 -> Color
emitBlack _ _ _ = color 0 0 0

mkDiffuseLightWithTex :: Texture -> Material
mkDiffuseLightWithTex tex =
  Material {
    scatter = const $ const $ pure Nothing,
    emit = tex.value
  }

mkDiffuseLight :: Color -> Material
mkDiffuseLight c =
  Material {
    scatter = const $ const $ pure Nothing,
    emit = const $ const $ const c
  }

mkLambertian :: Color -> Material
mkLambertian c = mkLambertianWithTex $ solidTex c

mkLambertianWithTex :: Texture -> Material
mkLambertianWithTex tex =
  Material
    { scatter = \(Ray _ _ inTime) hR@HitRecord {..} -> do
        d <- (\a -> if nearZero a then normal else a) <$> getRandomUnitVec
        return $ Just (tex.value u v p, Ray p (d <+> normal) inTime),
      emit = tex.value
    }

mkMetal :: Color -> Double -> Material
mkMetal c fuzz =
  Material
    { scatter = \r@(Ray _ inDirection inTime) hR@HitRecord {..} -> do
        randomVec <- getRandomUnitVec
        let reflectedRay = reflect inDirection normal
            fuzzedReflected = normalize reflectedRay <+> randomVec .^ fuzz
            scattered@(Ray _ scatteredDir _adobeDctVersion) = Ray p fuzzedReflected inTime

        if scatteredDir .* normal > 0 then return $ pure (c, scattered) else pure Nothing,
      emit = emitBlack
    }

mkDielectric :: Double -> Material
mkDielectric refractiveIndex =
  Material
    { scatter = \r@(Ray _ inDirection inTime) hR@HitRecord {..} -> do
        let attenuation = color 1.0 1.0 1.0
            ri =
              if frontFacing
                then 1.0 / refractiveIndex
                else refractiveIndex
            unitInDirection = normalize inDirection
            refractedRay = refract unitInDirection normal ri
            reflectedRay = reflect inDirection normal

            cosTheta = min (invert unitInDirection .* normal) 1.0 -- min 1.0 small angle floating pt errors
            sinTheta = sqrt (1 - cosTheta * cosTheta)
            cannotRefract = ri * sinTheta > 1.0
            schlickReflectance cosine r =
              let r0 = (1 - r) / (1 + r) * (1 - r) / (1 + r)
               in r0 + (1 - r0) * ((1 - cosine) ** 5)
        randomDouble <- getRandom :: Rand StdGen Double
        if cannotRefract || schlickReflectance cosTheta ri > randomDouble -- logic for total internal refraction
          then pure $ Just (attenuation, Ray p reflectedRay inTime)
          else pure $ Just (attenuation, Ray p refractedRay inTime),
      emit = emitBlack
    }

mkIsotropic :: Color -> Material
mkIsotropic c =
  Material
    { scatter = \r hR@HitRecord {..} -> do
        dir <- getRandomUnitVec
        return $ Just (c, Ray p dir t),
      emit = emitBlack
    }
