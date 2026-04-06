{-# LANGUAGE BangPatterns #-}

module Render (render) where

import BVH
import Camera
import Control.Monad.Random
import Control.Parallel.Strategies
import Data.ByteString.Builder (Builder, intDec, string7, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Graphics
import Hittable
import qualified Interval as I
import Random
import System.Random.SplitMix (SMGen, initSMGen, splitSMGen, nextWord64)
import Material
import HitRecord (HitRecord(HitRecord))

-- | Render a scene to a PPM file.
render :: FilePath -> [Hittable] -> Camera -> Int -> IO ()
render fpath world cam numBounces = do
  gen <- initSMGen
  let rowGens = take imageHeight $ iterate (snd . splitSMGen) gen
      rowResults =
        [ evalRand (renderRow y bvh cam numBounces) (mkStdGen (fromIntegral (fst (nextWord64 g))))
          | (y, g) <- zip [0 .. imageHeight - 1] rowGens
        ] `using` parList rdeepseq

  BL.writeFile fpath $ toPPM imageWidth imageHeight (concat rowResults)
  where
    !bvh        = bvhFromList world
    imageWidth  = cam.config.imageWidth
    imageHeight = cam.imageHeight

-- | Render all pixels in a single row.
renderRow :: Int -> BVHNode -> Camera -> Int -> Rand StdGen [Color]
renderRow y bvh cam numBounces =
  forM [0 .. imageWidth - 1] $ \x ->
    samplePixel x y bvh cam numBounces
  where
    imageWidth = cam.config.imageWidth

-- | Sample a pixel at (x, y) by averaging multiple random rays.
samplePixel :: Int -> Int -> BVHNode -> Camera -> Int -> Rand StdGen Color
samplePixel x y bvh cam numBounces = do
  offsets <- replicateM samplesPerPixel getSampleSquare
  colors  <- traverse (sampleRay x y bvh cam numBounces defocusAngle) offsets
  return . gammaCorrect . averageColor $ colors
  where
    samplesPerPixel = cam.config.samplesPerPixel
    defocusAngle    = cam.config.defocusAngle

-- | Shoot one ray through pixel (x, y) with a random sub-pixel offset.
sampleRay :: Int -> Int -> BVHNode -> Camera -> Int -> Double -> V3 -> Rand StdGen Color
sampleRay x y bvh cam numBounces defocusAngle offset = do
  origin <- if defocusAngle <= 0
              then pure cam.center
              else sampleDefocusDisk cam
  ray    <- shootRay origin x y offset cam
  rayColor ray bvh numBounces

-- | Construct a ray from an origin through pixel (x, y) with a sub-pixel offset.
shootRay :: V3 -> Int -> Int -> V3 -> Camera -> Rand StdGen Ray
shootRay origin x y (V3 offsetX offsetY _) cam = do
  let direction = pixelCenter (fromIntegral x + offsetX) (fromIntegral y + offsetY) cam
                    <-> origin
  time <- getRandomR (0, 1)
  return $ Ray origin direction time

-- | World-space position of a (possibly fractional) pixel coordinate.
pixelCenter :: Double -> Double -> Camera -> V3
pixelCenter x y cam =
  cam.pixel00Loc <+> cam.pixelDu .^ x <+> cam.pixelDv .^ y

-- | Sample a random point on the defocus disk.
sampleDefocusDisk :: Camera -> Rand StdGen V3
sampleDefocusDisk cam = do
  (V3 px py _) <- getRandomInUnitDisk
  return $ cam.center
        <+> cam.defocusDiskU .^ px
        <+> cam.defocusDiskV .^ py

-- | Trace a ray through the scene, returning its color.
{-# INLINE rayColor #-}
rayColor :: Ray -> BVHNode -> Int -> Rand StdGen Color
rayColor _ _ 0 = pure $ color 0 0 0
rayColor r@(Ray _ direction _) bvh depth =
  maybe (pure background) bounceRay (hitBVH bvh r (I.Interval 0.001 (1 / 0)))
  where
    bounceRay :: (HitRecord, Material) -> Rand StdGen Color
    bounceRay (hitRec, mat) = do
      result <- mat.scatter r hitRec
      maybe (pure $ color 0 0 0) continueTrace result

    continueTrace (attenuation, scattered) =
      (attenuation `componentMul`) <$> rayColor scattered bvh (depth - 1)

    background =
      let a = 0.5 * (toY (normalize direction) + 1)
      in white .^ (1 - a) <+> lightBlue .^ a

    white     = color 1.0 1.0 1.0
    lightBlue = color 0.5 0.7 1.0

-- | Serialize pixels to PPM format using ByteString.Builder.
toPPM :: Int -> Int -> [Color] -> BL.ByteString
toPPM w h pixels = toLazyByteString $
  header <> foldMap pixelLine pixels
  where
    header    = string7 "P3\n"
             <> intDec w <> string7 " " <> intDec h <> string7 "\n"
             <> string7 "255\n"
    pixelLine c = colorToBuilder c <> string7 "\n"

colorToBuilder :: Color -> Builder
colorToBuilder c =
  let (r, g, b) = colorToRGB c
  in intDec r <> string7 " " <> intDec g <> string7 " " <> intDec b
