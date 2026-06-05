{-# LANGUAGE ScopedTypeVariables #-}

module Render (render) where

import BVH
import Camera
import Control.Monad ( forM_, replicateM, when )
import Data.ByteString.Builder (Builder, intDec, string7, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Graphics
import Hittable
import qualified Interval as I
import Random
import Material
import HitRecord (HitRecord(HitRecord))
import System.IO (withFile, IOMode(..), hPutStr, stderr)
import System.Random.Stateful (IOGenM, UniformRange (..), newIOGenM)
import qualified Data.Massiv.Array as A
import System.Random (StdGen)
import qualified Data.Vector as V
import Control.Monad.Random ( initStdGen, getStdGen )
import Data.Massiv.Array (MArray)
import Data.IORef (newIORef, IORef, atomicModifyIORef')
import qualified Data.Vector.Mutable as MV
import System.Random.Shuffle (shuffle')

render :: FilePath -> Camera -> Int -> [Hittable] -> IO ()
render fpath cam numBounces world = do
  -- Precompute shuffle bijection
  gen <- getStdGen
  let scatterFwd = V.fromList $ shuffle' [0 .. total - 1] total gen
      scatterInv = V.create $ do
        v <- MV.new total
        V.iforM_ scatterFwd $ \i j -> MV.write v j i
        return v
  workerStates <- A.initWorkerStates A.Par (\_ -> newIOGenM =<< initStdGen)
  computed <- A.generateArrayLinearWS workerStates (A.Sz total)
                (\i g ->
                  let j         = scatterFwd V.! i
                      A.Ix2 y x = A.fromLinearIndex sz j
                  in samplePixel x y bvh cam numBounces g)
                :: IO (A.Array A.B A.Ix1 Color)
  let reordered = (A.makeArray A.Seq sz $ \(A.Ix2 y x) ->
        computed A.! (scatterInv V.! A.toLinearIndex sz (A.Ix2 y x)))
        :: A.Array A.B A.Ix2 Color
  withFile fpath WriteMode $ \h -> do
    BL.hPut h . toLazyByteString $ ppmHeader imageWidth imageHeight
    forM_ [0 .. imageHeight - 1] $ \y ->
      BL.hPut h . toLazyByteString $
        foldMap (\x -> colorToBuilder (reordered A.! A.Ix2 y x) <> string7 "\n")
                [0 .. imageWidth - 1]
  where
    !bvh        = bvhFromList world
    imageWidth  = cam.config.imageWidth
    imageHeight = cam.imageHeight
    total       = imageWidth * imageHeight
    sz          = A.Sz2 imageHeight imageWidth


ppmHeader :: Int -> Int -> Builder
ppmHeader w h =
  string7 "P3\n"
  <> intDec w <> string7 " " <> intDec h <> string7 "\n"
  <> string7 "255\n"

colorToBuilder :: Color -> Builder
colorToBuilder c =
  let (r, g, b) = colorToRGB c
  in intDec r <> string7 " " <> intDec g <> string7 " " <> intDec b

-- | Sample a pixel at (x, y) by averaging multiple random rays.
samplePixel :: Int -> Int -> Hittable -> Camera -> Int -> IOGenM StdGen -> IO Color
samplePixel x y bvh cam numBounces gen = do
  offsets <- replicateM samplesPerPixel (getSampleSquare gen)
  colors  <- traverse (sampleRay x y bvh cam numBounces defocusAngle gen) offsets
  return . gammaCorrect . averageColor $ colors
  where
    samplesPerPixel = cam.config.samplesPerPixel
    defocusAngle    = cam.config.defocusAngle

-- | Shoot one ray through pixel (x, y) with a random sub-pixel offset.
sampleRay :: Int -> Int -> Hittable -> Camera -> Int -> Double -> IOGenM StdGen -> V3 -> IO Color
sampleRay x y bvh cam numBounces defocusAngle gen offset = do
  origin <- if defocusAngle <= 0
              then pure cam.center
              else sampleDefocusDisk cam gen
  ray    <- shootRay origin x y offset cam gen
  rayColor ray bvh cam.background numBounces gen

-- | Construct a ray from an origin through pixel (x, y) with a sub-pixel offset.
shootRay :: V3 -> Int -> Int -> V3 -> Camera -> IOGenM StdGen -> IO Ray
shootRay origin x y (V3 offsetX offsetY _) cam gen = do
  let direction = pixelCenter (fromIntegral x + offsetX) (fromIntegral y + offsetY) cam
                    <-> origin
  time <- uniformRM (0, 1) gen
  return $ Ray origin direction time

-- | World-space position of a (possibly fractional) pixel coordinate.
pixelCenter :: Double -> Double -> Camera -> V3
pixelCenter x y cam =
  cam.pixel00Loc <+> cam.pixelDu .^ x <+> cam.pixelDv .^ y

-- | Sample a random point on the defocus disk.
sampleDefocusDisk :: Camera -> IOGenM StdGen -> IO V3
sampleDefocusDisk cam gen = do
  (V3 px py _) <- getRandomInUnitDisk gen
  return $ cam.center
        <+> cam.defocusDiskU .^ px
        <+> cam.defocusDiskV .^ py

-- | Trace a ray through the scene, returning its color.
{-# INLINE rayColor #-}
rayColor :: Ray -> Hittable -> (V3 -> Color) -> Int -> IOGenM StdGen -> IO Color
rayColor _ _ _ 0 _ = pure $ color 0 0 0
rayColor r@(Ray _ direction _) bvh background depth gen =
  maybe (pure $ background direction) (bounceRay gen) (bvh.hit r (I.Interval 0.001 (1 / 0)))
  where
    bounceRay :: IOGenM StdGen -> (HitRecord, Material) -> IO Color
    bounceRay gen (hitRec@(HitRecord p _ _ _ u v), mat) = do
      result <- mat.scatter gen r hitRec
      maybe (pure $ mat.emit u v p) (continueTrace gen) result

    continueTrace gen (attenuation, scattered) =
      (attenuation `modulate`) <$> rayColor scattered bvh background (depth - 1) gen

-- | Serialize pixels to PPM format using ByteString.Builder.
toPPM :: Int -> Int -> V.Vector Color -> BL.ByteString
toPPM w h pixels = toLazyByteString $
  header <> V.foldMap pixelLine pixels
  where
    header    = string7 "P3\n"
             <> intDec w <> string7 " " <> intDec h <> string7 "\n"
             <> string7 "255\n"
    pixelLine c = colorToBuilder c <> string7 "\n"
    colorToBuilder c =
      let (r, g, b) = colorToRGB c
      in intDec r <> string7 " " <> intDec g <> string7 " " <> intDec b
