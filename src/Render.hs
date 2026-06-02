

-- module Render (render) where

-- import BVH
-- import Camera
-- import Control.Monad.Random
-- import Data.ByteString.Builder (Builder, intDec, string7, toLazyByteString)
-- import qualified Data.ByteString.Lazy as BL
-- import Graphics
-- import Hittable
-- import qualified Interval as I
-- import Random
-- import Material
-- import HitRecord (HitRecord(HitRecord))
-- import System.Random.Stateful (IOGenM, UniformRange (..), newIOGenM)
-- import Data.List (unfoldr, sortOn)
-- import qualified Data.Massiv.Array as A
-- import Data.Massiv.Array (computeAs)
-- import qualified Data.Vector as V

-- render :: FilePath -> Camera -> Int -> [Hittable] -> IO ()
-- render fpath cam numBounces world = do
--   workerStates <- A.initWorkerStates A.Par (\_ -> newIOGenM =<< initStdGen)
--   rows <- A.generateArrayLinearWS workerStates (A.Sz imageHeight)
--             (\i g -> renderRow (rowIndex i) bvh cam numBounces g) :: IO (A.Array A.BN A.Ix1 (V.Vector Color))
--   let rowList = A.toList rows
--       ordered = map snd $ sortOn fst
--                   [ (rowIndex i, rowList !! i) | i <- [0 .. imageHeight - 1] ]
--   BL.writeFile fpath $ toPPM imageWidth imageHeight (V.concat ordered)
--   where
--     !bvh        = bvhFromList world
--     imageWidth  = cam.config.imageWidth
--     imageHeight = cam.imageHeight
--     rowIndex i  = (i * (imageHeight `div` 8 + 1)) `mod` imageHeight
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
import Control.Monad.Random (initStdGen)
import Data.Massiv.Array (MArray)
import Data.IORef (newIORef, IORef, atomicModifyIORef')

render :: FilePath -> Camera -> Int -> [Hittable] -> IO ()
render fpath cam numBounces world = do
  workerStates <- A.initWorkerStates A.Par (\_ -> newIOGenM =<< initStdGen)
  -- Pre-allocate entire image as a flat 2D mutable array.
  -- imageHeight × imageWidth Color values claimed upfront — memory is fixed and known.
  let sz    = A.Sz2 imageHeight imageWidth
      total = imageHeight * imageWidth
  img <- A.newMArray sz (color 0 0 0) :: IO (MArray A.RealWorld A.S A.Ix2 Color)
  -- Scatter compute order to mix cheap (background) and expensive (interior) pixels
  -- throughout the work queue, keeping all cores saturated until the end.
  -- Workers write directly to the correct (y, x) slot — no sorting or retention needed.
  -- counter <- newIORef 0 :: IO (IORef Int)
  (_ :: A.Array A.B A.Ix1 ()) <- A.generateArrayLinearWS workerStates (A.Sz total) $ \i g -> do
    let j        = (i * (total `div` 8 + 1)) `mod` total
        A.Ix2 y x = A.fromLinearIndex sz j
    c <- samplePixel x y bvh cam numBounces g
    A.writeM img (A.Ix2 y x) c
    -- n <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
    -- when (n `mod` reportEvery == 0) $
    --   hPutStr stderr $ progress n total
  frozen <- A.freezeS img
  -- Stream to disk one row at a time.
  -- Peak memory here is one row's Builder — a few KB regardless of image size or spp.
  withFile fpath WriteMode $ \h -> do
    BL.hPut h . toLazyByteString $ ppmHeader imageWidth imageHeight
    forM_ [0 .. imageHeight - 1] $ \y ->
      BL.hPut h . toLazyByteString $
        foldMap (\x -> colorToBuilder (frozen A.! A.Ix2 y x) <> string7 "\n")
                [0 .. imageWidth - 1]
  where
    !bvh        = bvhFromList world
    imageWidth  = cam.config.imageWidth
    imageHeight = cam.imageHeight
    -- reportEvery = max 1 (imageWidth * imageHeight `div` 1000)  -- report every 0.1%
    -- progress n total =
    --   let pct = (100 * n) `div` total
    --   in "\r" ++ show pct ++ "% (" ++ show n ++ "/" ++ show total ++ " pixels) "


ppmHeader :: Int -> Int -> Builder
ppmHeader w h =
  string7 "P3\n"
  <> intDec w <> string7 " " <> intDec h <> string7 "\n"
  <> string7 "255\n"

colorToBuilder :: Color -> Builder
colorToBuilder c =
  let (r, g, b) = colorToRGB c
  in intDec r <> string7 " " <> intDec g <> string7 " " <> intDec b

renderRow :: Int -> Hittable -> Camera -> Int -> IOGenM StdGen -> IO (V.Vector Color)
renderRow y bvh cam numBounces gen =
  V.generateM imageWidth $ \x ->
    samplePixel x y bvh cam numBounces gen
  where
    imageWidth = cam.config.imageWidth

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
