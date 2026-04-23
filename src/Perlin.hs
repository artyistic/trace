{-# LANGUAGE StrictData #-}

module Perlin where
import qualified Data.Vector.Unboxed as UV
import Data.Bits
import Graphics.Vec3
import System.Random.Stateful (IOGenM, UniformRange (uniformRM))
import System.Random (StdGen)
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as MUV

newtype PerlinTable a = PerlinTable (UV.Vector a)

ptLookup :: UV.Unbox a => PerlinTable a -> Int -> a
ptLookup (PerlinTable v) i = v UV.! (i .&. 255)

data Perlin = Perlin
  { randVec :: PerlinTable Double
  , permX   :: PerlinTable Int
  , permY   :: PerlinTable Int
  , permZ   :: PerlinTable Int
  }

generatePerlin :: IOGenM StdGen -> IO Perlin
generatePerlin gen = do
  rands <- UV.replicateM perlinLength $ uniformRM (0.0, 1.0) gen :: IO (UV.Vector Double)
  permX <- permute (UV.generate perlinLength id) gen
  permY <- permute (UV.generate perlinLength id) gen
  permZ <- permute (UV.generate perlinLength id) gen
  return $ Perlin (PerlinTable rands) (PerlinTable permX) (PerlinTable permY) (PerlinTable permZ)
  where
    perlinLength = 256

permute :: UV.Vector Int -> IOGenM StdGen -> IO (UV.Vector Int)
permute v gen = do
  mv <- UV.thaw v
  forM_ [UV.length v - 1, UV.length v - 2 .. 1] $ \i -> do
    j <- uniformRM (0, i) gen
    MUV.swap mv i j
  UV.freeze mv

noise :: Perlin -> V3 -> Double
noise table p = trilinearInterp cornerValue u v w
  where
    u = hermitianSmoothing $ p.x - fromIntegral (floor p.x :: Int)
    v = hermitianSmoothing $ p.y - fromIntegral (floor p.y :: Int)
    w = hermitianSmoothing $ p.z - fromIntegral (floor p.z :: Int)
    i = floor p.x :: Int
    j = floor p.y :: Int
    k = floor p.z :: Int
    cornerValue di dj dk = ptLookup table.randVec $
        ptLookup table.permX ((i + di) .&. 255) `xor`
        ptLookup table.permY ((j + dj) .&. 255) `xor`
        ptLookup table.permZ ((k + dk) .&. 255)
    hermitianSmoothing t = t * t * (3 - 2 * t)

trilinearInterp :: (Int -> Int -> Int -> Double) -> Double -> Double -> Double -> Double
trilinearInterp c u v w = sum
    [ weight i u * weight j v * weight k w * c i j k
    | i <- [0,1], j <- [0,1], k <- [0,1] ]
  where
    weight 0 t = 1 - t
    weight _ t = t