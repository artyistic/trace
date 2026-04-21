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
noise table p = ptLookup table.randVec (i `xor` j `xor` k)
  where
    i = ptLookup table.permX (truncate (4 * p.x) :: Int)
    j = ptLookup table.permY (truncate (4 * p.y) :: Int)
    k = ptLookup table.permZ (truncate (4 * p.z) :: Int)