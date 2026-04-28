{-# LANGUAGE StrictData #-}

module Perlin where
import qualified Data.Vector.Unboxed as UV
import Data.Bits
import Graphics.Vec3
import System.Random.Stateful (IOGenM, UniformRange (uniformRM))
import System.Random (StdGen)
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Functor ((<&>))
import Data.List (foldl')

newtype PerlinTable a = PerlinTable (UV.Vector a)

ptLookup :: UV.Unbox a => PerlinTable a -> Int -> a
ptLookup (PerlinTable v) i = v UV.! (i .&. 255)

data Perlin = Perlin
  { randX :: PerlinTable Double
  , randY :: PerlinTable Double
  , randZ :: PerlinTable Double
  , permX   :: PerlinTable Int
  , permY   :: PerlinTable Int
  , permZ   :: PerlinTable Int
  }


-- has to be a cleaner way to do this
generatePerlin :: IOGenM StdGen -> IO Perlin
generatePerlin gen = Perlin
  <$> getPerlinDoubles <*> getPerlinDoubles <*> getPerlinDoubles
  <*> permuteIndices   <*> permuteIndices   <*> permuteIndices
  where
    perlinLength = 256
    getPerlinDoubles = PerlinTable <$> UV.replicateM perlinLength (uniformRM (-1.0, 1.0) gen)
    permuteIndices   = PerlinTable <$> permute (UV.generate perlinLength id) gen

permute :: UV.Vector Int -> IOGenM StdGen -> IO (UV.Vector Int)
permute v gen = do
  mv <- UV.thaw v
  forM_ [UV.length v - 1, UV.length v - 2 .. 1] $ \i -> do
    j <- uniformRM (0, i) gen
    MUV.swap mv i j
  UV.freeze mv

noise :: Perlin -> V3 -> Double
noise table p = perlinInterp cornerValue u v w
  where
    u = p.x - fromIntegral (floor p.x :: Int)
    v = p.y - fromIntegral (floor p.y :: Int)
    w = p.z - fromIntegral (floor p.z :: Int)
    i = floor p.x :: Int
    j = floor p.y :: Int
    k = floor p.z :: Int
    cornerValue di dj dk = let
      ix = ptLookup table.permX ((i + di) .&. 255) `xor`
        ptLookup table.permY ((j + dj) .&. 255) `xor`
        ptLookup table.permZ ((k + dk) .&. 255)
      in V3 (ptLookup table.randX ix) (ptLookup table.randY ix) (ptLookup table.randZ ix)

perlinInterp :: (Int -> Int -> Int -> V3) -> Double -> Double -> Double -> Double
perlinInterp c u v w = sum
    [ weight i u * weight j v * weight k w * (c i j k .* V3 (u - fromIntegral i) (v - fromIntegral j) (w - fromIntegral k))
    | i <- [0,1], j <- [0,1], k <- [0,1] ]
  where
    smooth t = t * t * (3 - 2 * t)
    weight i t = if i == 0 then 1 - smooth t else smooth t
    hermitianSmoothing t = t * t * (3 - 2 * t)

-- turbulence with octaves 
turb :: Perlin -> V3 -> Int -> Double
turb table origin depth = abs $ sum . take depth $
    zipWith (\point w -> w * noise table point) points weights
  where
    points  = iterate (.^ 2) origin
    weights = iterate (* 0.5) 1.0