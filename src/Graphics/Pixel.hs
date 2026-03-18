{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Graphics.Pixel (Color, color, colorFromV3, averageColor, colorToRGB, gammaCorrected) where

import Graphics.Vec3
import Data.Word (Word8)
import Data.Bool
import Control.Parallel.Strategies (NFData)

-- Color are represented by [0,1] range
newtype Color = Color { rgb :: V3 }
  deriving (Eq, Show, Vec3, NFData)

-- Smart constructor that clamps values
color :: Double -> Double -> Double -> Color
color r g b = Color $ fromXYZ (clamp01 r) (clamp01 g) (clamp01 b)
  where clamp01 x = max 0 (min 1 x)

-- just a helper, calls color
colorFromV3 :: V3 -> Color
colorFromV3 v = color r g b
  where (r, g, b) = toXYZ v

-- interal conversions
toWord8 :: Double -> Word8
toWord8 x = fromIntegral (floor (x * 255.99))

-- a function to average Colors from list
-- here bc color smart constructor clamps
-- this is really a safer way to do color I think
averageColor :: [Color] -> Color
averageColor xs = let (sum, count) = foldl (\(s, c) x -> (s <+> x, c + 1)) (color 0 0 0, 0) xs
             in if count == 0 then color 0 0 0 else sum ./ count

colorToRGB :: Color -> (Int, Int, Int)
colorToRGB c =
  let (x, y, z) = toXYZ c
  in (to255 x, to255 y, to255 z)
  where
    to255 x = floor (clamp 0 1 x * 255) :: Int
    clamp minVal maxVal = max minVal . min maxVal

gammaCorrected :: Color -> Color
gammaCorrected = transform (\x -> if x > 0 then sqrt x else x)
