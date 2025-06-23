{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Graphics.Pixel (Color, color, toRGB8) where

import Graphics.Vec3
import Data.Word (Word8)
import qualified Codec.Picture as P

-- Color are represented by [0,1] range
newtype Color = Color { rgb :: V3 }
  deriving (Eq, Show, Vec3)

-- Smart constructor that clamps values
color :: Double -> Double -> Double -> Color
color r g b = Color $ fromXYZ (clamp01 r) (clamp01 g) (clamp01 b)
  where clamp01 x = max 0 (min 1 x)

-- interal conversions
toWord8 :: Double -> Word8
toWord8 x = fromIntegral (floor (x * 255.99))

-- exposed conversion
toRGB8 :: Color -> P.PixelRGB8
toRGB8 (Color v) = let (r, g, b) = toXYZ v in P.PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)
