{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Graphics.Pixel where

import Graphics.Vec3
import Data.Word (Word8)
import Control.Parallel.Strategies (NFData)
import Prelude as P
import qualified Data.Massiv.Array as A
-- Color are represented by 3 non negative doubles,
-- Color will be clamped at final rendering for ppm
newtype Color = Color { rgb :: V3 }
  deriving (Eq, Show, Vec3, NFData)

-- instance Semigroup Color where
--   (<>) = (<+>)

-- instance Monoid Color where
--   mempty = color 0 0 0 

-- Smart constructor that clamps values
color :: Double -> Double -> Double -> Color
color r g b = Color (V3 (max 0 r) (max 0 g) (max 0 b))

-- just a helper, calls color
colorFromV3 :: V3 -> Color
colorFromV3 v = color r g b
  where (r, g, b) = toXYZ v

-- a function to average Colors from list
averageColor :: [Color] -> Color
averageColor xs =
  let (total, count) = foldl (\(s, c) x -> (s <+> x, c + 1)) (color 0 0 0, 0 :: Int) xs
  in if count == 0 then color 0 0 0 else total ./ fromIntegral count

-- reinhard gloabl tonemapping
tonemap :: Color -> Color
tonemap (Color (V3 r g b)) = Color (V3 (f r) (f g) (f b))
  where f x = x / (1 + x)

colorToRGB :: Color -> (Int, Int, Int)
colorToRGB (Color (V3 r g b)) =
  (to255 r, to255 g, to255 b)
  where
    to255 x = floor (clamp 0 1 x * 255) :: Int
    clamp lo hi = max lo . min hi

gammaCorrect :: Color -> Color
gammaCorrect = transform (\x -> if x > 0 then sqrt x else x)


skyBox :: V3 -> Color
skyBox direction =
  white .^ (1 - a) <+> lightBlue .^ a
  where
    a = 0.5 * (toY (normalize direction) + 1)
    white     = color 1.0 1.0 1.0
    lightBlue = color 0.5 0.7 1.0
