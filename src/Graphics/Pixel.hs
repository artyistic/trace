{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Graphics.Pixel where

import Graphics.Vec3
    ( (./), (.^), (<+>), componentMul, mapV, normalize, V3(V3, y) )
import Data.Word (Word8)
import Control.Parallel.Strategies (NFData)
import Foreign (Storable)

-- Color are represented by 3 non negative doubles,
-- Color will be clamped at final rendering for ppm
newtype Color = Color { rgb :: V3 }
  deriving (Eq, Show, NFData, Storable)

instance Semigroup Color where
  (<>) c1 c2 = Color $ c1.rgb <+> c2.rgb 

instance Monoid Color where
  mempty = color 0 0 0 

-- some vector operations for color
-- color used to derive Vec3 typeclass
divideColor :: Color -> Double -> Color
divideColor c i = Color $ c.rgb ./ i

mulColor :: Color -> Double -> Color
mulColor c i = Color $ c.rgb .^ i

modulate :: Color -> Color -> Color
modulate a b = Color $ a.rgb `componentMul` b.rgb

-- Smart constructor that clamps values
color :: Double -> Double -> Double -> Color
color r g b = Color (V3 (max 0 r) (max 0 g) (max 0 b))

-- a function to average Colors from list
averageColor :: [Color] -> Color
averageColor xs =
  let (total, count) = foldl (\(s, c) x -> (s <> x, c + 1)) (color 0 0 0, 0 :: Int) xs
  in if count == 0 then color 0 0 0 else divideColor total (fromIntegral count)

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
gammaCorrect c = Color $ mapV (\x -> if x > 0 then sqrt x else x) c.rgb

skyBox :: V3 -> Color
skyBox direction =
  white `mulColor` (1 - a) <> lightBlue `mulColor` a
  where
    a = 0.5 * ((normalize direction).y + 1)
    white     = color 1.0 1.0 1.0
    lightBlue = color 0.5 0.7 1.0
