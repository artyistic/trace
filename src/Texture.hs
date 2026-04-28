module Texture where

import Graphics (Color, V3, mulColor)
import Graphics.Pixel (color)
import Codec.Picture
import Perlin (generatePerlin, noise, turb)
import System.Random.Stateful (IOGenM, StdGen)
import Graphics.Vec3

newtype Texture = Texture {value :: Double -> Double -> V3 -> Color}

solidTex :: Color -> Texture
solidTex c = Texture {value = \_ _ _ -> c}

-- checker pattern from two textures
checkerTex :: Double -> Texture -> Texture -> Texture
checkerTex scale tex1 tex2 =
  Texture
    { value = \u v p ->
        let (x, y, z) = fromV (floor . (/ scale)) p
         in if even (x + y + z) then tex1.value u v p else tex2.value u v p
    }

-- Use 'solidTex' directly inside the call
checkerTexFromColor :: Double -> Color -> Color -> Texture
checkerTexFromColor scale c1 c2 =
  checkerTex scale (solidTex c1) (solidTex c2)

imageTexture :: FilePath -> IO Texture
imageTexture fpath = do
  img <- readImage fpath
  case img of
    Left err -> error err
    Right dImage ->
      let !a = convertRGB8 dImage
          w = imageWidth a
          h = imageHeight a
      in return $ Texture {
        value = \u v p ->
          let u' = (max 0 . min 1) u
              v' = 1.0 - (max 0 . min 1) v
              i = truncate (u' * fromIntegral w)
              j = truncate (v' * fromIntegral h)
              (PixelRGB8 r g b ) = pixelAt a i j
          in color ((1.0/255.0) * fromIntegral r) ((1.0/255.0) * fromIntegral g) ((1.0/255.0) * fromIntegral b)
      }

perlinTexture :: IOGenM StdGen -> Double -> IO Texture
perlinTexture gen scale = do
  table <- generatePerlin gen
  return Texture
    { value = \u v p -> let depth = 7 in
        (color 0.5 0.5 0.5 `mulColor` (1 + sin (scale * p.z + 10 * turb table p depth)))
    }