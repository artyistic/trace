module Texture where

import Graphics (Color, V3)
import Graphics.Vec3 (fromV)

newtype Texture = Texture {value :: Double -> Double -> V3 -> Color}

solidTex :: Color -> Texture
solidTex c = Texture {value = \_ _ _ -> c}

-- checker pattern from two textures
checkerTex :: Double -> Texture -> Texture -> Texture
checkerTex scale tex1 tex2 =
  Texture
    { value = \u v p ->
        let (x, y, z) = fromV (floor . (/ scale)) p
         in if even (x + y + z) then value tex1 u v p else value tex2 u v p
    }

-- Use 'solidTex' directly inside the call
checkerTexFromColor :: Double -> Color -> Color -> Texture
checkerTexFromColor scale c1 c2 =
  checkerTex scale (solidTex c1) (solidTex c2)