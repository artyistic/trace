module Graphics.Ray where

import Graphics.Vec3
import Graphics.Point
import Graphics.Pixel
import qualified Codec.Picture as P

data Ray = Ray Point V3

at :: Ray -> Double -> Point
at (Ray a r) t = Point $ pt a <+> r .^ t

-- rayColor returns a rgb given a ray
rayColor :: Ray -> P.PixelRGB8
rayColor (Ray _ direction) = toRGB8 (p1 <+> p2)
  where p1 = color 1.0 1.0 1.0 .^ (1.0 - a)
        p2 = color 0.5 0.7 1.0 .^ a
        a = 0.5 * (toY (normalize direction) + 1)


