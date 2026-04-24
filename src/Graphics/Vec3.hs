module Graphics.Vec3 where

import Control.DeepSeq (NFData, rnf)

data V3 = V3 { x :: !Double, y :: !Double, z :: !Double }
  deriving (Eq, Show)

instance NFData V3 where
  rnf (V3 {}) = ()

origin :: V3
origin = V3 0 0 0

(<+>) :: V3 -> V3 -> V3
(<+>) a b = V3 (a.x + b.x) (a.y + b.y) (a.z + b.z)
infixl 6 <+>

(<->) :: V3 -> V3 -> V3
(<->) a b = V3 (a.x - b.x) (a.y - b.y) (a.z - b.z)
infixl 6 <->

(.*) :: V3 -> V3 -> Double
(.*) a b = a.x * b.x + a.y * b.y + a.z * b.z
infixl 7 .*

(><) :: V3 -> V3 -> V3
(><) a b = V3
  (a.y * b.z - b.y * a.z)
  (b.x * a.z - a.x * b.z)
  (a.x * b.y - b.x * a.y)
infixl 6 ><

(.^) :: V3 -> Double -> V3
(.^) v s = V3 (v.x * s) (v.y * s) (v.z * s)
infixl 7 .^

(./) :: V3 -> Double -> V3
(./) v s = v .^ (1 / s)
infixl 7 ./

componentMul :: V3 -> V3 -> V3
componentMul a b = V3 (a.x * b.x) (a.y * b.y) (a.z * b.z)

invert :: V3 -> V3
invert v = V3 (-v.x) (-v.y) (-v.z)

norm :: V3 -> Double
norm v = sqrt (v .* v)

lengthSquared :: V3 -> Double
lengthSquared v = v .* v

distance :: V3 -> V3 -> Double
distance a b = norm (a <-> b)

normalize :: V3 -> V3
normalize v = v .^ (1 / norm v)

reflect :: V3 -> V3 -> V3
reflect v n = v <-> (n .^ (2 * (v .* n)))

refract :: V3 -> V3 -> Double -> V3
refract uv n etaiOverEtat = rayOutPerpen <+> rayOutPara
  where
    cosTheta     = min (invert uv .* n) 1.0
    rayOutPerpen = (uv <+> (n .^ cosTheta)) .^ etaiOverEtat
    rayOutPara   = invert $ n .^ sqrt (abs (1.0 - lengthSquared rayOutPerpen))

nearZero :: V3 -> Bool
nearZero v = abs v.x < s && abs v.y < s && abs v.z < s
  where s = 1e-8

fromV :: (Double -> a) -> V3 -> (a, a, a)
fromV f (V3 x y z) = (f x, f y, f z)

mapV :: (Double -> Double) -> V3 -> V3
mapV f (V3 x y z) = V3 (f x) (f y) (f z)

zipV :: (Double -> Double -> Double) -> V3 -> V3 -> V3
zipV f a b = V3 (f a.x b.x) (f a.y b.y) (f a.z b.z)