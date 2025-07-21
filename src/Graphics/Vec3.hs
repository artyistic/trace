module Graphics.Vec3 where

import Control.Monad.Loops
import Control.Monad.Random
import qualified Interval as I

class Vec3 v where
  fromXYZ :: Double -> Double -> Double -> v
  toXYZ :: v -> (Double, Double, Double)
  origin :: v
  origin = fromXYZ 0 0 0

  transform :: (Double -> Double) -> v -> v
  transform f v = fromXYZ (f x) (f y) (f z)
    where
      (x, y, z) = toXYZ v

  -- vector operations
  zipV :: (Double -> Double -> Double) -> v -> v -> v
  zipV f v1 v2 = fromXYZ (f x1 x2) (f y1 y2) (f z1 z2)
    where
      (x1, y1, z1) = toXYZ v1
      (x2, y2, z2) = toXYZ v2

  -- addition
  (<+>) :: v -> v -> v
  (<+>) = zipV (+)
  infixl 6 <+>

  -- subtraction
  (<->) :: v -> v -> v
  (<->) = zipV (-)
  infixl 6 <->

  -- dot
  (.*) :: v -> v -> Double
  (.*) v1 v2 = x + y + z
    where
      (x, y, z) = toXYZ $ zipV (*) v1 v2
  infixl 7 .*

  -- cross
  (><) :: v -> v -> v
  (><) v1 v2 =
    fromXYZ
      (y1 * z2 - y2 * z1)
      (x2 * z1 - x1 * z2)
      (x1 * y2 - x2 * y1)
    where
      (x1, y1, z1) = toXYZ v1
      (x2, y2, z2) = toXYZ v2
  infixl 6 ><

  -- scalar mul
  (.^) :: v -> Double -> v
  (.^) v s = fromXYZ (x * s) (y * s) (z * s)
    where
      (x, y, z) = toXYZ v
  infixl 7 .^

  -- scalar division for conveninence
  -- div by zero NOT checked
  (./) :: v -> Double -> v
  (./) v s = v .^ (1 / s)

  -- L2 norm
  norm :: v -> Double
  norm v = sqrt (v .* v)

  distance :: v -> v -> Double
  distance v1 v2 = norm (v1 <-> v2)

  -- calculate unit vector
  normalize :: v -> v
  normalize v = v .^ (1 / norm v)

  -- length squared
  lengthSquared :: v -> Double
  lengthSquared v = x * x + y * y + z * z
    where
      (x, y, z) = toXYZ v

  -- flip the direction of a vector
  invert :: v -> v
  invert v = origin <-> v

  -- reflect v on normal n
  reflect :: v -> v -> v
  reflect v n = v <-> (n .^ (2 * (v .* n)))

  -- refract :: incoming ray ->
  -- normal vector on the point of contact ->
  -- ratio of refractive index
  refract :: v -> v -> Double -> v
  refract uv n etaiOverEtat =
    rayOutPerpen <+> rayOutPara
    where
      cosTheta = min (invert uv .* n) 1.0 -- min 1.0 small angle floating pt errors
      rayOutPerpen = (uv <+> (n .^ cosTheta)) .^ etaiOverEtat
      rayOutPara = invert $ n .^ sqrt (abs (1.0 - lengthSquared rayOutPerpen))

  componentMul :: v -> v -> v
  componentMul = zipV (*)

  toX :: v -> Double
  toX v = x'
    where
      (x', _, _) = toXYZ v
  toY :: v -> Double
  toY v = y'
    where
      (_, y', _) = toXYZ v
  toZ :: v -> Double
  toZ v = z'
    where
      (_, _, z') = toXYZ v

data V3 = V3 !Double !Double !Double
  deriving (Eq, Show)

instance Vec3 V3 where
  fromXYZ = V3
  toXYZ (V3 x y z) = (x, y, z)

-- Returns a vector to a random point in the [-.5,-.5]-[+.5,+.5] unit square on viewport
-- per documentation, the range of random of Doubles lies in [0, 1)
getSampleSquare :: Rand StdGen V3
getSampleSquare = liftM3 V3 (getRandomR (-0.5, 0.5)) (getRandomR (-0.5, 0.5)) (pure 0)

-- returns a unit cube vector
getRandomVec :: Double -> Double -> Rand StdGen V3
getRandomVec min max = liftM3 V3 (getRandomR (min, max)) (getRandomR (min, max)) (getRandomR (min, max))

-- return a unit sphere vector, by rejection method
-- ie keep finding until the normalized vector is within sphere
getRandomUnitBallVec :: Rand StdGen V3
getRandomUnitBallVec = (\x -> x ./ sqrt (lengthSquared x)) <$> iterateUntil inUnitBall (getRandomVec (-1) 1)
  where
    inUnitBall = I.contains (I.Interval 1e-160 1) . lengthSquared

-- ensure the vector has length at most one, and 1e-160 is a safe lower bound
-- to prevent funny underflowing which leads to catastrophic divide by zero

getRandomOnHemisphere :: V3 -> Rand StdGen V3
getRandomOnHemisphere normal = fmap orient getRandomUnitBallVec
  where
    orient v = if (v .* normal) > 0 then v else invert v

nearZero :: V3 -> Bool
nearZero v = abs x < s && abs y < s && abs z < s
  where
    (x, y, z) = toXYZ v
    s = 1e-8