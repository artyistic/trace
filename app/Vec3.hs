module Vec3 where

class Vec3 v where
  fromXYZ :: Double -> Double -> Double -> v
  toXYZ :: v -> (Double, Double, Double)
  origin :: v
  origin = fromXYZ 0 0 0

  -- vector operations
  zipV :: (Double -> Double -> Double) -> v -> v -> v
  zipV f v1 v2 = fromXYZ (f x1 x2) (f y1 y2) (f z1 z2)
    where
      (x1, y1, z1) = toXYZ v1
      (x2, y2, z2) = toXYZ v2

  -- addition
  (<+>) :: v -> v -> v
  (<+>) = zipV (+)

  -- subtraction
  (<->) :: v -> v -> v
  (<->) = zipV (-)

  -- dot
  (.*) :: v -> v -> Double
  (.*) v1 v2 = x + y + z
    where
      (x, y, z) = toXYZ $ zipV (*) v1 v2
  
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

  -- scalar mul
  (.^) :: v -> Double -> v
  (.^) v s = fromXYZ (x * s) (y * s) (z * s)
    where
      (x, y, z) = toXYZ v

  -- L2 norm
  norm :: v -> Double
  norm v = sqrt(v .* v)

  distance :: v -> v -> Double
  distance v1 v2 = norm (v1 <-> v2)

  -- calculate unit vector
  normalize :: v -> v
  normalize v = v .^ (1 / norm v)


data V3 = V3 !Double !Double !Double
  deriving (Eq, Show)

instance Vec3 V3 where
  fromXYZ = V3
  toXYZ (V3 x y z) = (x, y, z)

newtype Point = Point V3
  deriving (Eq)

newtype Color = Color V3
  deriving (Eq)