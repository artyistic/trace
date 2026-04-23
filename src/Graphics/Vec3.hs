  -- module Graphics.Vec3 where

  -- import Control.Monad.Loops
  -- import Control.Monad.Random
  -- import qualified Interval as I
  -- import Control.DeepSeq (NFData, rnf)

  -- class Vec3 v where
  --   fromV :: (Double -> a) -> v -> (a, a, a)
  --   fromV f v = (f x, f y, f z)
  --     where (x, y, z) = toXYZ v
  --   fromXYZ :: Double -> Double -> Double -> v
  --   toXYZ :: v -> (Double, Double, Double)
  --   origin :: v
  --   origin = fromXYZ 0 0 0

    -- transform :: (Double -> Double) -> v -> v
    -- transform f v = fromXYZ (f x) (f y) (f z)
    --   where
    --     (x, y, z) = toXYZ v

  --   -- vector operations
  --   zipV :: (Double -> Double -> Double) -> v -> v -> v
  --   zipV f v1 v2 = fromXYZ (f x1 x2) (f y1 y2) (f z1 z2)
  --     where
  --       (x1, y1, z1) = toXYZ v1
  --       (x2, y2, z2) = toXYZ v2

  --   {-# INLINE zipV #-}


  --   -- addition
  --   (<+>) :: v -> v -> v
  --   (<+>) = zipV (+)
  --   infixl 6 <+>

  --   -- subtraction
  --   (<->) :: v -> v -> v
  --   (<->) = zipV (-)
  --   infixl 6 <->

  --   -- dot
  --   (.*) :: v -> v -> Double
  --   (.*) v1 v2 = x + y + z
  --     where
  --       (x, y, z) = toXYZ $ zipV (*) v1 v2
  --   infixl 7 .*

  --   -- cross
  --   (><) :: v -> v -> v
  --   (><) v1 v2 =
  --     fromXYZ
  --       (y1 * z2 - y2 * z1)
  --       (x2 * z1 - x1 * z2)
  --       (x1 * y2 - x2 * y1)
  --     where
  --       (x1, y1, z1) = toXYZ v1
  --       (x2, y2, z2) = toXYZ v2
  --   infixl 6 ><

  --   -- scalar mul
  --   (.^) :: v -> Double -> v
  --   (.^) v s = fromXYZ (x * s) (y * s) (z * s)
  --     where
  --       (x, y, z) = toXYZ v
  --   infixl 7 .^

  --   -- scalar division for conveninence
  --   -- div by zero NOT checked
  --   (./) :: v -> Double -> v
  --   (./) v s = v .^ (1 / s)

  --   -- L2 norm
  --   norm :: v -> Double
  --   norm v = sqrt (v .* v)

  --   distance :: v -> v -> Double
  --   distance v1 v2 = norm (v1 <-> v2)

  --   -- calculate unit vector
  --   normalize :: v -> v
  --   normalize v = v .^ (1 / norm v)

  --   -- length squared
  --   lengthSquared :: v -> Double
  --   lengthSquared v = x * x + y * y + z * z
  --     where
  --       (x, y, z) = toXYZ v

  --   -- flip the direction of a vector
  --   invert :: v -> v
  --   invert v = origin <-> v

  --   -- reflect v on normal n
  --   reflect :: v -> v -> v
  --   reflect v n = v <-> (n .^ (2 * (v .* n)))

  --   -- refract :: incoming ray ->
  --   -- normal vector on the point of contact ->
  --   -- ratio of refractive index
  --   refract :: v -> v -> Double -> v
  --   refract uv n etaiOverEtat =
  --     rayOutPerpen <+> rayOutPara
  --     where
  --       cosTheta = min (invert uv .* n) 1.0 -- min 1.0 small angle floating pt errors
  --       rayOutPerpen = (uv <+> (n .^ cosTheta)) .^ etaiOverEtat
  --       rayOutPara = invert $ n .^ sqrt (abs (1.0 - lengthSquared rayOutPerpen))

  --   componentMul :: v -> v -> v
  --   componentMul = zipV (*)

  --   toX :: v -> Double
  --   toX v = x'
  --     where
  --       (x', _, _) = toXYZ v
  --   toY :: v -> Double
  --   toY v = y'
  --     where
  --       (_, y', _) = toXYZ v
  --   toZ :: v -> Double
  --   toZ v = z'
  --     where
  --       (_, _, z') = toXYZ v

  -- data V3 = V3 {x :: !Double, y :: !Double, z :: !Double}
  --   deriving (Eq, Show)

  -- instance NFData V3 where
  --   rnf (V3 {}) = ()

  -- -- an instance of uniformRange for V3
  -- -- mainly used by Random definitions
  -- -- instance UniformRange V3 where
  -- --   uniformRM (V3 lx ly lz, V3 hx hy hz) gen =
  -- --     V3 <$> uniformRM (lx, hx) gen
  -- --        <*> uniformRM (ly, hy) gen
  -- --        <*> uniformRM (lz, hz) gen

  -- --   isInRange (V3 lx ly lz, V3 hx hy hz) (V3 x y z) =
  -- --     isInRange (lx, hx) x &&
  -- --     isInRange (ly, hy) y &&
  -- --     isInRange (lz, hz) z

  -- instance Vec3 V3 where
  --   fromXYZ = V3
  --   toXYZ (V3 x y z) = (x, y, z)

  -- nearZero :: V3 -> Bool
  -- nearZero v = abs x < s && abs y < s && abs z < s
  --   where
  --     (x, y, z) = toXYZ v
  --     s = 1e-8

  -- withVec3 :: V3 -> (Double -> Double -> Double -> a) -> a
  -- withVec3 v f = f vx vy vz
  --   where (vx, vy ,vz) = toXYZ v

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