module Graphics.Vec3 where

import Control.DeepSeq (NFData, rnf)
import Foreign.Storable
import Foreign (castPtr)

data V3 = V3 { x :: {-# UNPACK #-} !Double, y :: {-# UNPACK #-} !Double, z :: {-# UNPACK #-} !Double }
  deriving (Eq, Show)

instance Storable V3 where
  sizeOf _ = 3 * sizeOf (undefined :: Double)
  alignment _ = alignment (undefined :: Double)
  peek ptr = V3 <$> peekElemOff (castPtr ptr) 0
                <*> peekElemOff (castPtr ptr) 1
                <*> peekElemOff (castPtr ptr) 2
  poke ptr (V3 x y z) = do
    pokeElemOff (castPtr ptr) 0 x
    pokeElemOff (castPtr ptr) 1 y
    pokeElemOff (castPtr ptr) 2 z
instance NFData V3 where
  rnf (V3 {}) = ()

origin :: V3
origin = V3 0 0 0

{-# INLINE (<+>) #-}
(<+>) :: V3 -> V3 -> V3
(<+>) a b = V3 (a.x + b.x) (a.y + b.y) (a.z + b.z)
infixl 6 <+>

{-# INLINE (<->) #-}
(<->) :: V3 -> V3 -> V3
(<->) a b = V3 (a.x - b.x) (a.y - b.y) (a.z - b.z)
infixl 6 <->

{-# INLINE (.*) #-}
(.*) :: V3 -> V3 -> Double
(.*) a b = a.x * b.x + a.y * b.y + a.z * b.z
infixl 7 .*

{-# INLINE (><) #-}
(><) :: V3 -> V3 -> V3
(><) a b = V3
  (a.y * b.z - b.y * a.z)
  (b.x * a.z - a.x * b.z)
  (a.x * b.y - b.x * a.y)
infixl 6 ><

{-# INLINE (.^) #-}
(.^) :: V3 -> Double -> V3
(.^) v s = V3 (v.x * s) (v.y * s) (v.z * s)
infixl 7 .^

{-# INLINE (./) #-}
(./) :: V3 -> Double -> V3
(./) v s = v .^ (1 / s)
infixl 7 ./

{-# INLINE componentMul #-}
componentMul :: V3 -> V3 -> V3
componentMul a b = V3 (a.x * b.x) (a.y * b.y) (a.z * b.z)

{-# INLINE invert #-}
invert :: V3 -> V3
invert v = V3 (-v.x) (-v.y) (-v.z)

{-# INLINE norm #-}
norm :: V3 -> Double
norm v = sqrt (v .* v)

{-# INLINE lengthSquared #-}
lengthSquared :: V3 -> Double
lengthSquared v = v .* v

{-# INLINE distance #-}
distance :: V3 -> V3 -> Double
distance a b = norm (a <-> b)

{-# INLINE normalize #-}
normalize :: V3 -> V3
normalize v = v .^ (1 / norm v)

{-# INLINE reflect #-}
reflect :: V3 -> V3 -> V3
reflect v n = v <-> (n .^ (2 * (v .* n)))

{-# INLINE refract #-}
refract :: V3 -> V3 -> Double -> V3
refract uv n etaiOverEtat = rayOutPerpen <+> rayOutPara
  where
    cosTheta     = min (invert uv .* n) 1.0
    rayOutPerpen = (uv <+> (n .^ cosTheta)) .^ etaiOverEtat
    rayOutPara   = invert $ n .^ sqrt (abs (1.0 - lengthSquared rayOutPerpen))

{-# INLINE nearZero #-}
nearZero :: V3 -> Bool
nearZero v = abs v.x < s && abs v.y < s && abs v.z < s
  where s = 1e-8

{-# INLINE fromV #-}
fromV :: (Double -> a) -> V3 -> (a, a, a)
fromV f (V3 x y z) = (f x, f y, f z)

{-# INLINE mapV #-}
mapV :: (Double -> Double) -> V3 -> V3
mapV f (V3 x y z) = V3 (f x) (f y) (f z)

{-# INLINE zipV #-}
zipV :: (Double -> Double -> Double) -> V3 -> V3 -> V3
zipV f a b = V3 (f a.x b.x) (f a.y b.y) (f a.z b.z)