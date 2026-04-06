{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module AABB where

import Control.Monad.State.Lazy
import Data.Maybe
import Graphics.Ray
import Graphics.Vec3
import Interval
import Data.Function
import Data.Foldable (maximumBy)
import Control.Monad.Random
import Data.Ord (comparing)

-- An AABB (Axis-Aligned Bounding Box) is three intervals on xyz planes
data AABB = AABB
  { x :: !Interval,
    y :: !Interval,
    z :: !Interval
  }
  deriving Show

aabbEmpty :: AABB
aabbEmpty = AABB empty empty empty

-- | used to pad flat aabb, like quads
pad :: Interval -> Interval
pad i = if size i < delta then expands i delta else i
  where delta = 0.0001

aabbFromInterval :: Interval -> Interval -> Interval -> AABB
aabbFromInterval x y z = AABB (pad x) (pad y) (pad z)
  where
    delta = 0.0001
    pad i = if size i < delta then expands i delta else i

aabbFromPoints :: V3 -> V3 -> AABB
aabbFromPoints (V3 aX aY aZ) (V3 bX bY bZ) =
  AABB
    (pad $ chooseInterval aX bX)
    (pad $ chooseInterval aY bY)
    (pad $ chooseInterval aZ bZ)
  where
    chooseInterval i j = if i <= j then Interval i j else Interval j i

aabbFromBoxes :: AABB -> AABB -> AABB
aabbFromBoxes (AABB boxAx boxAy boxAz) (AABB boxBx boxBy boxBz) =
  AABB
    (combineIntervals boxAx boxBx)
    (combineIntervals boxAy boxBy)
    (combineIntervals boxAz boxBz)

{-# INLINE collision #-}
collision :: AABB -> Ray -> Interval -> Bool
collision AABB{..} (Ray rO rD _) (Interval tMin tMax) =
  let (!rDx, !rDy, !rDz) = toXYZ rD
      (!rOx, !rOy, !rOz) = toXYZ rO

      !invDx = 1.0 / rDx
      !tx0   = (x.minVal - rOx) * invDx
      !tx1   = (x.maxVal - rOx) * invDx
      !tMin1 = max tMin (min tx0 tx1)
      !tMax1 = min tMax (max tx0 tx1)

      !invDy = 1.0 / rDy
      !ty0   = (y.minVal - rOy) * invDy
      !ty1   = (y.maxVal - rOy) * invDy
      !tMin2 = max tMin1 (min ty0 ty1)
      !tMax2 = min tMax1 (max ty0 ty1)

      !invDz = 1.0 / rDz
      !tz0   = (z.minVal - rOz) * invDz
      !tz1   = (z.maxVal - rOz) * invDz
      !tMin3 = max tMin2 (min tz0 tz1)
      !tMax3 = min tMax2 (max tz0 tz1)

  in tMin3 < tMax3

compareOnLongestAxis :: AABB -> (AABB -> AABB -> Ordering)
compareOnLongestAxis (AABB x y z) = compare `on` accessor
  where
    accessor = snd $ maximumBy (comparing fst) [(size x, (.x)), (size y, (.y)), (size z, (.z))]
