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
  { aabbX :: !Interval,
    aabbY :: !Interval,
    aabbZ :: !Interval
  }
  deriving Show

aabbEmpty :: AABB
aabbEmpty = AABB empty empty empty

aabbFromInterval :: Interval -> Interval -> Interval -> AABB
aabbFromInterval = AABB

aabbFromPoints :: V3 -> V3 -> AABB
aabbFromPoints pa pb =
  AABB
    (chooseInterval aX bX)
    (chooseInterval aY bY)
    (chooseInterval aZ bZ)
  where
    (aX, aY, aZ) = toXYZ pa
    (bX, bY, bZ) = toXYZ pb
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
      !tx0   = (aabbX.minVal - rOx) * invDx
      !tx1   = (aabbX.maxVal - rOx) * invDx
      !tMin1 = max tMin (min tx0 tx1)
      !tMax1 = min tMax (max tx0 tx1)

      !invDy = 1.0 / rDy
      !ty0   = (aabbY.minVal - rOy) * invDy
      !ty1   = (aabbY.maxVal - rOy) * invDy
      !tMin2 = max tMin1 (min ty0 ty1)
      !tMax2 = min tMax1 (max ty0 ty1)

      !invDz = 1.0 / rDz
      !tz0   = (aabbZ.minVal - rOz) * invDz
      !tz1   = (aabbZ.maxVal - rOz) * invDz
      !tMin3 = max tMin2 (min tz0 tz1)
      !tMax3 = min tMax2 (max tz0 tz1)

  in tMin3 < tMax3

compareOnLongestAxis :: AABB -> (AABB -> AABB -> Ordering)
compareOnLongestAxis (AABB x y z) = compare `on` accessor
  where
    accessor = snd $ maximumBy (comparing fst) [(size x, (.aabbX)), (size y, (.aabbY)), (size z, (.aabbZ))]
