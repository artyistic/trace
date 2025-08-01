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
    (V3 aX aY aZ) = pa
    (V3 bX bY bZ) = pb
    chooseInterval i j = if i <= j then Interval i j else Interval j i

aabbFromBoxes :: AABB -> AABB -> AABB
aabbFromBoxes (AABB boxAx boxAy boxAz) (AABB boxBx boxBy boxBz) =
  AABB
    (combineIntervals boxAx boxBx)
    (combineIntervals boxAy boxBy)
    (combineIntervals boxAz boxBz)



-- hit gets a ray and the corresponding interval where such ray is considered
-- ie the ray r is a ray from r at tMin to r at tMax
{-# INLINE collision #-}
collision :: AABB -> Ray -> Interval -> Bool
collision AABB {..} (Ray rO rD _) rI =
  isJust (evalStateT (mapM_ (liesInAxis rO) l) rI) -- ignoring result since there is no result, ()
  where
    liesInAxis :: V3 -> (Interval, Double, Double) -> StateT Interval Maybe ()
    liesInAxis rOrigin (Interval axisMin axisMax, rDirAxis, rOrigAxis) =
      StateT
        ( \(Interval tMin tMax) ->
            let !rDirAxisInv = 1.0 / rDirAxis
                !t0 = (axisMin - rOrigAxis) * rDirAxisInv
                !t1 = (axisMax - rOrigAxis) * rDirAxisInv
                !newTMin = max tMin (min t0 t1) -- min t0 t1 to ensure t0 is the min
                !newTMax = min tMax (max t0 t1) -- max is same idea as before
             in if newTMin < newTMax
                  then Just ((), Interval newTMin newTMax)
                  -- () is the result, ie no result
                  -- Interval tMin tMax is the new Interval we care about,
                  -- this is passed as the state

                  else Nothing -- Nothing for early exit
        )

    (!rDx, !rDy, !rDz) = toXYZ rD
    (!rOx, !rOy, !rOz) = toXYZ rO
    !l = zip3 [aabbX, aabbY, aabbZ] [rDx, rDy, rDz] [rOx, rOy, rOz]
    -- per axis tuples, replaces the for loop in cpp

compareOnLongestAxis :: AABB -> (AABB -> AABB -> Ordering)
compareOnLongestAxis (AABB x y z) =
  snd $ maximumBy (compare `on` fst) l
  where
    compareBy f = compare `on` f
    l = zip [x, y, z] [compareBy aabbX, compareBy aabbY, compareBy aabbZ]


randAxisCompare :: Rand StdGen (AABB -> AABB -> Ordering)
randAxisCompare = do
  randAxis <- fromList (zip [0,1,2] [1,1,1]) :: Rand StdGen Int
  case randAxis of
    0 -> return $ compareBy aabbX
    1 -> return $ compareBy aabbY
    2 -> return $ compareBy aabbZ
  where 
    compareBy f a b = compare (f a) (f b)
