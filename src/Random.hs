module Random where
import Control.Monad.Random
import Graphics.Vec3
import Control.Monad.Loops
import qualified Interval as I

-- Returns a vector to a random point in the [-.5,-.5]-[+.5,+.5] unit square on viewport
-- per documentation, the range of random of Doubles lies in [0, 1)
{-# INLINE getSampleSquare #-}
getSampleSquare :: Rand StdGen V3
getSampleSquare = liftM3 V3 (getRandomR (-0.5, 0.5)) (getRandomR (-0.5, 0.5)) (pure 0)

-- returns a unit cube vector
{-# INLINE getRandomVec #-}
getRandomVec :: Double -> Double -> Rand StdGen V3
getRandomVec min max = liftM3 V3 (getRandomR (min, max)) (getRandomR (min, max)) (getRandomR (min, max))

-- return a unit sphere vector, by rejection method
-- ie keep finding until the normalized vector is within sphere
{-# INLINE getRandomUnitBallVec #-}
getRandomUnitBallVec :: Rand StdGen V3
getRandomUnitBallVec = (\x -> x ./ sqrt (lengthSquared x)) <$> iterateUntil inUnitBall (getRandomVec (-1) 1)
  where
    inUnitBall = I.contains (I.Interval 1e-160 1) . lengthSquared

{-# INLINE getRandomInUnitDisk #-}

getRandomInUnitDisk :: Rand StdGen V3
getRandomInUnitDisk = iterateUntil inUnitDisk (liftM3 V3 (getRandomR (-1, 1)) (getRandomR (-1, 1)) (pure 0))
  where
    inUnitDisk = I.contains (I.Interval 1e-160 1) . lengthSquared

-- ensure the vector has length at most one, and 1e-160 is a safe lower bound
-- to prevent funny underflowing which leads to catastrophic divide by zero
{-# INLINE getRandomOnHemisphere #-}
getRandomOnHemisphere :: V3 -> Rand StdGen V3
getRandomOnHemisphere normal = fmap orient getRandomUnitBallVec
  where
    orient v = if (v .* normal) > 0 then v else invert v

{-# INLINE getRandomDouble #-}
getRandomDouble :: Rand StdGen Double
-- getRandomDouble = getRandomR (0, 1)
getRandomDouble = getRandom