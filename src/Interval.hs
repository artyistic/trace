module Interval where

-- DOUBLES ARE NOT BOUNDED BY HASKELL
-- THIS ASSUMES NAN WONT HAPPEN AND INF IS 1/0
-- Represents a closed interval [min, max]
data Interval = Interval
  { minVal :: Double
  , maxVal :: Double
  } deriving (Eq, Show)

instance Ord Interval where
  compare (Interval aMin _) (Interval bMin _) = compare aMin bMin


-- Default: empty interval [∞, -∞]
empty :: Interval
empty = Interval (1 / 0) (- (1 / 0))

-- Universe: [-∞, ∞]
universe :: Interval
universe = Interval (- (1 / 0)) (1 / 0)

-- Size of the interval (can be negative for empty)
size :: Interval -> Double
size (Interval lo hi) = hi - lo

-- Inclusive containment: x ∈ [min, max]
contains :: Interval -> Double -> Bool
contains (Interval lo hi) x = lo <= x && x <= hi

-- Exclusive containment: x ∈ (min, max)
surrounds :: Interval -> Double -> Bool
surrounds (Interval lo hi) x = lo < x && x < hi

-- pads an interval by delta
expands :: Interval -> Double -> Interval
expands (Interval min max) delta = Interval (min - padding) (max + padding)
  where padding = delta / 2

-- Create the interval tightly enclosing the two input intervals.
combineIntervals :: Interval -> Interval -> Interval
combineIntervals (Interval aMin aMax) (Interval bMin bMax) =
  Interval {
    minVal = min aMin bMin,
    maxVal = max aMax bMax
  }
