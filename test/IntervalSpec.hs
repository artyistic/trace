module IntervalSpec where

import Test.QuickCheck
import Interval
-- Interval properties
prop_containsInsideAlways :: Double -> Double -> Double -> Property
prop_containsInsideAlways lo hi x =
  lo < hi ==>
  let mid = (lo + hi) / 2
  in contains (Interval lo hi) mid