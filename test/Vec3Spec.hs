module Vec3Spec where

import Test.QuickCheck
import Graphics
import Control.Applicative (liftA3)

instance Arbitrary V3 where
  arbitrary = liftA3 V3 arbitrary arbitrary arbitrary


-- V3 properties
prop_addCommutative :: V3 -> V3 -> Bool
prop_addCommutative a b = a <+> b == b <+> a

prop_dotSelf :: V3 -> Bool
prop_dotSelf v = v .* v >= 0  -- dot with self always non-negative

prop_normalizeUnit :: V3 -> Property
prop_normalizeUnit v = 
  lengthSquared v > 1e-10 ==>  -- guard against zero vector
  abs (norm (normalize v) - 1.0) < 1e-10
