-- test/Spec.hs
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Vec3Spec
import IntervalSpec
main :: IO ()
main = defaultMain $ testGroup "tracerays"
  [ testGroup "Vec3"
      [ testProperty "add commutative"  prop_addCommutative
      , testProperty "dot self >= 0"    prop_dotSelf
      , testProperty "normalize unit"   prop_normalizeUnit
      ]
  , testGroup "Interval"
      [ testProperty "contains midpoint" prop_containsInsideAlways
      ]
  -- , testGroup "Sphere"
  --     [ testCase "ray hits sphere"  testSphereHit
  --     , testCase "ray misses sphere" testSphereMiss
  --     ]
  ]