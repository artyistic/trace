-- {-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}
module BVH where
import Hittable
import AABB
import qualified Data.Vector as V
import Data.List (sortBy)
import Data.Function
import Control.Monad.ST (runST)
import qualified Data.Vector.Algorithms.Intro as Intro
import Interval
import Graphics (Ray)
import Control.Applicative ((<|>))
import Control.Monad.Random

type Hittables = (V.Vector Hittable)

data BVHNode = InternalNode !AABB !BVHNode !BVHNode
             | LeafNode !Hittable
             | Empty

instance Show BVHNode where
  show bvh = case bvh of
    Empty -> ""
    LeafNode _ -> "Leaf"
    InternalNode b l r -> "(" ++ "Internal" ++ show l ++ show r ++ ")"


buildBoundingBox :: Hittables -> AABB
buildBoundingBox = foldr (aabbFromBoxes . bounding_box) aabbEmpty

bvhFromList :: [Hittable] -> BVHNode
bvhFromList l = fromHittables $ V.fromList l

fromHittables :: Hittables -> BVHNode
fromHittables l = do
  case length l of
    0 -> Empty
    1 -> LeafNode (l V.! 0)
    _ ->
      let
          sortedHittables = sortVectorBy comparator l
          midPt = length l `div` 2
          (fstHalf, sndHalf) = V.splitAt midPt sortedHittables
      in InternalNode bvhAABB (fromHittables fstHalf) (fromHittables sndHalf)
  where bvhAABB = buildBoundingBox l
        comparator = compareOnLongestAxis bvhAABB `on` bounding_box

sortVectorBy :: (a -> a -> Ordering) -> V.Vector a -> V.Vector a
sortVectorBy cmp vec = runST $ do
  mvec <- V.thaw vec                     -- Make a mutable copy
  Intro.sortBy cmp mvec                 -- In-place sort
  V.freeze mvec                         -- Return immutable sorted vector

{-# INLINE hitBVH #-}
hitBVH :: BVHNode -> Ray -> Interval -> Maybe (HitRecord, Material)
hitBVH bvh r i@(Interval tMin tMax) = case bvh of
  InternalNode box left right ->
    if collision box r i
      then
        let !hitLeft = hitBVH left r i
            !hitRightTMax = maybe tMax (hitT . fst) hitLeft
            !hitRight = hitBVH right r (Interval tMin hitRightTMax)
        in hitRight <|> hitLeft
      else Nothing
  LeafNode h -> hit h r i
  Empty -> Nothing