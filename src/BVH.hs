module BVH where

import AABB
import Control.Applicative ((<|>))
import Control.Monad.Random
import Control.Monad.ST (runST)
import Control.Monad.Trans.Maybe
import Data.Function
import Data.List (sortBy)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import Graphics (Ray)
import HitRecord
import Hittable
import Interval
import Material

type Hittables = (V.Vector Hittable)

data BVHNode
  = InternalNode !AABB !BVHNode !BVHNode
  | LeafNode !Hittable
  | Empty

instance Show BVHNode where
  show bvh = case bvh of
    Empty -> ""
    LeafNode _ -> "Leaf"
    InternalNode b l r -> "(" ++ "Internal" ++ show l ++ show r ++ ")"

buildBoundingBox :: Hittables -> AABB
buildBoundingBox = V.foldl' (\acc h -> aabbFromBoxes acc h.bbox) aabbEmpty

bvhFromList :: [Hittable] -> Hittable
bvhFromList l =
  Hittable
    { hit = hitBVH node,
      bbox = nodeBBox node
    }
  where
    node = fromHittables $ V.fromList l

    nodeBBox :: BVHNode -> AABB
    nodeBBox (InternalNode box _ _) = box
    nodeBBox (LeafNode h) = h.bbox
    nodeBBox Empty = aabbEmpty

fromHittables :: Hittables -> BVHNode
fromHittables l = case V.length l of
  0 -> Empty
  1 -> LeafNode (l V.! 0)
  _ -> InternalNode bvhAABB (fromHittables fstHalf) (fromHittables sndHalf)
    where
      sortedHittables = sortVectorBy comparator l
      midPt = V.length l `div` 2
      (fstHalf, sndHalf) = V.splitAt midPt sortedHittables
  where
    bvhAABB = buildBoundingBox l
    comparator = compareOnLongestAxis bvhAABB `on` (.bbox)

sortVectorBy :: (a -> a -> Ordering) -> V.Vector a -> V.Vector a
sortVectorBy cmp vec = runST $ do
  mvec <- V.thaw vec -- Make a mutable copy
  Intro.sortBy cmp mvec -- In-place sort
  V.freeze mvec -- Return immutable sorted vector

{-# INLINE hitBVH #-}
hitBVH :: BVHNode -> Ray -> Interval -> Maybe (HitRecord, Material)
hitBVH bvh r i@(Interval tMin tMax) = case bvh of
  InternalNode box left right ->
    if collision box r i
      then
        let lHit = hitBVH left r i
            prunedTMax = maybe tMax ((.t) . fst) lHit
            rHit = hitBVH right r (Interval tMin prunedTMax)
         in rHit <|> lHit
      else Nothing
  LeafNode h -> h.hit r i
  Empty -> Nothing