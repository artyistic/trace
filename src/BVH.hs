module BVH where

import AABB
import Control.Applicative ((<|>))
import Control.Monad.Random
import Data.Function
import Data.List (sortBy)
import Graphics (Ray)
import HitRecord
import Hittable
import Interval
import Material
import Data.Foldable

data BVHNode
  = InternalNode !AABB !BVHNode !BVHNode
  | LeafNode !Hittable
  | Empty

instance Show BVHNode where
  show bvh = case bvh of
    Empty -> ""
    LeafNode _ -> "Leaf"
    InternalNode b l r -> "(" ++ "Internal" ++ show l ++ show r ++ ")"

bvhFromList :: [Hittable] -> Hittable
bvhFromList l =
  let !node = fromHittables l
  in Hittable
    { hit = hitBVH node,
      bbox = nodeBBox node
    }
  where
    nodeBBox :: BVHNode -> AABB
    nodeBBox (InternalNode box _ _) = box
    nodeBBox (LeafNode h) = h.bbox
    nodeBBox Empty = aabbEmpty

fromHittables :: [Hittable] -> BVHNode
fromHittables []  = Empty
fromHittables [x] = LeafNode x
fromHittables xs  = InternalNode bvhAABB (fromHittables fstHalf) (fromHittables sndHalf)
  where
    bvhAABB              = buildBoundingBox xs
    sorted               = sortBy (compareOnLongestAxis bvhAABB `on` (.bbox)) xs
    midPt                = length xs `div` 2
    (fstHalf, sndHalf)   = splitAt midPt sorted

buildBoundingBox :: [Hittable] -> AABB
buildBoundingBox = foldl' (\acc h -> aabbFromBoxes acc h.bbox) aabbEmpty

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