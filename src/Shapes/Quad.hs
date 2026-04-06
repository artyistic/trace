module Shapes.Quad where

import AABB (aabbFromBoxes, aabbFromPoints)
import Control.Monad (guard)
import Graphics
import HitRecord (genHitRecord)
import Hittable
import Interval (Interval (Interval), contains)
import Material

-- |
-- Quad is defined by
-- pq, the starting corner.
--
-- u, a vector representing the first side. Q+u
-- gives one of the corners adjacent to Q
--
-- v, a vector representing the second side. Q+v
-- gives the other corner adjacent to Q
quad :: V3 -> V3 -> V3 -> Material -> Hittable
quad pq u v mat =
  let bboxDiag1 = aabbFromPoints pq (pq <+> u <+> v) -- will be padded
      bboxDiag2 = aabbFromPoints (pq <+> u) (pq <+> v)
      n = u >< v
      unitN = normalize n
      d = unitN .* pq
      w = n ./ (n .* n)
   in Hittable
        { hit = quadHit pq u v unitN d w mat,
          bbox = aabbFromBoxes bboxDiag1 bboxDiag2
        }

{-# INLINE quadHit #-}
quadHit :: V3 -> V3 -> V3 -> V3 -> Double -> V3 -> Material -> HitFun
quadHit pq u v unitN d w mat r@(Ray orig dir time) i = do
  let denom = unitN .* dir
      t = (d - unitN .* orig) / denom
      intersection = at r t
      planarHitPt = intersection <-> pq
      alpha = w .* (planarHitPt >< v)
      beta = w .* (u >< planarHitPt)
      isUnit = contains $ Interval 0 1
      hr = genHitRecord r intersection t unitN alpha beta

  guard (abs denom > 1e-8)
  guard (contains i t)
  guard (isUnit alpha)
  guard (isUnit beta)

  return (hr, mat)