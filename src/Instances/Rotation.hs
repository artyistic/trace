module Instances.Rotation where

import Graphics
import Hittable
import HitRecord
import Data.Bifunctor (Bifunctor(first))
import AABB
import Data.List (foldl')
import Interval

rotateY :: Hittable -> Double -> Hittable
rotateY primitive deg =
  Hittable
    { hit = \r i -> do
        let
          rotatedRay = Ray (rot r.orig) (rot r.dir) r.time

        res <- primitive.hit rotatedRay i

        Just $ first (\hr -> hr {p = rotInv hr.p, normal = rotInv hr.normal}) res,
      bbox = fromPoints $ map rotInv (corners primitive.bbox) 

    }
  where
    theta = deg * pi / 180
    cosTheta = cos theta
    sinTheta = sin theta
    rot :: V3 -> V3
    rot p = V3 (cosTheta * p.x - sinTheta * p.z) p.y (sinTheta * p.x + cosTheta * p.z)
    rotInv :: V3 -> V3
    rotInv p = V3 (cosTheta * p.x + sinTheta * p.z) p.y (- (sinTheta * p.x) + cosTheta * p.z)

fromPoints :: [V3] -> AABB
fromPoints pts =
  let 
    inf = 1/0
    negInf = -inf

    step (V3 xmin ymin zmin, V3 xmax ymax zmax) (V3 x y z) =
      ( V3 (min xmin x) (min ymin y) (min zmin z)
      , V3 (max xmax x) (max ymax y) (max zmax z)
      )

    (V3 xmin ymin zmin, V3 xmax ymax zmax) =
      foldl' step (V3 inf inf inf, V3 negInf negInf negInf) pts

  in AABB
      (Interval xmin xmax)
      (Interval ymin ymax)
      (Interval zmin zmax)

corners :: AABB -> [V3]
corners (AABB (Interval xmin xmax)
              (Interval ymin ymax)
              (Interval zmin zmax)) =
  [ V3 x y z
  | x <- [xmin, xmax]
  , y <- [ymin, ymax]
  , z <- [zmin, zmax]
  ]