{-# LANGUAGE ExistentialQuantification #-}
module Hittables where

import Hittable
import Interval
import qualified Data.Vector as V
import Control.Applicative
import Graphics.Ray

newtype HittableList = HittableList (V.Vector Hittable)

makeHittableList :: [Hittable] -> HittableList
makeHittableList l = HittableList $ V.fromList l

{-# INLINE hitWorld #-}
hitWorld :: HittableList -> Ray -> Interval -> Maybe (HitRecord, Material)
hitWorld (HittableList hl) r (Interval tMin tMax) = V.foldl' step Nothing hl
  where
    step :: Maybe (HitRecord, Material) -> Hittable -> Maybe (HitRecord, Material)
    step closest (Hittable hit) =
      let currTMax = maybe tMax (hitT . fst) closest
      in hit r (Interval tMin currTMax ) <|> closest
