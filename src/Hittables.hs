{-# LANGUAGE ExistentialQuantification #-}
module Hittables where

import Hittable
import Interval

newtype HittableList = HittableList [SomeHittable]

data SomeHittable = forall a. Hittable a => SomeHittable a

makeHittableList :: Hittable a => [a] -> HittableList
makeHittableList l = HittableList $ map SomeHittable l

instance Hittable HittableList where
  hit (HittableList hl) r (Interval tMin tMax) = scan hl Nothing tMax
    where scan :: [SomeHittable] -> Maybe HitRecord -> Double -> Maybe HitRecord
          scan [] closest _   = closest
          scan ((SomeHittable obj) : rest) closest tMax'
            = case hit obj r (Interval tMin tMax') of
              Just hr -> scan rest (Just hr) (hitT hr)
              Nothing -> scan rest closest tMax'