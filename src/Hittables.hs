{-# LANGUAGE ExistentialQuantification #-}
module Hittables where

import Hittable
import Interval
-- newtype HittableList = HittableList [SomeHittable]

-- data SomeHittable = forall a. Hittable a => SomeHittable a

-- makeHittableList :: Hittable a => [a] -> HittableList
-- makeHittableList l = HittableList $ map SomeHittable l

-- instance Hittable HittableList where
  -- hit (HittableList hl) r (Interval tMin tMax) = scan hl Nothing tMax
  --   where scan :: [SomeHittable] -> Maybe HitRecord -> Double -> Maybe HitRecord
  --         scan [] closest _   = closest
  --         scan ((SomeHittable obj) : rest) closest tMax'
  --           = case hit obj r (Interval tMin tMax') of
  --             Just hr -> scan rest (Just hr) (hitT hr)
  --             Nothing -> scan rest closest tMax'

import qualified Data.Vector as V
import Control.Applicative
import Graphics.Ray

newtype HittableList = HittableList (V.Vector Hittable)

-- data SomeHittable = forall a. Hittable a => SomeHittable a

makeHittableList :: [Hittable] -> HittableList
makeHittableList l = HittableList $ V.fromList l

{-# INLINE hitWorld #-}
hitWorld :: HittableList -> Ray -> Interval -> Maybe HitRecord
hitWorld (HittableList hl) r (Interval tMin tMax) = V.foldl' step Nothing hl
  where
    step :: Maybe HitRecord -> Hittable -> Maybe HitRecord
    step closest (Hittable hit) =
      let currTMax = maybe tMax hitT closest
      in hit r (Interval tMin currTMax ) <|> closest

-- instance Hittable HittableList where
--   {-# INLINE hit #-}
--   hit (HittableList hl) r (Interval tMin tMax) = V.foldl' step Nothing hl
--     where
--       step :: Maybe HitRecord -> SomeHittable -> Maybe HitRecord
--       step closest (SomeHittable obj) =
--         let currentTMax = maybe tMax hitT closest
--         in (hit obj r (Interval tMin currentTMax) <|> closest)
