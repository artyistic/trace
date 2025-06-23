module Hittable where

import Graphics.Ray
class Hittable a where
  hit :: a -> Ray -> Bool
