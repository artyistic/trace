{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Point where

import Graphics.Vec3

-- a point is just a vec3
newtype Point = Point {pt :: V3}
  deriving (Vec3)