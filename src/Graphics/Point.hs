{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Point (Point, toV3, fromV3, evalPoint, fromCoord) where

import Graphics.Vec3

-- a point is just a vec3
newtype Point = Point V3
  deriving (Vec3)

fromCoord :: Double -> Double -> Double -> Point
fromCoord = ((Point .) .) . V3

-- conversion helpers
toV3 :: Point -> V3
toV3 (Point v) = v

fromV3 :: V3 -> Point
fromV3 = Point

-- evaluation of point and V3, resulting into point
evalPoint :: Point -> (V3 -> V3) -> Point
evalPoint p f = fromV3 $ f $ toV3 p 