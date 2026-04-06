{-# LANGUAGE RecordWildCards #-}

module Camera
  ( Camera (..),
    CameraConfig (..),
    defaultCameraConfig,
    makeCamera
  )
where

import Graphics
import Hittable
import qualified Interval as I

-- | User-provided camera parameters.
-- Use 'defaultCameraConfig' and override only what you need:
--
-- > makeCamera defaultCameraConfig { cfgImageWidth = 1200, cfgVfov = 20 }
data CameraConfig = CameraConfig
  { aspectRatio     :: Double,
    imageWidth      :: Int,
    samplesPerPixel :: Int,
    defocusAngle    :: Double, -- ^ 0 = no defocus blur
    focusDistance   :: Double,
    vfov            :: Double, -- ^ Vertical field of view in degrees
    lookFrom        :: V3,
    lookAt          :: V3,
    vup             :: V3     -- ^ "Up" direction for camera orientation
  }

defaultCameraConfig :: CameraConfig
defaultCameraConfig = CameraConfig
  { aspectRatio     = 16.0 / 9.0,
    imageWidth      = 400,
    samplesPerPixel = 10,
    defocusAngle    = 0.0,
    focusDistance   = 10.0,
    vfov            = 90.0,
    lookFrom        = V3 0 0 0,
    lookAt          = V3 0 0 (-1),
    vup             = V3 0 1 0
  }

-- | Fully initialised camera. Construct with 'makeCamera'.
-- All derived fields are guaranteed valid; never construct directly.
data Camera = Camera
  { config       :: CameraConfig,
    center       :: V3,
    imageHeight  :: Int,
    pixelDu      :: V3,
    pixelDv      :: V3,
    pixel00Loc   :: V3,
    defocusDiskU :: V3,
    defocusDiskV :: V3
  }

-- | Build a 'Camera' from a 'CameraConfig', computing all derived fields.
makeCamera :: CameraConfig -> Camera
makeCamera cfg@CameraConfig{..} = Camera
  { config       = cfg,
    center       = lookFrom,
    imageHeight  = imageHeight,
    pixelDu      = pixelDu,
    pixelDv      = pixelDv,
    pixel00Loc   = pixel00Loc,
    defocusDiskU = defocusDiskU,
    defocusDiskV = defocusDiskV
  }
  where
    imageHeight = max 1 (floor $ fromIntegral imageWidth / aspectRatio)

    -- Viewport dimensions derived from vertical fov
    h              = tan (toRad vfov / 2)
    viewportHeight = 2.0 * h * focusDistance
    viewportWidth  = viewportHeight * fromIntegral imageWidth / fromIntegral imageHeight

    -- Orthonormal basis for camera coordinate frame
    w = normalize (lookFrom <-> lookAt)
    u = normalize (vup >< w)
    v = w >< u

    -- Viewport edge vectors
    viewportU = u            .^ viewportWidth
    viewportV = invert v     .^ viewportHeight

    -- Per-pixel delta vectors
    pixelDu = viewportU .^ (1 / fromIntegral imageWidth)
    pixelDv = viewportV .^ (1 / fromIntegral imageHeight)

    -- Upper-left corner of the viewport
    viewportUpperLeft =
      lookFrom
      <-> (w        .^ focusDistance)
      <-> (viewportU .^ 0.5)
      <-> (viewportV .^ 0.5)

    -- Centre of pixel (0,0)
    pixel00Loc = viewportUpperLeft <+> ((pixelDu <+> pixelDv) .^ 0.5)

    -- Defocus (depth-of-field) disk basis vectors
    defocusRadius = focusDistance * tan (toRad (defocusAngle / 2))
    defocusDiskU  = u .^ defocusRadius
    defocusDiskV  = v .^ defocusRadius

-- | Convert degrees to radians.
toRad :: Double -> Double
toRad deg = deg * (pi / 180)