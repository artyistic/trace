{-# LANGUAGE RecordWildCards #-}

module Camera
  ( Camera,
    CameraConfig (..),
    defaultCameraConfig,
    makeCamera,
    camConfig,
    camCenter,
    camImageHeight,
    camPixelDu,
    camPixelDv,
    camPixel00Loc,
    camDefocusDiskU,
    camDefocusDiskV
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
  { cfgAspectRatio     :: Double,
    cfgImageWidth      :: Int,
    cfgSamplesPerPixel :: Int,
    cfgDefocusAngle    :: Double, -- ^ 0 = no defocus blur
    cfgFocusDistance   :: Double,
    cfgVfov            :: Double, -- ^ Vertical field of view in degrees
    cfgLookFrom        :: V3,
    cfgLookAt          :: V3,
    cfgVup             :: V3     -- ^ "Up" direction for camera orientation
  }

defaultCameraConfig :: CameraConfig
defaultCameraConfig = CameraConfig
  { cfgAspectRatio     = 16.0 / 9.0,
    cfgImageWidth      = 400,
    cfgSamplesPerPixel = 10,
    cfgDefocusAngle    = 0.0,
    cfgFocusDistance   = 10.0,
    cfgVfov            = 90.0,
    cfgLookFrom        = V3 0 0 0,
    cfgLookAt          = V3 0 0 (-1),
    cfgVup             = V3 0 1 0
  }

-- | Fully initialised camera. Construct with 'makeCamera'.
-- All derived fields are guaranteed valid; never construct directly.
data Camera = Camera
  { camConfig       :: CameraConfig,
    camCenter       :: V3,
    camImageHeight  :: Int,
    camPixelDu      :: V3,
    camPixelDv      :: V3,
    camPixel00Loc   :: V3,
    camDefocusDiskU :: V3,
    camDefocusDiskV :: V3
  }

-- | Build a 'Camera' from a 'CameraConfig', computing all derived fields.
makeCamera :: CameraConfig -> Camera
makeCamera cfg@CameraConfig{..} = Camera
  { camConfig       = cfg,
    camCenter       = cfgLookFrom,
    camImageHeight  = imageHeight,
    camPixelDu      = pixelDu,
    camPixelDv      = pixelDv,
    camPixel00Loc   = pixel00Loc,
    camDefocusDiskU = defocusDiskU,
    camDefocusDiskV = defocusDiskV
  }
  where
    imageHeight = max 1 (floor $ fromIntegral cfgImageWidth / cfgAspectRatio)

    -- Viewport dimensions derived from vertical fov
    h              = tan (toRad cfgVfov / 2)
    viewportHeight = 2.0 * h * cfgFocusDistance
    viewportWidth  = viewportHeight * fromIntegral cfgImageWidth / fromIntegral imageHeight

    -- Orthonormal basis for camera coordinate frame
    w = normalize (cfgLookFrom <-> cfgLookAt)
    u = normalize (cfgVup >< w)
    v = w >< u

    -- Viewport edge vectors
    viewportU = u            .^ viewportWidth
    viewportV = invert v     .^ viewportHeight

    -- Per-pixel delta vectors
    pixelDu = viewportU .^ (1 / fromIntegral cfgImageWidth)
    pixelDv = viewportV .^ (1 / fromIntegral imageHeight)

    -- Upper-left corner of the viewport
    viewportUpperLeft =
      cfgLookFrom
      <-> (w        .^ cfgFocusDistance)
      <-> (viewportU .^ 0.5)
      <-> (viewportV .^ 0.5)

    -- Centre of pixel (0,0)
    pixel00Loc = viewportUpperLeft <+> ((pixelDu <+> pixelDv) .^ 0.5)

    -- Defocus (depth-of-field) disk basis vectors
    defocusRadius = cfgFocusDistance * tan (toRad (cfgDefocusAngle / 2))
    defocusDiskU  = u .^ defocusRadius
    defocusDiskV  = v .^ defocusRadius

-- | Convert degrees to radians.
toRad :: Double -> Double
toRad deg = deg * (pi / 180)