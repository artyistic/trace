module Camera
  ( Camera,
    camera,
    camAspectRatio,
    camImageWidth,
    camSamplesPerPixel,
    camDefocusAngle,
    camFocusDistance,
    camDefocusDiskU,
    camDefocusDiskV,
    camCenter,
    camImageHeight,
    camPixelDu,
    camPixelDv,
    camPixel00Loc
  )
where

import Control.Monad.Loops
import Control.Monad.Random
import Data.Bool (bool)
import Data.Function
import Graphics
import Hittable
import Hittables
import qualified Interval as I

-- camera is just defined by aspectRatio, imageWidth, and samplesPerPixel
data Camera = Camera
  { camAspectRatio :: Double,
    camImageWidth :: Int,
    camSamplesPerPixel :: Int,
    camDefocusAngle :: Double,
    camFocusDistance :: Double,
    camDefocusDiskU :: V3,
    camDefocusDiskV :: V3,
    -- when intialized, user will gve lookFrom lookAt and vup
    camCenter :: Point, -- will be lookFrom
    -- derived from above
    camImageHeight :: Int,
    camPixelDu :: V3,
    camPixelDv :: V3,
    camPixel00Loc :: Point
  }

-- init a camera
camera :: Double -> Int -> Int -> Double -> Point -> Point -> V3 -> Double -> Double -> Camera
camera aspectRatio imageWidth samplesPerPixel vfov lookFrom lookAt vup defocusAngle focusDistance =
  Camera
    aspectRatio
    imageWidth
    samplesPerPixel
    defocusAngle
    focusDistance
    defocusDiskU
    defocusDiskV
    cameraCenter
    imageHeight
    pixelDu
    pixelDv
    pixel00Loc
  where
    imageHeight = max (floor $ fromIntegral imageWidth / aspectRatio) 1

    -- vfov
    thetaRad = vfov * (pi / 180)
    h = tan $ thetaRad / 2
    viewportHeight = 2.0 * h * focusDistance
    viewportWidth = viewportHeight * on (/) fromIntegral imageWidth imageHeight

    -- lookFrom is where the camera is positioned
    cameraCenter = lookFrom

    -- u v w are the basis vector for the camera coord frame
    w = toV3 $ normalize (lookFrom <-> lookAt)
    u = normalize (vup >< w)
    v = w >< u

    viewportU = u .^ viewportWidth
    viewportV = invert v .^ viewportHeight

    pixelDu = viewportU .^ (1 / fromIntegral imageWidth)
    pixelDv = viewportV .^ (1 / fromIntegral imageHeight)

    viewportUpperLeft =
      evalPoint
        cameraCenter
        ( \x ->
            x
              <-> (w .^ focusDistance)
              <-> viewportU
              .^ 0.5
              <-> viewportV
              .^ 0.5
        )
    pixel00Loc = evalPoint viewportUpperLeft (<+> (pixelDu <+> pixelDv) .^ 0.5)

    -- camera's defocus basis vectors
    defocusRadius = focusDistance * tan ((defocusAngle / 2) * (pi / 180))
    defocusDiskU = u .^ defocusRadius
    defocusDiskV = v .^ defocusRadius
