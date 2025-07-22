module Camera (Camera, camera, render) where

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
  { aspectRatio :: Double,
    imageWidth :: Int,
    samplesPerPixel :: Int,

    -- when intialized, user will gve lookFrom lookAt and vup
    center :: Point, -- will be lookFrom
    -- derived from above
    imageHeight :: Int,
    pixelDu :: V3,
    pixelDv :: V3,
    pixel00Loc :: Point
  }

-- init a camera
camera :: Double -> Int -> Int -> Double -> Point -> Point -> V3 -> Camera
camera aspectRatio imageWidth samplesPerPixel vfov lookFrom lookAt vup =
  Camera
    aspectRatio
    imageWidth
    samplesPerPixel
    cameraCenter
    imageHeight
    pixelDu
    pixelDv
    pixel00Loc
  where
    imageHeight = max (floor $ fromIntegral imageWidth / aspectRatio) 1

    focalLength = distance lookFrom lookAt

    -- vfov
    thetaRad = vfov * (pi / 180)
    h = tan $ thetaRad / 2
    viewportHeight = 2.0 * h * focalLength
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
              <-> (w .^ focalLength)
              <-> viewportU .^ 0.5
              <-> viewportV .^ 0.5
        )
    pixel00Loc = evalPoint viewportUpperLeft (<+> (pixelDu <+> pixelDv) .^ 0.5)

render :: FilePath -> HittableList -> Camera -> StdGen -> Int -> IO ()
render
  fpath
  world
  cam@( Camera
      _
      imageWidth
      samplesPerPixel
      cameraCenter
      imageHeight
      pixelDu
      pixelDv
      pixel00Loc
    )
  gen
  numBounces =
    do
      let pixels = [pixelRenderer x y world | y <- [0 .. imageHeight - 1], x <- [0 .. imageWidth - 1]]
      t <- evalRandIO (sequenceA pixels)
      writeFile fpath $
        "P3\n"
          ++ show imageWidth
          ++ " "
          ++ show imageHeight
          ++ "\n255\n"
          ++ foldr (\x y -> colorToRGBString x ++ "\n" ++ y) "" t
    where
      -- for each x y position on viewPort, randomly sample pixels to construct a smooth edge
      -- obviously this does 10x work for each every pixel, super slow

      -- replicateM makes a random array of position offsets,
      -- then we map rayColors, average and gammaCorrect it
      pixelRenderer :: Int -> Int -> HittableList -> Rand StdGen Color
      pixelRenderer x y world' = do
        let sampleColor v = rayColor
                    (shootRay cameraCenter x y v cam)
                    world'
                    numBounces -- this ray trace a particular sample
        sampleSquares <- replicateM samplesPerPixel getSampleSquare
        gammaCorrected . averageColor <$> traverse sampleColor sampleSquares
      sampleSquares = replicateM samplesPerPixel getSampleSquare -- generate sample for each pixels

shootRay :: Point -> Int -> Int -> V3 -> Camera-> Ray
shootRay cameraCenter x y (V3 offsetX offsetY _) cam =
  let
    rayOrigin = cameraCenter
    rayDirection = toV3 $ pixelCenter (fromIntegral x + offsetX) (fromIntegral y + offsetY) cam
                    <-> rayOrigin
  in Ray rayOrigin rayDirection

pixelCenter :: Double -> Double -> Camera -> Point
pixelCenter x' y' (Camera _ _ _ _ _ pixelDu pixelDv pixel00Loc) = evalPoint pixel00Loc (\p -> p <+> pixelDu .^ x' <+> pixelDv .^ y')

rayColor :: Ray -> HittableList -> Int -> Rand StdGen Color
rayColor r@(Ray _ direction) world depth =
  if depth <= 0
    then pure $ color 0 0 0
    else
      maybe (pure (p1 <+> p2)) trace hitRecord
  where
    p1 = white .^ (1.0 - a)
    p2 = lightBlue .^ a
    a = 0.5 * (toY (normalize direction) + 1)
    hitRecord = hit world r (I.Interval 0.001 (1 / 0))
    trace hR = do
      -- let normal = hitNormal hR
      --     hitPt = hitP hR
      -- d <- getRandomOnHemisphere normal
      -- (.^ 0.5) <$> rayColor (Ray hitPt (d <+> normal)) world (depth - 1)
      let mat = hitMaterial hR
      scatterResult <- matScatter mat r hR
      maybe
        (pure $ color 0 0 0)
        (\(attenuation, scattered) -> (attenuation `componentMul`) <$> rayColor scattered world (depth - 1))
        scatterResult

-- d + normal for lambertian sphere

-- colors for background
white :: Color
white = color 1 1 1

lightBlue :: Color
lightBlue = color 0.5 0.7 1.0

pink :: Color
pink = color 1 0 0.906