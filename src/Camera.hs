module Camera (Camera, camera, render, rayColor) where

import Codec.Picture as P
import qualified Codec.Picture as P
import Codec.Picture.Types
import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.ST
import Data.Bool (bool)
import Data.Function
import Graphics
import Hittable
import Hittables
import qualified Interval as I
import Shapes.Sphere
import System.Random

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

render :: FilePath -> HittableList -> Camera -> StdGen -> IO ()
render
  fpath
  world
  ( Camera
      _
      imageWidth
      samplesPerPixel
      cameraCenter
      imageHeight
      pixelDu
      pixelDv
      pixel00Loc
    )
  gen =
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
      pixelRenderer x y world' =
        sampleSquares
          >>= ( fmap (gammaCorrected . averageColor)
                  . traverse
                    -- this here takes a sampled square and shoot rays with raycolor
                    ( \(V3 offsetX offsetY _) ->
                        rayColor
                          ( Ray
                              cameraCenter
                              ( toV3 $
                                  pixelCenter (fromIntegral x + offsetX) (fromIntegral y + offsetY)
                                    <-> cameraCenter
                              )
                          )
                          world'
                          10
                    )
              )

      sampleSquares = replicateM samplesPerPixel getSampleSquare
      pixelCenter x' y' = evalPoint pixel00Loc (\p -> p <+> pixelDu .^ x' <+> pixelDv .^ y')

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