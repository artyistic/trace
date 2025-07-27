module Render (render) where
import Hittables
import Camera
import Control.Monad.Random
import Graphics.Point
import Graphics
import Hittable
import qualified Interval as I
import Random

render :: FilePath -> HittableList -> Camera -> StdGen -> Int -> IO ()
render
  fpath
  world
  cam
  gen
  numBounces =
    do
      let
        pixels =
            [ pixelRenderer x y world
              | y <- [0 .. imageHeight - 1],
                x <- [0 .. imageWidth - 1]
            ]
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

      -- replicateM makes a random array of position offsets,
      -- then we map rayColors, average and gammaCorrect it
      imageWidth = camImageWidth cam
      samplesPerPixel = camSamplesPerPixel cam
      defocusAngle = camDefocusAngle cam
      focusDistance = camFocusDistance cam
      defocusDiskU = camDefocusDiskU cam
      defocusDiskV = camDefocusDiskV cam
      cameraCenter = camCenter cam
      imageHeight = camImageHeight cam
      pixelDu = camPixelDu cam
      pixelDv = camPixelDv cam
      pixel00Loc = camPixel00Loc cam

      pixelRenderer :: Int -> Int -> HittableList -> Rand StdGen Color
      pixelRenderer x y world' = do
        sampleSquares <- replicateM samplesPerPixel getSampleSquare
        let
          sampleColor v = do
              d <- sampleDefocusDisk cam
              let rayOrigin = if defocusAngle <= 0 then cameraCenter else d
              rayIn <- shootRay rayOrigin x y v cam -- a randomTime for moving spheres (random)
              rayColor
                rayIn
                world'
                numBounces -- this ray trace a particular sample
        gammaCorrected . averageColor <$> traverse sampleColor sampleSquares

shootRay :: Point -> Int -> Int -> V3 -> Camera -> Rand StdGen Ray
shootRay rayOrigin x y (V3 offsetX offsetY _) cam = do
  let rayDirection =
        toV3 $
          pixelCenter (fromIntegral x + offsetX) (fromIntegral y + offsetY) cam
            <-> rayOrigin
      randomRayTime = getRandomR (0, 1) :: Rand StdGen Double
  Ray rayOrigin rayDirection <$> randomRayTime

pixelCenter :: Double -> Double -> Camera -> Point
pixelCenter x' y' cam =
  let
    pixelDu = camPixelDu cam
    pixelDv = camPixelDv cam
    pixel00Loc = camPixel00Loc cam
  in
  evalPoint pixel00Loc (\p -> p <+> pixelDu .^ x' <+> pixelDv .^ y')

sampleDefocusDisk :: Camera -> Rand StdGen Point
sampleDefocusDisk cam = do
  let
      diskU = camDefocusDiskU cam
      diskV = camDefocusDiskV cam
      c = camCenter cam
  (V3 pX pY _) <- getRandomInUnitDisk
  return $ evalPoint c (\c -> c <+> diskU .^ pX <+> diskV .^ pY)

{-# INLINE rayColor #-}
rayColor :: Ray -> HittableList -> Int -> Rand StdGen Color
rayColor r@(Ray _ direction _) world depth =
  if depth <= 0
    then pure $ color 0 0 0
    else
      maybe (pure (p1 <+> p2)) trace hitResult
  where
    p1 = white .^ (1.0 - a)
    p2 = lightBlue .^ a
    a = 0.5 * (toY (normalize direction) + 1)
    hitResult = hitWorld world r (I.Interval 0.001 (1 / 0))
    trace hR = do
      let mat = snd hR
          hitRecord = fst hR
      scatterResult <- scatter mat r hitRecord
      maybe
        (pure $ color 0 0 0)
        ( \(attenuation, scattered) ->
            (attenuation `componentMul`) <$> rayColor scattered world (depth - 1)
        )
        scatterResult

    -- colors for background
    white = color 1 1 1
    lightBlue = color 0.5 0.7 1.0
    pink = color 1 0 0.906