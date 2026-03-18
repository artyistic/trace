module Main where

import Camera
import Scenes
import Graphics
import Render
import Control.Monad.Random
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

data RenderConfig = RenderConfig
  { rcImageWidth      :: Int,
    rcSamplesPerPixel :: Int,
    rcNumBounces      :: Int
  }

parseArgs :: [String] -> Either String RenderConfig
parseArgs [w, s, b] = case (readMaybe w, readMaybe s, readMaybe b) of
  (Just w', Just s', Just b') -> Right $ RenderConfig w' s' b'
  _                           -> Left "All arguments must be integers"
parseArgs _ = Left "Usage: tracerays <width> <samplesPerPixel> <numBounces>"

main :: IO ()
main = do
  args <- getArgs
  cfg  <- case parseArgs args of
    Left err -> putStrLn err >> exitFailure
    Right c  -> return c

  gen <- getStdGen
  let world = evalRand bigWorld gen
      cam   = makeCamera defaultCameraConfig
        { cfgAspectRatio     = 16.0 / 9.0,
          cfgVfov            = 20,
          cfgLookFrom        = V3 13 2 3,
          cfgLookAt          = V3 0 0 0,
          cfgVup             = V3 0 1 0,
          cfgDefocusAngle    = 0.6,
          cfgFocusDistance   = 10.0,
          cfgImageWidth      = rcImageWidth cfg,
          cfgSamplesPerPixel = rcSamplesPerPixel cfg
        }

  render "./output/test.ppm" world cam (rcNumBounces cfg)