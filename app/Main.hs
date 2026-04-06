module Main where

import Camera
import Control.Monad.Random
import Render
import Scenes
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Read (readMaybe)

data RenderConfig = RenderConfig
  { rcSceneIndex :: Maybe Int, -- Nothing = prompt user
    rcImageWidth :: Int,
    rcSamplesPerPixel :: Int,
    rcNumBounces :: Int
  }

parseArgs :: [String] -> Either String RenderConfig
parseArgs [w, s, b] = case (readMaybe w, readMaybe s, readMaybe b) of
  (Just w', Just s', Just b') -> Right $ RenderConfig Nothing w' s' b'
  _ -> Left "All arguments must be integers"
parseArgs [sc, w, s, b] = case (readMaybe sc, readMaybe w, readMaybe s, readMaybe b) of
  (Just sc', Just w', Just s', Just b')
    | sc' < 0 || sc' >= length scenes -> Left $ "Scene index out of range (0-" ++ show (length scenes - 1) ++ ")"
    | otherwise -> Right $ RenderConfig (Just sc') w' s' b'
  _ -> Left "All arguments must be integers"
parseArgs _ = Left "Usage: tracerays [sceneIndex] <width> <spp> <bounces>"

main :: IO ()
main = do
  args <- getArgs
  cfg <- case parseArgs args of
    Left err -> putStrLn err >> exitFailure
    Right c -> return c
  mScene <- case cfg.rcSceneIndex of
    Just i -> return (Just (scenes !! i))
    Nothing -> selectScene
  case mScene of
    Nothing -> putStrLn "Goodbye." >> exitSuccess
    Just scene -> do
      gen <- getStdGen
      world <- evalRandT scene.world gen
      let cam =
            makeCamera
              scene.camera
                { imageWidth = cfg.rcImageWidth,
                  samplesPerPixel = cfg.rcSamplesPerPixel
                }
      render "./output/test.ppm" world cam cfg.rcNumBounces