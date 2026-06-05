module Main where

import Camera
import Control.Monad.Random (getStdGen)
import Options.Applicative
import Render
import Scenes
import System.Exit (exitSuccess)
import System.Random.Stateful (newIOGenM)
import Text.Read (readMaybe)

data RenderConfig = RenderConfig
  { rcSceneIndex      :: Maybe Int
  , rcImageWidth      :: Int
  , rcSamplesPerPixel :: Int
  , rcNumBounces      :: Int
  }

readSceneIndex :: ReadM Int
readSceneIndex = eitherReader $ \s ->
  case readMaybe s of
    Nothing -> Left "Scene index must be an integer"
    Just i  | i < 0 || i >= length scenes ->
                Left $ "Scene index out of range (0-" ++ show (length scenes - 1) ++ ")"
             | otherwise -> Right i

argsParser :: Parser RenderConfig
argsParser = RenderConfig
  <$> optional (option readSceneIndex
      ( long "scene"
     <> short 's'
     <> metavar "INT"
     <> help ("Scene index to render (0-" ++ show (length scenes - 1) ++ "), omit to prompt")))
  <*> option auto
      ( long "width"
     <> short 'w'
     <> metavar "INT"
     <> value 800
     <> showDefault
     <> help "Image width in pixels")
  <*> option auto
      ( long "samples"
     <> short 'n'
     <> metavar "INT"
     <> value 100
     <> showDefault
     <> help "Samples per pixel")
  <*> option auto
      ( long "bounces"
     <> short 'b'
     <> metavar "INT"
     <> value 50
     <> showDefault
     <> help "Maximum ray bounces")

main :: IO ()
main = do
  cfg <- execParser $ info (argsParser <**> helper)
    ( fullDesc
   <> progDesc "Render a scene using path tracing"
   <> header "tracerays - a Haskell path tracer" )
  mScene <- case cfg.rcSceneIndex of
    Just i  -> return (Just (scenes !! i))
    Nothing -> selectScene
  case mScene of
    Nothing    -> putStrLn "Goodbye." >> exitSuccess
    Just scene ->
      let cam = makeCamera scene.camera
                  { imageWidth      = cfg.rcImageWidth
                  , samplesPerPixel = cfg.rcSamplesPerPixel
                  }
      in print scene.name
           >> getStdGen
           >>= newIOGenM
           >>= scene.build
           >>= render "./output/test.ppm" cam cfg.rcNumBounces