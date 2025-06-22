module Main where

import Vec3
main :: IO ()
main = do
  let a = V3 2 2 2
  print $ normalize a
