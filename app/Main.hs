module Main where

import           One


main :: IO ()
main = do
  putStrLn "One:"
  answer1 <- one
  print answer1
