module Main where

import           One
import           Three


main :: IO ()
main = do
  putStrLn "One:"
  answer1 <- one
  print answer1

  putStrLn "Three: "
  answer3 <- three
  print answer3
