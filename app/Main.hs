module Main where

import           One
import           Three
import           Five

main :: IO ()
main = do
  putStrLn "One:"
  answer1 <- one
  print answer1

  putStrLn "Three: "
  answer3 <- three
  print answer3

  putStrLn "Five:"
  answer5 <- five
  print answer5
