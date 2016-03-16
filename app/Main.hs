module Main where

import           One
import           Two
import           Three
import           Five

main :: IO ()
main = do
  putStrLn "One:"
  answer1 <- one
  print answer1

  putStrLn "Two: "
  answer2 <- two
  print answer2

  putStrLn "Three: "
  answer3 <- three
  print answer3

  putStrLn "Five:"
  answer5 <- five
  print answer5
