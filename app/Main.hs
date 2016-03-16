module Main where

import           One
import           Two
import           Three
import           Four
import           Five
import           Six
import           Eight

main :: IO ()
main = do
  putStrLn "One:"
  print =<< one

  putStrLn "Two: "
  print =<< two

  putStrLn "Three: "
  print =<< three

  putStrLn "Four:"
  print =<< four

  putStrLn "Five:"
  print =<< five

  putStrLn "Six: "
  print =<< six
