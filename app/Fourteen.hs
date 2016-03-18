module Fourteen where

import           Data.Maybe (fromJust)
import           Prelude    hiding (cycle)
import           Text.Regex

type Reindeer = (Int, Int, Int)

reindeer :: String -> Reindeer
reindeer str = let [a, b, c] = map read numbers
  in (a, b, c)
  where numbers = fromJust $ matchRegex (mkRegex "([0-9]+)[^0-9]+([0-9]+)[^0-9]+([0-9]+)") str

distanceFlown :: Int -> Reindeer -> Int
distanceFlown seconds (speed, fly, rest) = speed * flightTime
  where flightTime = full * fly + min fly partial
        (full, partial) = seconds `divMod` cycle
        cycle = fly + rest

fourteen :: IO Int
fourteen = do
  input <- readFile "input/14.txt"
  return . maximum . map (distanceFlown 2503 . reindeer) . lines $ input

