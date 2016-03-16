module Two where

import Data.List.Split

paperRequired :: Int -> Int -> Int -> Int
paperRequired l w h = 2 * a + 2 * b + 2 * c + minimum [a, b, c]
  where
    a = l * w
    b = w * h
    c = h * l

perLine :: String -> Int
perLine line = paperRequired l w h
  where
    [l, w, h] = map read $ splitOn "x" line

calculate :: [String] -> Int
calculate input = sum presents
  where
    presents = map perLine input

two :: IO Int
two = do
  input <- readFile "input/2.txt"
  return . calculate . lines $ input
