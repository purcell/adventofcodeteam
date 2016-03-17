module Seventeen where

import Data.List
import Data.Maybe (fromJust)

fill :: [Int] -> [Int]
fill buckets = buckets

permute :: Int -> [Int] -> [Int] -> [[Int]]
permute goal tried []
  | goal == (sum tried) = [tried]
  | otherwise = []
permute goal tried remaining
  | goal == (sum tried) = [tried]
  | otherwise = concat $ map (permute2 goal tried) (sublists remaining)

permute2 :: Int -> [Int] -> [Int] -> [[Int]]
permute2 goal tried [] = []
permute2 goal tried (x:xs)
  | x + (sum tried) <= goal = permute goal (tried ++ [x]) xs
  | otherwise = []

sublists :: [Int] -> [[Int]]
sublists [] = []
sublists (x:xs) = [(x:xs)] ++ (sublists xs)

seventeen :: IO Int
seventeen = do
  input <- readFile "input/17.txt"
  let buckets = map read $ lines input
    in return . length $ permute 150 [] buckets
