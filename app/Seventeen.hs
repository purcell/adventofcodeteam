module Seventeen where

import           Data.List

permute :: Int -> [Int] -> [Int] -> [[Int]]
permute goal tried []
  | goal == sum tried = [tried]
  | otherwise = []
permute goal tried remaining
  | goal == sum tried = [tried]
  | otherwise = concatMap (permute2 goal tried) (sublists remaining)

permute2 :: Int -> [Int] -> [Int] -> [[Int]]
permute2 _    _     [] = []
permute2 goal tried (x:xs)
  | x + sum tried <= goal = permute goal (tried ++ [x]) xs
  | otherwise = []

sublists :: [Int] -> [[Int]]
sublists = filter (not . null) . tails

seventeen :: IO Int
seventeen = do
  input <- readFile "input/17.txt"
  let buckets = map read $ lines input
    in return . length $ permute 150 [] buckets
