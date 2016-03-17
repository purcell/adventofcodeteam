module Twelve where

import Data.Char

sumJSON :: String -> Int
sumJSON [] = 0
sumJSON (x:xs)
  | (isDigit x) = plus (read [x]) xs
  | (x == '-') = minus 0 xs
  | otherwise = sumJSON xs

plus :: Int -> String -> Int
plus n [] = 0
plus n (x:xs)
  | (isDigit x) = plus (n * 10 + (read [x])) xs
  | otherwise = n + (sumJSON xs)

minus :: Int -> String -> Int
minus n [] = 0
minus n (x:xs)
  | (isDigit x) = minus ((n * 10) - (read [x])) xs
  | otherwise = n + (sumJSON xs)

twelve :: IO Int
twelve = do
  json <- readFile "input/12.txt"
  return $ sumJSON json
