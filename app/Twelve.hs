module Twelve where

import           Data.Char

sumJSON :: String -> Int
sumJSON [] = 0
sumJSON (x:xs)
  | isDigit x = plus (read [x]) xs
  | x == '-'  = minus 0 xs
  | otherwise = sumJSON xs

plus :: Int -> String -> Int
plus = applyOp (+)

minus :: Int -> String -> Int
minus = applyOp (-)


applyOp :: (Int -> Int -> Int) -> Int -> String -> Int
applyOp _  _ [] = 0
applyOp op n (x:xs)
  | isDigit x = minus (n * 10 `op` read [x]) xs
  | otherwise = n + sumJSON xs


twelve :: IO Int
twelve = do
  json <- readFile "input/12.txt"
  return $ sumJSON json
