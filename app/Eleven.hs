{-# LANGUAGE OverloadedStrings #-}
module Eleven where

import Data.String.Utils
import Data.List

eleven :: IO String
eleven = do
  input <- readFile "input/11.txt"
  return $ findNextValidPassword (strip input)

findNextValidPassword :: String -> String
findNextValidPassword current
  | conditionsMet next = next
  | otherwise = findNextValidPassword next
  where
    next = nextPassword current

nextPassword :: String -> String
nextPassword current
  | last current == 'z' = nextPassword (init current) ++ ['a']
  | otherwise = init current ++ [nextLetter (last current)]

nextLetter :: Char -> Char
nextLetter 'z' = 'a'
nextLetter letter = succ letter

conditionsMet :: String -> Bool
conditionsMet password =
  hasStraight password
  && not (isAmbiguous password)
  && hasTwoPairs password

hasStraight :: String -> Bool
hasStraight (a:b:c:rest)
  | c == succ b && b == succ a = True
  | otherwise = hasStraight (b:c:rest)
hasStraight _ = False

isAmbiguous :: String -> Bool
isAmbiguous password =
  elem 'i' password
  || elem 'o' password
  || elem 'l' password

hasTwoPairs :: String -> Bool
hasTwoPairs password =
  sum (map numberOfPairs groups) >= 2
  where
    groups = group password

numberOfPairs :: String -> Int
numberOfPairs word =
  length word `div` 2
