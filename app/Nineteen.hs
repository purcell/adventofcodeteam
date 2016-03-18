module Nineteen where

import Data.List.Split

allMutations :: String -> [(String, String)] -> [String]
allMutations string rules = uniq $ concatMap (mutations string) rules

matchAt :: Int -> String -> String -> Bool
matchAt position string substr = (take (length substr) (drop position string)) == substr

replaceAt :: Int -> Int -> String -> String -> String
replaceAt position count string substr = (take position string) ++ substr ++ (drop (position + count) string)

mutations :: String -> (String, String) -> [String]
mutations string (sub, rep) = concatMap (mutate string (sub, rep)) [0..((length string) - (length sub))]

mutate :: String -> (String, String) -> Int -> [String]
mutate base (sub, rep) position
  | matchAt position base sub = [replaceAt position (length sub) base rep]
  | otherwise = []

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq list = foldl addIfNotExists [] list

addIfNotExists :: Eq a => [a] -> a -> [a]
addIfNotExists list item
  | elem item list = list
  | otherwise = list ++ [item]

parseInput :: [String] -> ([(String, String)], String)
parseInput lines = (reverse molecules, medicine)
  where reversed = (reverse lines)
        medicine = head reversed
        molecules = map makeRule (reverse $ tail (tail reversed))

makeRule :: String -> (String, String)
makeRule str = let [sub, rep] = splitOn " => " str in (sub, rep)

nineteen :: IO Int
nineteen = do
  input <- readFile "input/19.txt"
  let (molecules, medicine) = parseInput $ lines input in return . length $ allMutations medicine molecules
