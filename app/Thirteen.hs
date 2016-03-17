module Thirteen where

import Data.List
import Data.String.Utils
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.List.Split as Split

type Person = String
type Happiness = Int
type Pair = ([Person], Happiness)
type Arrangement = [[Person]]
type HappinessMap = Map.Map [Person] Happiness

thirteen :: IO Happiness
thirteen = do
    text <- readFile "input/13-text.txt"
    let inputs = lines text
        people = nub (map (head . split " ") inputs)
        arrangements = allPossiblePairs people
        pairMap = foldl updateHappinessMap Map.empty inputs
      in return $ maxArrangementScore arrangements pairMap

maxArrangementScore :: [Arrangement] -> HappinessMap -> Happiness
maxArrangementScore arrangements pairMap =
  maximum $ map (arrangementScore pairMap) arrangements

arrangementScore :: HappinessMap -> Arrangement -> Happiness
arrangementScore pairMap arrangement =
  sum  $ map (fromJust . (`Map.lookup` pairMap)) sortedArrangement
  where
    sortedArrangement = map sort arrangement

allPossiblePairs :: [Person] -> [Arrangement]
allPossiblePairs people =
  map (Split.chunksOf 2 . cyclePerm . concatMap (replicate 2)) $ permutations people
  where
    cyclePerm list = last list : init list

updateHappinessMap :: HappinessMap -> String -> HappinessMap
updateHappinessMap pairMap inputLines
  | existingPair = Map.adjust (happiness +) pair pairMap
  | not existingPair = Map.insert pair happiness pairMap
  | otherwise = undefined
  where
    [personOne, _, direction, rawHappiness, _, _, _, _, _, _, personTwo] = split " " (init inputLines)
    pair = sort [personOne, personTwo]
    happiness = moodImpact direction rawHappiness
    existingPair = Map.member pair pairMap

moodImpact :: String -> String -> Int
moodImpact direction happiness
  | direction == "gain" = read happiness :: Int
  | direction == "lose" = (-1) * read happiness :: Int
  | otherwise = undefined
