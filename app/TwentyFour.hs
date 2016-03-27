module TwentyFour where

import           Control.Monad (guard)
import           Data.Function (on)
import           Data.List     (groupBy, sortBy, tails, unfoldr, (\\))
import           Data.Maybe    (fromJust, listToMaybe)
import           Data.Ord      (comparing)

newtype Package = Package { weight :: Int }
                  deriving (Show, Eq, Ord)

grouped3 :: [Package] -> Maybe [[Package]]
grouped3 pkgs = listToMaybe $ groupingsOf3 limit pkgs
  where limit = totalWeight pkgs `div` 3

groupingsOf3 :: Int -> [Package] -> [[[Package]]]
groupingsOf3 limit pkgs = do
  group1 <- bestGroups limit pkgs
  group2 <- groupsOfWeight limit (pkgs \\ group1)
  group3 <- groupsOfWeight limit ((pkgs \\ group1) \\ group2)
  guard (length pkgs == sum (map length [group1, group2, group3]))
  return [group1, group2, group3]

bestGroups :: Int -> [Package] -> [[Package]]
bestGroups limit pkgs = concatMap (sortBy (comparing entanglement)) $ groupBy ((==) `on` length) $ groupsOfWeight limit $ heaviestFirst pkgs


-- Unlike Data.List.subsequences, this returns subsequences in increasing length order
subsequences :: [a] -> [[a]]
subsequences xs = concat $ unfoldr f [([], xs)]
  where
    f :: [([a], [a])] -> Maybe ([[a]], [([a], [a])])
    f lasts = if null nexts then Nothing
              else Just (map fst nexts, nexts)
      where nexts = [ (soFar ++ [nxt], rest) | (soFar, remaining) <- lasts, (nxt, rest) <- splits remaining ]
    splits ys = zip ys (drop 1 $ tails ys)


heaviestFirst :: [Package] -> [Package]
heaviestFirst = sortBy (flip $ comparing weight)

groupsOfWeight :: Int -> [Package] -> [[Package]]
groupsOfWeight limit packages = filter ((limit ==) . totalWeight) $ subsequences packages

weights :: [Package] -> [Int]
weights = map weight

totalWeight :: [Package] -> Int
totalWeight = sum . weights

entanglement :: [Package] -> Int
entanglement = product . weights

packagesFrom :: FilePath -> IO [Package]
packagesFrom file = do
  text <- readFile file
  return $ Package . read <$> lines text

twentyFour :: IO Int
twentyFour = entanglement . head . fromJust . grouped3 <$> packagesFrom "input/24.txt"