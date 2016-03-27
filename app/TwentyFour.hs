module TwentyFour where

import           Control.Monad (guard)
import           Data.Function (on)
import           Data.List     (groupBy, sortBy, tails, unfoldr, (\\))
import           Data.Ord      (comparing)

newtype Package = Package { weight :: Int }
                  deriving (Show, Eq, Ord)

threeGroups :: [Package] -> [[[Package]]]
threeGroups pkgs = let limit = totalWeight pkgs `div` 3 in
  do
    group1 <- bestGroups limit pkgs
    group2 <- groupsOfWeight limit (pkgs \\ group1)
    group3 <- groupsOfWeight limit ((pkgs \\ group1) \\ group2)
    let groups = [group1, group2, group3] in do
      guard (length pkgs == sum (map length groups))
      return groups

fourGroups :: [Package] -> [[[Package]]]
fourGroups pkgs = let limit = totalWeight pkgs `div` 4 in
  do
    group1 <- bestGroups limit pkgs
    group2 <- groupsOfWeight limit (pkgs \\ group1)
    group3 <- groupsOfWeight limit ((pkgs \\ group1) \\ group2)
    group4 <- groupsOfWeight limit (((pkgs \\ group1) \\ group2) \\ group3)
    let groups = [group1, group2, group3, group4] in do
      guard (length pkgs == sum (map length groups))
      return groups

bestGroups :: Int -> [Package] -> [[Package]]
bestGroups limit pkgs = concatMap (sortBy (comparing entanglement)) $ groupBy ((==) `on` length) $ groupsOfWeight limit pkgs


-- Unlike Data.List.subsequences, this returns subsequences in increasing length order
subsequences :: [a] -> [[a]]
subsequences xs = concat $ unfoldr f [([], xs)]
  where
    f :: [([a], [a])] -> Maybe ([[a]], [([a], [a])])
    f lasts = if null nexts then Nothing
              else Just (map fst nexts, nexts)
      where nexts = [ (soFar ++ [nxt], rest) | (soFar, remaining) <- lasts, (nxt, rest) <- splits remaining ]
    splits ys = zip ys (drop 1 $ tails ys)


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

twentyFour :: IO (Int, Int)
twentyFour = do
  pkgs <- packagesFrom "input/24.txt"
  return (answer threeGroups pkgs,
          answer fourGroups pkgs)
    where answer f = entanglement . head . head . f
