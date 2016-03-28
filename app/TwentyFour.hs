module TwentyFour where

import           Control.Monad (guard)
import           Data.Function (on)
import           Data.List     (groupBy, sortBy, tails, unfoldr, (\\))
import           Data.Ord      (comparing)

newtype Package = Package { weight :: Int }
                  deriving (Show, Eq, Ord)

nGroups :: Int -> [Package] -> [[[Package]]]
nGroups n pkgs = do
  group1 <- bestGroups limit pkgs
  otherGroups <- takeGroups (n - 1) (pkgs \\ group1)
  let groups = group1 : otherGroups in do
    guard (length pkgs == length (concat groups))
    return groups
  where
    limit = totalWeight pkgs `div` n
    takeGroups 0 _  = return []
    takeGroups i ps = do
      g <- groupsOfWeight limit ps
      (g:) <$> takeGroups (i-1) (ps \\ g)

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
  return (answer 3 pkgs,
          answer 4 pkgs)
    where answer n = entanglement . head . head . nGroups n
