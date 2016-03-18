module Twenty where

import Data.Maybe (fromJust)
import Data.List (find, nub)
import Debug.Trace (traceShowId)

presentsForHouse :: Int -> Int
presentsForHouse num = sum $ map (* 10) elves
  where
    elves = factors num

factors :: Int -> [Int]
factors n = nub $ upToRoot ++ map (n `div`) upToRoot
  where
   upToRoot = isFactor $ [1..(floor . sqrt . fromIntegral $ n)]
   isFactor = filter ((== 0) . (n `mod`))


housesAndPresents :: [(Int, Int)]
housesAndPresents = map (\n -> (n, presentsForHouse n)) [1..]

solution :: Int
solution = fst . fromJust $ find enoughPresents housesAndPresents
  where enoughPresents (_, presents) = presents >= 29000000

twenty = solution