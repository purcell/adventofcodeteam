module Twenty where

import           Data.List  (find)
import           Data.Maybe (fromJust)

presentsForHouse :: Int -> Int
presentsForHouse num = sum $ map (* 10) elves
  where
    elves = factors num

factors :: Int -> [Int]
factors n | n <= 2 = [1..n]
factors n          = upToRoot ++ map (n `div`) upToRoot
  where
   upToRoot = isFactor [1..(floor . sqrt . fromIntegral $ n)]
   isFactor = filter ((== 0) . (n `mod`))


solution :: (Int -> Int) -> Int
solution f = fst . fromJust $ find enoughPresents $ housesAndPresents
  where enoughPresents (_, presents) = presents >= 29000000
        housesAndPresents = map (\n -> (n, f n)) [1..]


presentsForHousePart2 :: Int -> Int
presentsForHousePart2 num = sum $ map (* 11) elves
  where
    elves = filter ((num <=) . (* 50)) $ factors num


twenty = ( solution presentsForHouse
         , solution presentsForHousePart2 )
