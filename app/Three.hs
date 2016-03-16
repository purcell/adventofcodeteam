module Three where

import qualified Data.Set as S



countLocationsVisited :: String -> Int
countLocationsVisited arrows = (S.size . snd) (foldl followArrow (origin, S.singleton origin) arrows)
  where
    origin :: (Int, Int)
    origin = (0, 0)
    followArrow ((x, y), pastLocations) arrow = (newLocation, S.insert newLocation pastLocations)
      where newLocation =
              case arrow of
              '^' -> (x, y - 1)
              'v' -> (x, y + 1)
              '>' -> (x + 1, y)
              '<' -> (x - 1, y)
              _ -> error "bad arrow"



three :: IO Int
three = do
  text <- readFile "input/3.txt"
  return $ countLocationsVisited text
