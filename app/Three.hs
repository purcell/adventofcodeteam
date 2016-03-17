module Three where

import           Data.List (nub)


type Coord = (Int, Int)

countLocationsVisited :: String -> Int
countLocationsVisited arrows = length . nub $ scanl followArrow origin arrows
  where
    origin = (0, 0)

followArrow :: Coord -> Char -> Coord
followArrow (x, y) arrow = newLocation
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
