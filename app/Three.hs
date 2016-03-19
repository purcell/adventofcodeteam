module Three where

import           Data.List       (nub)
import           Data.List.Split (chunksOf)


type Coord = (Int, Int)

numDistinctLocations :: [Coord] -> Int
numDistinctLocations = length . nub

locationsVisited :: String -> [Coord]
locationsVisited = scanl followArrow (0,0)

followArrow :: Coord -> Char -> Coord
followArrow (x, y) arrow = newLocation
  where newLocation =
          case arrow of
          '^' -> (x, y - 1)
          'v' -> (x, y + 1)
          '>' -> (x + 1, y)
          '<' -> (x - 1, y)
          _ -> error "bad arrow"

locationsVisitedPart2 :: String -> [Coord]
locationsVisitedPart2 arrows = locationsVisited santa ++ locationsVisited robot
  where
    successiveArrows = chunksOf 2 arrows
    santa = map (!! 1) successiveArrows
    robot = map head successiveArrows



three :: IO (Int, Int)
three = do
  text <- readFile "input/3.txt"
  return ( numDistinctLocations $ locationsVisited text
         , numDistinctLocations $ locationsVisitedPart2 text )
