module Nine where

import Data.List

data Road = Road { start    :: String
                 , end      :: String
                 , distance :: Int
                 } deriving (Show)

data Path = Path { visited       :: [String]
                 , totalDistance :: Int
                 } deriving (Show)

nine :: IO String
nine = do
  text <- readFile "input/9.txt"
  return $ findShortestPath text

findShortestPath :: String -> String
findShortestPath input = totalDistance $ shortestPath $ generatePaths $ parse input

parse :: String -> ([String], [Road])
parse input = foldl parseRoad ([], []) (lines input)

parseRoad :: ([String], [Road]) -> String -> ([String], [Road])
parseRoad (locations, roads) input = (newLocations, newRoads)
  where
    route        = words input
    newLocations = nub $ locations ++ [route !! 0] ++ [route !! 2]
    newRoads     = roads ++ [Road { start    = route !! 0
                                  , end      = route !! 2
                                  , distance = read $ route !! 4
                                  }]

generatePaths :: ([String], [Road]) -> [Path]
generatePaths (locations, roads) = map (generatePath roads) (permutations locations)


generatePath :: [Road] -> [String] -> Path
generatePath roads (location:otherLocations) = generatePathHelper roads otherLocations Path { visited = [location], totalDistance = 0 }
generatePath _ []                            = emptyPath

generatePathHelper :: [Road] -> [String] -> Path -> Path
generatePathHelper roads (location:otherLocations) path = generatePathHelper roads otherLocations (addLocationToPath roads location path)
generatePathHelper _ [] path                            = path

addLocationToPath :: [Road] -> String -> Path -> Path
addLocationToPath roads location path = Path { visited       = newVisited
                                             , totalDistance = newTotalDistance
                                             }
  where
    newVisited       = (visited path) ++ [location]
    addedDistance    = distanceBetween roads location (last $ visited path)
    newTotalDistance = totalDistance path + addedDistance

distanceBetween :: [Road] -> String -> String -> Int
distanceBetween [] _  _ = 0
distanceBetween (road:otherRoads) startLocation endLocation
  | start road == startLocation && end road == endLocation = distance road
  | end road == startLocation && start road == endLocation = distance road
  | otherwise                                              = distanceBetween otherRoads startLocation endLocation

shortestPath :: [Path] -> Path
shortestPath [path]            = path
shortestPath (path:otherPaths) = shortestPathHelper otherPaths path
shortestPath []                = emptyPath

shortestPathHelper :: [Path] -> Path -> Path
shortestPathHelper [] path                      = path
shortestPathHelper (path:otherPaths) shortest
  | totalDistance path < totalDistance shortest = shortestPathHelper otherPaths path
  | otherwise                                   = shortestPathHelper otherPaths shortest

emptyPath :: Path
emptyPath = Path { visited = []
                 , totalDistance = 0
                 }

printPath :: Path -> String
printPath path = (intercalate " -> " (visited path)) ++ " = " ++ (show $ totalDistance path)
