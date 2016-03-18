module Eighteen where

import           Data.Map (Map)
import qualified Data.Map as Map

type Light = (Int, Int)

type Board = Map Light Bool

eighteen :: IO Int
eighteen = do
    initialBoard <- parseFile <$> readFile "input/18.txt"
    let endBoard = iterate run (Map.fromList initialBoard) !! 100

    return . length $ filter id (Map.elems endBoard)

parseFile :: String -> [(Light, Bool)]
parseFile input = concatMap (uncurry parseLine) (zip [0..99] lines')
    where
        lines' = lines input

parseLine :: Int -> String -> [(Light, Bool)]
parseLine y line = zip lightCoords (map parseLight line)
    where
        lightCoords = zip [0..99] (repeat y)

        parseLight '#'  = True
        parseLight '.'  = False
        parseLight _    = undefined

coords :: [Light]
coords = [(x, y) | x <- [0..99], y <- [0..99]]

run :: Board -> Board
run board = Map.fromList newLights
    where
        newLights = map (\(light, state) -> (light, toggle board light state)) (Map.toList board)

toggle :: Board -> Light -> Bool -> Bool
toggle board light True     = length (filter id (map (board Map.!) lights)) `elem` [2, 3]
    where
        lights = neighbours board light
toggle board light False    = length (filter id (map (board Map.!) lights)) == 3
    where
        lights = neighbours board light

neighbours :: Board -> Light -> [Light]
neighbours board (lx, ly) =
    [ (x, y)
    | x <- [lx - 1..lx + 1]
    , y <- [ly - 1..ly + 1]
    , (x, y) `Map.member` board
    , (x, y) /= (lx, ly)
    ]
