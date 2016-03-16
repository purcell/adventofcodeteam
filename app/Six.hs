module Six where

import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Utils

data Command = On Light Light | Off Light Light | Toggle Light Light
    deriving Show

type Light = (Int, Int)

six :: IO Int
six = do
    text <- readFile "input/6.txt"

    let commands = map parse (lines text)

    return . count $ foldl run Map.empty commands

parse :: String -> Command
parse line
    | "turn on" `startswith` line   = let [first, second] = split " through " (drop 8 line) in On (parseLight first) (parseLight second)
    | "turn off" `startswith` line  = let [first, second] = split " through " (drop 9 line) in Off (parseLight first) (parseLight second)
    | "toggle" `startswith` line    = let [first, second] = split " through " (drop 7 line) in Toggle (parseLight first) (parseLight second)
    | otherwise                     = undefined
    where
        parseLight coord = let [first, second] = map read (split "," coord) in (first, second)

run :: Map Light Bool -> Command -> Map Light Bool
run lights (On bottomLeft topRight)     = on lights bottomLeft topRight
run lights (Off bottomLeft topRight)    = off lights bottomLeft topRight
run lights (Toggle bottomLeft topRight) = toggle lights bottomLeft topRight

count :: Map Light Bool -> Int
count lights = length $ filter id (Map.elems lights)

on :: Map Light Bool -> Light -> Light -> Map Light Bool
on lights bottomLeft topRight = foldl (\lights light -> Map.insert light True lights) lights targets
    where
        targets = rectangle bottomLeft topRight

off :: Map Light Bool -> Light -> Light -> Map Light Bool
off lights bottomLeft topRight = foldl (flip Map.delete) lights targets
    where
        targets = rectangle bottomLeft topRight

toggle :: Map Light Bool -> Light -> Light -> Map Light Bool
toggle lights bottomLeft topRight = foldl (\lights light -> Map.insertWith (\_ old -> not old) light True lights) lights targets
    where
        targets = rectangle bottomLeft topRight

rectangle :: Light -> Light -> [Light]
rectangle (bx, by) (tx, ty) = [(x, y) | x <- [bx..tx], y <- [by..ty]]
