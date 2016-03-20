module Six where

import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Text.Parsec        hiding (State)
import           Text.Parsec.String

data Command = On Light Light | Off Light Light | Toggle Light Light
    deriving Show

type Light = (Int, Int)

six :: IO Int
six = do
    parsed <- parseFile "input/6.txt"
    case parsed of
      Right commands -> return . countOn $ foldl run Map.empty commands
      Left err -> error $ show err

parseFile :: String -> IO (Either ParseError [Command])
parseFile = parseFromFile (many1 (parseCommand <* newline) <* eof)

parseCommand :: Parser Command
parseCommand =
  try (On     <$> (string "turn on "  *> parseLight <* string " through ") <*> parseLight) <|>
  try (Off    <$> (string "turn off " *> parseLight <* string " through ") <*> parseLight) <|>
  try (Toggle <$> (string "toggle "   *> parseLight <* string " through ") <*> parseLight) <?> "command"
  where
    parseLight = (,) <$> parseNum <* string "," <*> parseNum <?> "light"
    parseNum = read <$> many1 digit <?> "number"

run :: Map Light Bool -> Command -> Map Light Bool
run lights (On bottomLeft topRight)     = on lights bottomLeft topRight
run lights (Off bottomLeft topRight)    = off lights bottomLeft topRight
run lights (Toggle bottomLeft topRight) = toggle lights bottomLeft topRight

countOn :: Map Light Bool -> Int
countOn lights = length $ filter id (Map.elems lights)

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
