module Sixteen where

import           Data.List          (find)
import           Data.Maybe         (fromJust)
import           Text.Parsec        hiding (State)
import           Text.Parsec.String




data Attribute = Attr { attrName  :: String
                      , attrValue :: Int }
                 deriving Eq

data Sue = Sue { sueNum   :: Int
               , sueAttrs :: [Attribute] }


isThisSueForYou :: [Attribute] -> Sue -> Bool
isThisSueForYou wanted (Sue _ attrs) = all (`elem` wanted) attrs


realSueAttrs :: [Attribute]
realSueAttrs = [ Attr "children" 3
               , Attr "cats" 7
               , Attr "samoyeds" 2
               , Attr "pomeranians" 3
               , Attr "akitas" 0
               , Attr "vizslas" 0
               , Attr "goldfish" 5
               , Attr "trees" 3
               , Attr "cars" 2
               , Attr "perfumes" 1 ]


number :: Parser Int
number = read <$> many1 digit

parseAttr :: Parser Attribute
parseAttr = Attr <$> (many1 lower <* string ": ") <*> number

parseSue :: Parser Sue
parseSue = Sue <$> (string "Sue " *> number) <*> (string ": " *> parseAttr `sepBy1` string ", ")

parseFile :: String -> IO (Either ParseError [Sue])
parseFile = parseFromFile (many1 (parseSue <* newline) <* eof)


sixteen :: IO Int
sixteen = do
  Right sues <- parseFile "input/16.txt"
  return . sueNum . fromJust $ find (isThisSueForYou realSueAttrs) sues
