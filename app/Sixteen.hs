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

isThisSueForYouPart2 :: [Attribute] -> Sue -> Bool
isThisSueForYouPart2 wanted (Sue _ attrs) = all matchingAttr attrs
  where
    matchingAttr a@(Attr "cats"  _)       = attrWhere (>) a
    matchingAttr a@(Attr "trees" _)       = attrWhere (>) a
    matchingAttr a@(Attr "pomeranians" _) = attrWhere (<) a
    matchingAttr a@(Attr "goldfish" _)    = attrWhere (<) a
    matchingAttr a                        = a `elem` wanted
    attrWhere cmp (Attr name val) = any matches wanted
      where matches (Attr wantedName wantedVal) = name == wantedName && val `cmp` wantedVal


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

sixteen :: IO (Int, Int)
sixteen = do
  Right sues <- parseFile "input/16.txt"
  return ( theSue (isThisSueForYou realSueAttrs) sues
         , theSue (isThisSueForYouPart2 realSueAttrs) sues )
    where theSue f = sueNum . fromJust . find f
