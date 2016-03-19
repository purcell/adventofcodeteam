module Fifteen where

import           Data.Foldable      (maximumBy)
import           Data.Ord           (comparing)
import           Text.Parsec        hiding (State)
import           Text.Parsec.String


data Properties = Properties { capacity   :: Int
                             , durability :: Int
                             , flavor     :: Int
                             , texture    :: Int
                             , calories   :: Int
                             }

data Ingredient = Ingredient { name       :: String
                             , properties :: Properties
                             }

data Quantity = Quantity { teaspoons  :: Int
                         , ingredient :: Ingredient
                         }

data Cookie = Cookie { quantities :: [Quantity] }

totalCalories :: Cookie -> Int
totalCalories = flip totalOf calories

totalScore :: Cookie -> Int
totalScore cookie = product totals
  where
    totals = map (totalOf cookie) [capacity, durability, flavor, texture]

totalOf :: Cookie -> (Properties -> Int) -> Int
totalOf (Cookie qs) f = max 0 (sum $ map (\q -> teaspoons q * (f . properties . ingredient) q) qs)

{-
butterscotch = Ingredient "Butterscotch" Properties { capacity = -1, durability = -2, flavor = 6, texture =  3, calories = 8 }
cinnamon     = Ingredient "Cinnamon" Properties { capacity = 2, durability = 3, flavor = -2, texture = -1, calories = 3 }

exampleCookie = Cookie [ Quantity 44 butterscotch, Quantity 56 cinnamon ]
-}


bestCookie :: [Ingredient] -> Cookie
bestCookie = maximumBy (comparing totalScore) . possibleCookies 100

possibleCookies :: Int -> [Ingredient] -> [Cookie]
possibleCookies maxTeaspoons ingredients = map Cookie (combos maxTeaspoons ingredients)
  where
    combos _   []     = []
    combos tsp [i]    = [[Quantity tsp i]]
    combos tsp (i:is) = concatMap (\n -> map ([Quantity n i] ++) (combos (tsp - n) is)) [0..tsp]


parseIngredient :: Parser Ingredient
parseIngredient = Ingredient <$> (many1 letter <* string ": ") <*> parseProperties

parseProperties :: Parser Properties
parseProperties = Properties <$> (string "capacity " *> number <* string ", ")
                             <*> (string "durability " *> number <* string ", ")
                             <*> (string "flavor " *> number <* string ", ")
                             <*> (string "texture " *> number <* string ", ")
                             <*> (string "calories " *> number)

number :: Parser Int
number = do
  sign <- try (string "-") <|> return ""
  digits <- many1 digit
  return . read $ (sign ++ digits)

parseFile :: String -> IO (Either ParseError [Ingredient])
parseFile = parseFromFile (many1 (parseIngredient <* newline) <* eof)

fifteen :: IO (Int, Int)
fifteen = do
  result <- parseFile "input/15.txt"
  case result of
    Right ingredients ->
      let cookies = possibleCookies 100 ingredients
          bestScore = totalScore . maximumBy (comparing totalScore)
      in
        return (bestScore cookies, bestScore (filter ((500 ==) . totalCalories) cookies))
    Left err -> error (show err)
