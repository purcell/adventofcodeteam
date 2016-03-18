module Fifteen where

data Ingredient = Ingredient { name :: String
                             , capacity :: Int
                             , durability :: Int
                             , flavour :: Int
                             , texture :: Int
                             } deriving (Show, Eq)

fifteen :: IO String
fifteen = do
  text <- readFile "input/15.txt"
  return $ show $ bestCookieScore text

bestCookieScore :: String -> [Ingredient]
bestCookieScore text = parseInput text

parseInput :: String -> [Ingredient]
parseInput input = map parseIngredient (lines input)

parseIngredient :: String -> Ingredient
parseIngredient input = Ingredient {
    name = init iname
  , capacity = read $ init icapacity
  , durability = read $ init idurability
  , flavour = read $ init iflavour
  , texture = read $ init itexture
}
  where
    [iname, _, icapacity, _, idurability, _, iflavour, _, itexture, _, _] = words input

combine :: [Ingredient] -> Int -> [Ingredient] -> [[Ingredient]]
combine existing target [] = []
combine existing target (x:[])
  | (length existing) == target = [existing]
  | otherwise = [existing ++ (replicate (target - (length existing)) x)]
combine existing target (x:xs)
  | (length existing) == target = [existing]
  | otherwise = (combine (existing ++ [x]) target (x:xs)) ++ (combine existing target xs)

counts :: [Ingredient] -> [Ingredient] -> [(Ingredient, Int)]
counts recipe ingredients = map (countIn recipe) ingredients

countIn :: [Ingredient] -> Ingredient -> (Ingredient, Int)
countIn [] ingredient = (ingredient, 0)
countIn (x:xs) ingredient
	| x == ingredient = (ingredient, 1 + countRest)
	| otherwise = (ingredient, countRest)
	where (_, countRest) = (countIn xs ingredient)
