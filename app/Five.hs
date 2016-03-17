module Five where

five :: IO Int
five = do
  text <- readFile "input/5.txt"
  return $ niceWords text

niceWords :: String -> Int
niceWords text = length $ filter nice (lines text)

nice :: String -> Bool
nice word = threeVowels word && doubleLetter word && not (excludeStrings word)

threeVowels :: String -> Bool
threeVowels word = 3 <= length (filter (\char -> elem char vowels) word)

vowels = ['a', 'e', 'i', 'o', 'u']

doubleLetter :: String -> Bool
doubleLetter (a:b:rest)
  | a == b    = True
  | otherwise = doubleLetter (b:rest)
doubleLetter _ = False

excludeStrings :: String -> Bool
excludeStrings (a:b:rest)
  | a == 'a' && b == 'b'    = True
  | a == 'c' && b == 'd'    = True
  | a == 'p' && b == 'q'    = True
  | a == 'x' && b == 'y'    = True
  | otherwise = excludeStrings (b:rest)
excludeStrings _ = False
