module Eight where

decodedLength :: [Char] -> Int
decodedLength [] = 0

decodedLength ('\\':stuff) =
  case stuff of
  '\\':rest -> 1 + decodedLength rest
  '"':rest -> 1 + decodedLength rest
  'x':rest -> 1 + decodedLength (drop 2 rest)
decodedLength ['"'] = 0
decodedLength ('"':xs) = decodedLength xs
decodedLength (x:xs) = 1 + decodedLength xs

extraSpace :: String -> Int
extraSpace encoded = length(encoded) - decodedLength(encoded)

calculateExtraSpace :: [String] -> Int
calculateExtraSpace input = sum allLines
  where allLines = map extraSpace input

eight :: IO Int
eight = do
  input <- readFile "input/8.txt"
  return . calculateExtraSpace . lines $ input
