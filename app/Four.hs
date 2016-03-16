module Four where

import Data.Hash.MD5
import Data.String.Utils

-- "abcdef609043" Hashes to "000001dbbfa"

-- value ++ (show 5)

bruteForce :: String -> Int -> Int
bruteForce initialValue seed =
  nextHash initialValue seed (foundMatch hashedString)
    where
      hashedString = hashString initialValue seed

nextHash :: String -> Int -> Bool -> Int
nextHash value seed True = seed
nextHash value seed False = bruteForce value (seed + 1)

foundMatch :: String -> Bool
foundMatch hash =
  startswith "00000" hash

hashString :: String -> Int -> String
hashString value seed = md5s $ Str (value ++ (show seed))

four :: IO Int
four = do
  initialValue <- readFile "input/4.txt"
  return $ bruteForce (strip initialValue) 0

