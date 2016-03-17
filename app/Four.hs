module Four where

import           Data.Hash.MD5
import           Data.List         (find)
import           Data.Maybe        (fromJust)
import           Data.String.Utils

-- "abcdef609043" Hashes to "000001dbbfa"

-- value ++ (show 5)

hashString :: String -> Int -> String
hashString value seed = md5s $ Str (value ++ show seed)

bruteForce :: String -> Int
bruteForce seed = fromJust $ find (startswith "00000" . hashString seed) [0..]

four :: IO Int
four = do
  initialValue <- readFile "input/4.txt"
  return $ bruteForce (strip initialValue)

