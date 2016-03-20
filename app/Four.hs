module Four where

import           Data.Hash.MD5
import           Data.List         (find)
import           Data.Maybe        (fromJust)
import           Data.String.Utils

-- "abcdef609043" Hashes to "000001dbbfa"

-- value ++ (show 5)

hashString :: String -> Int -> String
hashString value seed = md5s $ Str (value ++ show seed)

bruteForce :: String -> String -> Int
bruteForce prefix seed = fromJust $ find (startswith prefix . hashString seed) [0..]

four :: IO (Int, Int)
four = do
  initialValue <- strip <$> readFile "input/4.txt"
  return ( bruteForce "00000" initialValue
         , bruteForce "000000" initialValue )

