module Ten where

lookAndSay :: String -> String
lookAndSay (c:[]) = "1" ++ [c]
lookAndSay (c:cs) = runLength c cs 1

runLength :: Char -> String -> Int -> String
runLength c [] n = (show n) ++ [c]
runLength c (x:xs) n
  | c == x    = runLength c xs (n + 1)
  | otherwise = (runLength c [] n) ++ (lookAndSay (x:xs))

manyTimes :: Int -> String -> String
manyTimes 0 str = str
manyTimes n str = manyTimes (n - 1) (lookAndSay str)

ten :: IO Int
ten = do
  input <- readFile "input/10.txt"
  return . length $ manyTimes 40 (head (lines input))
