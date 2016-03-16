module One where



calculateFloor :: String -> Int
calculateFloor = foldl (flip upOrDown) 0

upOrDown :: Char -> Int -> Int
upOrDown '(' current = current + 1
upOrDown ')' current = current - 1
upOrDown _ _ = error "unexpected char"


one :: IO Int
one = do
  text <- readFile "input/1.txt"
  return $ calculateFloor text
