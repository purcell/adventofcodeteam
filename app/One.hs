module One where

import           Data.List  (find)
import           Data.Maybe (fromJust)

calculateFloor :: String -> Int
calculateFloor = foldl (flip upOrDown) 0

upOrDown :: Char -> Int -> Int
upOrDown '(' current = current + 1
upOrDown ')' current = current - 1
upOrDown _ _ = error "unexpected char"

calculateFirstBasementPosition :: String -> Int
calculateFirstBasementPosition instr =
  fst . fromJust . find ((== -1) . snd) $ zip [0..] $ scanl (flip upOrDown) 0 instr

one :: IO (Int, Int)
one = do
  text <- readFile "input/1.txt"
  return ( calculateFloor text
         , calculateFirstBasementPosition text )
