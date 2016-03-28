module Main where

import           Eight
import           Eighteen
import           Eleven
import           Fifteen
import           Five
import           Four
import           Fourteen
import           Nine
import           Nineteen
import           One
import           Seven
import           Seventeen
import           Six
import           Sixteen
import           Ten
import           Thirteen
import           Three
import           Twelve
import           Twenty
import           TwentyFour
import           TwentyOne
import           TwentyThree
import           Two


done :: Show a => Int -> IO a -> IO ()
done n runner = do
  putStrLn $ "Problem " ++ show n ++ ":"
  val <- runner
  print val
  putStrLn "------------"


main :: IO ()
main = do
  done 1 one
  done 2 two
  done 3 three
  done 4 four
  done 5 five
  done 6 six
  done 7 seven
  done 8 eight
  done 9 nine
  done 10 ten
  done 11 eleven
  done 12 twelve
  done 13 thirteen
  done 14 fourteen
  done 15 fifteen
  done 16 sixteen
  done 17 seventeen
  done 18 eighteen
  done 19 nineteen
  done 20 (return twenty)
  done 21 (return twentyOne)
  done 23 twentyThree
  done 24 twentyFour
