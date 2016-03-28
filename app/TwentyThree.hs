module TwentyThree where

import           Control.Monad.State
import           Text.Parsec         hiding (State)
import           Text.Parsec.String


data Reg = RegA | RegB
           deriving Show
data Offset = Offset Int
              deriving Show

data Instruction =
  Hlf Reg
  -- tpl r sets register r to triple its current value, then continues with the next instruction.
  | Tpl Reg
    -- inc r increments register r, adding 1 to it, then continues with the next instruction.
  | Inc Reg
    -- jmp offset is a jump; it continues with the instruction offset away relative to itself.
  | Jmp Offset
    -- jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
  | Jie Reg Offset
    -- jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).
  | Jio Reg Offset
    deriving Show


data CPUState = CPUState { instructions :: [Instruction]
                         , regA         :: Int
                         , regB         :: Int
                         , insPtr       :: Int }

load :: [Instruction] -> CPUState
load ins = CPUState ins 0 0 0

run :: State CPUState ()
run = do
  instr <- curInstr
  forM_ instr (\i -> do runInstr i; run)
  where
    curInstr = do
      (CPUState ins _ _ pos) <- get
      return $ if pos >= length ins then Nothing
               else Just (ins !! pos)

getReg :: Reg -> State CPUState Int
getReg RegA = gets regA
getReg RegB = gets regB

setReg :: Reg -> Int -> State CPUState ()
setReg RegA v = modify (\s -> s { regA = v })
setReg RegB v = modify (\s -> s { regB = v })

jump :: Int -> State CPUState ()
jump i = modify (\s -> s { insPtr = insPtr s + i })

runInstr :: Instruction -> State CPUState ()
runInstr (Hlf r) = getReg r >>= (setReg r . (`div` 2)) >> jump 1
runInstr (Tpl r) = getReg r >>= (setReg r . (* 3)) >> jump 1
runInstr (Inc r) = getReg r >>= (setReg r . (+ 1)) >> jump 1
runInstr (Jmp (Offset i)) = jump i
runInstr (Jie r (Offset i)) = getReg r >>= (\v -> return $ if v `mod` 2 == 0 then i else 1) >>= jump
runInstr (Jio r (Offset i)) = getReg r >>= (\v -> return $ if v == 1 then i else 1) >>= jump


parseInstruction :: Parser Instruction
parseInstruction = try (Hlf <$> (string "hlf " *> register))
  <|> try (Tpl <$> (string "tpl " *> register))
  <|> try (Inc <$> (string "inc " *> register))
  <|> try (Jmp <$> (string "jmp " *> offset))
  <|> try (Jie <$> (string "jie " *> register) <*> (string ", " *> offset))
  <|> try (Jio <$> (string "jio " *> register) <*> (string ", " *> offset))
  <?> "instruction"
  where
    register = (char 'a' *> return RegA) <|> (char 'b' *> return RegB) <?> "register"
    offset = Offset <$> number <?> "offset"

number :: Parser Int
number = do
  sign <- char '-' <|> char '+'
  digits <- many1 digit
  return . (* (if sign == '-' then -1 else 1)) . read $ digits

parseFile :: String -> IO (Either ParseError [Instruction])
parseFile = parseFromFile (many1 (parseInstruction <* newline) <* eof)

twentyThree :: IO (Int, Int)
twentyThree = do
  parsed <- parseFile "input/23.txt"
  case parsed of
    Left err -> error $ show err
    Right prog -> return ( bAfter run
                         , bAfter (setReg RegA 1 >> run))
      where bAfter action = regB $ execState action (load prog)
