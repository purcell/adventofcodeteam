module Seven where

import           Control.Monad.State
import           Data.Bits
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Word           (Word16)
import           Text.Parsec         hiding (State)
import           Text.Parsec.String


data Wire = Wire String
            deriving (Eq, Show, Ord)

data Input = WireInput Wire
           | LiteralInput Word16
             deriving (Show)

data Gate = Literal Input
          | And Input Input
          | Or Input Input
          | LShift Input Int
          | RShift Input Int
          | Not Input
            deriving (Show)


data Connection = Connection Gate Wire
                  deriving (Show)
instance Eq Connection where
    (Connection _ a) == (Connection _ b) = a == b
instance Ord Connection where
    compare (Connection _ a) (Connection _ b) = compare a b


outputOfWire :: Wire -> State (Map Wire Gate) Word16
outputOfWire wire = do
    gate <- gets (M.! wire)
    lit <- signal gate

    modify $ M.insert wire (Literal $ LiteralInput lit)

    return lit
  where
    signal :: Gate -> State (Map Wire Gate) Word16
    signal (Literal i) = input i
    signal (And i1 i2) = (.&.) <$> input i1 <*> input i2
    signal (Or i1 i2) = (.|.) <$> input i1 <*> input i2
    signal (LShift i1 bits) = flip shiftL bits <$> input i1
    signal (RShift i1 bits) = flip shiftR bits <$> input i1
    signal (Not i1) = complement <$> input i1
    input :: Input -> State (Map Wire Gate) Word16
    input (WireInput w) = outputOfWire w
    input (LiteralInput v) = return v

overrideWire :: Map Wire Gate -> Wire -> Word16 -> Map Wire Gate
overrideWire conns wire val = M.insert wire (Literal (LiteralInput val)) conns

parseWire :: Parser Wire
parseWire = Wire <$> many1 lower <?> "wire"

parseInput :: Parser Input
parseInput = (try (LiteralInput <$> parseWord16) <|> try (WireInput <$> parseWire)) <?> "input"

parseWord16 :: Parser Word16
parseWord16 = read <$> many1 digit <?> "number"

parseGate :: Parser Gate
parseGate = (try parseNot <|> try parseAnd <|> try parseOr <|> try parseLShift <|> try parseRShift <|> try parseLiteral) <?> "gate"
  where
    parseLiteral = Literal <$> parseInput
    parseAnd     = And <$> (parseInput <* string " AND ") <*> parseInput
    parseOr      = Or <$> (parseInput <* string " OR ") <*> parseInput
    parseLShift  = LShift <$> (parseInput <* string " LSHIFT ") <*> parseNumber
    parseRShift  = RShift <$> (parseInput <* string " RSHIFT ") <*> parseNumber
    parseNot     = Not <$> (string "NOT " *> parseInput)
    parseNumber  = read <$> many1 digit


parseConn :: Parser Connection
parseConn = Connection <$> (parseGate <* string " -> ") <*> parseWire

parseFile :: String -> IO (Either ParseError [Connection])
parseFile = parseFromFile (many1 (parseConn <* newline) <* eof)

connectionMap ::[Connection] -> Map Wire Gate
connectionMap = M.fromList . map (\(Connection gate wire) -> (wire, gate))

seven :: IO (Word16, Word16)
seven = do
  result <- parseFile "input/7.txt"
  case result of
    Right connections -> return (valueOnA, valueOnAPart2)
      where
        circuit = connectionMap $ sort connections
        runCircuit = evalState (outputOfWire (Wire "a"))
        valueOnA = runCircuit circuit
        valueOnAPart2 = runCircuit (overrideWire circuit (Wire "b") valueOnA)
    Left err -> error (show err)


