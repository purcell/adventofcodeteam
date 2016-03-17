module Seven where


import           Data.Bits
import           Data.List          (intersect)
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Sequence      (unfoldl)
import           Data.Word          (Word16)
import           Debug.Trace
import           Text.Parsec
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


outputOfWire :: Map Wire Gate -> Wire -> Word16
outputOfWire conns wire = signal (conns M.! traceShowId wire)
  where
    signal (Literal i) = input i
    signal (And i1 i2) = input i1 .&. input i2
    signal (Or i1 i2) = input i1 .|. input i2
    signal (LShift i1 bits) = shiftL (input i1) bits
    signal (RShift i1 bits) = shiftR (input i1) bits
    signal (Not i1) = complement (input i1)
    input :: Input -> Word16
    input (WireInput w) = outputOfWire conns w
    input (LiteralInput v) = v

parseWire :: Parser Wire
parseWire = do
  name <- many1 lower
  return (Wire name)

parseInput :: Parser Input
parseInput = (try (LiteralInput <$> parseWord16) <|> try (WireInput <$> parseWire)) <?> "input"

parseWord16 :: Parser Word16
parseWord16 = read <$> many1 digit

parseGate :: Parser Gate
parseGate = (try parseNot <|> try parseAnd <|> try parseOr <|> try parseLShift <|> try parseRShift <|> try parseLiteral) <?> "gate"
  where
    parseLiteral = Literal <$> parseInput
    parseAnd = And <$> (parseInput <* string " AND ") <*> parseInput
    parseOr = Or <$> (parseInput <* string " OR ") <*> parseInput
    parseLShift = LShift <$> (parseInput <* string " LSHIFT ") <*> parseNumber
    parseRShift = RShift <$> (parseInput <* string " RSHIFT ") <*> parseNumber
    parseNot = Not <$> (string "NOT " *> parseInput)
    parseNumber = read <$> many1 digit


parseConn :: Parser Connection
parseConn = Connection <$> (parseGate <* string " -> ") <*> parseWire

parseFile :: String -> IO (Either ParseError [Connection])
parseFile = parseFromFile (many1 (parseConn <* newline) <* eof)

connectionMap ::[Connection] -> Map Wire Gate
connectionMap = M.fromList . map (\(Connection gate wire) -> (wire, gate))

seven :: IO Word16
seven = do
  result <- parseFile "input/7.txt"
  case result of
    Right connections -> do
      print (length connections)
      return $ outputOfWire (connectionMap connections) (Wire "a")
    Left err -> error (show err)


