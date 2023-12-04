module Aoc.Solve.Four
  ( solveDay4,
  )
where

import Control.Applicative
import Control.Monad ((<=<))
import Data.Char (isDigit, isSpace)
import Data.HashMap.Strict ((!?))
import qualified Data.HashMap.Strict as M
import qualified Data.HashTable.IO as HT
import qualified Data.Set as S
import System.IO

-- * Types

type Input = [String]

type CardID = Int

type Number = Int

type NumSet = S.Set Number

data Card
  = Card
      -- | Card ID
      CardID
      -- | Winning Numbers
      NumSet
      -- | Our Numbers
      NumSet
  deriving (Show)

getID :: Card -> CardID
getID (Card cID _ _) = cID

type CardMap = M.HashMap CardID (NumSet, NumSet)

type HashTable k v = HT.BasicHashTable k v

type CardCounts = HashTable CardID Int

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

data Buffer
  = Buffer
      -- | Cards to process
      CardCounts
      -- | Processed cards
      CardCounts
  deriving (Show)

-- * Parser type Alternative implementation

instance Functor Parser where
  fmap f (Parser p) = Parser p'new
    where
      p'new s = do
        (s', x) <- p s
        Just (s', f x)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)

  (Parser p'l) <*> (Parser p'r) = Parser p'new
    where
      p'new s = do
        (s', f) <- p'l s
        (s'', x) <- p'r s'
        Just (s'', f x)

instance Alternative Parser where
  empty = Parser $ const Nothing

  (Parser p'l) <|> (Parser p'r) =
    Parser $ \s -> p'l s <|> p'r s

-- * Parsers

mkCharParser :: Char -> Parser Char
mkCharParser c = Parser f
  where
    f [] = Nothing
    f (x : xs)
      | c == x = Just (xs, c)
      | otherwise = Nothing

mkStringParser :: String -> Parser String
mkStringParser = traverse mkCharParser

mkSpanParser :: (Char -> Bool) -> Parser String
mkSpanParser p = Parser f
  where
    f s =
      let (is, isnt) = span p s
       in Just (isnt, is)

ensureNonempty :: Parser [a] -> Parser [a]
ensureNonempty (Parser p) = Parser f
  where
    f s = do
      (s', xs) <- p s
      if null xs
        then Nothing
        else Just (s', xs)

mkSepByParser :: Parser a -> Parser b -> Parser [b]
mkSepByParser s e =
  (:) <$> e <*> many (s *> e)
    <|> pure []

whiteParser :: Parser String
whiteParser = mkSpanParser isSpace

intParser :: Parser Int
intParser = read <$> ensureNonempty (mkSpanParser isDigit)

cardIDParser :: Parser CardID
cardIDParser = mkStringParser "Card" *> whiteParser *> intParser <* mkCharParser ':' <* whiteParser

numbersParser :: Parser (S.Set Number)
numbersParser = S.fromList <$> mkSepByParser whiteParser intParser

cardParser :: Parser Card
cardParser = Card <$> cardIDParser <*> numbersParser <*> (pipeParser *> numbersParser)
  where
    pipeParser = whiteParser *> mkCharParser '|' <* whiteParser

parseCard :: String -> Maybe (String, Card)
parseCard = runParser cardParser

-- * Actual Solution

getWinners :: (Ord a) => (S.Set a, S.Set a) -> S.Set a
getWinners (ws, ns) = ns `S.intersection` ws

solutionPart1 :: Input -> Maybe Int
solutionPart1 ls =
  sum
    . map (\x -> 2 ^ (x - 1))
    . filter (> 0)
    . map (S.size . getWinners . (\(Card _ ws ns) -> (ws, ns)) . snd)
    <$> mapM parseCard ls

mkCardMap :: [Card] -> CardMap
mkCardMap = foldr (\(Card cid ws ms) acc -> M.insert cid (ws, ms) acc) M.empty

processCardID :: CardMap -> CardID -> [CardID]
processCardID cardM cID = case cardM !? cID of
  Nothing -> []
  Just t ->
    let n = S.size $ getWinners t
     in [cID + 1 .. cID + n]

insertAdd :: (Num a) => a -> Maybe a -> (Maybe a, ())
insertAdd n Nothing = (Just n, ())
insertAdd n (Just v) = (Just (v + n), ())

processCardIDs :: [CardID] -> CardMap -> Buffer -> IO Buffer
processCardIDs [] _ b = return b
processCardIDs (cID : rest) cardM (Buffer todoHT doneHT) = do
  (Just multiples) <- HT.lookup todoHT cID
  let newCards = processCardID cardM cID
  HT.mutate doneHT cID (insertAdd multiples)
  mapM_ (\k -> HT.mutate todoHT k (insertAdd multiples)) newCards
  processCardIDs rest cardM (Buffer todoHT doneHT)

tallyCounts :: CardCounts -> IO Int
tallyCounts = return . sum . map snd <=< HT.toList

initializeBuffer :: [CardID] -> IO Buffer
initializeBuffer ks = do
  todoHT <- HT.new
  doneHT <- HT.new
  mapM_ (\k -> HT.insert todoHT k 1) ks
  return (Buffer todoHT doneHT)

processCardMap :: [CardID] -> CardMap -> IO Int
processCardMap cIDsToProcess'ordered cardM = do
  initialBuffer <- initializeBuffer cIDsToProcess'ordered
  (Buffer _ resultingCard) <- processCardIDs cIDsToProcess'ordered cardM initialBuffer
  tallyCounts resultingCard

solutionPart2 :: [String] -> IO (Maybe Int)
solutionPart2 ls =
  case mapM parseCard ls of
    Nothing -> return Nothing
    Just ts -> do
      let cards = map snd ts
      let cIDs = map getID cards
      let cardM = mkCardMap cards
      val <- processCardMap cIDs cardM
      return (Just val)

solveDay4 :: FilePath -> IO ()
solveDay4 input'path = do
  input'handle <- openFile input'path ReadMode
  input'string <- hGetContents input'handle

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " ++ show (solutionPart1 input'lines)

  answerPart2 <- solutionPart2 input'lines
  putStrLn $ "Solution to part 2: " ++ show answerPart2

  hClose input'handle
