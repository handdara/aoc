{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Aoc.Solve.Four where

--   ( solveDay4,
--   )
-- where

import Control.Applicative
import Data.Char (isDigit, isSpace)
import qualified Data.List as L
import System.IO

-- * Types

type Input = [String]

type CardID = Int

type Number = Int

data Card
  = Card
      -- | Card ID
      CardID
      -- | Winning Numbers
      [Number]
      -- | Our Numbers
      [Number]
  deriving (Show)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

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

numbersParser :: Parser [Number]
numbersParser = mkSepByParser whiteParser intParser

cardParser :: Parser Card
cardParser = Card <$> cardIDParser <*> numbersParser <*> (pipeParser *> numbersParser)
  where
    pipeParser = whiteParser *> mkCharParser '|' <* whiteParser

parseCard = runParser cardParser

-- * Actual Solution

getWinners (Card _ ws ns) = ws `L.intersect` ns

solutionPart1 :: Input -> Maybe Int
solutionPart1 ls =
  sum
    . map (\x -> 2 ^ (x - 1))
    . filter (> 0)
    . map (length . getWinners . snd)
    <$> mapM parseCard ls

solutionPart2 :: Input -> String
solutionPart2 _ = "In Progress"

solveDay4 :: FilePath -> IO ()
solveDay4 input'path = do
  input'handle <- openFile input'path ReadMode
  input'string <- hGetContents input'handle

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " ++ show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " ++ show (solutionPart2 input'lines)

  hClose input'handle
