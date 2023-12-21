module Aoc.Parse
  ( Parser (..),
    mkCharParser,
    mkStringParser,
    mkSpanParser,
    ensureNonempty,
    mkSepByParser,
    whiteParser,
    intParser,
    genericCharParser,
    lineEmpty,
    (<=<),
    ($>),
    (<|>),
    some,
    many,
    optional,
    genericStringParser,
  )
where

import Control.Applicative
import Control.Monad ((<=<))
import Data.Char (isDigit, isSpace)
import Data.Functor (($>))

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
  (:)
    <$> e
    <*> many (s *> e)
    <|> pure []

whiteParser :: Parser String
whiteParser = mkSpanParser isSpace

intParser :: Parser Int
intParser = read <$> ensureNonempty (mkSpanParser isDigit)

genericCharParser :: Parser Char
genericCharParser = Parser f
  where
    f [] = Nothing
    f (x : xs) = Just (xs, x)

genericStringParser :: Parser String
genericStringParser = many genericCharParser

lineEmpty :: String -> Bool
lineEmpty = all isSpace
