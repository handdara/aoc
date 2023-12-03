module Aoc.Solve.Two
  ( solveDay2,
  )
where

import Control.Applicative
import Data.Char (isDigit, isSpace)
import System.IO

data Cube = Red Int | Green Int | Blue Int deriving (Show)

data CubeSet = CubeSet
  { redIn :: Int,
    greenIn :: Int,
    blueIn :: Int
  }
  deriving (Show)

data Game = Game
  { getID :: Int,
    getSets :: [CubeSet]
  }

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

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

whiteParser :: Parser String
whiteParser = mkSpanParser isSpace

intParser :: Parser Int
intParser = read <$> ensureNonempty (mkSpanParser isDigit)

gameIDParser :: Parser Int
gameIDParser = mkStringParser "Game" *> whiteParser *> intParser <* mkCharParser ':'

cubeParser :: Parser Cube
cubeParser = redCubeParser <|> greenCubeParser <|> blueCubeParser
  where
    redCubeParser = Red <$> (whiteParser *> intParser <* whiteParser <* mkStringParser "red")
    greenCubeParser = Green <$> (whiteParser *> intParser <* whiteParser <* mkStringParser "green")
    blueCubeParser = Blue <$> (whiteParser *> intParser <* whiteParser <* mkStringParser "blue")

-- for some reason the following implementation is broken, why?
-- mkSepByParser :: Parser a -> Parser b -> Parser [b]
-- mkSepByParser p'sep p'val = many (p'valsep <|> p'val)
--   where
--     p'valsep = p'val <* p'sep

mkSepByParser :: Parser a -> Parser b -> Parser [b]
mkSepByParser s e =
  (:) <$> e <*> many (s *> e)
    <|> pure []

cubeListParser :: Parser [Cube]
cubeListParser = mkSepByParser sepParser cubeParser
  where
    sepParser = whiteParser *> mkCharParser ',' <* whiteParser

cubeListToCubeSet :: [Cube] -> CubeSet
cubeListToCubeSet = foldl f (CubeSet 0 0 0)
  where
    f (CubeSet r g b) (Red r') = CubeSet (max r r') g b
    f (CubeSet r g b) (Green g') = CubeSet r (max g g') b
    f (CubeSet r g b) (Blue b') = CubeSet r g (max b b')

cubeSetParser :: Parser CubeSet
cubeSetParser = cubeListToCubeSet <$> cubeListParser

cubeSetListParser :: Parser [CubeSet]
cubeSetListParser = mkSepByParser sepParser cubeSetParser
  where
    sepParser = whiteParser *> mkCharParser ';' <* whiteParser

gameParser :: Parser Game
gameParser = Game <$> gameIDParser <*> cubeSetListParser

parseGame :: String -> Maybe Game
parseGame s = snd <$> runParser gameParser s

isValidGame :: CubeSet -> Game -> Bool
isValidGame bag game = foldl f True (getSets game)
  where
    f acc testSet =
      acc
        && redIn testSet <= redIn bag
        && greenIn testSet <= greenIn bag
        && blueIn testSet <= blueIn bag

filterValidGames :: CubeSet -> [Game] -> [Game]
filterValidGames bag gs = [g | g <- gs, isValidGame bag g]

solutionPart1 :: [String] -> Maybe Int
solutionPart1 ls = sum . map getID . filterValidGames elfsBag <$> mapM parseGame ls
  where
    elfsBag = CubeSet 12 13 14

fdMinCubeSet :: Game -> CubeSet
fdMinCubeSet game = foldl f (CubeSet 0 0 0) sets
  where
    sets = getSets game
    f (CubeSet ra ga ba) (CubeSet rt gt bt) = CubeSet (max ra rt) (max ga gt) (max ba bt)

power :: CubeSet -> Int
power (CubeSet r g b) = r * g * b

solutionPart2 :: [String] -> Maybe Int
solutionPart2 ls = sum . map (power . fdMinCubeSet) <$> mapM parseGame ls

solveDay2 :: FilePath -> IO ()
solveDay2 input'path = do
  input'handle <- openFile input'path ReadMode
  input'string <- hGetContents input'handle

  let input'lines = lines input'string
  -- let games'maybe = mapM parseGame input'lines
  -- let games'valid = filterValidGames elfsBag <$> games'maybe
  -- let games'valid'ids = map getID <$> games'valid
  -- let games'powers = map (power . fdMinCubeSet) <$> games'maybe

  -- putStrLn $ "Valid games: " ++ show games'valid'ids
  putStrLn $ "Solution to part 1: " ++ show (solutionPart1 input'lines)

  -- putStrLn $ "Powers: " ++ show games'powers
  putStrLn $ "Solution to part 2: " ++ show (solutionPart2 input'lines)

  hClose input'handle
