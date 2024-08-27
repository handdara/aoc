module Aoc.Solve.Five
  ( solveDay5,
  )
where

import Control.Applicative
import Control.Monad (replicateM, (<=<))
import Data.Char (isDigit, isSpace)
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Map.Strict ((!))

-- * Types

type Input = [String]

-- | count from to
data Move = Move Int Int Int deriving (Show)

type CrateTag = Char

type StackID = Int

type Stacks = M.Map StackID [CrateTag]

type StackLayer = [[CrateTag]]

data CampState = CampState Stacks [Move] deriving (Show)

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

-- * Atom Parsers

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

mkSepByParser :: Parser a -> Parser b -> Parser [b]
mkSepByParser s e =
  (:) <$> e <*> many (s *> e)
    <|> pure []

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

-- * Puzzle Parsers

mvP :: Parser Move
mvP = Move <$> mvNumP <*> frNumP <*> toNumP
  where
    mvNumP = mkStringParser "move" *> whiteParser *> intParser <* whiteParser
    frNumP = mkStringParser "from" *> whiteParser *> intParser <* whiteParser
    toNumP = mkStringParser "to" *> whiteParser *> intParser <* whiteParser

crateTagP :: Parser CrateTag
crateTagP = Parser f
  where
    f [] = Nothing
    f (c : cs) = Just (cs, c)

crateP :: Parser [CrateTag]
crateP = (: []) <$> (mkCharParser '[' *> crateTagP <* mkCharParser ']') <|> emptyP
  where
    emptyP = mkStringParser "   " $> []

mkStackLayerP :: Int -> Parser StackLayer
mkStackLayerP numStacks = replicateM numStacks (crateP <* sepP)
  where
    sepP = mkCharParser ' ' <|> pure ' '

stackIDsP :: Parser [StackID]
stackIDsP = whiteParser *> mkSepByParser whiteParser intParser

-- * Actual Soln

dumpLayer :: [StackID] -> StackLayer -> Stacks -> Stacks
dumpLayer id's layer stack
  | length id's /= length layer = error "dumpLayer: length mismatch"
  | otherwise = foldr foldFunc stack (zip id's layer)
  where
    foldFunc (sID, tag'wrapped) = M.insertWith (flip (<>)) sID tag'wrapped

mkStacks :: [StackID] -> [StackLayer] -> Stacks
mkStacks stackID's = foldr (dumpLayer stackID's) emptyStacks
  where
    emptyStacks = foldr (`M.insert` []) M.empty stackID's

lineEmpty :: String -> Bool
lineEmpty = all isSpace

splitInitAndMoves :: Input -> ([String], [String])
splitInitAndMoves = break lineEmpty

prepareInput :: Input -> Maybe CampState
prepareInput i = do
  let (initStacks'lines, moves'lines) = splitInitAndMoves i
  let initStacks'lines'rev = reverse initStacks'lines

  (_, stackIDs) <- runParser stackIDsP (head initStacks'lines'rev)
  let stackLayerP = mkStackLayerP (length stackIDs)
  layers'parse <- mapM (runParser stackLayerP) (tail initStacks'lines'rev)
  moves'parse <- mapM (runParser mvP) (tail moves'lines)

  let layers = map snd layers'parse
  let stacks'initial = mkStacks stackIDs layers
  let moves = map snd moves'parse

  return $ CampState stacks'initial moves

doMv :: Move -> Stacks -> Stacks
doMv (Move 0 _ _) stacks = stacks
doMv (Move amt fr to) stacks = stacks''
  where
    (took, left) = splitAt amt (stacks ! fr)
    stacks' = M.insert fr left stacks
    stacks'' = M.insertWith (\depositing old -> reverse depositing <> old) to took stacks'

mvSingle :: CampState -> CampState
mvSingle (CampState stacks []) = CampState stacks []
mvSingle (CampState stacks (mv:mvs)) = CampState stacks'new mvs
  where
    stacks'new = doMv mv stacks

mvAll :: CampState -> CampState
mvAll (CampState stacks []) = CampState stacks []
mvAll state = mvAll (mvSingle state)

getTopCrates :: CampState -> [CrateTag]
getTopCrates (CampState stacks _) = map (\ x -> head (stacks ! x)) stackIDs
  where
    stackIDs = L.sort . M.keys $ stacks

solutionPart1 :: Input -> Maybe [CrateTag]
solutionPart1 = return . getTopCrates . mvAll <=< prepareInput

doMv'9001 :: Move -> Stacks -> Stacks
doMv'9001 (Move 0 _ _) stacks = stacks
doMv'9001 (Move amt fr to) stacks = stacks''
  where
    (took, left) = splitAt amt (stacks ! fr)
    stacks' = M.insert fr left stacks
    stacks'' = M.insertWith (<>) to took stacks'

mvSingle'9001 :: CampState -> CampState
mvSingle'9001 (CampState stacks []) = CampState stacks []
mvSingle'9001 (CampState stacks (mv:mvs)) = CampState stacks'new mvs
  where
    stacks'new = doMv'9001 mv stacks

mvAll'9001 :: CampState -> CampState
mvAll'9001 (CampState stacks []) = CampState stacks []
mvAll'9001 state = mvAll'9001 (mvSingle'9001 state)

solutionPart2 :: Input -> Maybe [CrateTag]
solutionPart2 = return . getTopCrates . mvAll'9001 <=< prepareInput

solveDay5 :: FilePath -> IO ()
solveDay5 input = do
  input'string <- readFile input

  let puzzleInput = lines input'string :: Input

  let answerPart1 = solutionPart1 puzzleInput

  putStrLn $ "Solution to part 1: " <> show answerPart1

  let answerPart2 = solutionPart2 puzzleInput

  putStrLn $ "Solution to part 2: " <> show answerPart2
