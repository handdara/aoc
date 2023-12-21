module Aoc.Solve.Fifteen
  ( solveDay15,
  -- dbg
  )
where

import Aoc.Parse
import Data.Char (ord)

-- * Types

type Input = String

-- * Parsing

hashStringP :: Parser [String]
hashStringP = mkSepByParser (mkCharParser ',') (mkSpanParser (/=','))

prepareInput :: [Char] -> Maybe [String]
prepareInput = return . snd <=< runParser hashStringP . filter (/= '\n')

-- * Solution

updateHash :: Int -> Char -> Int
updateHash v c =
  let v'  = v + ord c
      v'' = v' * 17
   in v'' `mod` 256

hash :: String -> Int 
hash = foldl updateHash 0

solutionPart1 :: [Char] -> Maybe Int
solutionPart1 = return . sum . map hash <=< prepareInput

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String)

solveDay15 :: FilePath -> IO ()
solveDay15 input'path = do
  input'string <- readFile input'path

  -- putStrLn "Input: "
  -- mapM_ putStrLn input'lines

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'string)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'string)
