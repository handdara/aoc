{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Twelve where

-- module Aoc.Solve.Twelve
--   ( solveDay12,
--     module Aoc.Parse,
--     -- dbg
--     Input,
--     Status (..),
--     Group,
--     RecordRow (..),
--     SpringRecord,
--     solutionPart1,
--     solutionPart2,
--     calcPresentGroups,
--     countPossible,
--     springRecordP,
--     parseInput,
--   )
-- where

import Aoc.Parse
import Control.Parallel.Strategies
import qualified Data.List as L
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Debug.Trace (trace)
import Control.DeepSeq (deepseq)

-- * Types

type Input = String

data Status = OK | Broken | Unknown deriving (Eq, Ord)

instance Show Status where
  show OK = "✓"
  -- show OK = "."
  show Broken = "X"
  show Unknown = "?"

type StatusList = [Status]

type Group = Int

data RecordRow = RecordRow StatusList [Group]

instance Show RecordRow where
  show (RecordRow ss gs) =
    "{ "
      <> foldMap show ss
      <> " : "
      <> show gs
      <> " }"

type SpringRecord = [RecordRow]

-- * Parsing

okP :: Parser Status
okP = mkCharParser '.' $> OK

brokenP :: Parser Status
brokenP = mkCharParser '#' $> Broken

unknownP :: Parser Status
unknownP = mkCharParser '?' $> Unknown

entryP :: Parser Status
entryP = okP <|> brokenP <|> unknownP

entriesP :: Parser [Status]
entriesP = some entryP

groupsP :: Parser [Group]
groupsP = mkSepByParser (mkCharParser ',') intParser

rowP :: Parser RecordRow
rowP =
  RecordRow
    <$> (entriesP <* whiteParser)
    <*> groupsP

springRecordP :: Parser SpringRecord
springRecordP = mkSepByParser (mkCharParser '\n') rowP

parseInput = return . snd <=< runParser springRecordP

-- * Solutions

tracer msg x = trace (msg <> show x) x

calcPresentGroups sl =
  let (_, _, gs'found) = until hitUnknownOrEmpty advance (sl, 0, [])
   in gs'found
  where
    hitUnknownOrEmpty (sl', _, _) = null sl' || head sl' == Unknown
    advance ([Broken], g'curr, gs'prev) = ([], 1, gs'prev <> [g'curr + 1])
    advance (OK : sl', 0, gs'prev) = (sl', 0, gs'prev)
    advance (Broken : sl', 0, gs'prev) = (sl', 1, gs'prev)
    advance (Broken : sl', g'curr, gs'prev) = (sl', g'curr + 1, gs'prev)
    advance (OK : sl', g'curr, gs'prev) = (sl', 0, gs'prev <> [g'curr])
    advance (_, _, _) = error "unexpected sequence"

portionInvalid :: [Group] -> StatusList -> Bool
portionInvalid gs sl = not . and $ zipWith (==) (calcPresentGroups sl) gs

countPossible :: [Group] -> StatusList -> Int
countPossible gs sl
  | portionInvalid gs sl = 0
  | otherwise = case break (== Unknown) sl of
      (_, []) ->
        if calcPresentGroups sl == gs
          then 1
          else 0
      (gs'known, _ : gs'rest) ->
        let sl'ok = countPossible gs (gs'known <> (OK : gs'rest))
            sl'broken = countPossible gs (gs'known <> (Broken : gs'rest))
         in sl'ok `seq` sl'ok + sl'broken
-- in sl'ok + sl'broken

-- countAll :: SpringRecord -> [Integer]
countAll :: ([Group] -> StatusList -> Int) -> [RecordRow] -> [Integer]
countAll countingFunc rows = results `deepseq` results
  where
    results =
      map (fromIntegral . \(RecordRow sl gs) -> countingFunc gs sl) rows
        `using` parList rdeepseq
        -- `using` parList rseq

solutionPart1 :: Input -> Maybe Integer
solutionPart1 =
  return
    . sum
    . countAll countPossible
    <=< parseInput

unfoldRecordRow :: Int -> RecordRow -> RecordRow
unfoldRecordRow foldNum (RecordRow sl gs) =
  RecordRow
    (L.intercalate [Unknown] $ replicate foldNum sl)
    (mconcat $ replicate foldNum gs)

validSpacing ::
  StatusList ->
  [Group] ->
  [Group] ->
  Bool
validSpacing [] (_ : _) (_ : _) = False
validSpacing sl [] [] = Broken `notElem` sl
validSpacing sl (spc : spcs) (g : gs) = 
  let (l, m'r) = splitAt spc sl
      (m, r) = splitAt g m'r
   in ((Broken `notElem` l) && (OK `notElem` m))
        && validSpacing r spcs gs
validSpacing _ _ _ = error "shouldn't reach here"

renderSG :: [Group] -> [Group] -> String
renderSG spcs grps =
  foldMap show . mconcat $
      zipWith (\x y -> replicate x OK <> replicate y Broken) spcs grps

explodeOnIdx :: Int -> [Group] -> [Group] -> StatusList -> [[Group]]
explodeOnIdx i spcs gs sl =
  -- trace
  --   ( "explodeOnIdx:\n\t i = " <> show i
  --       <> "\n\tspcs = " <> show spcs
  --       <> "\n\tgs = " <> show gs
  --       <> "\n\tsl = " <> foldMap show sl
  --       <> "\n\tres= " <> renderSG spcs gs 
  --   ) $
    case splitAt i spcs of
      (spcs'l, spc : spcs'r) -> do
        let upTo'max = length sl - sum gs - sum spcs'l - sum spcs'r
        let gs'l = take (length spcs'l) gs
        let num'l = sum spcs'l + sum gs'l
        let fstBroken = head . dropWhile (\(x,i') -> num'l <= i' && x/=Broken) $ zip sl [0..]
        spc' <- [spc .. upTo'max]
        return $ spcs'l <> (spc' : spcs'r)
      (_, []) -> [spcs]

cntGrpArrgs ::
  Int ->
  [Group] ->
  [Group] ->
  StatusList ->
  Int
cntGrpArrgs i spcs gs sl
  -- | i > length spcs = trace ("cntGrpArrgs: i = "<>show i <>", len(spcs) = " <>show (length spcs)) 0
  | i > length spcs = if validSpacing sl spcs gs then 1 else 0
  | otherwise =
      -- let spcs'new = tracer "cntGrpArrgs:\n\t expanded list = " $ explodeOnIdx i spcs gs sl
      let spcs'new = explodeOnIdx i spcs gs sl
       in sum . map (\sn -> cntGrpArrgs (i + 1) sn gs sl) $ spcs'new

cntAllGrpArrgs :: [Group] -> StatusList -> Int
cntAllGrpArrgs gs = cntGrpArrgs 0 spaces gs
  where
    spaces = 0 : replicate (length gs - 1) 1 :: [Group]

-- solutionPart2 :: Int -> Input -> Maybe Integer
solutionPart2 foldNum =
  return
    . sum
    . countAll cntAllGrpArrgs
    . map (unfoldRecordRow foldNum)
    <=< parseInput

solveDay12 :: String -> FilePath -> IO ()
solveDay12 foldNum'str input'path = do
  input'string <- readFile input'path
  let foldNum = read foldNum'str :: Int

  -- putStrLn "Input: "
  -- putStrLn input'string

  -- let x = parseInput input'string
  -- let (Just y) = map (unfoldRecordRow foldNum) <$> x
  -- putStrLn "Unfolded: "
  -- mapM_ print y

  t0 <- getCurrentTime
  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'string)
  t1 <- getCurrentTime
  putStrLn $ "Solution to part 2: " <> show (solutionPart2 foldNum input'string)
  t2 <- getCurrentTime

  let durPart1 = diffUTCTime t1 t0
  putStrLn $ "Part 1 Solution time: " <> show durPart1

  let durPart2 = diffUTCTime t2 t1
  putStrLn $ "Part 2 Solution time: " <> show durPart2
