module Aoc.Solve.Twelve
  ( solveDay12,
  )
where

import Aoc.Parse
import Control.Parallel.Strategies
import Data.Function (on)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

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

parseInput :: String -> Maybe SpringRecord
parseInput = return . snd <=< runParser springRecordP

-- * Solutions

calcPresentGroups :: (Num b, Eq b) => [Status] -> [b]
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

countPossible :: [Group] -> StatusList -> Integer
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

countAll :: ([Group] -> StatusList -> Integer) -> [RecordRow] -> [Integer]
countAll countingFunc rows = results
  where
    results =
      map (fromIntegral . \(RecordRow sl gs) -> countingFunc gs sl) rows
        `using` parList rdeepseq

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

validSpacing :: StatusList -> [Group] -> [Group] -> Bool
validSpacing [] (_ : _) (_ : _) = False
validSpacing sl [] [] = Broken `notElem` sl
validSpacing sl (spc : spcs) (g : gs) =
  let (l, m'r) = splitAt spc sl
      (m, r) = splitAt g m'r
   in ((Broken `notElem` l) && (OK `notElem` m))
        && validSpacing r spcs gs
validSpacing _ _ _ = error "shouldn't reach here"

checkSingleSpcGrp :: StatusList -> Group -> Group -> Bool
checkSingleSpcGrp sl'right spc g =
  let (l, m'_) = splitAt spc sl'right
      (m, _) = splitAt g m'_
   in ((Broken `notElem` l) && (OK `notElem` m))

explodeOnIdx :: Int -> [Group] -> [Group] -> StatusList -> [[Group]]
explodeOnIdx i spcs gs sl =
  case splitAt i spcs of -- 'l := to the left, 'r := to the right
    (spcs'l, spc : spcs'r) -> do
      let upTo'max = length sl - sum gs - sum spcs'l - sum spcs'r
      let sl'r = drop (sum $ zipWith (+) spcs'l gs) sl
      let nextBroken = fromMaybe upTo'max $ L.elemIndex Broken sl'r
      spc' <- [spc .. min upTo'max nextBroken]
      ([spcs'l <> (spc' : spcs'r) | checkSingleSpcGrp sl'r spc' (gs !! i)])
    (_, []) -> [spcs]

cntGrpArrgs :: Int -> [Group] -> [Group] -> StatusList -> Integer
cntGrpArrgs i spcs gs sl
  | i > length spcs = if validSpacing sl spcs gs then 1 else 0
  | otherwise =
      let spcs'new = explodeOnIdx i spcs gs sl
          arrgs'new =
            map (\sn -> cntGrpArrgs (i + 1) sn gs sl) spcs'new
              `using` parList rdeepseq
       in sum arrgs'new

arrangements :: StatusList -> [Group] -> [Group] -> Int -> Integer
arrangements [] [] [] _ = 1
arrangements [] _ _ _ = 0
arrangements vs [spc] [grp] _ =
  let maxSpc = length vs - grp
      nextBroken = fromMaybe maxSpc $ L.elemIndex Broken vs
      spcs'possible = [spc .. min maxSpc nextBroken]
   in fromIntegral . length . filter (\spc' -> validSpacing vs [spc'] [grp]) $ spcs'possible
arrangements vs [] [] _ =
  if Broken `elem` vs then 0 else 1
arrangements vs spcs grps i =
  let cardG = length spcs
      i'fixed'maybe =
        let dotIdxs = filter (\i' -> vs !! i' == OK) [1 .. length vs - 1]
            distFrI = abs . (`subtract` i)
         in case L.sortBy (compare `on` distFrI) dotIdxs of
              [] -> Nothing
              (x : _) -> Just x
   in case i'fixed'maybe of
        Nothing -> cntAllGrpArrgs grps vs
        Just i'split ->
          sum $
            do
              j <- [0 .. cardG]
              let (vs'l, vs'r) = splitAt i'split vs
                  il = length vs'l `div` 2
                  ir = length vs'r `div` 2
              let (spcs'l, spcs'r) = splitAt j spcs
                  (grps'l, grps'r) = splitAt j grps
                  results = (arrangements vs'l spcs'l grps'l il, arrangements vs'r spcs'r grps'r ir)
                    `using` parTuple2 rseq rseq
              return $ uncurry (*) results

cntAllGrpArrgs :: [Group] -> StatusList -> Integer
cntAllGrpArrgs gs = cntGrpArrgs 0 spaces gs
  where
    spaces = 0 : replicate (length gs - 1) 1 :: [Group]

cntAllGrpArrgs' :: StatusList -> [Group] -> Integer
cntAllGrpArrgs' vs grps = arrangements vs spcs grps 0
  where
    spcs = 0 : replicate (length grps - 1) 1 :: [Group]

solutionPart2 :: Int -> Input -> Maybe Integer
solutionPart2 foldNum =
  return
    . sum
    . countAll (flip cntAllGrpArrgs')
    . map (unfoldRecordRow foldNum)
    <=< parseInput

solveDay12 :: String -> FilePath -> IO ()
solveDay12 unfoldNum'str input'path = do
  input'string <- readFile input'path
  let unfoldNum = read unfoldNum'str :: Int

  t0 <- getCurrentTime
  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'string)
  t1 <- getCurrentTime
  t2 <- getCurrentTime
  putStrLn $ "Solution to part 2: " <> show (solutionPart2 unfoldNum input'string)
  t3 <- getCurrentTime

  let durPart1 = diffUTCTime t1 t0
  putStrLn $ "Part 1 Solution time: " <> show durPart1

  let durPart3 = diffUTCTime t3 t2
  putStrLn $ "Part 2 Solution time: " <> show durPart3
