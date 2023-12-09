module Aoc.Solve.Nine
  ( solveDay9,
  )
where

type Input = [String]

type SigVal = Int

type Signal = [SigVal]

-- * Parsing

prepareInput :: Input -> [Signal]
prepareInput = map (map read . words)

-- * Solutions

diff :: Signal -> Signal
diff = zipWith (-) <$> tail <*> init

allEqual :: Signal -> Bool
allEqual [] = True
allEqual xs = all (== head xs) (tail xs)

appendNextDeriv :: [Signal] -> [Signal]
appendNextDeriv = (<>) <$> id <*> (: []) . diff . last

getDiscDerivs :: Signal -> [Signal]
getDiscDerivs = until (allEqual . last) appendNextDeriv . (: [])

propPair :: Signal -> Signal -> Signal
propPair y dy = y <> [last y + last dy]

propSingle :: [Signal] -> [Signal]
propSingle = scanr propPair [0]

solutionPart1 :: Input -> SigVal
solutionPart1 = sum . map (last . head . propSingle . getDiscDerivs) . prepareInput

backpropPair :: Signal -> Signal -> Signal
backpropPair y dy = head y - head dy : y

backpropSingle :: [Signal] -> [Signal]
backpropSingle = scanr backpropPair [0]

solutionPart2 :: Input -> SigVal
solutionPart2 = sum . map (head . head . backpropSingle . getDiscDerivs) .prepareInput

solveDay9 :: FilePath -> IO ()
solveDay9 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
