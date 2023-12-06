module Aoc.Solve.Six
  ( solveDay6,
  )
where

type Input = [String]

type Time = Int

type Distance = Int

type RaceRecord = (Time, Distance)

prepareInput :: Input -> [RaceRecord]
prepareInput [time'line, dist'line] =
  zip
    (map read . tail . words $ time'line)
    (map read . tail . words $ dist'line)
prepareInput _ = error "input isn't two lines"

simulateRace :: Time -> Time -> Distance
simulateRace raceDuration chargeTime = velocity * moveTime
  where
    velocity = chargeTime
    moveTime = max (raceDuration - chargeTime) 0

calcAllResults :: Time -> [RaceRecord]
calcAllResults raceDuration = zip chargeTimes distances
  where
    chargeTimes = [0..raceDuration]
    distances = map (simulateRace raceDuration) chargeTimes

calcMinWinningChargeTime :: Time -> Distance -> Time
calcMinWinningChargeTime raceDuration distRecord =
  case winStart of
    [] -> error "no winners!"
    ((minTime,_):_) -> minTime
  where
    results = calcAllResults raceDuration
    winStart = dropWhile (\(_, d) -> d <= distRecord) results

getWinnerCount :: Time -> Distance -> Int
getWinnerCount raceDuration distRecord = raceDuration + 1 - (2 * numInitialLosers)
  where
    numInitialLosers = calcMinWinningChargeTime raceDuration distRecord

solutionPart1 :: Input -> Int
solutionPart1 = product . map (uncurry getWinnerCount) . prepareInput

prepareInput'part2 :: Input -> RaceRecord
prepareInput'part2 [time'line, dist'line] = (t, d)
  where
    t = read . mconcat . tail . words $ time'line
    d = read . mconcat . tail . words $ dist'line
prepareInput'part2 _ = error "input isn't two lines"

solutionPart2 :: Input -> Int
solutionPart2 = uncurry getWinnerCount . prepareInput'part2

solveDay6 :: FilePath -> IO ()
solveDay6 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  -- putStrLn "Input: "
  -- mapM_ putStrLn input'lines

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
