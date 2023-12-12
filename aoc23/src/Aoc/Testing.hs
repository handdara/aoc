module Aoc.Testing
  ( testing,
  )
where

import Data.Text hiding (map)
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies
import System.CPUTime

factorial :: (Integral a) => a -> a
factorial n
  | n < 0 = error "factorial: cant call on negative"
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

xs :: (Num b, Enum b) => b -> [b]
xs i = map (+i) [0 .. 31]

testing :: Text -> IO ()
testing i = do
  putStrLn "Doing some testing!"

  let i'int = read (unpack i) :: Integer

  putStr "Factorial eval..."
  t0'cpu <- getCPUTime
  let r = map factorial (xs i'int) `using` parList rdeepseq
  _ <- sum r `deepseq` return ()
  -- putStrLn $ "Result: " <> show (sum r)
  t1'cpu <- getCPUTime
  putStrLn "done."
  let t0 = fromIntegral t0'cpu / 10e12 :: Double
  let t1 = fromIntegral t1'cpu / 10e12 :: Double
  putStrLn $ "Eval time: " <> show (t1 - t0)
