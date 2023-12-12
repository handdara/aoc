module Aoc.Testing
  ( testing,
  )
where

import Data.Text hiding (map)

factorial :: (Integral a) => a -> a
factorial n
  | n < 0 = error "factorial: cant call on negative"
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

xs = [100000 .. 100031] :: [Integer]

xs'fact = map factorial xs

testing :: Text -> IO ()
testing i = do
  putStrLn "Doing some testing!"

  let x = read (unpack i) :: Integer
  putStrLn $ show x <> "! = " <> show (factorial x)

  print xs'fact

