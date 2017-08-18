{-
- Author:  Zubin Kadva, 902772316, zkadva2016@my.fit.edu
- Course:  CSE 5400, Fall 2016
- Project: Paths on a grid
-}

module Main where

-- Program execution begins here
main :: IO()
main = do nums <- readInput
          let integers = map (read :: String -> Integer) nums    -- Convert string to integer
          putStrLn "\n"
          compute integers

-- Read input as a list of string
readInput :: IO [String]
readInput = do
  x <- getLine    -- Continue input till matching condition occurs
  if x == "0 0"
    then return []
    else do xs <- readInput
            return (words x ++ xs)

-- Pairing inputs and computing paths
compute :: [Integer] -> IO ()
compute [] = return ()
compute [_] = return ()
compute (x1:x2:xs) = print(grid (x1, x2)) >> compute xs

-- Calculate paths on a n x m grid
grid :: (Integer, Integer) -> Integer
grid (n, m) = factorial(n + m) `div` (factorial m * factorial n)

-- Compute factorial of a number
factorial :: Integer -> Integer
factorial n = if n == 0 then 1 else n * factorial (n - 1)