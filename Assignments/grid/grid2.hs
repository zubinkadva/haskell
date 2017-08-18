{-
- Author:  Zubin Kadva, zkadva2016@my.fit.edu
- Course:  CSE 5400, Fall 2016
- Project: Paths on a grid
-}

module Main where

main :: IO()
main = do nums <- readData
          let integers = map (read :: String -> Integer) nums
          putStrLn "\n"
          compute integers

grid :: Integer -> Integer -> Integer
grid n m = factorial(n+m) `div` (factorial m * factorial n)

factorial :: Integer -> Integer
factorial n = if n == 0 then 1 else n * factorial (n - 1)

readData :: IO [String]
readData = do
  x <- getLine
  if x == "0 0"
    then return []
    else do xs <- readData
            return (words x ++ xs)

compute :: [Integer] -> IO ()
compute [_] = return ()
compute [] = return ()
compute (x1:x2:xs) = print(grid x1 x2) >> compute xs