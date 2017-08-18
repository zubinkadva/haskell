{-
Author:  Zubin Kadva, 902772316, zkadva2016@my.fit.edu
Course:  CSE 5400, Fall 2016
Project: Paths on a grid
Example run:
5 4
126
1 1
2
0 0
-}

module Main where

-- Program execution begins here
main :: IO()
main = interact (unlines . (map show) . pair . (map read) . words)

-- Pairing input list by two and computing result
pair :: [Integer] -> [Integer]
pair (0:0:_) = [] -- Terminating condition
pair (x1:x2:xs) = grid(x1, x2) : pair xs
--pair (x1:x2:xs) = grid(x1+x2, min x1 x2) : pair xs
pair (_) = []

-- Calculate paths on a n x m grid
grid :: (Integer, Integer) -> Integer
grid (n, m) = factorial(n + m) `div` (factorial m * factorial n)
--grid (n, x) = foldl (\z y -> z*(n-y+1) `div` y) 1 [1..x]

-- Compute factorial of a number
factorial :: Integer -> Integer
factorial n = if n == 0 then 1 else n * factorial (n - 1)
--factorial n = foldl (*) 1 [1..n]
