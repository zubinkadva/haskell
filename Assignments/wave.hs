{-
Author:  Zubin Kadva, 902772316, zkadva2016@my.fit.edu
Course:  CSE 5400, Fall 2016
Project: Delta wave
Example run:
3
4
43
1614282136160911722
-}

module Main where

-- Program execution begins here
main :: IO ()
main = interact (unlines . (map show) . map modTen . (map read) . words)

-- Compute Motzkin number
{-
wave :: Integer -> Integer
wave 0 = 1
wave 1 = 1
wave n = ((3 * n - 3) * wave (n - 2) + (2 * n + 1) * wave (n - 1)) `div` (n + 2)
-}

-- Memoized version of Motzkin number computation
mWave :: Int -> Integer
mWave = (map wave [0..] !!)
  where wave 0 = 1
        wave 1 = 1
        wave n = ((3 * fromIntegral n - 3) * mWave (n - 2) + 
          (2 * fromIntegral n + 1) * mWave (n - 1)) 
          `div` (fromIntegral n + 2)    -- fromIntegral is needed since output should be a large integer

-- Computing output mod 10^100 (Because the output is quite large sometimes)
modTen :: Int -> Integer
modTen n = (mWave n) `mod` 10 ^ (100 :: Integer)