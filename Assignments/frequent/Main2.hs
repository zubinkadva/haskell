{-
Author:  Zubin Kadva, 902772316, zkadva2016@my.fit.edu
Course:  CSE 5400, Fall 2016
Project: Frequent values
Example run:
3 10
-1 -1 1 1 1 1 3 10 10 10
2 3
1 10
5 10
0

1 
4
3
-}

module Main where 

import Data.List (nub, maximumBy)
import Data.Function (on)
import Control.Applicative

-- Program execution begins here
main :: IO()
main = do
  _ <- getLine
  arr <- map (read :: String -> Integer) . words <$> getLine
  queryList <- readData
  let queries = map (read :: String -> Integer) queryList
  putStrLn ""
  mapM_ print $ compute queries arr  

-- Construct a sublist
sublist :: Integer -> Integer -> [Integer] -> [Integer]
sublist start end list = take (fromInteger end - fromInteger start) . drop (fromInteger start) $ list

-- Calculate the resulting list
compute :: [Integer] -> [Integer] -> [Integer]
compute [_] [_] = []
compute [_] [] = []
compute [_] (_:_) = []
compute [] (_:_) = []
compute [] [] = []
compute (x1:x2:xs) list = result : compute xs list where
  result = frequency $ sublist x1 x2 list 

-- Read query list, end at terminating condition
readData :: IO [String]
readData = do
  x <- getLine
  if x == "0"
    then return []
    else do xs <- readData
            return (words x ++ xs)

-- Return count of the most frequent element in a list
frequency :: [Integer] -> Integer
frequency list = toInteger (snd $ maximumBy (compare `on` snd) counts) where
  counts = nub [(element, count) | element <- list, let count = length (filter (element ==) list)]