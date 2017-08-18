{-
Author: Zubin Kadva, 902772316, zkadva2016@my.fit.edu
Course: CSE 5400, Fall 2016
Project: Baby blocks
Example run:
1
10 20 30
2
6 8 10
5 5 5	
0
Case 1: maximum height = 40
Case 2: maximum height = 21
-}

module Main where 

import Control.Monad (replicateM)
import Prelude hiding (max,repeat)

type Block = (Int,Int,Int)
type Tower = [Block]
type Answers = [[Int]]

-- Program execution begins here
main :: IO()
main = do
  blocks <- readData
  putStr $ unlines $ zipWith (\i answer -> "Case " ++ show i ++ ": maximum height = " ++ answer) [1..] $ map (show . solve . blockPair) blocks

-- Continue input till terminating condition
readData :: IO Answers
readData = do
  x <- getLine
  if x == "0" 
   then return []
   else do
    arr <- replicateM (read x :: Int) getLine
    let blocks = map (read :: String -> Int) $ words $ unwords arr
    xs <- readData
    return (blocks:xs)

-- Pair into groups of 3 (l,w,h)
blockPair :: [Int] -> Tower
blockPair (0:0:0:_) = []
blockPair (x:y:z:xs) = (x,y,z) : (blockPair xs)
blockPair (_) = []

-- Solve the longest increasing subsequence problem 
-- https://en.wikipedia.org/wiki/Longest_increasing_subsequence
solve :: Tower -> Int
solve blocks = max $ repeat combinations initial
 where combinations = rotations blocks
       initial = map (:[]) combinations

-- All possible rotations of a block		  
rotations :: Tower -> Tower
rotations = concatMap (\(x,y,z) -> [(x,y,z),(y,z,x),(z,x,y)])

-- Build a tower with a constraint - w(j) < w(i) and d(j) < d(i)
build :: Block -> Block -> Bool
build (w1,d1,_) (w2,d2,_) = w2 < w1 && d2 < d1 || w2 < d1 && d2 < w1

-- We need to add height of the previous block to build a tower	
sumHeights :: Tower -> Int
sumHeights = sum . map (\(_,_,h) -> h)

-- Find the heighest tower length
max :: [Tower] -> Int
max = maximum . map sumHeights

-- Create a new tower if a block can be placed on top
create :: Tower -> Tower -> [Tower]
create tower@(top:_) = map (:tower) . filter (build top)

-- Repeat until no more blocks can be placed on top
repeat :: Tower -> [Tower] -> [Tower]
repeat blocks towers 
 | null new = towers
 | otherwise = repeat blocks new
  where new = concatMap (flip create blocks) towers