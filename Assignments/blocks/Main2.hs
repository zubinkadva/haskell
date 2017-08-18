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

import Data.List (sortOn)
import Data.Vector (fromList,(!),generate)
import Data.Ord (Down(..))
import Data.Functor ()
import Control.Monad (replicateM)
import Control.Applicative ()

type Tower = [(Int,Int,Int)]
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
solve blocks = heights ! (3*n-1)
  where   
    n = length blocks
    -- Sort descending according to h with all possible rotations
    sort' = fromList . sortOn(\(x,y,_) -> Down (x*y)) . concatMap(\(x,y,z) -> [(x,y,z),(y,z,x),(z,x,y)]) $ blocks
    height h = let (_,_,z) = sort' ! h in z
    width w = let (_,y,_) = sort' ! w in y
    len l = let (x,_,_) = sort' ! l in x
    max' m = heights ! m   
    -- It is important that w(j) > w(i) and l(j) > l(i) and then add to h(i)
    heights = generate (3*n) (\i -> height i + maximum([max' j | j <- [0..(i-1)],width j > width i,len j > len i] ++ [0]))