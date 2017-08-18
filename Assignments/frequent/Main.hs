{-
Author:  Zubin Kadva, 902772316, zkadva2016@my.fit.edu
Author:  Aditya Karanjkar, 902832282, akaranjkar2016@my.fit.edu
Course:  CSE 5400, Fall 2016
Project: Frequent values
-}

import Data.List (sort, group)
import Data.Functor
import Control.Applicative

-- Program execution begins here
main :: IO()
main = do
  _ <- getLine
  intervalTree <- buildTree . intervals . map (read :: String -> Int) . words <$> getLine
  queryList <- readData
  let queries = map (read :: String -> Int) queryList
  mapM_ print $ compute queries intervalTree

-- Calculate resulting list
compute :: [Int] -> Tree -> [Int]
compute [] _ = []
compute _ EmptyTree = []
compute [_] _ = []
compute (x1:x2:xs) tree = result : compute xs tree
    where result = maximum $ query x1 x2 tree

-- Read query list, end at terminating condition
readData :: IO [String]
readData = do
  x <- getLine
  if x == "0"
    then return []
    else do xs <- readData
            return (words x ++ xs)

-- Build interval tuple list
intervals :: [Int] -> [(Int,Int)]
intervals = neighbors . prefixSums . counts
    where counts = map length . group . sort
          prefixSums = scanl (+) 1
          neighbors xs = zip xs (tail xs)

-- Interval tree
data Tree = EmptyTree | Node (Int,Int) (Tree) (Tree) deriving (Show)

-- Build balanced interval tree
buildTree :: [(Int, Int)] -> Tree
buildTree [] = EmptyTree
buildTree x = Node (x !! mid)
                   (buildTree $ take mid x)
                   (buildTree $ drop (mid+1) x)
    where mid = length x `quot` 2

-- Query the interval tree
query :: Int -> Int -> Tree -> [Int]
query _ _ EmptyTree = []
query from to (Node (s,e) left right)
    | from < s && to < s = query from to left -- Starts and ends before interval
    | from > e-1 && to > e-1 = query from to right -- Starts and ends after interval
    | from == s && to == e-1 = [e-s] -- Equal to interval
    | from == s && to < e-1 = [to+1-s] -- Starts at interval, ends within interval
    | from > s && to == e-1 = [e-from] -- Starts within interval, ends at interval
    | from > s && to < e-1 = [to+1-from] -- Starts and ends within interval
    | from == s && to > e-1 = [e-s] ++ (query e to right) -- Starts at interval, ends after interval
    | from < s && to == e-1 = (query from s left) ++ [e-s] -- Starts before interval, ends at interval end
    | from < s && to > e-1 = (query from s left) ++ [e-s] ++ (query e to right) -- Starts and ends before and after interval
    | from < s && to < e-1 = (query from s left) ++ [to+1-s] -- Starts before interval, ends within interval
    | from > s && to > e-1 = [e-from] ++ (query e to right) -- Starts within interval, ends after interval
query _ _ _ = []
