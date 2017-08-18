{-
 - Author: Aditya Karanjkar, Zubin Kadva
 - Course: CSE5400, Fall 2016
 - Project: Triangle Game
-}

module Main where

import qualified Data.List as List

main :: IO()
main = interact (unlines . map showResult . getCases)

getCases :: String -> [[[Integer]]]
getCases = pair . discard . lines

discard :: [[Char]] -> [[Char]]
discard x = filter (\u -> u /= "$") $ filter (\v -> v /= "*") x

pair :: [String] -> [[[Integer]]]
pair (u:v:w:x:y:z:xs) = b : pair xs
    where a = [u] ++ [v] ++ [w] ++ [x] ++ [y] ++ [z]
          b = map (map read . words) a :: [[Integer]]
pair _ = []

rotateRight :: [Integer] -> [Integer]
rotateRight [] = []
rotateRight [x] = [x]
rotateRight [x,y,z] = [z,x,y]

triangleRotations :: [Integer] -> [[Integer]]
triangleRotations x = [r0,r1,r2]
    where
        r0 = x
        r1 = rotateRight x
        r2 = rotateRight $ rotateRight x

trianglePermutations :: [a] -> [[a]]
trianglePermutations x = List.permutations x

triangleCombos :: [[Integer]] -> [[[Integer]]]
triangleCombos [u,v,w,x,y,z] = [[a,b,c,d,e,f] | a <- triangleRotations u, b <- triangleRotations v, c <- triangleRotations w, d <- triangleRotations x, e <- triangleRotations y, f <- triangleRotations z]

validTriangle :: Eq a => [[a]] -> Bool
validTriangle [a,b,c,d,e,f] = if (a !! 1 == b !! 0 && b !! 1 == c !! 0 && c !! 1 == d !! 0 && d !! 1 == e !! 0 && e !! 1 == f !! 0 && f !! 1 == a !! 0) then True else False

validTriangleList :: Eq a => [[[a]]] -> [[[a]]]
validTriangleList x = filter validTriangle x

triangleSums :: Num b => [[[b]]] -> [b]
triangleSums x = map sumUp x
    where sumUp a = sum $ map last a

maxSum :: Ord a => [a] -> Maybe a
maxSum [] = Nothing
maxSum x = Just (maximum x)

solve :: [[Integer]] -> Maybe Integer
solve = maxSum . concat . map triangleSums . map validTriangleList . map triangleCombos . trianglePermutations

showResult :: [[Integer]] -> [Char]
showResult x =
    case solve x of
        Nothing -> "none"
        Just v -> show v
{-
[1,4,20], [3,1,5], [50,2,3], [5,2,7], [7,5,20], [4,7,50]
[1,4,20], [4,7,50], [7,5,20], [5,2,7], [2,3,50], [3,1,5]

[[[4,20,1], [20,7,5], [7,5,2], [5,3,1], [3,50,2], [50,4,7]], [[1,4,20], [4,7,50], [7,5,20], [5,2,7], [2,3,50], [3,1,5]], [[1,4,20], [3,1,5], [50,2,3], [5,2,7], [7,5,20], [4,7,50]]]

[[10,1,20], [20,2,30], [30,3,40], [40,4,50], [50,5,60], [10,6,60]]
[[10,1,20], [20,2,30], [30,3,40], [40,4,50], [50,5,60], [60,6,10]]
-}
