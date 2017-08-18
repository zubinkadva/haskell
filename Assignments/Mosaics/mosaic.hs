import Data.Bits (shiftR)
import Control.Monad

unknown = 0
black = 1
white = 2
nw = 3
ne = 4
se = 5
sw = 6

getColor :: Int -> Int -> Int
getColor shape k
    | shape < nw = shape
    | otherwise = 1 + (k + 15 - 2 * shape) `mod` 8 `div` 4

makeQuad :: [[Int]] -> Int -> Int -> Int
makeQuad grid x y = (grid !! r !! c) + (grid !! r !! (c+1))*8 + (grid !! (r+1) !! (c+1))*64 + (grid !! (r+1) !! c)*512
    where r = fromIntegral x
          c = fromIntegral y

numTriangles :: [[Int]] -> Int
numTriangles [] = 0
numTriangles grid = sumTriangles grid
    where sumTriangles = sum . map (sum . filter (>=3))

isQuadLegal :: Int -> Bool
isQuadLegal q = if thrd xx then False else True
    where xx = foldl switchCycle (False,0,False) (cycles q)

switchCycle (active, wCount, illegalQuad) color = case color of
    1 -> if (active && (odd wCount || (wCount == 6)))
             then (active, wCount, True)
             else (True, 0, illegalQuad)
    2 -> (active, wCount + 1, illegalQuad)
    0 -> (False, wCount, illegalQuad)

thrd (_,_,z) = z
fst' (x,_,_,_,_) = x
snd' (_,y,_,_,_) = y

cycles :: Int -> [Int]
cycles q = getColor (q `mod` 8) 5 : getColor (q `mod` 8) 4 : getColor ((shiftR (q::Int) 3) `mod` 8) 7 : getColor ((shiftR (q::Int) 3) `mod` 8) 6 :
 getColor ((shiftR (q::Int) 6) `mod` 8) 1 : getColor ((shiftR (q::Int) 6) `mod` 8) 0 : getColor ((shiftR (q::Int) 9) `mod` 8) 3 :
 getColor ((shiftR (q::Int) 9) `mod` 8) 2 : 
 getColor (q `mod` 8) 5 : getColor (q `mod` 8) 4 : getColor ((shiftR (q::Int) 3) `mod` 8) 7 : getColor ((shiftR (q::Int) 3) `mod` 8) 6 :
 getColor ((shiftR (q::Int) 6) `mod` 8) 1 : getColor ((shiftR (q::Int) 6) `mod` 8) 0 : getColor ((shiftR (q::Int) 9) `mod` 8) 3 :
 getColor ((shiftR (q::Int) 9) `mod` 8) 2  : []

isLegal = map isQuadLegal [0..4095]

delta = [[0,1],[0,-1],[1,0],[-1,0]]

userGrid = do
    inp <- readInts
    inps <- replicateM (head inp) getLine
    return (inps)

original :: [String] -> [String]
original [] = []
original x = [topDownBorder] ++ paddedMiddle ++ [topDownBorder]
    where topDownBorder = ['*'] ++ replicate (length (x !! 0)) '*' ++ ['*']
          paddedMiddle = (map (\x -> "*" ++ x ++ "*") x)

workingGrid :: [[Int]]
workingGrid = map (map (\x -> if x == '.' then unknown else black)) $ original userGrid

legalBlack :: [[Int]] -> Int -> Int -> Bool
legalBlack grid r c = if (original userGrid !! r !! c == '*') then True else let
        tri = fst' (foldl constrainedBlack (0,0,grid,r,c) delta)
        unknownCount = snd' (foldl constrainedBlack (0,0,grid,r,c) delta)
        need = read ((original userGrid) !! 2 !! 2 : []) :: Int
        in (tri <= need && need <= tri + unknownCount)

constrainedBlack (tri, unknownCount, grid, r, c) d
    | (grid !! (r + d !! 0) !! (c + d !! 1)) == 0 = (tri, unknownCount + 1, grid, r, c)
    | (grid !! (r + d !! 0) !! (c + d !! 1)) >= 3 = (tri + 1, unknownCount , grid, r, c)

readInts :: IO [Int]
readInts = fmap (map read.words) getLine

myGrid x = map (map read . words) x :: [[Int]]

--solve x = map solveLine x


