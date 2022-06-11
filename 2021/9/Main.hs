module Main where

import Data.Char
import Data.Foldable
import Data.List

main :: IO ()
main = do
  input <- map (map digitToInt) . lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [[Int]] -> Int
p1 = sum . map (foldr' (\((n,t1),(_,t2)) s -> if t1 && t2 then s + n + 1 else s) 0) . findLowPoints

p2 :: [[Int]] -> Int
p2 = product . take 3 . reverse . sort . basinSizes

findLowPoints :: [[Int]] -> [[((Int,Bool),(Int,Bool))]]
findLowPoints grid = let
  lowPoints = map (lowPointsRow . toTriplets) grid
  lowPoints' = transpose $ map (lowPointsRow . toTriplets) (transpose grid)
  in zipWith zip lowPoints lowPoints'

toTriplets :: [Int] -> [(Int,Int,Int)]
toTriplets [] = []
toTriplets [x] = []
toTriplets (x':y':xs') = (x'+1,x',y'):toTriplets' (x':y':xs')
  where toTriplets' [x,y] = [(x,y,y+1)]
        toTriplets' (x:y:z:xs) = (x,y,z) : toTriplets' (y:z:xs)
        toTriplets' _ = undefined

lowPointsRow :: [(Int,Int,Int)] -> [(Int,Bool)]
lowPointsRow = foldr' (\(x,y,z) bs -> (y,y < x && y < z) : bs) []

basinCells :: [[Int]] -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
basinCells g i j acc = let 
  cur = val g i j 
  acc1 = if val g (i-1) j > cur && val g (i-1) j /= 9 && (i-1,j) `notElem` acc then basinCells g (i-1) j ((i-1,j):acc) else acc
  acc2 = if val g (i+1) j > cur && val g (i+1) j /= 9 && (i+1,j) `notElem` acc1 then basinCells g (i+1) j ((i+1,j):acc1) else acc1
  acc3 = if val g i (j-1) > cur && val g i (j-1) /= 9 && (i,j-1) `notElem` acc2 then basinCells g i (j-1) ((i,j-1):acc2) else acc2
  acc4 = if val g i (j+1) > cur && val g i (j+1) /= 9 && (i,j+1) `notElem` acc3 then basinCells g i (j+1) ((i,j+1):acc3) else acc3
  in acc4

lowPointIndices :: [[Int]] -> [(Int, Int)]
lowPointIndices = concatMap lowPointIndicesRow . transpose . map (zip [0..]) . transpose . map (zip [0..]) . findLowPoints

lowPointIndicesRow :: [(Int,(Int,((Int,Bool),(Int,Bool))))] -> [(Int,Int)]
lowPointIndicesRow = foldr' f []
  where f (x,(y,((_,True),(_,True)))) acc = (x,y):acc
        f _ acc = acc

basinSizes :: [[Int]] -> [Int]
basinSizes g = map (\(x,y) -> 1 + length (basinCells g x y [])) $ lowPointIndices g

val :: [[Int]] -> Int -> Int -> Int
val g i j
  | i < 0 || j < 0 || i >= length g || j >= length (head g) = -1
  | otherwise = g !! i !! j

