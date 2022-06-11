module Main where

import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [String] -> Int
p1 input = count ps []
  where ps = points $ map readLineSeg input

p2 :: [String] -> Int
p2 input = count ps []
  where ps = points' $ map readLineSeg input

readLineSeg :: String -> ((Int,Int),(Int,Int))
readLineSeg c = ((read fstt,read sndd),(read thrd,read frth))
  where fstt = takeWhile isDigit c
        sndd = takeWhile isDigit $ fromJust $ stripPrefix (fstt ++ ",") c
        thrd = takeWhile isDigit $ fromJust $ stripPrefix (fstt ++ "," ++ sndd ++ " -> ") c
        frth = takeWhile isDigit $ fromJust $ stripPrefix (fstt ++ "," ++ sndd ++ " -> " ++ thrd ++ ",") c

points :: [((Int,Int),(Int,Int))] -> [(Int,Int)]
points [] = []
points (((x1,y1),(x2,y2)):ps)
  | x1 == x2 && y1 <= y2 = zip (repeat x1) [y1 .. y2] ++ points ps
  | x1 == x2 && y2 <= y1 = zip (repeat x1) [y2 .. y1] ++ points ps
  | x1 <= x2 && y1 == y2 = zip [x1 .. x2] (repeat y2) ++ points ps
  | x2 <= x1 && y1 == y2 = zip [x2 .. x1] (repeat y2) ++ points ps
  | otherwise = points ps

points' :: [((Int,Int),(Int,Int))] -> [(Int,Int)]
points' [] = []
points' (((x1,y1),(x2,y2)):ps)
  | abs (x1 - x2) == abs (y1 - y2) && x1 <= x2 && y1 <= y2 = zip [x1 .. x2] [y1 .. y2] ++ points' ps
  | abs (x1 - x2) == abs (y1 - y2) && x1 <= x2 && y2 <= y1 = zip [x1 .. x2] (reverse [y2 .. y1]) ++ points' ps
  | abs (x1 - x2) == abs (y1 - y2) && x2 <= x1 && y1 <= y2 = zip (reverse [x2 .. x1]) [y1 .. y2] ++ points' ps
  | abs (x1 - x2) == abs (y1 - y2) && x2 <= x1 && y2 <= y1 = zip [x2 .. x1] [y2 .. y1] ++ points' ps
  | otherwise = points ps

count :: [(Int,Int)] -> [(Int,Int)] -> Int
count [] _ = 0
count (p:ps) seen = if notElem p seen && p `elem` ps then 1 + count ps (p:seen) else count ps seen
