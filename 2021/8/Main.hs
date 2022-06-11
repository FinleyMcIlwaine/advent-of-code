module Main where

import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  input <- map readInput . lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [([String], [String])] -> Int
p1 = foldr (\(ins, outs) -> (+) (length $ filter (\s -> or [is1 s, is4 s, is7 s, is8 s]) outs)) 0

p2 = sum . map outputNum

readInput :: String -> ([String], [String])
readInput s = (splitOn " " . dropWhileEnd (== ' ') $ takeWhile (/= '|') s, splitOn " " . drop 2 $ dropWhile (/= '|') s)

is1 s = length s == 2

is4 s = length s == 4

is7 s = length s == 3

is8 s = length s == 7

str0 ss = head $ filter (\s -> length s == 6 && length (s \\ str7 ss) == 3 && s /= str9 ss) ss

str1 ss = head $ filter is1 ss

str2 ss = head $ filter (\s -> length s == 5 && length (s `union` str5 ss) == 7) ss

str3 ss = head $ filter (\s -> length s == 5 && length (str5 ss \\ s) == 1 && length (str2 ss \\ s) == 1) ss

str4 ss = head $ filter is4 ss

str5 ss = head $ filter (\s -> length s == 5 && length (str6 ss \\ s) == 1 && length (str1 ss \\ s) == 1) ss

str6 ss = head $ filter (\s -> length s == 6 && length (str8 ss \\ s) == 1 && length ((str8 ss \\ s) `intersect` str1 ss) == 1) ss

str7 ss = head $ filter is7 ss

str8 ss = head $ filter is8 ss

str9 ss = head $ filter (\s -> length s == 6 && length (s \\ str4 ss) == 2) ss

intStrs :: [String] -> [String]
intStrs ss = map sort [str0 ss, str1 ss, str2 ss, str3 ss, str4 ss, str5 ss, str6 ss, str7 ss, str8 ss, str9 ss]

toInt :: String -> [String] -> Int
toInt s ss = fromJust $ elemIndex (sort s) ss

outputNum :: ([String], [String]) -> Int
outputNum (ss, outs) = fst $ foldr' (\n (res, w) -> (res + n * w, w * 10)) (0, 1) digits
  where
    digits = map (\s -> toInt s (intStrs ss)) outs
