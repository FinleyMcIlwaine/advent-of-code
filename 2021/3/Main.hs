module Main where

import Data.Char
import Data.List
import Control.Monad.Fix

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [String] -> Int
p1 bs = epsilon bs * gamma bs

p2 :: [String] -> Int
p2 bs = o2GenRating bs * co2ScrubRating bs

epsilon :: [String] -> Int
epsilon = toInt . bits

gamma :: [String] -> Int
gamma = toInt . map (succ . negate) . bits

bits :: [String] -> [Int]
bits = map ((\(os,zs) -> fromEnum $ length os >= length zs) . partition (=='1')) . transpose

toInt :: [Int] -> Int
toInt bs = sum (zipWith (*) (reverse bs) (iterate (*2) 1))

mark :: (Int -> Bool) -> [[Char]] -> Int -> [([Char], Bool)]
mark keep bss pos = zip bss . map (keep . digitToInt) . (!! pos) $ transpose bss

iter :: (Int -> Bool) -> [[Char]] -> Int -> [[Char]]
iter keep bss pos = map fst $ filter snd $ mark keep bss pos

o2Filter :: [[Char]] -> [[Char]]
o2Filter = filt (==)

co2Filter :: [[Char]] -> [[Char]]
co2Filter = filt (/=)

filt :: (Int -> Int -> Bool) -> [[Char]] -> [[Char]]
filt eq bss = foldl (\bss' n -> if length bss' == 1 then bss' else iter (eq (bits bss' !! n)) bss' n) bss [0..length (head bss) - 1]

o2GenRating :: [[Char]] -> Int
o2GenRating = fix (\f bs -> if length bs == 1 then toInt (bits bs) else f $ o2Filter bs)

co2ScrubRating :: [[Char]] -> Int
co2ScrubRating = fix (\f bss -> if length bss == 1 then toInt (bits bss) else f $ co2Filter bss)
