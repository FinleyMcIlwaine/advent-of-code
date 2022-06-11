{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Char
import Data.List

main :: IO ()
main = do
  input <- map onToggleOff . lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [Act] -> Int
p1 = sum . map sum . foldl (flip act) grid

p2 :: [Act] -> Int
p2 = sum . map sum . foldl (flip actp2) grid

data Act = On ((Int,Int),(Int,Int)) | Off ((Int,Int),(Int,Int)) | Toggle ((Int,Int),(Int,Int)) deriving Show

instance {-# OVERLAPPING #-} Show [[Int]] where
  show [] = ""
  show (bs:bss) = show bs ++ "\n" ++ show bss

ps :: Act -> ((Int,Int),(Int,Int))
ps (On ps) = ps
ps (Off ps) = ps
ps (Toggle ps) = ps

onToggleOff :: String -> Act
onToggleOff s
  | "turn on" `isPrefixOf` s = On coords
  | "turn off" `isPrefixOf` s = Off coords
  | "toggle" `isPrefixOf` s = Toggle coords
  where fstCoords = read . (\s -> '(':s++")") . takeWhile (/= ' ') $ dropWhile (not . isDigit) s
        sndCoords = read . (\s -> '(':s++")") . reverse . takeWhile (/= ' ') $ reverse s
        coords = (fstCoords,sndCoords)

grid :: [[Int]]
grid = replicate 1000 $ replicate 1000 0

testGrid :: [[Int]]
testGrid = replicate 15 $ replicate 15 0

act :: Act -> [[Int]] -> [[Int]]
act actt [] = []
act actt bsss = act' (0,0) (toGate actt) bsss
  where act' (i,j) gate [] = []
        act' (i,j) gate (bs:bss) = act'' (i,j) gate bs : act' (i+1,j) gate bss
        act'' (i,j) gate [] = []
        act'' (i,j) gate (b:bs)
          | (i,j) `isIn` ps actt = gate b : act'' (i,j+1) gate bs
          | otherwise = b : act'' (i,j+1) gate bs
        isIn (i,j) ((k,l),(m,n)) = i >= k && i <= m && j >= l && j <= n
        toGate (On _) = const 1
        toGate (Off _) = const 0
        toGate (Toggle _) = succ . negate

actp2 :: Act -> [[Int]] -> [[Int]]
actp2 actt [] = []
actp2 actt bsss = act' (0,0) (toGate actt) bsss
  where act' (i,j) gate [] = []
        act' (i,j) gate (bs:bss) = act'' (i,j) gate bs : act' (i+1,j) gate bss
        act'' (i,j) gate [] = []
        act'' (i,j) gate (b:bs)
          | (i,j) `isIn` ps actt = gate b : act'' (i,j+1) gate bs
          | otherwise = b : act'' (i,j+1) gate bs
        isIn (i,j) ((k,l),(m,n)) = i >= k && i <= m && j >= l && j <= n
        toGate (On _) = (+1)
        toGate (Off _) = (\n -> if n < 0 then 0 else n) . pred
        toGate (Toggle _) = (+2)
