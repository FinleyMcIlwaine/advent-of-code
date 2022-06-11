module Main where

import Data.Char

main :: IO ()
main = do
  input <- map toTriple . lines <$> getContents
  print $ p1 input
  print $ p2 input

toTriple :: String -> (Int,Int,Int)
toTriple s = read $ '(' : [ if x == 'x' then ',' else x | x <- s ] ++ ")"

minPair :: (Int,Int,Int) -> (Int,Int)
minPair (x,y,z)
  | x <= y && y <= z = (x,y)
  | x <= y && z <= y = (x,z)
  | y <= x && x <= z = (y,x)
  | y <= x && z <= x = (y,z)
  | z <= x && x <= y = (z,x)
  | z <= x && y <= x = (z,y)

wrappingPaper :: (Int,Int,Int) -> Int
wrappingPaper x@(l,w,h) = 2*l*w + 2*w*h + 2*l*h + uncurry (*) (minPair x)

p1 :: [(Int,Int,Int)] -> Int
p1 = sum . map wrappingPaper

ribbon :: (Int,Int,Int) -> Int
ribbon x@(l,w,h) = l*w*h + (*2) (uncurry (+) $ minPair x)

p2 :: [(Int,Int,Int)] -> Int
p2 = sum . map ribbon