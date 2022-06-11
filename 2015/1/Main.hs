module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = do
  input <- getContents
  print (p1 input)
  print (p2 input)

f :: Int -> Char -> Int
f x '(' = x + 1
f x _ = x - 1

p1 :: String -> Int
p1 = foldl f 0

p2 :: String -> Maybe Int
p2 = findIndex (<0) . scanl f 0