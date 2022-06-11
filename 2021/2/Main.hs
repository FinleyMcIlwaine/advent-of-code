module Main where

import Data.Char
import Data.List

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [String] -> Int
p1 = uncurry (*) . foldl move (0,0)

p2 :: [String] -> Int
p2 = (\(x,y,z) -> x * y) . foldl move' (0,0,0)

move :: (Int,Int) -> String -> (Int,Int)
move (x,y) s
  | "forward" `isPrefixOf` s = (x+n',y)
  | "down" `isPrefixOf` s = (x,y+n')
  | "up" `isPrefixOf` s = (x,y-n')
  | otherwise = error "invalid move"
  where n' = read (dropWhile (not . isDigit) s)

move' :: (Int,Int,Int) -> String -> (Int,Int,Int)
move' (x,y,z) s
  | "forward" `isPrefixOf` s = (x+n',y+z*n',z)
  | "down" `isPrefixOf` s = (x,y,z+n')
  | "up" `isPrefixOf` s = (x,y,z-n')
  | otherwise = error "invalid move"
  where n' = read (dropWhile (not . isDigit) s)
