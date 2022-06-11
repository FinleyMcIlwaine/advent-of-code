{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Foldable
import Data.List
import qualified Data.Map as M
import Data.Maybe

instance {-# OVERLAPPING #-} Show (M.Map (Int, Int) Int) where
  show m = unlines [[if isJust (m M.!? (x, y)) then '#' else '.' | x <- [0 .. x']] | y <- [0 .. y' + 1]]
    where
      (Just (((x', y'), _), _)) = M.maxViewWithKey m

main :: IO ()
main = do
  (grid, folds) <- (\(coords, [] : folds) -> (foldr' readCoord M.empty coords, foldr' readFold [] folds)) . (\ls -> splitAt (fromJust $ elemIndex "" ls) ls) . lines <$> getContents
  print $ p1 folds grid
  print $ p2 folds grid

p1 :: [(Char, Int)] -> M.Map (Int, Int) Int -> Int
p1 (f : _) m = M.size $ foldIt f m

p2 :: [(Char, Int)] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
p2 folds grid = foldl' (flip foldIt) grid folds

readCoord :: String -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
readCoord raw = let (x, ',' : y) = splitAt (fromJust $ elemIndex ',' raw) raw in M.insertWith (+) (read x, read y) 1

readFold :: String -> [(Char, Int)] -> [(Char, Int)]
readFold raw = let (cs, '=' : l) = splitAt (fromJust $ elemIndex '=' raw) raw in ((last cs, read l) :)

foldIt :: (Char, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
foldIt (c, l) = M.foldrWithKey' (M.insertWith (+) . foldPt c l) M.empty

foldPt :: Char -> Int -> (Int, Int) -> (Int, Int)
foldPt 'x' l (x, y)
  | x > l = (l - (x - l), y)
  | otherwise = (x, y)
foldPt 'y' l (x, y)
  | y > l = (x, l - (y - l))
  | otherwise = (x, y)
