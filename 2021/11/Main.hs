{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Char
import Data.Foldable
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified GHC.Read as M

main :: IO ()
main = do
  input <- toMap . lines <$> getContents
  print $ p1 input

-- print $ p2 input

p1 :: M.Map (Int, Int) Int -> Int
p1 input = snd $ iterate step (input, 0) !! 10000000

p2 :: M.Map (Int, Int) Int -> Int
p2 input = fromJust $ findIndex (all0 . fst) $ iterate step (input, 0)

all0 = M.foldr' ((&&) . (== 0)) True

step :: (M.Map (Int, Int) Int, Int) -> (M.Map (Int, Int) Int, Int)
step (m, i) = (\(m', n) -> (m', n + i)) . second [] $ first m

toMap :: [String] -> M.Map (Int, Int) Int
toMap = foldr' (\(i, l) m -> foldr' (\(j, p) m' -> M.insert (i, j) (digitToInt p) m') m l) M.empty . zip [0 ..] . map (zip [0 ..])

first :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
first = M.map succ

instance {-# OVERLAPPING #-} Show (M.Map (Int, Int) Int) where
  show m = unlines . map (map (intToDigit . snd)) $ chunksOf (maxj + 1) (M.toList m)
    where
      (maxi, maxj) = maxes m

second :: [(Int, Int)] -> M.Map (Int, Int) Int -> (M.Map (Int, Int) Int, Int)
second alreadyFlashed m =
  let flashes = getFlashes m \\ alreadyFlashed
   in if null flashes
        then (foldr' (\(x, y) mm -> M.update (const (pure 0)) (x, y) mm) m alreadyFlashed, length alreadyFlashed)
        else second (alreadyFlashed `union` flashes) (foldr' (\(x, y) mm -> flash (x, y) mm) m flashes)

getFlashes :: M.Map (Int, Int) Int -> [(Int, Int)]
getFlashes = M.foldrWithKey' (\(i, j) n -> if n > 9 then ((i, j) :) else id) []

flash :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
flash (i, j) m =
  let m' = M.update (const (pure 0)) (i, j) m
   in foldr'
        (\(x, y) -> M.update (pure . succ) (x, y))
        m'
        [ (x, y)
          | x <- [i - 1 .. i + 1],
            x >= 0 && x <= maxi,
            y <- [j - 1 .. j + 1],
            y >= 0 && y <= maxj
        ]
  where
    (maxi, maxj) = maxes m

maxes :: M.Map (Int, Int) Int -> (Int, Int)
maxes = fst . head . M.toDescList
