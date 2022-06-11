{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Char
import Data.List (foldl1')

data Tree a = N a | S (Tree a) (Tree a) deriving (Eq)

instance Show a => Show (Tree a) where
  show (N n) = show n
  show (S t1 t2) = "[" ++ show t1 ++ "," ++ show t2 ++ "]"

instance Read a => Read (Tree a) where
  readsPrec _ [] = []
  readsPrec d sn
    | isDigit (head sn) = [(N (read (takeWhile isDigit sn)), dropWhile isDigit sn)]
    | '[' /= head sn = []
    | otherwise = case readsPrec d (tail sn) :: [(Tree a, String)] of
      [] -> []
      [(se1, r)] ->
        if null r || head r /= ','
          then []
          else case readsPrec d (tail r) :: [(Tree a, String)] of
            [] -> []
            [(se2, r')] -> [(S se1 se2, tail r') | not (null r' || head r' /= ']')]

main :: IO ()
main = do
  input <- map (read :: String -> Tree Int) . lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [Tree Int] -> Int
p1 = magnitude . foldl1' (\l r -> reduce $ S l r)

p2 :: [Tree Int] -> Int
p2 ts = maximum [max (magnitude . reduce $ S l r) (magnitude . reduce $ S r l) | l <- ts, r <- ts, l /= r]

explode :: Tree Int -> Maybe (Tree Int, Int, Int)
explode = explode' 0
  where
    explode' 4 (S (N nl) (N nr)) = Just (N 0, nl, nr)
    explode' d (S l r) = case explode' (d + 1) l of
      Just (t, nl, nr) -> Just (S t (addL nr r), nl, 0)
      Nothing -> case explode' (d + 1) r of
        Just (t, nl, nr) -> Just (S (addR nl l) t, 0, nr)
        Nothing -> Nothing
    explode' _ n = Nothing
    addL n (N m) = N (n + m)
    addL n (S l r) = S (addL n l) r
    addR n (N m) = N (n + m)
    addR n (S l r) = S l (addR n r)

split :: Tree Int -> Maybe (Tree Int)
split (N n)
  | n < 10 = Nothing
  | otherwise = Just (S (N $ n `div` 2) (N $ (n+1) `div` 2))
split (S l r) = case split l of
  Just l' -> Just (S l' r)
  Nothing -> case split r of
    Just r' -> Just (S l r')
    Nothing -> Nothing

reduce :: Tree Int -> Tree Int
reduce t = case explode t of
  Just (t', _, _) -> reduce t'
  Nothing -> maybe t reduce (split t)

magnitude :: Tree Int -> Int
magnitude (N n) = n
magnitude (S l r) = magnitude l * 3 + magnitude r * 2
