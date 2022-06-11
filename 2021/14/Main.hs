module Main where

import Data.Foldable (Foldable (foldr'))
import Data.List.Split (splitOn)
import qualified Data.Map as M

main :: IO ()
main = do
  (template, rules) <- (\([template], [] : rules) -> (template, foldr' toRule M.empty rules)) . splitAt 1 . lines <$> getContents
  print $ p1 template rules
  print $ p2 template rules

p1 :: String -> M.Map (Char, Char) Char -> Int
p1 template rules = commonsDiff . M.insertWith (+) (last template, last template) 1 $ iterate (`expand` rules) (pairs' template) !! 10

p2 :: String -> M.Map (Char, Char) Char -> Int
p2 template rules = commonsDiff . M.insertWith (+) (last template, last template) 1 $ iterate (`expand` rules) (pairs' template) !! 40

toRule :: String -> M.Map (Char, Char) Char -> M.Map (Char, Char) Char
toRule r m = let [[c1, c2], [c3]] = splitOn " -> " r in M.insert (c1, c2) c3 m

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

pairs' :: Ord a => [a] -> M.Map (a, a) Int
pairs' = foldr' (\(e1, e2) -> M.insertWith (+) (e1, e2) 1) M.empty . pairs

expand :: M.Map (Char, Char) Int -> M.Map (Char, Char) Char -> M.Map (Char, Char) Int
expand ps m = M.foldrWithKey' check M.empty ps
  where
    check (c1, c2) n m' = case M.lookup (c1, c2) m of
      Just c -> M.insertWith (+) (c, c2) n $ M.insertWith (+) (c1, c) n m'
      Nothing -> m'

commonsDiff :: M.Map (Char, Char) Int -> Int
commonsDiff m = uncurry (-) (maximum accum, minimum accum)
  where
    reduce (c1, c2) n = M.insertWith (+) c1 n
    accum = M.foldrWithKey' reduce M.empty m
