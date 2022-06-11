{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Foldable
import Data.List

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [String] -> Int
p1 = foldr' (\(c, _, b) acc -> if not b then toPoints c + acc else acc) 0 . map (`match` [])

p2 :: [String] -> Int
p2 = (\l -> l !! (length l `div` 2)) . sort . map (foldl' (\acc c -> toPoints' c + acc * 5) 0 . snd . (`complete` [])) . filter needsCompleted

match :: String -> String -> (Char, Char, Bool)
match [] [] = ('x', 'x', True)
match [] (s : ss) = (s, closer s, False)
match (c : cs) []
  | isOpening c = match cs [c]
  | isClosing c = (c, c, False)
match (c : cs) (s : ss)
  | isOpening c = match cs (c : s : ss)
  | isClosing c = if c == closer s then match cs ss else (c, c, False)

needsCompleted :: String -> Bool
needsCompleted = (\(c1, c2, b) -> not b && not (isClosing c1 && isClosing c2)) . (`match` [])

complete :: String -> String -> (String, String)
complete s acc = case match (s ++ acc) [] of
  (_, _, True) -> (s, acc)
  (c1, c2, False) -> complete s (acc ++ [c2])

isOpening :: Char -> Bool
isOpening = (`elem` "([<{")

isClosing :: Char -> Bool
isClosing = (`elem` ")]>}")

toPoints :: Char -> Int
toPoints ')' = 3
toPoints ']' = 57
toPoints '}' = 1197
toPoints '>' = 25137
toPoints _ = 0

toPoints' :: Char -> Int
toPoints' ')' = 1
toPoints' ']' = 2
toPoints' '}' = 3
toPoints' '>' = 4
toPoints' _ = 0

closer :: Char -> Char
closer '(' = ')'
closer '[' = ']'
closer '<' = '>'
closer '{' = '}'
closer _ = 'y'
