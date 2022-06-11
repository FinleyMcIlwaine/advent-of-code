module Main where

import Data.Map.Strict hiding (foldl, foldr)
import Prelude hiding (lookup)

main :: IO ()
main = do
  input <- getContents
  print $ p1 input
  print $ p2 input

data State = State {coords :: (Int, Int), visited :: Map (Int, Int) Int} deriving (Show)

initial = State {coords = (0, 0), visited = insert (0, 0) 1 empty}

f :: State -> Char -> State
f s c = case lookup newPos (visited s) of
  Just n -> s {coords = newPos, visited = update (pure . (+ 1)) newPos (visited s)}
  Nothing -> s {coords = newPos, visited = insert newPos 1 (visited s)}
  where
    newPos = pos c (coords s)

pos :: Char -> (Int, Int) -> (Int, Int)
pos 'v' (x, y) = (x, y -1)
pos '>' (x, y) = (x + 1, y)
pos '^' (x, y) = (x, y + 1)
pos '<' (x, y) = (x -1, y)

p1 :: String -> Int
p1 = length . visited . foldl f initial

p2 :: String -> Int
p2 s = length . visited $ foldl f (santaState {coords = (0, 0)}) roboSanta
  where
    (santa, roboSanta) = foldr (\(x, i) (s, rs) -> if even i then (x : s, rs) else (s, x : rs)) ([], []) (zip s [0 ..])
    santaState = foldl f initial santa
