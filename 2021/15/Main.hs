{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.State
import Data.Char
import Data.Foldable (Foldable (foldr'), minimumBy)
import qualified Data.Map as M
import Data.Maybe

inf :: Int
inf = maxBound

main :: IO ()
main = do
  -- input <- toMap . lines <$> getContents
  input <- toMap . lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: M.Map (Int, Int) Int -> Int
p1 input = snd . head . M.toDescList . spt $ execState runDijkstra (initST input)

p2 :: M.Map (Int, Int) Int -> Int
p2 = p1 . fullMap

toMap :: [String] -> M.Map (Int, Int) Int
toMap = foldr' (\(i, l) m -> foldr' (\(j, p) m' -> M.insert (i, j) (digitToInt p) m') m l) M.empty . zip [0 ..] . map (zip [0 ..])

data DijkstraST = DijkstraST
  { graph :: M.Map (Int, Int) Int,
    current :: (Int, Int),
    unvisited :: M.Map (Int, Int) Int,
    spt :: M.Map (Int, Int) Int,
    bounds :: (Int, Int)
  }
  deriving (Show)

initST :: M.Map (Int, Int) Int -> DijkstraST
initST g =
  DijkstraST
    { graph = g,
      current = (0, 0),
      unvisited = M.foldrWithKey' (\k _ m' -> M.insert k (if k == (0, 0) then 0 else inf) m') M.empty g,
      spt = M.empty,
      bounds = fst $ M.findMax g
    }

runDijkstra :: State DijkstraST ()
runDijkstra = do
  done <- gets (null . unvisited)
  if done
    then return ()
    else do
      ((x, y), v) <- minimumNode
      st <- get
      forM_ [(x', y') | (x', y') <- [(x -1, y), (x, y + 1), (x + 1, y), (x, y -1)], x' >= 0, y' >= 0, x' <= fst (bounds st), y' <= snd (bounds st), M.member (x', y') (unvisited st)] (dijkstraUpdate (fromJust $ M.lookup (x, y) (spt st)))
      runDijkstra

minimumNode :: State DijkstraST ((Int, Int), Int)
minimumNode = do
  (k, v) <- gets (M.foldrWithKey' (\k c (k', c') -> if c <= c' then (k, c) else (k', c')) ((-1, -1), inf) . unvisited)
  modify (\s -> s {unvisited = M.delete k $ unvisited s, spt = M.insert k v (spt s)})
  return (k, v)

dijkstraUpdate :: Int -> (Int, Int) -> State DijkstraST ()
dijkstraUpdate c (x, y) = do
  curTent <- tentativeCost (x, y)
  r <- risk (x, y)
  when (c + r < curTent) $ do
    setCost (c + r) (x, y)

risk :: (Int, Int) -> State DijkstraST Int
risk (x, y) = gets (fromJust . M.lookup (x, y) . graph)

tentativeCost :: (Int, Int) -> State DijkstraST Int
tentativeCost (x, y) = gets (fromJust . M.lookup (x, y) . unvisited)

setCost :: Int -> (Int, Int) -> State DijkstraST ()
setCost c (x, y) = modify (\s -> s {unvisited = M.update (pure . const c) (x, y) (unvisited s)})

fullMap :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
fullMap m =
  let (maxX, maxY) = fst . head $ M.toDescList m
   in M.foldrWithKey'
        ( \(x, y) r m' ->
            foldr'
              ( \(x'', y'', r') m'' -> M.insert (x'', y'') (newRisk (r + r')) m''
              )
              m'
              [ (x', y', dx + dy) | (dx, x') <- zip [0 .. 4] (map ((+ x) . (* (maxX + 1))) [0 .. 4]), (dy, y') <- zip [0 .. 4] (map ((+ y) . (* (maxY + 1))) [0 .. 4])
              ]
        )
        M.empty
        m

newRisk :: Int -> Int
newRisk n
  | n > 9 = n - 9
  | otherwise = n
