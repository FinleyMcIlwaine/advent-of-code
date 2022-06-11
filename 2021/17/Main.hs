module Main where

import Control.Monad.State
import Data.Char (isDigit)
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = do
  input <- readTargetArea <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: M.Map (Int, Int) Int -> Int
p1 ta = let ((minXVel, _), (_, maxYVel)) = velBounds ta in snd $ launch ta (minXVel,maxYVel)

p2 :: M.Map (Int, Int) Int -> Int
p2 ta =
  let ((minX, minY), (maxX, maxY)) = velBounds ta
   in foldr' (\v n -> if fst (launch ta v) then 1 + n else n) 0 [(xVel, yVel) | xVel <- [minX .. maxX], yVel <- [minY .. maxY]]

readTargetArea :: String -> M.Map (Int, Int) Int
readTargetArea s =
  let s' = dropWhile (not . isDigit) s
      (xs, rs) = readRange s'
      rs' = dropWhile (not . (\c -> c == '-' || isDigit c)) rs
      (ys, _) = readRange rs'
   in foldr' (uncurry M.insert) M.empty [((x, y), 0) | x <- xs, y <- ys]

readRange :: String -> ([Int], String)
readRange ss =
  let i1 = read (takeWhile (\c -> isDigit c || c == '-') ss)
      i2 = read . takeWhile (\c -> isDigit c || c == '-') . dropWhile (== '.') $ dropWhile (\c -> isDigit c || c == '-') ss
   in ([i1 .. i2], dropWhile (\c -> isDigit c || c == '-') . dropWhile (== '.') $ dropWhile (\c -> isDigit c || c == '-') ss)

launch :: M.Map (Int,Int) Int -> (Int,Int) -> (Bool,Int)
launch ta vel = evalState (steps ta minYPos) ((0, 0), vel)
  where minYPos = snd . fst $ M.findMin ta

steps :: M.Map (Int, Int) Int -> Int -> State ((Int, Int), (Int, Int)) (Bool, Int)
steps ta minY = do
  (x, y) <- step
  if y <= minY
    then return (M.member (x, y) ta, y)
    else do
      (through, maxY) <- steps ta minY
      return (through || M.member (x, y) ta, if y > maxY then y else maxY)

step :: State ((Int, Int), (Int, Int)) (Int, Int)
step = do
  ((xPos, yPos), (xVel, yVel)) <- get
  let newPos = (xPos + xVel, yPos + yVel)
  put (newPos, (newVel xVel, yVel - 1))
  return newPos
  where
    newVel v
      | v > 0 = v -1
      | v < 0 = v + 1
      | otherwise = 0

velBounds :: M.Map (Int, Int) Int -> ((Int, Int), (Int, Int))
velBounds ta =
  ( (fromJust $ find (\d -> sum [0 .. d] >= minX) [0 ..], minY),
    (maxX, abs minY -1)
  )
  where
    (minX, minY) = fst $ M.findMin ta
    (maxX, maxY) = fst $ M.findMax ta
