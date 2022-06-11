module Main where

import qualified Data.Map.Strict as M
import Data.Foldable

main :: IO ()
main = do
  input <- (\ns -> read $ '[':ns++"]") <$> getContents
  print $ p1 input
  print $ p2 input

getDayCount :: Int -> [Int] -> Int
getDayCount d = count . (!!d) . iterate runDay . toMap

p1 :: [Int] -> Int
p1 = getDayCount 80

p2 :: [Int] -> Int
p2 = getDayCount 256

runDay :: M.Map Int Int -> M.Map Int Int
runDay ps = case M.lookup 0 ps of
  Nothing -> M.mapKeys pred ps
  Just n -> M.insertWith (+) 6 n . M.insert 8 n . M.mapKeys pred $ M.delete 0 ps

count :: M.Map Int Int -> Int
count = foldr' (\(_,n) acc -> n + acc) 0 . M.toList

toMap :: [Int] -> M.Map Int Int
toMap = foldr' (\f ps -> M.insertWith (+) f 1 ps) M.empty
