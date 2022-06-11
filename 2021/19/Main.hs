{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Char
import Data.Foldable
import Data.List (foldl1', transpose, nub)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import Debug.Trace (traceShowId)
import Data.Either (partitionEithers)

main :: IO ()
main = do
  input <- map (map (map (read :: String -> Int) . splitOn ",") . tail) . splitOn [""] . lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 (first:rest) = length . nub . concatMap fst $ align [(first, [0,0,0])] [first] rest
p2 (first:rest) = maximum . map (uncurry manhattan) . pick2 . map snd $ align [(first, [0,0,0])] [first] rest

type Coords = [Int]
type Scanner = [Coords]

align :: [(Scanner,Coords)] -> [Scanner] -> [Scanner] -> [(Scanner, Coords)]
align result _ [] = result
align result (ref:refs) scanners = align (found ++ result) (map fst found ++ refs) notFound
  where (found,notFound) = partitionEithers
                          [
                            maybe (Right scanner) Left . safeHead $ align2 ref scanner | scanner <- scanners
                          ]

align2 :: Scanner -> Scanner -> [(Scanner, Coords)]
align2 a b = [(map (add pos) o, pos) | o <- orientations b, pos <- overlap a o]

overlap :: Scanner -> Scanner -> [Coords]
overlap as bs = M.keys . M.filter (>= 12) . M.fromListWith (+) . map (, 1) $ 
                diff <$> as <*> bs

orientations :: Scanner -> [Scanner]
orientations ps = transpose $ map orientationsCoords ps

orientationsCoords :: Coords -> [Coords]
orientationsCoords p = scanl (flip ($)) p steps
  where steps = [r,t,t,t,r,t,t,t,r,t,t,t, r.t.r.r, t,t,t,r,t,t,t,r,t,t,t]
        r [x,y,z] = [x,z,-y]
        t [x,y,z] = [-y,x,z]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

pick2 :: [a] -> [(a,a)]
pick2 [] = []
pick2 (x:xs) = map (x ,) xs ++ pick2 xs

manhattan :: Coords -> Coords -> Int
manhattan a b = sum $ map abs $ diff a b
 
diff :: Coords -> Coords -> Coords
diff = zipWith (-)

add :: Coords -> Coords -> Coords
add = zipWith (+)
