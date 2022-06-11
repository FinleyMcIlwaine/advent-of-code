module Main where
import Data.List.Split (splitOn)
import Data.List
import Data.Foldable (Foldable(foldr'))

main :: IO ()
main = do
  input <- map toEdge . lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [(String, String, Int)] -> Int
p1 = minimum . costs
p2 :: [(String, String, Int)] -> Int
p2 = maximum . costs

costs :: [(String, String, Int)] -> [Int]
costs input = map (cost input) . permutations $ places input

toEdge :: String -> (String,String,Int)
toEdge ss = (if s <= t then s else t, if s > t then s else t,cost)
  where [s, _, t, _, c] = splitOn " " ss
        cost = read c

places :: [(String,String,Int)] -> [String]
places = (\(sources, targets, _) -> nub $ sources `union` targets) . unzip3

cost :: [(String,String,Int)] -> [String] -> Int
cost g xs = cost' g xs 0
  where cost' g [] n = n
        cost' g [x] n = n
        cost' g (x:y:xs) n
          | x <= y = cost' g (y:xs) (edgeCost g (x,y) + n) 
          | otherwise = cost' g (y:xs) (edgeCost g (y,x) + n) 

edgeCost :: [(String,String,Int)] -> (String,String) -> Int
edgeCost g (x,y) = foldr' (\(x',y',c') c -> if x' == x && y' == y then c' else c) 1000 g
