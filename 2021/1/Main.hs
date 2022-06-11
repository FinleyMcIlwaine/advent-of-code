module Main where

main :: IO ()
main = do
  input <- map read . lines <$> getContents 
  print $ p1 input
  print $ p2 input

p1 :: [Int] -> Int
p1 = foldr (\(x,y) n -> n + if y > x then 1 else 0) 0 . pairs

p2 :: [Int] -> Int
p2 = p1 . tripletSums

pairs xs = zip xs $ tail xs

tripletSums (x:y:z:xs) = x+y+z : tripletSums (y:z:xs)
tripletSums _ = []