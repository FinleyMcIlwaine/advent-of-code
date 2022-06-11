module Main where

main :: IO ()
main = do
  input <- (\ns -> read $ '[':ns++"]") <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [Int] -> Int
p1 xs = minimum (map (align (.+) xs) [0..maximum xs])

p2 :: [Int] -> Int
p2 xs = minimum (map (align (+.) xs) [0..maximum xs])

align :: (Int -> Int -> Int) -> [Int] -> Int -> Int
align cost xs n = align' n 0 xs
  where align' _ acc [] = acc 
        align' n acc (x:xs) = align' n (acc + cost n x) xs

(.+) :: Int -> Int -> Int
(.+) x y = abs (x - y)

(+.) :: Int -> Int -> Int
(+.) x y
  | x == y = 0
  | x > y = sum [1..abs x - y]
  | otherwise = sum [1..abs y - x]
