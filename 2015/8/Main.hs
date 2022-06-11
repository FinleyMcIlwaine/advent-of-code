module Main where

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [String] -> Int
p1 = foldr (\s n -> length s - dataSize s + n) 0

p2 :: [String] -> Int
p2 = foldr (\s n -> length (show s) - length s + n) 0

dataSize :: String -> Int
dataSize = dataSize' 0
  where dataSize' n [] = n - 2
        dataSize' n ('\\':'\\':xs) = dataSize' (n + 1) xs
        dataSize' n ('\\':'"':xs) = dataSize' (n + 1) xs
        dataSize' n ('\\':'x':_:_:xs) = dataSize' (n + 1) xs 
        dataSize' n (x:xs) = dataSize' (n + 1) xs
