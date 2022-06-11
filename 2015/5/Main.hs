module Main where

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: [String] -> Int
p1 = foldr (\s c -> if nice s then c + 1 else c) 0

p2 :: [String] -> Int
p2 = foldr (\s c -> if nicep2 s then c + 1 else c) 0

nice :: String -> Bool
nice s = f s && g s && h s

nicep2 :: String -> Bool
nicep2 s = i s && j s 

f :: String -> Bool
f = (>=3) . foldr isVowel 0
  where isVowel 'a' = (+1)
        isVowel 'e' = (+1)
        isVowel 'i' = (+1)
        isVowel 'o' = (+1)
        isVowel 'u' = (+1)
        isVowel _ = id

g :: String -> Bool
g = any (uncurry (==)) . pairs


h :: String -> Bool
h = all notBad . pairs
  where notBad ('a','b') = False
        notBad ('c','d') = False
        notBad ('p','q') = False
        notBad ('x','y') = False
        notBad _         = True

pairs :: [a] -> [(a, a)]
pairs s = zip s (tail s)

i :: String -> Bool
i = check . pairs
  where check [] = False
        check [p] = False
        check ((w,x):(y,z):ps) = (w,x) `elem` ps || check ((y,z):ps)

j :: String -> Bool
j s = any (uncurry (==)) $ zip s (tail (tail s))
