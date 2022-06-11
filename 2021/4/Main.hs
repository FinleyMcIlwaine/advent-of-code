module Main where

import Data.Char
import Data.List

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 input = play (readPlays $ head input) (readBoards $ tail (tail input))

p2 input = play' (readPlays $ head input) (readBoards $ tail (tail input))

readPlays :: String -> [Int]
readPlays [] = []
readPlays (',':cs) = readPlays cs
readPlays cs = read (takeWhile isDigit cs) : readPlays (dropWhile isDigit cs)

readBoards :: [String] -> [[[(Int,Bool)]]]
readBoards [] = []
readBoards ([]:bs) = readBoards bs
readBoards bs = readBoard (takeWhile (/= "") bs) : readBoards (dropWhile (/= "") bs)

readBoard :: [String] -> [[(Int,Bool)]]
readBoard = map readLine

readLine :: String -> [(Int,Bool)]
readLine [] = []
readLine (' ':cs) = readLine cs
readLine cs = (read (takeWhile isDigit cs),False) : readLine (dropWhile isDigit cs)

playMoveLine :: Int -> [(Int,Bool)] -> [(Int,Bool)]
playMoveLine n [] = []
playMoveLine n ((n',b):bs)
  | n == n' = (n',True) : playMoveLine n bs
  | otherwise = (n',b) : playMoveLine n bs

playMoveBoard :: Int -> [[(Int,Bool)]] -> [[(Int,Bool)]]
playMoveBoard n = map (playMoveLine n)

playMove :: Int -> [[[(Int,Bool)]]] -> [[[(Int,Bool)]]]
playMove n = map (playMoveBoard n)

play :: [Int] -> [[[(Int,Bool)]]] -> Int
play [] bs = 0
play (p:ps) bs = let bs' = playMove p bs in
  if winner bs' 0 /= -1 then
    sumUnmarked (bs' !! winner bs' 0) * p
  else
    play ps bs'

play' :: [Int] -> [[[(Int,Bool)]]] -> Int
play' [] bs = 0
play' (p:ps) bs = let bs' = playMove p bs in
  if winner bs' 0 /= -1 && length bs' == 1 then
    sumUnmarked (head bs') * p
  else if winner bs' 0 /= -1 then
    play' ps (filterWinners bs')
  else
    play' ps bs'

filterWinners bs = if winner bs 0 == -1 then bs else
  filterWinners $ take (winner bs 0) bs ++ drop (winner bs 0 + 1) bs

winner :: [[[(Int,Bool)]]] -> Int -> Int
winner [] _  = -1
winner (b:bs) n = if boardWins b then n else winner bs (n+1)

boardWins b = any (all snd) b || any (all snd) (transpose b)

sumUnmarked :: [[(Int,Bool)]] -> Int
sumUnmarked b = sum $ map (sum . map (\(n,b') -> if not b' then n else 0)) b
