{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import qualified Data.Map as M
import Numeric (readHex)
import Data.Bifunctor

main :: IO ()
main = do
  input <- fst . parsePacket . hexToBits <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: Packet -> Int
p1 (Literal (H v _) _) = v
p1 (Operator (H v _) ps) = v + sum (map p1 ps)

p2 :: Packet -> Int
p2 = eval
eval :: Packet -> Int
eval (Literal _ n) = n
eval (Operator (H _ id) ps) =
  case id of
    0 -> sum $ map eval ps
    1 -> product $ map eval ps
    2 -> minimum $ map eval ps
    3 -> maximum $ map eval ps
    5 -> fromEnum $ eval (head ps) > eval (ps !! 1)
    6 -> fromEnum $ eval (head ps) < eval (ps !! 1)
    7 -> fromEnum $ eval (head ps) == eval (ps !! 1)

-- Lexing

intToBits :: Int -> [Int]
intToBits n = map (\i -> if i `elem` os then 1 else 0) [3, 2, 1, 0]
  where
    os = marks n
    marks m
      | m <= 0 = []
      | otherwise = floor (logBase 2 (fromIntegral m)) : marks (m - 2 ^ floor (logBase 2 (fromIntegral m)))

bitsToInt :: [Int] -> Int
bitsToInt = foldr (\(w, x) n -> w * x + n) 0 . zip (iterate (* 2) 1) . reverse

hexToBits :: String -> [Int]
hexToBits = concatMap (intToBits . fst . head . readHex . pure)

data Packet = Literal Header Int | Operator Header [Packet] deriving (Show, Eq)

data Header = H {version :: Int, idd :: Int} deriving (Show, Eq)


-- Parsing

parsePacket :: [Int] -> (Packet,[Int])
parsePacket bs = let (h,bs') = parseHeader bs in
  if idd h == 4 then
    let (n,bs'') = parseLiteral bs' in
      (Literal h n,bs'')
  else
    let (ps,bs'') = parseOperator bs' in
      (Operator h ps, bs'')

parseHeader :: [Int] -> (Header,[Int])
parseHeader bs = (\(v, i) -> (H {version = bitsToInt v, idd = bitsToInt i}, drop 6 bs)) . splitAt 3 $ take 6 bs

parseLiteral :: [Int] -> (Int,[Int])
parseLiteral bs_ = (\(bs,n) -> (bitsToInt bs, drop (n*5) bs_)) $ litBits bs_
  where litBits (1:bs) = let
          (bs',n) = litBits (drop 4 bs)
          in (take 4 bs ++ bs',n+1)
        litBits (0:bs) = (take 4 bs,1)

parseOperator :: [Int] -> ([Packet],[Int])
parseOperator  (0:bs) = let (subPacketLen,bs') = parseLength bs
                            go bs'' = let (p,bs''') = parsePacket bs'' in if length bs' - length bs''' >= subPacketLen then (pure p,bs''') else first (p :) $ go bs'''
                        in go bs'
parseOperator  (1:bs) = let (numSubPackets,bs') = parseNum bs
                            go 0 bs'' = ([],bs'')
                            go n bs'' = let (p,bs''') = parsePacket bs'' in first (p :) $ go (n-1) bs'''
                        in go numSubPackets bs'

parseLength :: [Int] -> (Int, [Int])
parseLength bs = (bitsToInt (take 15 bs), drop 15 bs)

parseNum :: [Int] -> (Int, [Int])
parseNum bs = (bitsToInt (take 11 bs), drop 11 bs)
