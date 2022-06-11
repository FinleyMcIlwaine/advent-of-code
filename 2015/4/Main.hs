{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.UTF8 (toString, fromString)
import Crypto.Hash.MD5 (hash)
import Data.ByteString (index, append,  ByteString, isPrefixOf )
import Data.List (takeWhile,length)
import Numeric (showHex)

pkey :: ByteString
pkey = "bgvyzdsv"

main :: IO ()
main = do
  print p1
  print p2

hashes :: [ByteString]
hashes = map (hash . append pkey . fromString . show) [0..]

hasFiveZeros :: ByteString -> Bool
hasFiveZeros bs = showHex (index bs 0) "" == "0" && showHex (index bs 1) "" == "0" && elem (showHex (index bs 2) "") (map (`showHex` "") [1..15])

p1 :: Int
p1 = length . takeWhile (not . hasFiveZeros) $ hashes

hasSixZeros bs = showHex (index bs 0) "" == "0" && showHex (index bs 1) "" == "0" && showHex (index bs 2) "" == "0"

p2 :: Int
p2 = length . takeWhile (not . hasSixZeros) $ hashes