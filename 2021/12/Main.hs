{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.List
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = do
  input <- toGraph . lines <$> getContents
  print $ p1 input
  print $ p2 input

p1 :: M.Map String [String] -> Int
p1 input = length $ fromJust $ evalState (pathsFrom legalSpotP1 "start" input) M.empty

p2 :: M.Map String [String] -> Int
p2 input = length $ fromJust $ evalState (pathsFrom legalSpotP2 "start" input) M.empty

toGraph :: [String] -> M.Map String [String]
toGraph = foldr' putNode M.empty

putNode :: String -> M.Map String [String] -> M.Map String [String]
putNode l graph =
  let (source, '-' : target) = splitAt (fromJust $ elemIndex '-' l) l
   in M.insertWith (<>) target [source] $ M.insertWith (<>) source [target] graph

pathsFrom :: (String -> State (M.Map String Int) Bool) -> String -> M.Map String [String] -> State (M.Map String Int) (Maybe [[String]])
pathsFrom check source graph
  | source == "end" = return (pure [["end"]])
  | otherwise = do
    legal <- check source
    if not legal
      then return Nothing
      else do
        let neighbors = fromJust (M.lookup source graph)
         in do
              if isSmall source then modify (M.insertWith (+) source 1) else pure ()
              st <- get
              rs <- mapM (\n -> withState (const st) (pathsFrom check n graph)) neighbors
              return . pure . map (source :) . concat $ catMaybes rs

legalSpotP1 :: String -> State (M.Map String Int) Bool
legalSpotP1 source = do
  st <- get
  return . not $ elem source (M.keys st) && isSmall source

legalSpotP2 :: String -> State (M.Map String Int) Bool
legalSpotP2 source = do
  st <- get
  let ks = M.keys st
   in if source == "start" && source `elem` ks
        then return False
        else
          if source `notElem` M.keys st
            then return True
            else
              if 2 `elem` map snd (M.toList st)
                then return False
                else return True

isSmall :: String -> Bool
isSmall = all isLower
