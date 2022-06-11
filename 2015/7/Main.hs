module Main where

import LexCircuit (Token, mkPosToken)
import ParCircuit (myLexer, pCircuit)
import System.Exit (exitFailure)
import qualified Data.Map as M
import AbsCircuit
import Control.Monad.State.Lazy
import Control.Monad.Identity
import Data.Bits
import Data.Either (fromRight)
import Data.Word (Word16)
import Data.Maybe (fromJust)

type CircuitState = M.Map Ident Word16

testCircuit :: Circuit
testCircuit = fromRight undefined $ pCircuit (myLexer "123 -> x 456 -> y x AND y -> d x OR y -> e x LSHIFT 2 -> f y RSHIFT 2 -> g NOT x -> h NOT y -> i")

main :: IO ()
main = getContents >>= run

p1 :: Circuit -> Word16
p1 = fromJust . M.lookup (Ident "a") . emulate

p2 :: Circuit -> Word16
p2 circ = fromJust . M.lookup (Ident "a") . emulate $ setb circ
  where setb (Circuit cs) = Circuit (setb' cs)
        setb' [] = []
        setb' (c:cs) = case c of
          (Conn (PureG (NumExp _)) (Ident "b")) -> Conn (PureG (NumExp (fromIntegral $ p1 circ))) (Ident "b"):cs
          c' -> c : setb' cs

run :: String -> IO ()
run s = case pCircuit (myLexer s) of
  Left err -> do
    putStrLn "\nFailed to parse circuit ...\n"
    putStrLn "Tokens:"
    mapM_ (putStrLn . showPosToken . mkPosToken) ts
    putStrLn err
    exitFailure
  Right tree -> do
    print $ p1 tree
    print $ p2 tree
  where
    ts = myLexer s
    showPosToken ((l, c), t) = concat [show l, ":", show c, "\t", show t]

runConn :: Conn -> StateT CircuitState Identity ()
runConn (Conn (PureG (NumExp n)) ident) = modify (M.insert ident (fromInteger n))
runConn (Conn (PureG (IdentExp ident1)) ident2) = do
  signal <- gets (M.lookup ident1)
  case signal of
    Just sig -> modify (M.insert ident2 sig)
    Nothing -> return ()
runConn (Conn (AndG (NumExp n) ident1) ident2) = do
  signal <- gets (M.lookup ident1)
  case signal of
    Just sig -> modify (M.insert ident2 (fromInteger n .&. sig))
    Nothing -> return ()
runConn (Conn (AndG (IdentExp ident1) ident2) ident3) = do
  signal1 <- gets (M.lookup ident1)
  signal2 <- gets (M.lookup ident2)
  case (signal1,signal2) of
    (Just sig1,Just sig2) -> modify (M.insert ident3 (sig1 .&. sig2))
    _ -> return ()
runConn (Conn (OrG ident1 ident2) ident3) = do
  signal1 <- gets (M.lookup ident1)
  signal2 <- gets (M.lookup ident2)
  case (signal1,signal2) of
    (Just sig1,Just sig2) -> modify (M.insert ident3 (sig1 .|. sig2))
    _ -> return ()
runConn (Conn (RShiftG ident1 n) ident2) = do
  signal1 <- gets (M.lookup ident1)
  case signal1 of
    Just sig1 -> modify (M.insert ident2 (shift sig1 (fromInteger $ negate n)))
    Nothing -> return ()
runConn (Conn (LShiftG ident1 n) ident2) = do
  signal1 <- gets (M.lookup ident1)
  case signal1 of
    Just sig1 -> modify (M.insert ident2 (shift sig1 (fromInteger n)))
    Nothing -> return ()
runConn (Conn (NotG ident1) ident2) = do
  signal1 <- gets (M.lookup ident1)
  case signal1 of
    Just sig1 -> modify (M.insert ident2 (complement sig1))
    Nothing -> return ()

stepM :: Circuit -> StateT CircuitState Identity ()
stepM (Circuit conns) = mapM_ runConn conns

emulateM :: Circuit -> StateT CircuitState Identity ()
emulateM c = do
  stepM c
  s <- get
  stepM c
  s' <- get
  if s == s' then return () else emulateM c

emulate :: Circuit -> CircuitState
emulate c = execState (emulateM c) M.empty
