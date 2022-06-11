{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Byte (digitChar, alphaNumChar, char, lowerChar, space, string)
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void BS.ByteString

type Wire = (BS.ByteString, Int)

data Gate
  = Pure Wire
  | And Wire Wire Wire
  | Or Wire Wire Wire
  | LShift Wire Int Wire
  | RShift Wire Int Wire
  | Not Wire Wire
  deriving (Show)

main :: IO ()
main = do
  input <- BS.getContents
  case parse parseCircuit "" input of
    Left x -> putStrLn "bad parse"
    Right gs -> print gs

parseCircuit :: Parser [Gate]
parseCircuit = do
  g <- parseGate
  rest <- optional . try $ do
    void (string "\n")
    parseCircuit
  case rest of
    Nothing -> return [g]
    Just gs -> return (g : gs)

parseGate :: Parser Gate
parseGate = try andOr <|> try shift <|> try nott <|> try puree

andOr :: Parser Gate
andOr = do
  w1 <- wire
  void space
  gate <- andOrType
  void space
  w2 <- wire
  void $ space >> string "->" >> space
  gate w1 w2 <$> wire

shift :: Parser Gate
shift = do
  w1 <- wire
  void space
  gate <- shiftType
  void space
  shif <- L.lexeme space L.decimal
  void $ space >> string "->" >> space
  gate w1 shif <$> wire

puree :: Parser Gate
puree = do
  signal <- L.lexeme space L.decimal
  void $ space >> "->" >> space
  w <- wire
  return $ Pure (fst w, signal)

nott :: Parser Gate
nott = do
  void (string "NOT" >> space)
  w1 <- wire
  void (space >> string "->" >> space)
  Not w1 <$> wire

andOrType :: Parser (Wire -> Wire -> Wire -> Gate)
andOrType =
  choice
    [ And <$ string "AND",
      Or <$ string "OR"
    ]

shiftType :: Parser (Wire -> Int -> Wire -> Gate)
shiftType =
  choice
    [ LShift <$ string "LSHIFT",
      RShift <$ string "RSHIFT"
    ]

wire :: Parser Wire
wire = some lowerChar >>= (\ident -> return (ident,0)) . BS.pack
