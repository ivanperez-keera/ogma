{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Pretty.Simple     as PS

-- main
main :: IO ()
main = do
  content <- TIO.readFile "IBD"
  result <- runParserT programParser "IBD" content
  case result of
    Left err   -> putStrLn $ errorBundlePretty err
    Right prog -> PS.pPrint prog

-- Parser type
type Parser = ParsecT Void Text IO

-- AST types
data GraphDir = TB | LR | TD deriving (Show, Read)

data Program = Program GraphDir [Subgraph] [Connection] deriving (Show)

data Subgraph = Subgraph String String [Port] deriving (Show)

data Port = Port String deriving (Show)  -- just store ID and label string

data Connection = Connection String String deriving (Show)

graphHeader :: Parser GraphDir
graphHeader = do
  lexeme (string "graph")
  dir <- lexeme (string "TB" <|> string "LR" <|> string "TD")
  return $ read $ T.unpack dir

subgraphParser :: Parser Subgraph
subgraphParser = do
  lexeme (string "subgraph")
  ident <- lexeme identParser
  _ <- lexeme (char '[')
  title <- manyTill anySingle (char ']')
  void $ many eol
  ports <- many (try (notFollowedBy (lexeme (string "end")) *> portParser))
  lexeme (string "end")

  return $ Subgraph ident title ports

portParser :: Parser Port
portParser = do
  sc
  pid <- lexeme identParser
  _ <- char '['
  e <- manyTill anySingle (char ']')
  void $ many eol
  return $ Port pid

connectionParser :: Parser Connection
connectionParser = do
  sc
  from <- lexeme identParser
  lexeme (string "-->")
  to <- lexeme identParser
  return $ Connection from to

programParser :: Parser Program
programParser = do
  sc
  dir <- graphHeader
  sc
  subgraphs <- many subgraphParser
  sc
  conns <- many connectionParser
  eof
  return $ Program dir subgraphs conns

-- | Parse identifier ignoring keywords
identParser :: Parser String
identParser = try $ do
    ident <- some (alphaNumChar <|> char '_' <|> char ':')
    if ident `elem` reservedWords
      then fail $ "reserved word: " ++ ident
      else return ident

reservedWords :: [String]
reservedWords = ["end", "graph", "subgraph"]

-- | Auxiliary subparsers

-- skip whitespace and comments
sc :: Parser ()
sc = void $ many (void (char ' ') <|> void (char '\t') <|> void eol)

commentLine :: Parser ()
commentLine = do
  _ <- string "%%"
  _ <- manyTill anySingle (void eol <|> eof)
  return ()

lexeme :: Parser a -> Parser a
lexeme p = p <* sc
