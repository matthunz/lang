{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( LineColumn (..),
    Span (..),
    Spanned (..),
    getLineColumn,
    spanned,
    int,
    Expr (..),
    parse,
  )
where

import Data.Aeson
import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)

data LineColumn = LineColumn Int Int
  deriving (Show)

instance ToJSON LineColumn where
  toJSON (LineColumn l c) = object ["line" .= l, "column" .= c]

data Span = Span LineColumn LineColumn
  deriving (Show)

instance ToJSON Span where
  toJSON (Span start end) = object ["start" .= start, "end" .= end]

data Spanned a = Spanned Span a
  deriving (Show)

instance (ToJSON a) => ToJSON (Spanned a) where
  toJSON (Spanned s x) = object ["span" .= s, "value" .= x]

getLineColumn :: Parser LineColumn
getLineColumn = fmap (\pos -> LineColumn (sourceLine pos) (sourceColumn pos)) getPosition

spanned :: Parser a -> Parser (Spanned a)
spanned f = do
  start <- getLineColumn
  x <- f
  end <- getLineColumn
  return $ Spanned (Span start end) x

int :: Parser (Spanned Int)
int = spanned (read <$> many1 digit)

data Expr = IntLit (Spanned Int) deriving (Show)

instance ToJSON Expr where
  toJSON (IntLit (Spanned _ x)) = object ["int" .= x]

expr :: Parser Expr
expr = IntLit <$> int

parse :: SourceName -> String -> Either ParseError Expr
parse = runParser expr ()
