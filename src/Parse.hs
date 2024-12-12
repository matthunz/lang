{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( LineColumn (..),
    Span (..),
    Spanned (..),
    getLineColumn,
    spanned,
    int,
    BinaryOp (..),
    binaryOp,
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

data Op = Add deriving (Show)

instance ToJSON Op where
  toJSON Add = "+"

op :: Parser (Spanned Op)
op = spanned (char '+' >> return Add)

data BinaryOp = BinaryOp Expr (Spanned Op) Expr deriving (Show)

instance ToJSON BinaryOp where
  toJSON (BinaryOp l o r) = object ["lhs" .= l, "op" .= o, "rhs" .= r]

binaryOp :: Parser BinaryOp
binaryOp = try $ do
  l <- int
  o <- op
  BinaryOp (IntLit l) o <$> expr

data Expr = BinaryOpExpr (Spanned BinaryOp) | IntLit (Spanned Int) deriving (Show)

instance ToJSON Expr where
  toJSON (IntLit (Spanned _ x)) = object ["int" .= x]
  toJSON (BinaryOpExpr (Spanned _ x)) = object ["binary_op" .= x]

expr :: Parser Expr
expr = BinaryOpExpr <$> spanned binaryOp <|> IntLit <$> int

parse :: SourceName -> String -> Either ParseError Expr
parse = runParser expr ()
