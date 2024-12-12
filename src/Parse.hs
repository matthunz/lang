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

import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)

data LineColumn = LineColumn Int Int
  deriving (Show)

data Span = Span LineColumn LineColumn
  deriving (Show)

data Spanned a = Spanned Span a
  deriving (Show)

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

expr :: Parser Expr
expr = IntLit <$> int

parse :: SourceName -> String -> Either ParseError Expr
parse = runParser expr ()
