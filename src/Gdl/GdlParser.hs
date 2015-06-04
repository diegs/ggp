{-# Language GADTs #-}

{-
 - A parser for the Game Description Language (GDL).
 -
 - For more information on GDL, see:
 - http://arrogant.stanford.edu/ggp/chapters/chapter_02.html
 - http://logic.stanford.edu/classes/cs227/2013/readings/gdl_spec.pdf
 -}

module Gdl.GdlParser where

import Control.Applicative
import Text.Parsec hiding (optional, many, (<|>))
import Text.Parsec.String

-- Sexpr parser: lexes raw input into recursive lists of symbols.
data SExpr a = Symbol a
             | SExpr [SExpr a]

sexprs :: Parser [SExpr String]
sexprs = sep *> many sexpr

-- A separator is any amount of whitespace and/or comments.
sep :: Parser ()
sep = () <$ many (comment <|> (space *> pure ()))
  where comment = () <$ (char ';' *> manyTill anyChar endOfLine)

-- A sexpr is any number of sexprs and alphanumeric tokens enclosed by parens and
-- separated by separators.
sexpr :: Parser (SExpr String)
sexpr = SExpr <$> (between openParens closeParens $ sepBy sexpr' sep)
  where sexpr' = sexpr <|> (symbol <* sep)
        symbol = Symbol <$> many1 alphaNum
        openParens = char '(' *> sep
        closeParens = char ')' *> sep

-- Relation parser: parses sexprs into relations.

-- Game parser: turns relations into valid game definitions.

