{-
 - A parser for the Game Description Language (GDL).
 - 
 - For more information on GDL, see:
 - http://arrogant.stanford.edu/ggp/chapters/chapter_02.html
 - http://logic.stanford.edu/classes/cs227/2013/readings/gdl_spec.pdf
 -}

module Gdl.GdlParser where

import Control.Applicative

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String

data Elem = Variable String deriving Show
data SExpr = Atom String | List [SExpr] deriving Show

variable :: Parser Elem
variable = liftA Variable $ (char '?') *> liftA2 (:) letter (many alphaNum)

symbol :: Parser String
symbol = many1 alphaNum <* spaces

openParens :: Parser Char
openParens = char '(' <* spaces

closeParens :: Parser Char
closeParens = char ')' <* spaces

sexpr :: Parser SExpr
sexpr = openParens *> list <* closeParens
  where atom = liftA Atom symbol
        list = liftA List (many (atom <|> sexpr))
