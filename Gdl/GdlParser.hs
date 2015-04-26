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


sexprFile :: Parser [[String]]
sexprFile = sep *> many (sexpr (sepBy symbol sep))

-- Lexing.
comment :: Parser ()
comment = () <$ (char ';' *> manyTill anyChar endOfLine)

sep :: Parser ()
sep = () <$ many (comment <|> (space *> pure ()))

openParens :: Parser ()
openParens = char '(' *> sep

closeParens :: Parser ()
closeParens = char ')' *> sep

sexpr :: Parser a -> Parser a
sexpr = between openParens closeParens

symbol :: Parser String
symbol = many1 alphaNum

variable :: Parser String
variable = (:) <$> char '?' <*> symbol

-- Parsing.
data Prop = PRelation Relation
          | PProp [String] deriving Show  --TODO: make this Prop
data Role = Role String deriving Show
data Action = Action String deriving Show
data Utility = Utility String deriving Show  --TODO: make this Int
data Relation = RRole Role
              | RBase Prop
              | RInput Role Action
              | RInit Prop
              | RTrue Prop
              | RNext Prop
              | RDoes Prop
              | RLegal Role Action
              | RGoal Role Utility
              | RTerminal
              deriving Show

-- TODO: fix this definition.
rel :: String -> Parser a -> Parser a
rel tok inner = try $ sexpr $ string tok *> sep *> inner <* sep

prop :: Parser Prop
prop = (PRelation <$> prelation) <|> (PProp <$> sexpr (sepBy symbol sep))

prelation :: Parser Relation
prelation = role
        <|> base
        <|> input
        <|> rinit
        <|> true
        <|> next
        <|> does
        <|> legal
        <|> goal
        <|> terminal

role :: Parser Relation
role = rel "role" ((RRole . Role) <$> symbol)

base :: Parser Relation
base = rel "base" (RBase <$> prop)

input :: Parser Relation
input = rel "input" input'
  where input' = RInput <$> (Role <$> symbol <* sep) <*> (Action <$> symbol)

rinit :: Parser Relation
rinit = rel "init" (RInit <$> prop)

true :: Parser Relation
true = rel "true" (RTrue <$> prop)

next :: Parser Relation
next = rel "next" (RNext <$> prop)

does :: Parser Relation
does = rel "does" (RDoes <$> prop)

legal :: Parser Relation
legal = rel "legal" legal'
  where legal' = RLegal <$> (Role <$> symbol <* sep) <*> (Action <$> symbol)

goal :: Parser Relation
goal = rel "goal" goal'
  where goal' = RGoal <$> (Role <$> symbol <* sep) <*> (Utility <$> symbol)

terminal :: Parser Relation
terminal = string "terminal" *> sep *> pure RTerminal

prod :: Parser [Prop]
prod = sexpr $ string "<=" *> sep *> many prop <* sep

