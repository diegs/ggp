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
import Data.Char
import Text.Parsec hiding (optional, many, (<|>))
import Text.Parsec.String

-- | Sexpr parser: lexes raw input into recursive lists of symbols.
data SExpr a = Atom a
             | SExpr [SExpr a]
             deriving Show

-- | Parses arbitrary sexprs, discarding preceding comments/whitespace.
sexprs :: Parser a -> Parser [SExpr a]
sexprs p = sep *> many (sexpr p)

-- | A sexpr is any number of sexprs and alphanumeric tokens enclosed by parens and
-- separated by separators.
sexpr :: Parser a -> Parser (SExpr a)
sexpr p = SExpr <$> (between openParens closeParens $ sepBy sexpr' sep)
  where sexpr' = (sexpr p) <|> atom
        atom = Atom <$> (p <* sep)
        openParens = char '(' *> sep
        closeParens = char ')' *> sep

-- | A separator is any combination of whitespace and comments.
sep :: Parser [Char]
sep = many (comment <|> space)
  where comment = char ';' <* manyTill anyChar eoComment
        eoComment = (endOfLine *> pure ()) <|> eof

-- | Relation parser: parses sexprs into relations.

data Entity = Constant String
            | Variable String
            deriving Show

data Relation = Role      -- role(a) means that a is a role in the game.
              | Base      -- base(p) means that p is a base proposition in the game.
              | Input     -- input(r,a) means that a is a feasible action for role r.
              | Init      -- init(p) means that the proposition p is true in the initial state.
              | True      -- true(p) means that the proposition p is true in the current state.
              | Does      -- does(r,a) means that role r performs action a in the current state.
              | Next      -- next(p) means that the proposition p is true in the next state.
              | Legal     -- legal(r,a) means it is legal for role r to play action a in the current state.
              | Goal      -- goal(r,n) means that player the current state has utility n for player r.
              | Terminal  -- terminal means that the current state is a terminal state.

-- | A GDL description is an open logic program with the following input and output relations:
-- (1) A GDL game description must give complete definitions for role, base, input, init.
-- (2) It must define legal and goal and terminal in terms of an input true relation.
-- (3) It must define next in terms of input true and does relations. Since does and true are
-- treated as inputs, there must not be any rules with either of these relations in the head.

entity :: Parser Entity
entity = variable <|> constant
  where variable = (Variable . strToLower) <$> (char '?' *> many1 identifier)
        constant = (Constant . strToLower) <$> many1 identifier
        identifier = alphaNum
        strToLower = map Data.Char.toLower

relation :: Parser [SExpr Entity]
relation = sexprs entity

-- Game parser: turns relations into valid game definitions.

