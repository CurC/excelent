module Excelent.Parser where

-- Examples of parser input and output
-- Input: "(1, 2)"
-- Output: RefAbs (1, 2)
--
-- Input: "$(1, -1)"
-- Output: RefRel (1, -1)
--
-- Input: "(1, 2) + $(1, -1)"
-- Output: Plus (RefAbs (1, 2)) (RefRel (1, -1))

import Text.Parsec hiding (Empty)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Language
import Excelent.Definition

type ExprParser = Parsec String () Expr

style :: TokenParser st
style = makeTokenParser emptyDef

tuplify :: Integer -> Integer -> (Int, Int)
tuplify a b = (fromInteger a, fromInteger b)

intTuple :: Parsec String () (Int, Int)
intTuple = tuplify <$> integer style
                   <*  symbol style ","
                   <*> integer style

-- https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
expression :: ExprParser
expression = addition

addition :: ExprParser
addition = try (Plus <$> primary <* symbol style "+" <*> addition) <|> primary

primary :: ExprParser
primary = try refRel
    <|> try refAbs
    <|> try constF
    <|> try constI
    <|> (Empty <$ eof)

refAbs :: ExprParser
refAbs = RefAbs <$> between (symbol style "(") (symbol style ")") intTuple

constI :: ExprParser
constI = ConstInt . fromInteger <$> integer style

constF :: ExprParser
constF = ConstDouble <$> float style

refRel :: ExprParser
refRel = RefRel <$> between (symbol style "$(") (symbol style ")") intTuple
