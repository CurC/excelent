module Excelent.Parser where

-- Examples of parser input and output
-- Input: "[1, 2]"
-- Output: RefAbs (1, 2)
--
-- Input: "$[1, -1]"
-- Output: RefRel (1, -1)
--
-- Input: "[1, 2] + $[1, -1]"
-- Output: OperPlus (RefAbs (1, 2)) (RefRel (1, -1))


import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Language
import Definition

type ExpressionParser = Parsec String ()

style :: TokenParser st
style = makeTokenParser emptyDef

tuplify :: Integer -> Integer -> (Int, Int)
tuplify a b = (fromInteger a, fromInteger b)

intTuple :: Parsec String () (Int, Int)
intTuple = tuplify <$> integer style
                   <*  symbol style ","
                   <*> integer style

refAbs ::  Parsec String () Expr
refAbs = RefAbs <$> between (symbol style "[") (symbol style "]") intTuple

refRel ::  Parsec String () Expr
refRel = RefRel <$> between (symbol style "$[") (symbol style "]") intTuple
