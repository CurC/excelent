{-# LANGUAGE LambdaCase #-}
module Excelent.Print where

import Data.Maybe
import Excelent.Definition
import qualified Data.Map as M

-- | Prints the expression in the given position using its show instance
printF :: Position -> FormulaData -> String
printF p f = maybe "" show (M.lookup p f)

-- | Prints the calculated value in the given position, otherwise nothing
printV :: Position -> ViewData -> String
printV p v = case M.lookup p v of
    Just x  -> case x of
        Left  s -> show s
        Right i -> show i
    Nothing -> ""
