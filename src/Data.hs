{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module Data where

import qualified Data.Map as M
import Control.Monad.State
import Data.Functor.Foldable (Fix(..), unfix, cata)
import Data.Maybe
import Debug.Trace


data Expr a = ConstInt Int
    | OperPlus (Expr a) (Expr a)
    | RefRel Position
    | RefAbs Position
    deriving (Functor)

type Expr' = Fix Expr
type ExprA a = Expr a -> a

instance Show a => Show (Expr a) where
    show (ConstInt i) = "(ConstInt " ++ show i ++ ")"
    show (OperPlus i1 i2) = "(OperPlus " ++ show i1 ++ " + " ++ show i2 ++ ")"
    show (RefRel p) = "(RefRel " ++ show p ++ ")"
    show (RefAbs p) = "(RefAbs " ++ show p ++ ")"

type Position = (Int, Int)
type Size = (Int, Int)

type FormulaData = M.Map Position (Expr Int)
type ViewData = M.Map Position ViewValue
type ViewValue = (Either String Int)

data Env = Env {
        formulas :: FormulaData,
        view :: ViewData,
        port :: ViewPort
    }

data ViewPort = ViewPort {
    position :: Position,
    size :: Size
}

evalExpr :: ExprA (Position -> Env -> ViewValue)
evalExpr (ConstInt i)     pos _   = Right i
evalExpr (OperPlus i1 i2) pos env = do
    f <- i1
    g <- i2
    return $ f pos env + g pos env
evalExpr (RefRel p) pos env = fromMaybe
    (case M.lookup p (formulas env) of
        Nothing -> Left ""
        Just e -> cata evalExpr e p env)
    (M.lookup p (view env))
evalExpr (RefAbs p) pos env = fromMaybe
    (case M.lookup p (formulas env) of
        Nothing -> Left ""
        Just e -> cata evalExpr e p env)
    (M.lookup p (view env))

-- calculate :: Position -> Env -> ViewValue
-- calculate p env = fromMaybe
--     (case M.lookup p (formulas env) of
--         Nothing -> Left ""
--         Just e -> evalExpr e p env)
--     (M.lookup p (view env))

evalExpr' = cata evalExpr (Fix (ConstInt 1)) (0, 0) env
    where
        env = Env {
                formulas = M.empty,
                view = M.empty,
                port = ViewPort {
                        position = (0, 0),
                        size = (0, 0)
                    }
            }

eval :: ViewPort -> FormulaData -> ViewData
eval vp fd = undefined

render :: ViewPort -> ViewData -> [[String]]
render vp vd = undefined

format :: [[String]] -> IO ()
format = undefined

computation :: State FormulaData FormulaData
computation = do
    x <- get
    put (M.insert (1 :: Int, 0 :: Int) (ConstInt 1) x)
    y <- get
    return y

example :: IO ()
example = print $ runState computation M.empty

insert :: Position -> Expr Int -> FormulaData -> FormulaData
insert = undefined
