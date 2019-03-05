{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Data where

import qualified Data.Map as M
import Data.Functor.Foldable
import Data.Maybe
import Debug.Trace
import GHC.Generics (Generic)


data Expr = ConstInt Int
    | OperPlus Expr Expr
    | RefRel Position
    | RefAbs Position
    deriving (Generic)

data Expr' a = ConstInt' Int
    | OperPlus' a a
    | RefRel' Position
    | RefAbs' Position
    deriving (Functor, Generic)

type Algebra f a = f a -> a

type instance Base Expr = Expr'
instance Recursive Expr
instance Corecursive Expr

instance Show Expr where
    show (ConstInt i) = "(ConstInt " ++ show i ++ ")"
    show (OperPlus i1 i2) = "(OperPlus " ++ show i1 ++ " + " ++ show i2 ++ ")"
    show (RefRel p) = "(RefRel " ++ show p ++ ")"
    show (RefAbs p) = "(RefAbs " ++ show p ++ ")"

type Position = (Int, Int)
type Size = (Int, Int)

type FormulaData = M.Map Position Expr
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

evalAlg :: Algebra Expr' (Position -> Env -> ViewValue)
evalAlg (ConstInt' i)     pos env = Right i
evalAlg (OperPlus' i1 i2) pos env = do
    f <- i1 pos env
    g <- i2 pos env
    return $ f + g
evalAlg (RefRel' p) pos env = fromMaybe
    (case M.lookup p (formulas env) of
        Nothing -> Left ""
        Just e -> cata evalAlg e p env)
    (M.lookup p (view env))
evalAlg (RefAbs' p) pos env = fromMaybe
    (case M.lookup p (formulas env) of
        Nothing -> Left ""
        Just e -> cata evalAlg e p env)
    (M.lookup p (view env))

-- calculate :: Position -> Env -> ViewValue
-- calculate p env = fromMaybe
--     (case M.lookup p (formulas env) of
--         Nothing -> Left ""
--         Just e -> evalExpr e p env)
--     (M.lookup p (view env))

e1 :: Expr
e1 = ConstInt 1

e2 :: Expr
e2 = RefAbs (0, 0)

env1 :: Env
env1 = Env {
        formulas = M.empty,
        view = M.empty,
        port = ViewPort {
                position = (0, 0),
                size = (0, 0)
            }
    }

env2 :: Env
env2 = Env {
        formulas = M.insert (0, 0) (ConstInt 2) M.empty,
        view = M.empty,
        port = ViewPort {
                position = (0, 0),
                size = (0, 0)
            }
    }

evalExpr e env = cata evalAlg e (0, 0) env

eval :: ViewPort -> FormulaData -> ViewData
eval vp fd = undefined

-- render :: ViewPort -> ViewData -> [[String]]
-- render vp vd = undefined

-- format :: [[String]] -> IO ()
-- format = undefined

-- computation :: State FormulaData FormulaData
-- computation = do
--     x <- get
--     put (M.insert (1 :: Int, 0 :: Int) (ConstInt 1) x)
--     y <- get
--     return y

-- example :: IO ()
-- example = print $ runState computation M.empty

-- insert :: Position -> Expr Int -> FormulaData -> FormulaData
-- insert = undefined
