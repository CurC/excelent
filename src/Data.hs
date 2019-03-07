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
    } deriving (Show)

data ViewPort = ViewPort {
        position :: Position,
        size :: Size
    } deriving (Show)


evalAlg :: Expr' (Position -> Env -> (Env, ViewValue)) -> (Position -> Env -> (Env, ViewValue))
evalAlg (ConstInt' i)         pos env = (env, Right i)
evalAlg (OperPlus' exp1 exp2) pos env = (env2, do
        i <- vval1
        j <- vval2
        return $ i + j)
    where
        (env1, vval1) = exp1 pos env
        (env2, vval2) = exp2 pos env1

evalAlg (RefRel' p) pos env = undefined
evalAlg (RefAbs' p) pos env = case M.lookup p (view env) of
    Nothing -> case M.lookup p (formulas env) of
        Nothing -> (env, Left "Empty cell referenced.")
        Just e -> let (newEnv, val) = cata evalAlg e p env in (newEnv {view = M.insert p val (view newEnv)}, val)
    Just e -> (env, e)

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

env3 :: Env
env3 = Env {
        formulas = M.insert (0, 0) (RefAbs (1, 0)) (M.insert (1, 0) (ConstInt 2) M.empty),
        view = M.empty,
        port = ViewPort {
                position = (0, 0),
                size = (10, 10)
            }
    }

evalExpr :: Expr -> Position -> Env -> (Env, ViewValue)
evalExpr e p env = cata evalAlg e p env

evalCell :: Position -> Env -> Env
evalCell pos env = case M.lookup pos (view env) of
    Just v -> env
    Nothing -> case M.lookup pos (formulas env) of
        Just expr -> env { view = M.insert pos (snd $ evalExpr expr pos env) (view env) }
        Nothing -> env { view = M.insert pos (Left "") (view env) }

eval :: Env -> Env
eval env@Env{view = v, formulas = f, port = vp} = resultEnv
    where
        resultEnv = foldr evalCell env positions
        positions = [(top + i, left + j) | i <- [0..h], j <- [0..w]]
        ViewPort {size = (w, h), position = (top, left)} = vp

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
