{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Definition where

import Data.Functor.Foldable
import GHC.Generics (Generic)
import qualified Data.Map as M
import Algebra.Graph.AdjacencyMap
import Excelent.Eval.Graph

data Expr = ConstInt Int
    | ConstFloat Float
    | OperPlus Expr Expr
    | RefRel Position
    | RefAbs Position
    deriving (Generic, Read)

data Expr' a = ConstInt' Int
    | ConstFloat' Float
    | OperPlus' a a
    | RefRel' Position
    | RefAbs' Position
    deriving (Functor, Generic)

type Algebra f a = f a -> a

type instance Base Expr = Expr'
instance Recursive Expr
instance Corecursive Expr

instance Show Expr where
    show (ConstInt i) = show i
    show (OperPlus i1 i2) = show i1 ++ " + " ++ show i2
    show (RefRel p) = "rel" ++ show p
    show (RefAbs p) = "abs" ++ show p

type Position = (Int, Int)
type Size = (Int, Int)

type FormulaData = M.Map Position Expr
type ViewData = M.Map Position ViewValue
type ViewValue = (Either String Int)

data Env = Env {
        formulas :: FormulaData,
        view :: ViewData,
        port :: ViewPort,
        evalGraph :: NodeGraph
    } deriving (Show)

data ViewPort = ViewPort {
        position :: Position,
        size :: Size
    } deriving (Show)

e1 :: Expr
e1 = ConstInt 1

e2 :: Expr
e2 = RefAbs (0, 0)

env1 :: Env
env1 = Env {
        formulas = M.insert (0, 0) (OperPlus (RefAbs (1, 0)) (RefAbs (2, 0))) (M.insert (1, 0) (ConstInt 2) (M.insert (2, 0) (ConstInt 7) M.empty)),
        view = M.empty,
        evalGraph = GA.empty,
        port = ViewPort {
                position = (0, 0),
                size = (10, 10)
            }
    }

env2 :: Env
env2 = Env {
        formulas = M.insert (0, 0) (RefAbs (1, 0)) (M.insert (1, 0) (RefAbs (0, 0)) M.empty),
        view = M.empty,
        evalGraph = GA.empty,
        port = ViewPort {
                position = (0, 0),
                size = (10, 10)
            }
    }

env3 :: Env
env3 = Env {
        formulas = M.insert (0, 0) (RefAbs (1, 0)) (M.insert (1, 0) (ConstInt 2) M.empty),
        view = M.empty,
        evalGraph = GA.empty,
        port = ViewPort {
                position = (0, 0),
                size = (10, 10)
            }
    }
