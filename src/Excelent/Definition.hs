{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Excelent.Definition where

import Data.Functor.Foldable
import GHC.Generics (Generic)
import qualified Data.Map as M
import Algebra.Graph.AdjacencyMap as GA
import Control.Lens.Combinators hiding (view)
import Control.Lens.Getter
import Control.Lens

data Expr = ConstInt Int
    | Plus Expr Expr
    | RefRel Position
    | RefAbs Position
    deriving (Generic, Read)

data Expr' a = ConstInt' Int
    | Plus' a a
    | RefRel' Position
    | RefAbs' Position
    deriving (Functor, Generic)

type Algebra f a = f a -> a

type instance Base Expr = Expr'
instance Recursive Expr
instance Corecursive Expr

instance Show Expr where
    show (ConstInt i) = show i
    show (Plus i1 i2) = show i1 ++ " + " ++ show i2
    show (RefRel p) = "$" ++ show p
    show (RefAbs p) = show p

type Position = (Int, Int)
type Size = (Int, Int)

type FormulaData = M.Map Position Expr
type ViewData = M.Map Position ViewValue
type ViewValue = (Either String Int)
type NodeGraph = GA.AdjacencyMap Position

data Env = Env {
        _formulas :: FormulaData,
        _view :: ViewData,
        _port :: ViewPort,
        _graph :: NodeGraph
    } deriving (Show)

data ViewPort = ViewPort {
        _position :: Position,
        _size :: Size
    } deriving (Show)

makeLenses ''Env
makeLenses ''ViewPort

initial :: ViewPort -> Env
initial nPort = env3 & port .~ nPort
    --     Env {
    --     formulas = M.empty,
    --     view = M.empty,
    --     graph = GA.empty,
    --     port = port
    -- }

e1 :: Expr
e1 = ConstInt 1

e2 :: Expr
e2 = RefAbs (0, 0)

env1 :: Env
env1 = Env {
        _formulas = M.insert (0, 0) (Plus (RefAbs (1, 0)) (RefAbs (2, 0))) (M.insert (1, 0) (ConstInt 2) (M.insert (2, 0) (ConstInt 7) M.empty)),
        _view = M.empty,
        _graph = GA.empty,
        _port = ViewPort {
                _position = (0, 0),
                _size = (10, 10)
            }
    }

env2 :: Env
env2 = Env {
        _formulas = M.insert (0, 0) (RefAbs (1, 0)) (M.insert (1, 0) (RefAbs (0, 0)) M.empty),
        _view = M.empty,
        _graph = GA.empty,
        _port = ViewPort {
                _position = (0, 0),
                _size = (10, 10)
            }
    }

relref :: Position -> Expr
relref = RefRel

plus :: Expr -> Expr -> Expr
plus = Plus

i :: Int -> Expr
i = ConstInt

env3 :: Env
env3 = Env {
        _formulas =
            M.insert (0, 1) (plus (relref (0, 1)) (i 1))
                (M.insert (0, 2) (plus (relref (0, 1)) (i 1))
                    (M.insert (0, 3) (plus (relref (0, 1)) (i 1))
                        (M.insert (0, 4) (plus (relref (0, 1)) (i 1))
                            (M.insert (0, 5) (i 1) M.empty)))),
        _view = M.empty,
        _graph = GA.empty,
        _port = ViewPort {
                _position = (0, 0),
                _size = (10, 10)
            }
    }
