{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Excelent.Definition where

import Algebra.Graph.AdjacencyMap as GA
import Control.Lens hiding (Empty)
import Control.Lens.Combinators hiding (view, Empty)
import Control.Lens.Getter
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import GHC.Generics (Generic)
import qualified Data.Map as M

type Position = (Int, Int)
type Size = (Int, Int)

data Expr = Empty
    | ConstInt Int
    | ConstDouble Double
    | Plus Expr Expr
    | RefRel Position
    | RefAbs Position
    deriving (Generic, Read)

makeBaseFunctor ''Expr

type Algebra f a = f a -> a

type FormulaData = M.Map Position Expr
type ViewData    = M.Map Position ViewValue
type ViewValue   = (Either String Value)
data Value       = I Int
                 | D Double

data Type            = TInt | TDouble | TEmpty | TInvalid deriving (Eq)
type TypeEnvironment = M.Map Position Type
type NodeGraph       = GA.AdjacencyMap Position

instance Show Expr where
    show Empty = ""
    show (ConstInt i) = show i
    show (ConstDouble d) = show d
    show (Plus i1 i2) = show i1 ++ " + " ++ show i2
    show (RefRel p) = "$" ++ show p
    show (RefAbs p) = show p

instance Show Value where
    show (I i) = show i
    show (D d) = show d

instance Show Type where
    show TInt     = "Int"
    show TDouble  = "Double"
    show TEmpty   = "Empty"
    show TInvalid = "Error"

data Env = Env {
        _formulas :: FormulaData,
        _view :: ViewData,
        _port :: ViewPort,
        _types :: TypeEnvironment,
        _graph :: NodeGraph
    } deriving (Show)

data ViewPort = ViewPort {
        _position :: Position,
        _size :: Size
    } deriving (Show)

makeLenses ''Env
makeLenses ''ViewPort

initial :: ViewPort -> Env
initial nPort = demo & port .~ nPort
    --     Env {
    --     _formulas = M.empty,
    --     _view = M.empty,
    --     _graph = = NodeGraph {
    --         _deps = GA.empty
    --         _types = M.empty
    --     },
    --     port = port
    -- }

initialEnv :: Env
initialEnv = Env {
        _formulas = M.empty,
        _view = M.empty,
        _graph = GA.empty,
        _types = M.empty,
        _port = ViewPort {
                _position = (0, 0),
                _size = (10, 10)
            }
    }

demo :: Env
demo = Env {
        _formulas =
            M.insert (1, 2) (Plus (RefRel (0, -1)) (ConstInt 1))
                (M.insert (1, 1) (Plus (RefRel (0, -1)) (ConstInt 1))
                    (M.insert (0, 4) (Plus (RefRel (0, -1)) (ConstInt 1))
                        (M.insert (0, 3) (Plus (RefRel (0, -1)) (ConstInt 1))
                            (M.insert (0, 2) (Plus (RefRel (0, -1)) (ConstInt 1))
                                (M.insert (0, 1) (Plus (RefRel (0, -1)) (ConstInt 1))
                                    (M.insert (0, 0) (ConstInt 1) M.empty)))))),
        _view = M.empty,
        _graph = GA.empty,
        _types = M.empty,
        _port = ViewPort {
                _position = (0, 0),
                _size = (10, 10)
            }
    }
