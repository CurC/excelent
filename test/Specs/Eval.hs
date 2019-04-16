module Specs.Eval where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Control.Lens
import Control.Lens.Getter
import qualified Algebra.Graph.AdjacencyMap as GA
import qualified Data.Map as M

import Excelent.Definition
import Excelent.Eval.Eval

evalProps :: TestTree
evalProps = testGroup "Evaluations" [constProps, refProps, addProps]

constProps :: TestTree
constProps = testGroup "Constants" [
        testProperty "Integer" intConstValid
    ]

addProps :: TestTree
addProps = testGroup "Addition" [
        testProperty "Integer" additionValid
    ]

refProps :: TestTree
refProps = testGroup "Reference" [
        testProperty "Absolute Valid" absRefValid,
        testProperty "Absolute Random" absRefRandom
  ]

absRefValid :: (Int, Int) -> Int -> Bool
absRefValid pos i = i == expectInt (snd (evalExpr (RefRel pos) (0, 0) env))
    where
        env = emptyEnv & formulas %~ M.insert pos (ConstInt i)

absRefRandom :: [(Int, Int)] -> (Int, Int) -> Int -> Bool
absRefRandom insPs refP i = if refP `elem` insPs
        then i == expectInt evaluated
        else case evaluated of
            Left _ -> True
            Right _ -> False
    where
        evaluated = snd (evalExpr (RefRel refP) (0, 0) env)
        env = foldr (\ins env -> env & formulas %~ M.insert ins (ConstInt i)) emptyEnv insPs

additionValid :: Int -> Int -> Bool
additionValid i1 i2 = i1 + i2 ==
    expectInt (snd (evalExpr (Plus (ConstInt i1) (ConstInt i2)) (0, 0) emptyEnv))

intConstValid :: Int -> Bool
intConstValid i1 = i1 ==
    expectInt (snd (evalExpr (ConstInt i1) (0, 0) emptyEnv))

emptyEnv :: Env
emptyEnv = Env {
        _formulas = M.empty,
        _view = M.empty,
        _graph = GA.empty,
        _port = ViewPort {
            _position = (0, 0),
            _size = (0, 0)
        }
    }

expectInt :: ViewValue -> Int
expectInt v = case v of
    Left (Error s) -> error ("Expected Int, got: " ++ s)
    Right (I i) -> i

expectError :: ViewValue -> String
expectError v = case v of
    Left (Error s) -> s
    Right i -> error ("Expected String, got: " ++ show i)
