module Specs.Eval where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import qualified Data.Map as M
import qualified Algebra.Graph.AdjacencyMap as GA

import Excelent.Definition
import Excelent.Eval.Eval

evalProps :: TestTree
evalProps = testGroup "Evaluations" [constProps, refProps, addProps]

constProps :: TestTree
constProps = testGroup "Constants"
  [
      testProperty "Integer" intConstValid
  ]

addProps :: TestTree
addProps = testGroup "Addition"
  [
      testProperty "Integer" additionValid
  ]

refProps :: TestTree
refProps = testGroup "Reference"
  [
      testProperty "Absolute Valid" absRefValid,
      testProperty "Absolute Random" absRefRandom
  ]

absRefValid :: (Int, Int) -> Int -> Bool
absRefValid pos i = i == expectInt (snd (evalExpr (RefRel pos) pos env))
    where
        env = M.insert pos (ConstInt i) M.empty

absRefRandom :: [(Int, Int)] -> Int -> Bool
absRefRandom insPs refP i = if refP `elem` insPs
        then i == expectInt (snd (evalExpr (RefRel refP) refP env))
        else case evalExpr (RefRel refP) refP env of
            Left _ -> True
            Right _ -> False
    where
        env = foldr (`M.insert` ConstInt i) M.empty insPs

additionValid :: Int -> Int -> Bool
additionValid i1 i2 = i1 + i2 ==
    expectInt (snd (evalExpr (Plus (ConstInt i1) (ConstInt i2)) (0, 0) emptyEnv))

intConstValid :: Int -> Bool
intConstValid i1 = i1 ==
    expectInt (snd (evalExpr (ConstInt i1) (0, 0) emptyEnv))

emptyEnv :: Env
emptyEnv = Env {
        formulas = M.empty,
        view = M.empty,
        graph = GA.empty,
        port = ViewPort {
            position = (0, 0),
            size = (0, 0)
        }
    }

expectInt :: ViewValue -> Int
expectInt v = case v of
    Left s -> error ("Expected Int, got: " ++ s)
    Right i -> i

expectError :: ViewValue -> String
expectError v = case v of
    Left s -> s
    Right i -> error ("Expected String, got: " ++ show i)
