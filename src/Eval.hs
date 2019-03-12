module Eval where

import Data.Graph.Inductive.Graph
import Data.Functor.Foldable
import Definition
import qualified Data.Map as M

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

evalExpr :: Expr -> Position -> Env -> (Env, ViewValue)
evalExpr = cata evalAlg

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
        positions = concat (inView vp)

inView :: ViewPort -> [[Position]]
inView ViewPort {size = (w, h), position = (top, left)} =
    [[(top + i, left + j) | i <- [0..h]] | j <- [0..w]]

type EvalGraph = Graph Position ()

dependencies :: Env -> Graph
dependencies Env {formulas = f} = undefined