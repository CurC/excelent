module Excelent.Eval.Eval where

import qualified Data.Map as M
import qualified Data.Set as S
import Algebra.Graph.AdjacencyMap as G
import Data.Functor.Foldable
import Excelent.Definition
import Data.NumInstances.Tuple


evalAlg :: Algebra Expr' (Position -> Env -> (Env, ViewValue))
evalAlg (ConstInt' i)         pos env = (env, Right i)
evalAlg (Plus' exp1 exp2) pos env     = (env2, do
        i <- vval1
        j <- vval2
        return $ i + j)
    where
        (env1, vval1) = exp1 pos env
        (env2, vval2) = exp2 pos env1
evalAlg (RefRel' p) pos env = doLookup (pos + p) env
evalAlg (RefAbs' p) pos env = doLookup p env

doLookup :: Position -> Env -> (Env, ViewValue)
doLookup pos env = case M.lookup pos (view env) of
    Nothing -> case M.lookup pos (formulas env) of
        Nothing -> (env, Left "Empty cell referenced.")
        Just e -> let (newEnv, val) = cata evalAlg e pos env in
            (newEnv {view = M.insert pos val (view newEnv)}, val)
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

invalidateView :: [Position] -> Env -> Env
invalidateView ps env@Env {view = v} = env { view = M.withoutKeys v set }
    where
        set = S.fromList ps
