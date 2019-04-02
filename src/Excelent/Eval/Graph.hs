module Excelent.Eval.Graph where

import Prelude
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.NonEmpty hiding (length, head)

import Algebra.Graph
import Algebra.Graph.AdjacencyMap as GA
import Algebra.Graph.AdjacencyMap.Algorithm as GAA
import Algebra.Graph.NonEmpty.AdjacencyMap as GNA
import Control.Lens hiding (view)
import Control.Lens.Combinators hiding (view)
import Data.Functor.Foldable
import Data.NumInstances.Tuple
import Debug.Trace

import Excelent.Eval.Eval
import Excelent.Eval.Checker
import Excelent.Definition

-- | Algebra for calculating the dependencies of a single given cell. This is
-- not done recursively, so only the direct dependencies are given.
--
-- These multitude of small subgraph (one for each cell) are then overlayed to
-- create the final dependency graph
graphAlg :: Algebra ExprF (Position -> NodeGraph)
graphAlg (ConstIntF _)     pos = GA.vertex pos
graphAlg (ConstDoubleF _)  pos = GA.vertex pos
graphAlg (PlusF exp1 exp2) pos = GA.overlay (exp1 pos) (exp2 pos)
graphAlg (RefRelF p)       pos = GA.edge pos (p + pos)
graphAlg (RefAbsF p)       pos = GA.edge pos p

node :: Expr -> Position -> NodeGraph
node = cata graphAlg

initializeGraph :: Env -> Env
initializeGraph env
    = typeChecked
    where
        envWithGraph = env & graph .~
            foldr (\(pos, exp) g -> GA.overlay (node exp pos) g)
                GA.empty forms
        typeChecked = fst (foldr (\(pos, exp) (m, _) -> checkType exp pos m)
            (envWithGraph, TEmpty) forms)
        forms = M.toList (env ^. formulas)

changeCell :: Position -> Expr -> Env -> (Env, [Position])
changeCell p exp env = (env & graph .~ newGraph,  p : toRecalculate)
    where
        new = node exp p
        edgeTargetsToRemove = GA.postSet p (env ^. graph)
        removed = S.foldr (GA.removeEdge p) (env ^. graph) edgeTargetsToRemove
        newGraph = GA.overlay removed new
        toRecalculate = dfs [p] (GA.transpose $ env ^. graph)

insertAndEvalGraph :: Position -> Expr -> Env -> Env
insertAndEvalGraph pos expr env
    = foldr evalCell typeChecked toRecalculate
    where
        inserted = env & formulas %~ M.insert pos expr
        (envWithGraph, toRecalculate) = changeCell pos expr inserted
        invalidated = invalidateView toRecalculate envWithGraph
        cycles = checkCycles invalidated
        typeChecked = fst (checkType expr pos cycles)

checkCycles :: Env -> Env
checkCycles env = foldr insertError env (concatMap onlyCycles cyclicGraphs)
    where
        cyclicGraphs = GA.vertexList (GAA.scc $ env ^. graph)
        insertError v env =
            env & view %~ M.insert v (Left "Error: Cycle detected")
        onlyCycles = (\l ->
            if length l <= 1 && GA.hasEdge (head l) (head l) (env ^. graph)
                then l
                else []) . toList . GNA.vertexList1
