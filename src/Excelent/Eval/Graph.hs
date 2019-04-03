module Excelent.Eval.Graph where

import Prelude
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List.NonEmpty hiding (length, head, map)

import Algebra.Graph hiding (Empty)
import Algebra.Graph.AdjacencyMap as GA hiding (Empty)
import Algebra.Graph.AdjacencyMap.Algorithm as GAA
import Algebra.Graph.NonEmpty.AdjacencyMap as GNA
import Control.Lens hiding (view, Empty)
import Control.Lens.Combinators hiding (view, Empty)
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
graphAlg EmptyF            pos = GA.vertex pos
graphAlg (ConstIntF _)     pos = GA.vertex pos
graphAlg (ConstDoubleF _)  pos = GA.vertex pos
graphAlg (PlusF exp1 exp2) pos = GA.overlay (exp1 pos) (exp2 pos)
graphAlg (RefRelF p)       pos = GA.edge pos (p + pos)
graphAlg (RefAbsF p)       pos = GA.edge pos p

node :: Expr -> Position -> NodeGraph
node = cata graphAlg

-- | Initialize the environment by generating the graph, checking for any
-- existing cycles and inserting type errors where relevant.
initializeGraph :: Env -> Env
initializeGraph env
    = checked
    where
        envWithGraph = env & graph .~
            foldr (\(pos, exp) g -> GA.overlay (node exp pos) g)
                GA.empty forms
        checked = insertErrors fPos envWithGraph
        forms = M.toList (env ^. formulas)
        fPos = map fst (M.toList (env ^. formulas))

-- | Recalculates the graph given that the expression at the position changed
-- and also returns the cells that need to be recalculated
changeCell :: Position -> Expr -> Env -> (Env, [Position])
changeCell p exp env = (env & graph .~ newGraph,  p : toRecalculate)
    where
        new = node exp p
        edgeTargetsToRemove = GA.postSet p (env ^. graph)
        removed = S.foldr (GA.removeEdge p) (env ^. graph) edgeTargetsToRemove
        newGraph = GA.overlay removed new
        toRecalculate = dfs [p] (GA.transpose $ env ^. graph)

type Cycles = S.Set Position
type CycleReferences = S.Set Position

-- | Checks for cycle related errors and inserts these into the view
insertErrors :: [Position] -> Env -> Env
insertErrors ps env = withErrors
    where
        cycles = checkCycles env
        transposed = GA.transpose (env ^. graph)
        cycleRefs = foldr (\c s ->
            S.union s (S.fromList (GAA.reachable c transposed))) S.empty cycles
        withErrors = foldr (\pos m ->
            if S.member pos cycleRefs
                then if S.member pos cycles
                    then m & view %~ M.insert pos cycleError
                    else m & view %~ M.insert pos cycleRefError
                else fst $ checkType
                    (fromMaybe Empty (M.lookup pos (m ^. formulas))) pos m)
            env ps

-- | Insert the expression at the given position and update any relevant cells
insertAndEvalGraph :: Position -> Expr -> Env -> Env
insertAndEvalGraph pos expr env
    = foldr evalCell checked toRecalculate
    where
    inserted = env & formulas %~ M.insert pos expr
    (envWithGraph, toRecalculate) = changeCell pos expr inserted
    invalidated = invalidateView toRecalculate envWithGraph
    checked = insertErrors toRecalculate invalidated

-- | Returns the positions involved in a cycle
checkCycles :: Env -> S.Set Position
checkCycles env = S.fromList poss
    where
        poss = concatMap onlyCycles cyclicGraphs
        cyclicGraphs = GA.vertexList (GAA.scc $ env ^. graph)
        onlyCycles = (\l ->
            if length l > 1 || GA.hasEdge (head l) (head l) (env ^. graph)
                then l
                else []) . toList . GNA.vertexList1

cycleError :: ViewValue
cycleError = Left (Error "Cycle detected")

cycleRefError :: ViewValue
cycleRefError = Left (Error "Cycle referenced")