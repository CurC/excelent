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
import Control.Lens hiding (Empty, view)
import Control.Monad.State
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
initializeGraph :: Excel ()
initializeGraph
    = do
        env <- get
        let forms        = M.toList (env ^. formulas)
        let positions    = map fst forms
        put (env & graph .~ initGraph forms)
        checkErrors positions

-- | Generate the base dependency graph based on each expressions in the given
-- cells
initGraph :: [(Position, Expr)] -> NodeGraph
initGraph = foldr (\(pos, exp) g -> GA.overlay (node exp pos) g) GA.empty

-- | Recalculates the graph given that the expression at the position changed
-- and also returns the cells that need to be recalculated
changeCell :: Position -> Expr -> Excel [Position]
changeCell p exp = do
    env <- get
    let new           = node exp p
    let next          = GA.postSet p (env ^. graph)
    let removed       = S.foldr (GA.removeEdge p) (env ^. graph) next
    let newGraph      = GA.overlay removed new
    let toRecalculate = dfs [p] (GA.transpose $ env ^. graph)
    put (env & graph .~ newGraph)
    return (p : toRecalculate)

type Cycles = S.Set Position
type CycleReferences = S.Set Position

-- | Checks for cycle related errors and inserts these into the view
checkErrors :: [Position] -> Excel ()
checkErrors positions = do
    env <- get
    let cycles    = getCycles env
    let cycleRefs = getCycleRefs (env ^. graph) cycles
    mapM_ (insertErrors cycles cycleRefs) positions

insertErrors :: Cycles -> CycleReferences -> Position -> Excel ()
insertErrors cycles cycleRefs pos = do
    env <- get
    if S.member pos cycleRefs
        then if S.member pos cycles
            then put $ env & view %~ M.insert pos cycleError
            else put $ env & view %~ M.insert pos cycleRefError
        else do
            _ <- checkType (fromMaybe Empty (M.lookup pos (env ^. formulas)))
                    pos
            return ()

-- | Insert the expression at the given position and update any relevant cells
insertAndEvalGraph :: Position -> Expr -> Excel ()
insertAndEvalGraph pos expr
    = do
    modify (\env -> env & formulas %~ M.insert pos expr)
    toRecalculate <- changeCell pos expr
    invalidateView toRecalculate
    checkErrors toRecalculate
    mapM_ evalCell toRecalculate

getCycleRefs :: NodeGraph -> Cycles -> CycleReferences
getCycleRefs graph = foldr (\cyc refs ->
        S.union refs (S.fromList (GAA.reachable cyc transposed))) S.empty
    where
        transposed = GA.transpose graph

-- | Returns the positions involved in a cycle
getCycles :: Env -> Cycles
getCycles env = S.fromList poss
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