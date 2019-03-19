module Excelent.Eval.Graph where

import Algebra.Graph
import Algebra.Graph.AdjacencyMap as GA
import Algebra.Graph.AdjacencyMap.Algorithm as GAA
import Data.Functor.Foldable
import Excelent.Eval.Eval
import Definition
import qualified Data.Map as M
import qualified Data.Set as S

type NodeGraph = GA.AdjacencyMap Position

graphAlg :: Algebra Expr' (Position -> NodeGraph)
graphAlg (ConstInt' i)         pos = GA.vertex pos
graphAlg (OperPlus' exp1 exp2) pos = GA.overlay (exp1 pos) (exp2 pos)
graphAlg (RefRel' p)           pos = GA.edge (p + pos) pos
graphAlg (RefAbs' p)           pos = GA.edge p pos

graph :: Expr -> Position -> NodeGraph
graph = cata graphAlg

initializeGraph :: Env -> NodeGraph
initializeGraph Env {formulas = f}
    = foldr (\(pos, exp) g -> GA.overlay (graph exp pos) g) GA.empty (M.toList f)

changeCell :: NodeGraph -> Position -> Expr -> (NodeGraph, [Position])
changeCell g p exp = (newGraph, p : toRecalculate)
    where
        new = graph exp p
        removed = GA.removeVertex p g
        newGraph = GA.overlay removed new
        inputs = S.toList $ GA.preSet p g
        toRecalculate = dfs inputs removed

cycles :: NodeGraph -> [Position]
cycles g = GA.vertexList treeCycles
    where
        trees = map GA.tree (GAA.dfsForest g)
        treeCycles = GA.overlays $ filter (not . GAA.isAcyclic) trees
