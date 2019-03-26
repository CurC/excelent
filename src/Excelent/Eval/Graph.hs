module Excelent.Eval.Graph where

import Algebra.Graph
import Algebra.Graph.AdjacencyMap as GA
import Algebra.Graph.AdjacencyMap.Algorithm as GAA
import Data.Functor.Foldable
import Excelent.Eval.Eval
import Excelent.Definition
import qualified Data.Map as M
import qualified Data.Set as S
import Data.NumInstances.Tuple
import Control.Lens hiding (view)
import Control.Lens.Combinators hiding (view)

graphAlg :: Algebra Expr' (Position -> NodeGraph)
graphAlg (ConstInt' i)     pos = GA.vertex pos
graphAlg (Plus' exp1 exp2) pos = GA.overlay (exp1 pos) (exp2 pos)
graphAlg (RefRel' p)       pos = GA.edge pos (p + pos)
graphAlg (RefAbs' p)       pos = GA.edge pos p

node :: Expr -> Position -> NodeGraph
node = cata graphAlg

initializeGraph :: Env -> Env
initializeGraph env
    = env & graph .~
        foldr (\(pos, exp) g -> GA.overlay (node exp pos) g)
            GA.empty (M.toList f)
    where
        f = env ^. formulas

changeCell :: Position -> Expr -> Env -> (Env, [Position])
changeCell p exp env = (env & graph .~ newGraph,  p : toRecalculate)
    where
        new = node exp p
        edgeTargetsToRemove = GA.postSet p (env ^. graph)
        removed = S.foldr (GA.removeEdge p) (env ^. graph) edgeTargetsToRemove
        newGraph = GA.overlay removed new
        toRecalculate = dfs [p] (GA.transpose $ env ^. graph)

cycles :: NodeGraph -> [Position]
cycles g = GA.vertexList treeCycles
    where
        trees = map GA.tree (GAA.dfsForest g)
        treeCycles = GA.overlays $ filter (not . GAA.isAcyclic) trees

insertAndEvalGraph :: Position -> Expr -> Env -> Env
insertAndEvalGraph pos expr env
    = foldr evalCell invalidated toRecalculate
    where
        inserted = env & formulas %~ M.insert pos expr
        (envWithGraph, toRecalculate) = changeCell pos expr inserted
        invalidated = invalidateView toRecalculate envWithGraph
