module Excelent.Eval.Eval where

import qualified Data.Map as M
import qualified Data.Set as S
import Algebra.Graph.AdjacencyMap as G
import Control.Monad
import Control.Monad.State
import Data.Functor.Foldable
import Excelent.Definition
import Data.NumInstances.Tuple
import Control.Lens hiding (Empty, view)

-- | The algebra expressing the evaluation of Expr structures into its results.
-- Expressions are dependent on the environment supplied and can on its own
-- also change the current environment in the form of 'caching' the
-- results of any cells calculated in the mean time.
evalAlg :: Algebra ExprF (Position -> Env -> (Env, ViewValue))
evalAlg EmptyF            _   env = (env, Left $ NoError "")
evalAlg (ConstIntF i)     _   env = (env, Right (I i))
evalAlg (ConstDoubleF d)  _   env = (env, Right (D d))
evalAlg (PlusF exp1 exp2) pos env = (env2, do
    i <- vval1
    j <- vval2
    sumVals i j)
  where
    sumVals (I i1) (I i2) = Right (I (i1 + i2))
    sumVals (D d1) (D d2) = Right (D (d1 + d2))
    sumVals _ _ = Left $ InternalError "Missed type error"
    (env1, vval1) = exp1 pos env
    (env2, vval2) = exp2 pos env1
evalAlg (RefRelF p) pos env = doLookup (pos + p) env
evalAlg (RefAbsF p) pos env = doLookup p env

-- | Try and either lookup the value of the given position if it was ever
-- calculated, or calculate it and save any cells calculated along the way in
-- the environment
doLookup :: Position -> Env -> (Env, ViewValue)
doLookup pos env = case M.lookup pos (env^.formulas) of
    Nothing    -> emptyCellReference
    Just Empty -> emptyCellReference
    Just e -> case M.lookup pos (env^.view) of
        Nothing ->
            let (newEnv, val) = cata evalAlg e pos env
            in (newEnv & view %~ M.insert pos val, val)
        Just e' -> (env, e')
  where
    emptyCellReference = (env, Left $ Warning "Empty cell referenced")

-- | Evaluate the given expression using the current position and its environment.
-- This is done using a catamorphism, which is automatically derived using the
-- recursion-schemes library
evalExpr :: Expr -> Position -> Env -> (Env, ViewValue)
evalExpr = cata evalAlg

-- | Ensures that the cell at the given position is evaluated, or in other words,
-- that the view record in the environment contains a value for the cell at
-- the position
evalCell :: Position -> Excel ()
evalCell pos = do
    env <- get
    put $ case M.lookup pos (env^.view) of
        Just _  -> env
        Nothing -> env & view %~ M.insert pos (case M.lookup pos (env^.formulas) of
            Just expr -> snd $ evalExpr expr pos env
            Nothing   -> Left $ NoError "")

-- | Evaluate all of the cells 'visible' to the viewport
eval :: Excel ()
eval = do
    env <- get
    let positions = concat $ inView $ env^.port
    mapM_ evalCell positions

-- | Generates all positions which are in view as a list of columns
inView :: ViewPort -> [[Position]]
inView vp =
    [ [vp^.position & _1 +~ i & _2 +~ j | i <- [0 .. vp^.size._1]]
    | j <- [0 .. vp^.size._2]
    ]

-- | Removes the values in the ViewData in the environment at the given positions,
-- so that they can be recalculated. The types of the cells also need to be
-- recalculated
invalidateView :: [Position] -> Excel ()
invalidateView ps =
  let f = (`M.withoutKeys` S.fromList ps)
  in modify ((types %~ f) . (view %~ f))
