module Excelent.Eval.Checker where

import Data.Functor.Foldable
import Excelent.Eval.Eval
import Excelent.Definition
import qualified Data.Map as M
import qualified Data.Set as S
import Data.NumInstances.Tuple
import Control.Lens hiding (view)
import Control.Lens.Combinators hiding (view)

-- | Type checking algebra which calculates the type of the given expression
-- and also insert the error into the view if an error occured
typeAlg :: Algebra ExprF (Position -> Env -> (Env, Type))
typeAlg (ConstIntF _)     pos env =
    let t = TInt
    in insertType pos t env
typeAlg (ConstDoubleF _)  pos env =
    let t = TDouble
    in insertType pos t env
typeAlg (PlusF exp1 exp2) pos env =
    let (env1, t1) = exp1 pos env
        (env2, t2) = exp2 pos env1
        typeError = typeErrorString (show t1 ++ " + " ++ show t2)
    in if not (t1 == t2) || t1 == TInvalid || t2 == TInvalid
            then insertType pos TInvalid (env & view %~ M.insert pos typeError)
            else insertType pos t1 env
typeAlg (RefRelF p)       pos env =
    let t = fetchType (p + pos) env
    in insertType pos t env
typeAlg (RefAbsF p)       pos env =
    let t = fetchType p env
    in insertType pos t env

checkType :: Expr -> Position -> Env -> (Env, Type)
checkType = cata typeAlg

insertType :: Position -> Type -> Env -> (Env, Type)
insertType pos t env = (env & types %~ M.insert pos t, t)

fetchType :: Position -> Env -> Type
fetchType pos env = case M.lookup pos (env ^. types) of
    Just t -> t
    Nothing -> TEmpty

typeErrorString :: String -> Either String Value
typeErrorString s = Left ("Type error: " ++ s)
