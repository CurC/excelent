module Excelent.Eval.Checker where

import Data.Functor.Foldable
import Excelent.Eval.Eval
import Excelent.Definition
import qualified Data.Map as M
import qualified Data.Set as S
import Data.NumInstances.Tuple
import Control.Monad.State
import Control.Lens hiding (view)

-- | Type checking algebra which calculates the type of the given expression
-- and also insert the error into the view if an error occured
typeAlg :: Algebra ExprF (Position -> Excel Type)
typeAlg EmptyF            pos = insertType pos TEmpty
typeAlg (ConstIntF _)     pos = insertType pos TInt
typeAlg (ConstDoubleF _)  pos = insertType pos TDouble
typeAlg (PlusF exp1 exp2) pos = do
    t1 <- exp1 pos
    t2 <- exp2 pos
    let typeError = typeErrorString (show t1 ++ " + " ++ show t2)
    env <- get
    if (t1 /= t2) || t1 == TInvalid || t2 == TInvalid
        then do
            put (env & view %~ M.insert pos typeError)
            insertType pos TInvalid
        else insertType pos t1
typeAlg (RefRelF p)       pos = do
    t <- fetchType (p + pos)
    insertType pos t
typeAlg (RefAbsF p)       pos = do
    t <- fetchType p
    insertType pos t

checkType :: Expr -> Position -> Excel Type
checkType = cata typeAlg

insertType :: Position -> Type -> Excel Type
insertType pos t = do
    env <- get
    put (env & types %~ M.insert pos t)
    return t

fetchType :: Position -> Excel Type
fetchType pos = do
    env <- get
    case M.lookup pos (env ^. types) of
        Just t -> return t
        Nothing -> case M.lookup pos (env ^. formulas) of
            Just expr -> checkType expr pos
            Nothing -> return TEmpty

typeErrorString :: String -> Either Error Value
typeErrorString s = Left (TypeError s)
