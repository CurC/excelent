{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Data where

import qualified Data.Map as M
import Control.Monad.State (MonadState)

data Expr a where
    ConstInt :: Int -> Expr Int
    OperPlus :: Expr Int -> Expr Int -> Expr Int
    RefRel :: Position -> Expr a
    RefAbs :: Position -> Expr a

newtype Position = Position (Int, Int)
newtype Size = Size (Int, Int)

type FormulaData = M.Map Position (Expr Int)
type ViewData = M.Map Position (Either String Int)

instance MonadState FormulaData where
    get :: FormulaData
    get = undefined

data ViewPort = ViewPort {
    position :: Position,
    size :: Size
}

eval :: ViewPort -> FormulaData -> ViewData
eval vp fd = undefined

render :: ViewPort -> ViewData -> [[String]]
render vp vd = undefined

format :: [[String]] -> IO ()
format = undefined

-- example = do
--     insert (0, 0) 2
--     insert (0, 1) 1
--     insert (0, 2) plus

-- insert :: Position -> Expr Int -> FormulaData -> FormulaData
