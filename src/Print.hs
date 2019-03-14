module Print where

import Definition
import Excelent.Eval.Eval
import Data.Maybe
import Data.List
import qualified Text.PrettyPrint.Boxes as T
import qualified Data.Map as M

renderView :: Env -> [[String]]
renderView Env {view = v, port = vp } = viewData
    where
        viewData = map (map (\p -> case p `M.lookup` v of
            Nothing -> emptyCellPlaceholder
            Just v -> case v of
                Left s -> if s == "" then emptyCellPlaceholder else s
                Right val -> show val)) positions
        positions = inView vp

renderFormulas :: Env -> [[String]]
renderFormulas Env {formulas = f, port = vp } = fData
    where
        fData = map (map (\p -> case p `M.lookup` f of
            Nothing -> emptyCellPlaceholder
            Just x -> show x)) positions
        positions = inView vp


emptyCellPlaceholder :: String
emptyCellPlaceholder = "_____"

format :: [[String]] -> T.Box
format = formatTable (repeat T.right) 2

printView :: Env -> IO ()
printView env = T.printBox . format . renderView $ eval env

printForms :: Env -> IO ()
printForms env = T.printBox . format . renderFormulas $ eval env

-- https://github.com/treeowl/boxes/pull/5/files
formatTable :: [T.Alignment] -> Int -> [[String]] -> T.Box
formatTable als sep cols = T.punctuateH T.top sep' $ map (uncurry singleCol) colal
  where als' = cycle als
        cols' = transpose cols
        colal = zip als' cols'
        sep' = T.emptyBox 0 sep

singleCol :: T.Alignment -> [String] -> T.Box
singleCol al = T.vcat al . map T.text