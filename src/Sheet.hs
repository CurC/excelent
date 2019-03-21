module Sheet where

import Data.Array
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text.Zipper
import Data.NumInstances.Tuple
import qualified Text.Parsec as P

import Brick.AttrMap
import Brick.Focus
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Graphics.Vty

import Data.NumInstances.Tuple

import Excelent.Definition
import Excelent.Eval.Eval
import Excelent.Parser
import Print
import Debug.Trace

data State = State {
        focus :: FocusRing Position,
        widgets :: Array Position (Editor String Position),
        env :: Env
    }

data Dir =  N | S | W | E

divideIntoGroupsOf :: Int -> [a] -> [[a]]
divideIntoGroupsOf n [] = [[]]
divideIntoGroupsOf n xs =
    let (xs1, xs2) = splitAt n xs in xs1 : divideIntoGroupsOf n xs2

main :: IO State
main = defaultMain app initialState

initialState :: State
initialState = State {
        focus = focusRing $ indices editors',
        widgets = editors',
        env = initial ViewPort {
            size = (numberOfRows, numberOfColumns),
            position = (0, 0)
        }
    }
    where
        editors' = editors

editors :: Array Position (Editor String Position)
editors = array ((0, 0), (numberOfRows - 1, numberOfColumns - 1))
    [ ((i, j), editor (i, j) (Just 1) "")
    | i <- [0..numberOfRows - 1], j <- [0..numberOfColumns - 1]
    ]

numberOfRows, numberOfColumns :: Int
numberOfRows = 4
numberOfColumns = 4

app :: App State e Position
app = App
    { appDraw = draw
    , appChooseCursor = focusRingCursor focus
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap defAttr []
    }

draw :: State -> [Widget Position]
draw state'
    = [vBox $ hBox <$> divideIntoGroupsOf numberOfColumns ws]
  where
    r = focus state'
    eds = widgets state'
    ws = map border $ elems $ withFocusRing r (renderEditor $ str . head) <$> eds

show' :: State -> Position -> Array Position (Editor String Position)
show' state@State {widgets = w, env = e} d
    = w //
        [((i,j),
            if not (inFocus (i,j) d)
                then applyEdit ((\w -> foldr insertChar w (reverse $ printV (i, j) v)) . clearZipper) (ed i j)
                else applyEdit ((\w -> foldr insertChar w (reverse $ printF (i, j) f)) . clearZipper) (ed i j))
        | i <- [0..fst (size p) - 1], j <- [0..snd (size p) - 1]
        ]
    where
    Env{formulas = f, view = v, port = p} = e
    ed i j = widgets state ! (i,j)
    pos = fromJust $ focusGetCurrent (focus state)
    inFocus e d = pos + d == e


--Insert result of eval, except for the one in focus.
updateEditors :: State -> Dir -> State
updateEditors state dir = case dir of
    W -> state' {focus = ring w, widgets = show' state' w}
    E -> state' {focus = ring e, widgets = show' state' e}
    N -> state' {focus = ring n, widgets = show' state' n}
    S -> state' {focus = ring s, widgets = show' state' s}
  where
    ring d = focusSetCurrent (pos + d) (focus state')
    insertedText = getEditContents (widgets state ! pos)
    parsed = P.parse expression "" (concat insertedText)
    oldEnv :: Env
    oldEnv = case parsed of
        Left err -> env state
        Right expr -> (env state) { view = M.empty, formulas = M.insert pos expr (formulas $ env state)}
    newEnv = eval oldEnv
    state' = state {env = newEnv}
    form' = formulas $ env state'
    view' = view $ env state'
    pos = fromJust $ focusGetCurrent (focus state)
    w = ( 0,-1)
    e = ( 0, 1)
    n = (-1, 0)
    s = ( 1, 0)

{-
Array Position (Editor String Position)
updateEditors (r,eds) = eds // [((i,j), if not (inFocus (i, j)) then applyEdit (insertChar 'a' . clearZipper) (ed i j) else ed i j)
                               | i <- [1..numberOfRows], j <- [1..numberOfColumns]
                               ]
    where ed i j = eds ! (i,j)
          inFocus e = fromJust (focusGetCurrent r) == e
-}
handleEvent :: State -> BrickEvent Position e -> EventM Position (Next State)
handleEvent state' (VtyEvent e) = case e of
    EvKey KLeft  [] -> continue $ updateEditors state' W
    EvKey KRight [] -> continue $ updateEditors state' E
    EvKey KUp    [] -> continue $ updateEditors state' N
    EvKey KDown  [] -> continue $ updateEditors state' S
    EvKey KEnter [] -> continue $ updateEditors state' S
    EvKey KEsc   [] -> halt state'
    _               -> do
        ed' <- handleEditorEvent e ed
        continue state' {widgets = widgets state' // [(pos, ed')]}
  where
    ed = widgets state' ! pos
    pos = fromJust $ focusGetCurrent (focus state')
handleEvent state' _ = continue state'
