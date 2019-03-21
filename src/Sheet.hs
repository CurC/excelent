module Sheet where

import Data.Array
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text.Zipper
import Data.NumInstances.Tuple

import Brick.AttrMap
import Brick.Focus
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Graphics.Vty

import Excelent.Eval.Eval

type State = (FocusRing Position, Array Position (Editor String Position))
type Position = (Int, Int)

data Dir =  N | S | W | E

divideIntoGroupsOf :: Int -> [a] -> [[a]]
divideIntoGroupsOf n [] = [[]]
divideIntoGroupsOf n xs =
    let (xs1, xs2) = splitAt n xs in xs1 : divideIntoGroupsOf n xs2

main :: IO State
main = defaultMain app initialState

initialState :: State
initialState = (focusRing $ indices editors, editors)

editors :: Array Position (Editor String Position)
editors = array ((1, 1), (numberOfRows, numberOfColumns))
    [ ((i, j), editor (i, j) (Just 1) "")
    | i <- [1..numberOfRows], j <- [1..numberOfColumns]
    ]

numberOfRows, numberOfColumns :: Int
numberOfRows = 14
numberOfColumns = 4

app :: App State e Position
app = App
    { appDraw = draw
    , appChooseCursor = focusRingCursor fst
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap defAttr []
    }

draw :: State -> [Widget Position]
draw (r, eds) = [vBox $ hBox <$> divideIntoGroupsOf numberOfColumns ws]
  where
    ws = map border $ elems $ withFocusRing r (renderEditor $ str . head) <$> eds

--Insert result of eval, except for the one in focus.
updateEditors :: State -> Dir -> State
updateEditors (r,eds) dir = case dir of
    W -> (focus w, showData w)
    E -> (focus e, showData e)
    N -> (focus n, showData n)
    S -> (focus s, showData s)    
  where
    focus d = focusSetCurrent (pos + d) r
    showData d = eds // [((i,j), if not (inFocus (i,j) d) then applyEdit (insertChar 'a' . clearZipper) (ed i j) else ed i j) 
                      | i <- [1..numberOfRows], j <- [1..numberOfColumns]
                      ]
    inFocus e d = pos + d == e
    pos = fromJust $ focusGetCurrent r
    ed i j = eds ! (i,j)
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
handleEvent s@(r, eds) (VtyEvent e) = case e of
    EvKey KLeft  [] -> continue $ updateEditors s W
    EvKey KRight [] -> continue $ updateEditors s E
    EvKey KUp    [] -> continue $ updateEditors s N
    EvKey KDown  [] -> continue $ updateEditors s S
    EvKey KEnter [] -> continue $ updateEditors s S
    EvKey KEsc   [] -> halt s
    _               -> do
        ed' <- handleEditorEvent e ed
        continue (r, eds // [(pos, ed')])
  where
    ed = eds ! pos
    pos = fromJust $ focusGetCurrent r
handleEvent s _ = continue s
