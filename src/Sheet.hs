module Sheet where

import Data.Array
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text.Zipper

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

instance Monoid Int where
    mempty = 0
    mappend = (<>)

instance Semigroup Int where
    (<>) = (+)

data Dir = Up | Down | Left | Right

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
updateEditors (r,eds) Left  = (focus l, showData l)
updateEditors (r,eds) Right = (focus r, showData r)
updateEditors (r,eds) Up    = (focus u, showData u)
updateEditors (r,eds) Down  = (focus d, showData d)    
    where
        focus dir = focusSetCurrent (pos <> dir)
        showData dir = undefined
        pos = fromJust $ focusGetCurrent r
        l = ( 0,-1)
        r = ( 0, 1)
        u = (-1, 0)
        d = ( 1, 0)

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
    EvKey KLeft  [] -> continue $ updateEditors s Left
    EvKey KRight [] -> continue $ updateEditors s Right
    EvKey KUp    [] -> continue $ updateEditors s Up
    EvKey KDown  [] -> continue $ updateEditors s Down
    EvKey KEnter [] -> continue $ updateEditors s Down
    EvKey KEsc   [] -> halt s
    _               -> do
        ed' <- handleEditorEvent e ed
        continue (r, eds // [(pos, ed')])
  where
    ed = eds ! pos
    pos = fromJust $ focusGetCurrent r
handleEvent s _ = continue s
