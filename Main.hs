module Main where

import Data.Array
import Data.Maybe
import Data.Monoid ((<>))

import Brick.AttrMap
import Brick.Focus
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Graphics.Vty

type State = (FocusRing Position, Array Position (Editor String Position))
type Position = (Int, Int)

instance Monoid Int where
    mempty = 0
    mappend = (+)

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
numberOfRows = 4
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
    ws = elems $ withFocusRing r (renderEditor $ str . head) <$> eds

handleEvent :: State -> BrickEvent Position e -> EventM Position (Next State)
handleEvent (r, eds) (VtyEvent e) = case e of
    EvKey KLeft  [] -> continue (focusSetCurrent (pos <> (0, -1)) r, eds)
    EvKey KRight [] -> continue (focusSetCurrent (pos <> (0, 1)) r, eds)
    EvKey KUp    [] -> continue (focusSetCurrent (pos <> (-1, 0)) r, eds)
    EvKey KDown  [] -> continue (focusSetCurrent (pos <> (1, 0)) r, eds)
    _               -> do
        ed' <- handleEditorEvent e ed
        continue (r, eds // [(pos, ed')])
  where
    ed = eds ! pos
    pos = fromJust $ focusGetCurrent r
handleEvent s _ = continue s
