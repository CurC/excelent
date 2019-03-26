{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Array
import Data.List (groupBy, intersperse)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

import Brick.AttrMap
import Brick.Focus
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Control.Lens.Getter
import Control.Lens.Lens (Lens', (&))
import Control.Lens.Setter
import Control.Lens.Tuple
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

type State =
    ( Array (Int, Int) (Editor String (Int, Int))
    , FocusRing (Int, Int)
    , Bool
    )

toList' :: Ix a => Array (a, a) b -> [[b]]
toList' = (mapped.mapped %~ snd)
    . groupBy (\x y -> x^._1._1 == y^._1._1)
    . assocs

-- | NOTE that focus is not a valid Lens.
-- However, it suffices for our purposes.
focus :: Eq n => Lens' (FocusRing n) n
focus f = \r -> flip focusSetCurrent r <$> f (fromJust $ focusGetCurrent r)

main :: IO State
main = defaultMain app initialState

initialState :: State
initialState = (editors, focusRing $ indices editors, isEditing)
  where
    editors = array ((1, 1), (numberOfRows, numberOfColumns))
        [ ((i, j), ed)
        | i <- [1..numberOfRows]
        , j <- [1..numberOfColumns]
        , let ed = editor (i, j) (Just 1) mempty
        ]
    (numberOfRows, numberOfColumns) = (8, 8)
    isEditing = False

app :: App State e (Int, Int)
app = App
    { appDraw = draw
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = attrMap'
    }

draw :: State -> [Widget (Int, Int)]
draw s = [table]
  where
    table = joinBorders
        $ vBox
        $ intersperse hBorder
        $ tableData & mapped %~ vLimit 1 . hBox . intersperse vBorder
    tableData = zipWith (:) headerColumnData (headerRowData:rowData)
    (numberOfRows, numberOfColumns) = (bounds $ s^._1)^._2
    headerColumnData =
        [ withAttr n $ padLeftRight 1 $ str $ if i == 0 then " " else show i
        | i <- [0..numberOfRows]
        , let isFocused = s^._2.focus._1 == i
        , let n = "headerCell" <> if isFocused then "focused" else mempty
        ]
    headerRowData =
        [ withAttr n $ hCenter $ str $ show j
        | j <- [1..numberOfColumns]
        , let isFocused = s^._2.focus._2 == j
        , let n = "headerCell" <> if isFocused then "focused" else mempty
        ]
    rowData = toList' $ s^._1 & mapped %~
        padLeftRight 1 . withFocusRing (s^._2) (renderEditor $ str . head)

chooseCursor :: State
             -> [CursorLocation (Int, Int)]
             -> Maybe (CursorLocation (Int, Int))
chooseCursor s = if s^._3 then focusRingCursor (view _2) s else const Nothing

handleEvent :: State
            -> BrickEvent (Int, Int) e
            -> EventM (Int, Int) (Next State)
handleEvent s (VtyEvent e@(EvKey key [])) = if s^._3 then handler1 else handler2
  where
    handler1 = case key of
        KEsc     -> continue $ s & _3 %~ not
        KEnter   -> handleEvent (s & _3 %~ not) (VtyEvent $ EvKey KDown [])
        _        -> do
            ed <- handleEditorEvent e $ (s^._1) ! (s^._2.focus)
            continue $ s & _1 %~ (// [(s^._2.focus, ed)])
    handler2 = continue $ case key of
        KEnter   -> s & _3 %~ not
        KLeft    -> s & _2.focus._2 -~ 1
        KRight   -> s & _2.focus._2 +~ 1
        KUp      -> s & _2.focus._1 -~ 1
        KDown    -> s & _2.focus._1 +~ 1
        _        -> s
handleEvent s _ = continue s

attrMap' :: State -> AttrMap
attrMap' = const $ attrMap defAttr
    [ ("headerCell" <> "focused", bg brightBlack)
    , (editFocusedAttr, bg blue)
    ]
