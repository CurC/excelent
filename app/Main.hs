{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Array
import Control.Lens hiding (view)
import Control.Lens.Getter hiding (view)
import Control.Lens.Lens (Lens', (&))
import Control.Monad.State (get, put, execState)
import Control.Lens.Setter
import Control.Lens.Tuple
import Data.List (groupBy, intersperse)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text.Zipper
import qualified Data.Map as M
import qualified Text.Parsec as P

import Brick.AttrMap
import Brick.Focus
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Excelent.Definition
import Excelent.Eval.Eval
import Excelent.Eval.Graph
import Excelent.Parser
import Excelent.Print
import Debug.Trace

type Cell = Editor String Position

-- | The State determines the user interface.
-- Events may potentially modify this state.
data State = State {
        _focusRing' :: FocusRing Position,
        _widgets :: Array Position Cell,
        _env :: Env,
        _isEditing :: Bool
    }

-- | Used to automatically derive lenses for our data type State.
makeLenses ''State

-- | Converts a matrix to a list of lists (rows).
-- toList' is basically a more specific version of toList for Arrays.
toList' :: Ix a => Array (a, a) b -> [[b]]
toList' = (mapped.mapped %~ snd)
    . groupBy (\x y -> x^._1._1 == y^._1._1)
    . assocs

-- | focus is a Simple Lens that allows us to conveniently get and set the focus
-- of a FocusRing.
-- NOTE that focus is not a valid Lens (i.e. it does not satisfy all Lens laws).
-- However, it suffices for our purposes.
focus :: Eq n => Lens' (FocusRing n) n
focus f = \r -> flip focusSetCurrent r <$> f (fromJust $ focusGetCurrent r)

main :: IO State
main = defaultMain app initialState

initialState :: State
initialState = State
    { _focusRing' = focusRing $ Data.Array.indices editors'
    , _widgets = editors'
    , _isEditing = False
    , _env = execState (initializeGraph >> eval) (initial viewport)
    }
  where
    editors' = editors viewport
    viewport = ViewPort
        { _size = (11, 6)
        , _position = (0, 0)
        }

-- | Creates a matrix of the dimensions as specified in the viewport.
editors :: ViewPort -> Array Position Cell
editors vp = array ((0, 0), (rows, cols))
    [ ((i, j), editor (i, j) (Just 1) "")
    | i <- [0..rows], j <- [0..cols]
    ]
  where
    (posR, posC) = vp^.position
    (rows, cols) = (vp & size.both -~ 1)^.size

app :: App State e (Int, Int)
app = App
    { appDraw = draw
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return . show'
    , appAttrMap = attrMap'
    }

swapEditorContentsWith :: String -> Cell -> Cell
swapEditorContentsWith xs = applyEdit (insertMany xs . clearZipper)

-- | Extracts the editor at the given position from the state.
getEditor :: State -> Position -> Cell
getEditor state pos = (state^.widgets) ! pos

-- | Sets the editor contents of the editors to either the evaluated or
-- unevaluated expression depending on the current focus and the value of
-- isEditing.
show' :: State -> State
show' state = state & widgets %~ (// editors')
  where
    editors' =
        [ ((i, j), editors'')
        | i <- [0..rows - 1], j <- [0..cols - 1]
        , let pos = state^.env.port.position & _1 +~ i & _2 +~ j
        , let editors'' = flip swapEditorContentsWith (getEditor state (i, j)) $
            if state^.focusRing'.focus == (i, j) && state^.isEditing
            then printF pos $ state^.env.formulas
            else printV pos $ state^.env.view
        ]
    (rows, cols) = state^.env.port.size

draw :: State -> [Widget (Int, Int)]
draw s = [table]
  where
    table = joinBorders
        $ vBox
        $ intersperse hBorder
        $ tableData & mapped %~ vLimit 1 . hBox . intersperse vBorder
    tableData = zipWith (:) headerColumnData (headerRowData:rowData)
    (posRows, posColumns) = s^.env.port.position
    (numberOfRows, numberOfColumns) = s^.env.port.size
    headerColumnData =
        let m = maximum $ [posRows .. posRows + numberOfRows - 1]
            & mapped %~ textWidth . show
        in  [ withAttr n $ pad $ str headerString
            | i <- [posRows .. posRows + numberOfRows]
            , let isFocused =
                s^.focusRing'.focus._1 + s^.env.port.position._1 == i - 1
            , let n = "headerCell" <> if isFocused then "focused" else mempty
            , let headerString = if i == posRows then " " else show (i - 1)
            , let k = m - textWidth headerString
            , let pad = padLeft (Pad (k + 2)) . padRight (Pad 1)
            ]
    headerRowData =
        [ withAttr n $ hCenter $ str $ show (j - 1)
        | j <- [posColumns + 1 .. posColumns + numberOfColumns]
        , let isFocused =
            s^.focusRing'.focus._2 + s^.env.port.position._2 == j - 1
        , let n = "headerCell" <> if isFocused then "focused" else mempty
        ]
    rowData =
        let f = padLeftRight 1
            . withFocusRing (s^.focusRing') (renderEditor $ str . head)
        in toList' $ s^.widgets & mapped %~ f

-- | This function chooses which of the zero or more cursor locations reported
-- by the rendering process should be selected as the one to use to place the
-- cursor. If this returns Nothing, no cursor is placed.
chooseCursor :: State
             -> [CursorLocation (Int, Int)]
             -> Maybe (CursorLocation (Int, Int))
chooseCursor s =
    if s^.isEditing
    then focusRingCursor (^.focusRing') s
    else const Nothing

--Insert result of eval, except for the one in focus.
updateEditors :: State -> State
updateEditors state = show' state'
  where
    internalPos  = state^.env.port.position + state^.focusRing'.focus
    pos = state^.focusRing'.focus
    insertedText = getEditContents $ (state^.widgets) ! pos
    parsed = P.parse expression "" $ concat insertedText
    newEnv = case parsed of
        Left err   -> state^.env
        Right expr -> execState (insertAndEvalGraph internalPos expr) $
            state^.env
    state' = state & env .~ newEnv

-- | This function takes the current application state and an event and returns
-- an action to be taken and a corresponding transformed application state.
handleEvent :: State
            -> BrickEvent (Int, Int) e
            -> EventM (Int, Int) (Next State)
handleEvent s (VtyEvent e@(EvKey key [])) =
    if s^.isEditing then handler1 else handler2
  where
    handler1 = case key of
        KEsc   -> halt s
        KEnter -> handleEvent (updateEditors $ s & isEditing %~ not) $
            VtyEvent $ EvKey KDown []
        _      -> do
            ed <- handleEditorEvent e $ (s^.widgets) ! (s^.focusRing'.focus)
            continue $ s & widgets %~ (// [(s^.focusRing'.focus, ed)])
    handler2 = case key of
        KEsc   -> halt s
        KEnter -> continue $ show' $ s & isEditing %~ not
        KLeft  -> continue $ updateFocusOrViewport s _2 0 (-1)
        KRight -> continue $
            updateFocusOrViewport s _2 (s^.env.port.size._2 - 1) 1
        KUp    -> continue $ updateFocusOrViewport s _1 0 (-1)
        KDown  -> continue $
            updateFocusOrViewport s _1 (s^.env.port.size._1 - 1) 1
        _      -> continue s
handleEvent s (VtyEvent e@(EvResize x y)) = continue $ show' $ resize s (x, y)
handleEvent s _ = continue s

resize :: State -> (Int, Int) -> State
resize s (x, y) = s & focusRing' .~ focusRing (Data.Array.indices newEditors)
                    & widgets .~ newEditors
                    & env.port .~ newPort
  where
    newEditors = editors newPort
    newPort = (s^.env.port) & size._1 .~ (y `quot` 2) - 1

updateFocusOrViewport :: State -> Lens Position Position Int Int -> Int -> Int -> State
updateFocusOrViewport s lens check i
    | check == 0 && s ^. env . port . position . lens == 0
    = s
    | s^.focusRing'.focus.lens == check
    = show' $ s & env.port.position.lens +~ i
    | otherwise
    = s & focusRing'.focus.lens +~ i

-- | The attribute map that should be used during rendering.
-- This determines how widgets with a certain attribute (name) look.
attrMap' :: State -> AttrMap
attrMap' = const $ attrMap defAttr
    [ ("headerCell" <> "focused", bg brightBlack)
    , (editFocusedAttr, bg blue)
    ]
