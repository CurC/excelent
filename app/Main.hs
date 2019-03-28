module Main where

import Data.Array
import Data.List
import Control.Lens hiding (view)
import Control.Lens.Combinators hiding (view)
import Control.Lens.Getter hiding (view)
import Control.Lens.Lens (Lens', (&))
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
import Graphics.Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Excelent.Definition
import Excelent.Eval.Eval
import Excelent.Eval.Graph
import Excelent.Parser
import Excelent.Print

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

-------------------------------------------------------------------------------
--
-- TODO : Viewport movement
-- TODO : Fix Viewport boundaries moving left when at zero should do nothing
-- TODO : Tests
-- TODO : Documentation
--
-------------------------------------------------------------------------------

main :: IO State
main = defaultMain app initialState

initialState :: State
initialState = State
    { _focusRing' = focusRing $ Data.Array.indices editors'
    , _widgets = editors'
    , _isEditing = False
    , _env = initializeGraph $ eval $ initial viewport
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
    (rows, cols) = vp & size.both ~- 1

app :: App State e (Int, Int)
app = App
    { appDraw = draw
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return . show'
    , appAttrMap = attrMap'
    }

swapEditorContentsWith :: String -> Cell -> Cell
swapEditorContentsWith xs = applyEdit ((`insertMany` xs) . clearZipper)

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
        [ ((i, j), editors')
        | i <- [0..rows - 1], j <- [0..cols - 1]
        , let editors' = if state^.focusRing'.focus == (i, j) && state^.isEditing
                         then swapEditorContentsWith (printF (i, j) (state^.env.formulas)) (gW (i, j))
                         else swapEditorContentsWith (printF (i, j) (state^.env.view)) (gW (i, j))
        ]
    gW = getEditor state
    (rows, cols) = state^.env.port.size

draw :: State -> [Widget (Int, Int)]
draw s = [table]
  where
    table = joinBorders
        $ vBox
        $ intersperse hBorder
        $ tableData & mapped %~ vLimit 1 . hBox . intersperse vBorder
    tableData = zipWith (:) headerColumnData (headerRowData:rowData)
    (numberOfRows, numberOfColumns) = s ^. env . port . size
    headerColumnData =
        [ withAttr n $ padLeftRight 1 $ str $ if i == 0 then " " else show (i - 1)
        | i <- [0..numberOfRows]
        , let isFocused = s^.focusRing'.focus._1 == i - 1
        , let n = "headerCell" <> if isFocused then "focused" else mempty
        ]
    headerRowData =
        [ withAttr n $ hCenter $ str $ show (j - 1)
        | j <- [1..numberOfColumns]
        , let isFocused = s^.focusRing'.focus._2 == j - 1
        , let n = "headerCell" <> if isFocused then "focused" else mempty
        ]
    rowData = toList' $ s ^. widgets & mapped %~
        padLeftRight 1 . withFocusRing (s^.focusRing') (renderEditor $ str . head)

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
    pos          = state^.focusRing'.focus
    insertedText = getEditContents $ (state^.widgets) ! pos
    parsed       = P.parse expression "" $ concat insertedText
    newEnv       = case parsed of
        Left err   -> state^.env
        Right expr -> insertAndEvalGraph pos expr $ state^.env
    state' = state & env .~ newEnv
    env'   = state'^. env

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
            ed <- handleEditorEvent e $ (s ^. widgets) ! (s^. focusRing' .focus)
            continue $ s & widgets %~ (// [(s^.focusRing'.focus, ed)])
    handler2 = case key of
        KEsc   -> halt s
        KEnter -> continue $ show' (s & isEditing %~ not)
        KLeft  -> continue $ s & focusRing' . focus . _2 -~ 1
        KRight -> continue $ s & focusRing' . focus . _2 +~ 1
        KUp    -> continue $ s & focusRing' . focus . _1 -~ 1
        KDown  -> continue $ s & focusRing' . focus . _1 +~ 1
        _      -> continue s
handleEvent s _ = continue s

-- | The attribute map that should be used during rendering.
-- This determines how widgets with a certain attribute (name) look.
attrMap' :: State -> AttrMap
attrMap' = const $ attrMap defAttr
    [ ("headerCell" <> "focused", bg brightBlack)
    , (editFocusedAttr, bg blue)
    ]
