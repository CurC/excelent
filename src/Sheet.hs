{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Array
import Data.List
import Data.Maybe
import Data.Monoid ((<>))
import Data.NumInstances.Tuple
import Data.Text.Zipper
import qualified Data.Map as M
import qualified Text.Parsec as P
import Data.List (groupBy, intersperse)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Control.Lens.Getter
import Control.Lens.Lens (Lens', (&))
import Control.Lens.Setter
import Control.Lens.Tuple

import Brick.AttrMap
import Brick.Focus
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Excelent.Definition
import Excelent.Eval.Eval
import Excelent.Eval.Graph
import Excelent.Parser
import Print

data State = State {
        _focusRing' :: FocusRing Position,
        _widgets :: Array Position Cell,
        _env :: Env,
        _isEditing :: Bool
    }

makeLenses ''State

data Dir =  N | S | W | E | None
type Cell = Editor String Position

toList' :: Ix a => Array (a, a) b -> [[b]]
toList' = (mapped.mapped %~ snd)
    . groupBy (\x y -> x^._1._1 == y^._1._1)
    . assocs

-- | NOTE that focus is not a valid Lens.
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
initialState = State {
        focus = focusRing $ indices editors',
        widgets = editors',
        isEditing = False,
        env = initializeGraph $ eval $ initial viewport
    }
    where
        editors' = editors viewport
        viewport = ViewPort {
            size = (10, 6),
            position = (0, 0)
        }

editors :: ViewPort -> Array Position Cell
editors vp = array ((0, 0), (rows, cols))
    [ ((i, j), editor (i, j) (Just 1) "")
    | i <- [0..rows], j <- [0..cols]
    ]
    where
        (rows, cols) = size vp - (1, 1)

app :: App State e (Int, Int)
app = App
    { appDraw = draw
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = attrMap'
    }

-- draw :: State -> [Widget Position]
-- draw state'
--     = [vBox $ hBox <$> divideIntoGroupsOf cols ws]
--   where
--     (rows, cols) = size p
--     r = focus state'
--     eds = widgets state'
--     ws = map border $ elems $ withFocusRing r (renderEditor $ str . head) <$> eds
--     Env{formulas = f, view = v, port = p} = env state'

-- show' :: State -> Dir -> Array Position Cell
-- show' state@State {widgets = w, env = e} d
--     = w //
--         [((i,j),
--             if not (inFocus pos d (i,j))
--                 then refreshWidget printV v (i, j) (gW (i, j))
--                 else refreshWidget printF f (i, j) (gW (i, j)))
--         | i <- [0..rows - 1], j <- [0..cols - 1]]
--     where
--     gW = getWidget state
--     (rows, cols) = size p
--     Env{ formulas = f, view = v, port = p } = e
--     pos = currentPosition (focus state)

-- refreshWidget :: (Position -> a -> String) -> a -> Position -> Cell -> Cell
-- refreshWidget strF data' coord = swapEditorContents (strF coord data')

-- swapEditorContents :: String -> Cell -> Cell
-- swapEditorContents xs = applyEdit ((`insertString` xs) . clearZipper)

-- insertString :: TextZipper String -> String -> TextZipper String
-- insertString = foldl' (flip insertChar)

-- -- Determine if the given position is in focus if moving in the
-- -- specified direction
-- inFocus :: Position -> Dir -> Position -> Bool
-- inFocus current dir check = current + move dir == check

-- -- Get the current position in focus
-- currentPosition :: FocusRing Position -> Position
-- currentPosition foc = fromJust $ focusGetCurrent foc

-- -- Get the widget at the given position
-- getWidget :: State -> Position -> Cell
-- getWidget state pos = widgets state ! pos

-- move :: Dir -> (Int, Int)
-- move W    = ( 0,-1)
-- move E    = ( 0, 1)
-- move N    = (-1, 0)
-- move S    = ( 1, 0)
-- move None = ( 0, 0)

-- updateFocus :: Dir -> FocusRing Position -> FocusRing Position
-- updateFocus dir foc = focusSetCurrent (currentPosition foc + move dir) foc

-- --Insert result of eval, except for the one in focus.
-- updateEditors :: State -> Dir -> State
-- updateEditors state dir = state' {
--         focus = updateFocus dir (focus state'),
--         widgets = show' state' dir
--     }
--   where
--     pos          = currentPosition (focus state)
--     insertedText = getEditContents (widgets state ! pos)
--     parsed       = P.parse expression "" (concat insertedText)
--     newEnv       = case parsed of
--         Left err -> env state
--         Right expr -> insertAndEvalGraph pos expr (env state)
--     state' = state {env = newEnv}
--     env'@Env{formulas = form', view = view'} = env state'

-- handleEvent :: State -> BrickEvent Position e -> EventM Position (Next State)
-- handleEvent state (VtyEvent e) = case e of
--     EvKey KLeft  [] -> continue $ updateEditors state W
--     EvKey KRight [] -> continue $ updateEditors state E
--     EvKey KUp    [] -> continue $ updateEditors state N
--     EvKey KDown  [] -> continue $ updateEditors state S
--     EvKey KEnter [] -> continue $ updateEditors state S
--     EvKey KEsc   [] -> halt state
--     _               -> do
--         ed' <- handleEditorEvent e ed
--         continue state {widgets = widgets state // [(pos, ed')]}
--   where
--     ed = widgets state ! pos
--     pos = currentPosition (focus state)
-- handleEvent state _ = continue state

draw :: State -> [Widget (Int, Int)]
draw s = [table]
  where
    table = joinBorders
        $ vBox
        $ intersperse hBorder
        $ tableData & mapped %~ vLimit 1 . hBox . intersperse vBorder
    tableData = zipWith (:) headerColumnData (headerRowData:rowData)
    (numberOfRows, numberOfColumns) = s ^. env ^. port ^. positions
    headerColumnData =
        [ withAttr n $ padLeftRight 1 $ str $ if i == 0 then " " else show i
        | i <- [0..numberOfRows]
        , let isFocused = s^.focusRing'.focus._1 == i
        , let n = "headerCell" <> if isFocused then "focused" else mempty
        ]
    headerRowData =
        [ withAttr n $ hCenter $ str $ show j
        | j <- [1..numberOfColumns]
        , let isFocused = s^.focusRing'.focus._2 == j
        , let n = "headerCell" <> if isFocused then "focused" else mempty

    rowData = toList' $ s ^. widgets & mapped %~
        padLeftRight 1 . withFocusRing (s^.focusRing') (renderEditor $ str . head)

chooseCursor :: State
             -> [CursorLocation (Int, Int)]
             -> Maybe (CursorLocation (Int, Int))
chooseCursor s = if s^._3 then focusRingCursor (view _2) s else const Nothing

--Insert result of eval, except for the one in focus.
updateEditors :: State -> State
updateEditors state = state' {
        widgets = show' state' dir
    }
  where
    pos          = currentPosition (focus state)
    insertedText = getEditContents (widgets state ! pos)
    parsed       = P.parse expression "" (concat insertedText)
    newEnv       = case parsed of
        Left err -> env state
        Right expr -> insertAndEvalGraph pos expr (env state)
    state' = state {env = newEnv}
    env'@Env{formulas = form', view = view'} = env state'

handleEvent :: State
            -> BrickEvent (Int, Int) e
            -> EventM (Int, Int) (Next State)
handleEvent s (VtyEvent e@(EvKey key [])) = if s^. isEditing then handler1 else handler2
  where
    handler1 = case key of
        KEsc     -> continue $ (updateEditors s) & isEditing %~ not &
        KEnter   -> handleEvent (updateEditors s & isEditing %~ not) (VtyEvent $ EvKey KDown [])
        _        -> do
            ed <- handleEditorEvent e $ (s ^. widgets) ! (s^. focusRing' .focus)
            continue $ s & widgets %~ (// [(s^.focusRing'.focus, ed)])
    handler2 = continue $ case key of
        KEnter   -> s & isEditing %~ not
        KLeft    -> s & focusRing' . focus . _2 -~ 1
        KRight   -> s & focusRing' . focus . _2 +~ 1
        KUp      -> s & focusRing' . focus . _1 -~ 1
        KDown    -> s & focusRing' . focus . _1 +~ 1
        _        -> s
handleEvent s _ = continue s

attrMap' :: State -> AttrMap
attrMap' = const $ attrMap defAttr
    [ ("headerCell" <> "focused", bg brightBlack)
    , (editFocusedAttr, bg blue)
    ]
