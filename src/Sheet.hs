{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Maybe
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Monoid ((<>))
import Data.NumInstances.Tuple
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
import Print

import Debug.Trace


data Dir =  N | S | W | E | None
type Cell = Editor String Position

data State = State {
        _focusRing' :: FocusRing Position,
        _widgets :: Array Position Cell,
        _env :: Env,
        _isEditing :: Bool
    }

makeLenses ''State

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
        _focusRing' = focusRing $ Data.Array.indices editors',
        _widgets = editors',
        _isEditing = False,
        _env = initializeGraph $ eval $ initial viewport
    }
    where
        editors' = editors viewport
        viewport = ViewPort {
            _size = (11, 6),
            _position = (0, 0)
        }

editors :: ViewPort -> Array Position Cell
editors vp = array ((0, 0), (rows, cols))
    [ ((i, j), editor (i, j) (Just 1) "")
    | i <- [0..rows], j <- [0..cols]
    ]
    where
        (rows, cols) = vp ^.size - (1, 1)

app :: App State e (Int, Int)
app = App
    { appDraw = draw
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return . refreshWindow
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


refreshWidget :: (Position -> a -> String) -> a -> Position -> Cell -> Cell
refreshWidget strF data' coord = swapEditorContents (strF coord data')

swapEditorContents :: String -> Cell -> Cell
swapEditorContents xs = applyEdit ((`insertString` xs) . clearZipper)

insertString :: TextZipper String -> String -> TextZipper String
insertString = foldl' (flip insertChar)

-- Determine if the given position is in focus if moving in the
-- specified direction
inFocus :: Position -> Dir -> Position -> Bool
inFocus current dir check = current + move dir == check

-- Get the widget at the given position
getWidget :: State -> Position -> Cell
getWidget state pos = (state ^. widgets) ! pos

move :: Dir -> (Int, Int)
move W    = ( 0,-1)
move E    = ( 0, 1)
move N    = (-1, 0)
move S    = ( 1, 0)
move None = ( 0, 0)

show' :: State -> Array Position Cell
show' state
    = (state ^. widgets) //
        [((i,j),
            if pos == (i, j) && state ^. isEditing
                then refreshWidget printF (state ^. env . formulas) (i, j) (gW (i, j))
                else refreshWidget printV (state ^. env . view) (i, j) (gW (i, j)))
        | i <- [0..rows - 1], j <- [0..cols - 1]]
    where
    gW = getWidget state
    (rows, cols) = state ^. env . port . size
    pos = state ^. focusRing' . focus

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
        padLeftRight 1 . withFocusRing (s ^. focusRing') (renderEditor $ str . head)

chooseCursor :: State
             -> [CursorLocation (Int, Int)]
             -> Maybe (CursorLocation (Int, Int))
chooseCursor s = if s ^. isEditing then focusRingCursor (^. focusRing') s else const Nothing

--Insert result of eval, except for the one in focus.
updateEditors :: State -> State
updateEditors state = state' & widgets .~ show' state'
  where
    pos          = state ^. focusRing' . focus
    insertedText = getEditContents ((state ^. widgets) ! pos)
    parsed       = P.parse expression "" (concat insertedText)
    newEnv       = case parsed of
        Left err   -> state ^. env
        Right expr -> insertAndEvalGraph pos expr (state ^. env)
    state' = state & env .~ newEnv
    env'   = state' ^. env

--Insert result of eval, except for the one in focus.
refreshWindow :: State -> State
refreshWindow state = state & widgets .~ show' state

handleEvent :: State
            -> BrickEvent (Int, Int) e
            -> EventM (Int, Int) (Next State)
handleEvent s (VtyEvent e@(EvKey key []))
    = if s ^. isEditing
        then handler1
        else handler2
  where
    handler1 = case key of
        KChar 'q'  -> halt s
        -- KEsc     -> continue $ updateEditors (s & isEditing %~ not)
        KEnter   -> handleEvent (updateEditors (s & isEditing %~ not)) (VtyEvent $ EvKey KDown [])
        _        -> do
            ed <- handleEditorEvent e $ (s ^. widgets) ! (s^. focusRing' .focus)
            continue $ s & widgets %~ (// [(s^.focusRing'.focus, ed)])
    handler2 = case key of
        KChar c  -> if c == 'q' then halt s else continue s
        KEnter   -> continue $ refreshWindow (s & isEditing %~ not)
        KLeft    -> continue $ s & focusRing' . focus . _2 -~ 1
        KRight   -> continue $ s & focusRing' . focus . _2 +~ 1
        KUp      -> continue $ s & focusRing' . focus . _1 -~ 1
        KDown    -> continue $ s & focusRing' . focus . _1 +~ 1
        _        -> continue s
handleEvent s _ = continue s

attrMap' :: State -> AttrMap
attrMap' = const $ attrMap defAttr
    [ ("headerCell" <> "focused", bg brightBlack)
    , (editFocusedAttr, bg blue)
    ]
