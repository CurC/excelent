module Sheet where

import Data.Array
import Data.List
import Data.Maybe
import Data.Monoid ((<>))
import Data.NumInstances.Tuple
import Data.Text.Zipper
import qualified Data.Map as M
import qualified Text.Parsec as P

import Brick.AttrMap
import Brick.Focus
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Graphics.Vty

import Excelent.Definition
import Excelent.Eval.Eval
import Excelent.Eval.Graph
import Excelent.Parser
import Print

data State = State {
        focus :: FocusRing Position,
        widgets :: Array Position (Editor String Position),
        env :: Env
    }

data Dir =  N | S | W | E | None

divideIntoGroupsOf :: Int -> [a] -> [[a]]
divideIntoGroupsOf n [] = [[]]
divideIntoGroupsOf n xs =
    let (xs1, xs2) = splitAt n xs in xs1 : divideIntoGroupsOf n xs2

-------------------------------------------------------------------------------
--
-- TODO : Viewport movement
-- TODO : Fix Viewport boundaries moving left when at zero should do nothing
-- TODO : Fix recalculation based on graph (is being done but does not
--        work for some reason)
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
        env = initializeGraph $ eval $ initial viewport
    }
    where
        editors' = editors viewport
        viewport = ViewPort {
            size = (10, 6),
            position = (0, 0)
        }

editors :: ViewPort -> Array Position (Editor String Position)
editors vp = array ((0, 0), (rows, cols))
    [ ((i, j), editor (i, j) (Just 1) "")
    | i <- [0..rows], j <- [0..cols]
    ]
    where
        (rows, cols) = size vp - (1, 1)

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
    = [vBox $ hBox <$> divideIntoGroupsOf cols ws]
  where
    (rows, cols) = size p
    r = focus state'
    eds = widgets state'
    ws = map border $ elems $ withFocusRing r (renderEditor $ str . head) <$> eds
    Env{formulas = f, view = v, port = p} = env state'

show' :: State -> Dir -> Array Position (Editor String Position)
show' state@State {widgets = w, env = e} d
    = w //
        [((i,j),
            if not (inFocus pos d (i,j))
                then newWidget printV v (i, j)
                else newWidget printF f (i, j))
        | i <- [0..fst (size p) - 1], j <- [0..snd (size p) - 1]]
    where
    Env{formulas = f, view = v, port = p} = e
    pos = currentPosition (focus state)
    newWidget f data' coord =
        swapEditorContents (f coord data') (getWidget state coord)

swapEditorContents :: String -> Editor String Position -> Editor String Position
swapEditorContents xs = applyEdit ((`insertString` xs) . clearZipper)

insertString :: TextZipper String -> String -> TextZipper String
insertString = foldl' (flip insertChar)

-- Determine if the given position is in focus if moving in the
-- specified direction
inFocus :: Position -> Dir -> Position -> Bool
inFocus current dir check = current + move dir == check

-- Get the current position in focus
currentPosition :: FocusRing Position -> Position
currentPosition foc = fromJust $ focusGetCurrent foc

-- Get the widget at the given position
getWidget :: State -> Position -> Editor String Position
getWidget state pos = widgets state ! pos

move :: Dir -> (Int, Int)
move W    = ( 0,-1)
move E    = ( 0, 1)
move N    = (-1, 0)
move S    = ( 1, 0)
move None = ( 0, 0)

updateFocus :: Dir -> FocusRing Position -> FocusRing Position
updateFocus dir foc = focusSetCurrent (currentPosition foc + move dir) foc

--Insert result of eval, except for the one in focus.
updateEditors :: State -> Dir -> State
updateEditors state dir = state' {
        focus = updateFocus dir (focus state'),
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

handleEvent :: State -> BrickEvent Position e -> EventM Position (Next State)
handleEvent state (VtyEvent e) = case e of
    EvKey KLeft  [] -> continue $ updateEditors state W
    EvKey KRight [] -> continue $ updateEditors state E
    EvKey KUp    [] -> continue $ updateEditors state N
    EvKey KDown  [] -> continue $ updateEditors state S
    EvKey KEnter [] -> continue $ updateEditors state S
    EvKey KEsc   [] -> halt state
    _               -> do
        ed' <- handleEditorEvent e ed
        continue state {widgets = widgets state // [(pos, ed')]}
  where
    ed = widgets state ! pos
    pos = currentPosition (focus state)
handleEvent state _ = continue state
