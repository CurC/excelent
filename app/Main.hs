module Main where

import Brick
import Definition
import Excelent.Eval.Eval
import Print

data State = State { env :: Env
                   , cursor :: Position }

data Dir = Left | Right | Up | Down

type Name = ()

data Event = Move Dir
           | Insert Char
           | Delete Char

app :: App Env Event Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- Handling events

handleEvent :: Env -> BrickEvent Name Event -> EventM Name (Next Env)
handleEvent = undefined

-- Drawing

drawUI :: Env -> [Widget Name]
drawUI = undefined

theMap :: AttrMap
theMap = undefined

main :: IO ()
main = undefined