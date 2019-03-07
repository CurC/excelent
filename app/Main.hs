module Main where

import Lib
import Data

main :: IO ()
main = someFunc

type ProgramState = State Env
