{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Lib
import Data
import Control.Monad
import Control.Monad.IO.Class
import System.Console.StructuredCLI
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.Default (def)


main :: IO ()
main = do
    -- let initial = initialState
    let initial = env3
    evalStateT run initial
        where run = do
                result <- runCLI "some CLI" def root
                either (error.show) return result

type Excel a = CommandsT (StateT Env IO) a

root :: Excel ()
root = do
    pView
    pForm

pView :: Excel ()
pView = command "eval" "Evaluate the current sheet" $ do
    state <- get
    liftIO . printView $ state
    return NoAction

pForm :: Excel ()
pForm = command "print" "Print the current sheet contents" $ do
    state <- get
    liftIO . printForms $ state
    return NoAction

initialState :: Env
initialState = Env {
        view = M.empty,
        formulas = M.empty,
        port = ViewPort { size = (10, 10), position = (0, 0) }
    }