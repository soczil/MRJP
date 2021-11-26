module Main where

import System.IO
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith, exitFailure)

import Control.Monad (when)

import Latte.Par
import Latte.ErrM

import Typechecker

finishTypechecker :: (String, Bool) -> IO ()
finishTypechecker (msg, error) = do
    hPutStrLn stderr msg
    when error $ exitWith $ ExitFailure 1

runCompiler :: String -> IO ()
runCompiler filePath = do
    latteProgram <- readFile filePath
    case pProgram (myLexer latteProgram) of
        Ok prog -> do
            result <- check prog
            finishTypechecker result
        Bad msg -> hPutStrLn stderr msg

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Error: no input file"
        (filePath:_) -> runCompiler filePath
