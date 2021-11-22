module Main where

import System.IO
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)

import Latte.Par
import Latte.ErrM

import Typechecker

finishTypechecker :: (String, Bool) -> IO ()
finishTypechecker (_, False) = return ()
finishTypechecker (msg, True) = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1

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
