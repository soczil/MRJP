module Main where

import System.IO
import System.Environment (getArgs)
import System.Process (system)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith, exitFailure)
import System.FilePath

import Control.Monad (when)

import Text.Printf

import Latte.Par
import Latte.ErrM

import Typechecker (check)
import Compiler (compile)

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
            compiledCode <- compile prog
            let llFilePath = dropExtension filePath ++ ".ll"
            let bcFilePath = dropExtension filePath ++ ".bc"
            let runtimeFilePath = "../lib/runtime.bc"
            let fooFilePath = dropExtension filePath ++ "foo.bc"
            writeFile llFilePath compiledCode
            ExitSuccess <- 
                system $ printf "llvm-as %s -o %s" llFilePath fooFilePath
            ExitSuccess <- 
                system $ printf "llvm-link %s %s -o %s" 
                fooFilePath runtimeFilePath bcFilePath
            ExitSuccess <- system $ printf "rm -f %s" fooFilePath
            putStr compiledCode
        Bad msg -> do
            hPutStrLn stderr $ "ERROR\n" ++ msg
            exitWith $ ExitFailure 1

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Error: no input file"
        (filePath:_) -> runCompiler filePath
