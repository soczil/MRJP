module Main where

import System.IO (readFile)
import System.Environment (getArgs)

import Instant.Par
import Instant.ErrM
import Instant.Abs

compile :: Program -> IO String
compile = undefined 

parseAndCompile :: String -> IO ()
parseAndCompile instantProgram = do
    case pProgram (myLexer instantProgram) of
        Ok prog -> do
            result <- compile prog
            return ()
        Bad msg -> putStrLn msg

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No path to file"
        (x:_) -> do
            instantProgram <- readFile x
            parseAndCompile instantProgram
