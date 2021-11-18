module Main where

import System.IO
import System.Environment (getArgs)

runCompiler :: String -> IO ()
runCompiler filePath = undefined

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Error: no input file"
        (filePath:_) -> runCompiler filePath
