module LLVM where

import System.IO (readFile, writeFile, hPutStrLn, stderr)
import System.Environment (getArgs)

import qualified Data.Set as S

import Instant.Par
import Instant.ErrM
import Instant.Abs

type LLVMMonad = (S.Set Ident, Int)

compile :: Program -> String -> IO String
compile = undefined

runCompiler :: String -> IO ()
runCompiler filePath = do
    instantProgram <- readFile filePath
    case pProgram (myLexer instantProgram) of
        Ok prog -> do
            return ()
        Bad msg -> hPutStrLn stderr $ "Error: " ++ msg

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Error: enter the path to the input file"
        (file:_) -> runCompiler file

