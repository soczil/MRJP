module LLVM where

import System.IO (readFile, writeFile, hPutStrLn, stderr)
import System.Environment (getArgs)

import Control.Monad.State

import qualified Data.Set as S

import Instant.Par
import Instant.ErrM
import Instant.Abs

type LLVMMonad = State (S.Set Ident, Int)

compileExp :: Exp -> LLVMMonad String
compileExp (ExpAdd e1 e2) = undefined
compileExp (ExpSub e1 e2) = undefined
compileExp (ExpMul e1 e2) = undefined
compileExp (ExpDiv e1 e2) = undefined
compileExp (ExpLit id) = undefined
compileExp (ExpVar id) = undefined

compileStmt :: Stmt -> LLVMMonad String
compileStmt (SAss id e) = undefined
compileStmt (SExp e) = undefined

compileProgram :: [Stmt] -> LLVMMonad String
compileProgram stmts = do
    result <- mapM compileStmt stmts
    return $ concat result

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

