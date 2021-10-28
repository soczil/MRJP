module Main where

import System.IO (readFile)
import System.Environment (getArgs)

import Control.Monad.State

import qualified Data.Map as M

import Instant.Par
import Instant.ErrM
import Instant.Abs

type JVMState = (M.Map Ident Int, Int)
type JVMMonad = State JVMState

emptyState :: JVMState
emptyState = (M.empty, 1)

compileExp :: Exp -> JVMMonad String
compileExp (ExpAdd e1 e2) = undefined
compileExp (ExpSub e1 e2) = undefined
compileExp (ExpMul e1 e2) = undefined
compileExp (ExpDiv e1 e2) = undefined
compileExp (ExpLit const) = undefined
compileExp (ExpVar id) = undefined

compileStmt :: Stmt -> JVMMonad String
compileStmt (SAss id e) = undefined
compileStmt (SExp e) = compileExp e

compileProgram :: [Stmt] -> JVMMonad String
compileProgram = undefined

compile :: Program -> IO String
compile (Prog stmts) = do
    let runS = runState (compileProgram stmts) emptyState
    return "a"

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
