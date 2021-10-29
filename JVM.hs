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

constIdx = [0, 1, 2, 3, 4, 5]

emptyState :: JVMState
emptyState = (M.empty, 1)

compileExp :: Exp -> JVMMonad String
compileExp (ExpAdd e1 e2) = undefined
compileExp (ExpSub e1 e2) = undefined
compileExp (ExpMul e1 e2) = undefined
compileExp (ExpDiv e1 e2) = undefined
compileExp (ExpLit n) = return $ "iconst" ++ gap ++ show n ++ "\n"
    where gap = if n `elem` constIdx then "_" else " "
compileExp (ExpVar id) = undefined

compileStmt :: Stmt -> JVMMonad String
compileStmt (SAss id e) = undefined
compileStmt (SExp e) = compileExp e

compileProgram :: [Stmt] -> JVMMonad String
compileProgram [] = return ""
compileProgram (x:xs) = do
    result1 <- compileStmt x
    result2 <- compileProgram xs
    return $ result1 ++ result2

compile :: Program -> IO String
compile (Prog stmts) = do
    let (compiledCode, _) = runState (compileProgram stmts) emptyState
    return compiledCode

parseAndCompile :: String -> IO ()
parseAndCompile instantProgram = do
    case pProgram (myLexer instantProgram) of
        Ok prog -> do
            result <- compile prog
            putStr result
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
