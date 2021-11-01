module Main where

import System.IO (readFile)
import System.Environment (getArgs, getProgName)

import Control.Monad.State

import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Instant.Par
import Instant.ErrM
import Instant.Abs

type StackInfo = (Int, Int)
type JVMState = (M.Map Ident Int, Int)
type JVMMonad = State JVMState

constIdx = [0, 1, 2, 3, 4, 5] -- FIXME!!!!!!!!!!
varIdx = [0, 1, 2, 3] -- FIXME!!!!!!!!!!!!!!!!!

emptyState :: JVMState
emptyState = (M.empty, 1)

getArithmeticExpStackSize :: Exp -> Exp -> StackInfo -> StackInfo
getArithmeticExpStackSize e1 e2 (current, maximal) = do
    let (current1, maximal1) = getExpStackSize e1 (current, maximal)
    let newMax = max maximal maximal1
    let (current2, maximal2) = getExpStackSize e2 (current1, newMax)
    (current2 - 1, max newMax maximal2)

getConstExpStackSize :: StackInfo -> StackInfo
getConstExpStackSize (current, maximal) = do
    let newStackSize = current + 1
    (newStackSize, max newStackSize maximal)

getExpStackSize :: Exp -> StackInfo -> StackInfo
-- getExpStackSize (ExpAdd e1 e2) = undefined
-- getExpStackSize (ExpSub e1 e2) = undefined
-- getExpStackSize (ExpMul e1 e2) = undefined
-- getExpStackSize (ExpDiv e1 e2) = undefined
getExpStackSize (ExpLit _) = getConstExpStackSize
getExpStackSize (ExpVar _) = getConstExpStackSize

getStmtStackSize :: Stmt -> (Int, Int) -> (Int, Int)
getStmtStackSize (SAss _ e) = getExpStackSize e
getStmtStackSize (SExp e) = getExpStackSize e

getStackLimit :: [Stmt] -> Int
getStackLimit = undefined

compileArithmeticExp :: Exp -> Exp -> String -> JVMMonad String
compileArithmeticExp e1 e2 opCode = do
    result1 <- compileExp e1
    result2 <- compileExp e2
    return $ result1 ++ result2 ++ opCode ++ "\n"

compileExp :: Exp -> JVMMonad String
compileExp (ExpAdd e1 e2) = compileArithmeticExp e2 e1 "iadd"
compileExp (ExpSub e1 e2) = compileArithmeticExp e1 e2 "isub"
compileExp (ExpMul e1 e2) = compileArithmeticExp e1 e2 "imul"
compileExp (ExpDiv e1 e2) = compileArithmeticExp e1 e2 "idiv"
compileExp (ExpLit n) = return $ instr ++ show n ++ "\n"
    where instr
            | n `elem` constIdx = "iconst_"
            | n > 127 = "ldc "
            | otherwise = "bipush "
compileExp (ExpVar id) = do
    (vars, _) <- get
    let varNum = vars M.! id
    let gap = if varNum `elem` varIdx then "_" else " " 
    return $ "iload" ++ gap ++ show varNum ++ "\n"

compileStmt :: Stmt -> JVMMonad String
compileStmt (SAss id e) = do
    (vars, counter) <- get
    let idx = fromMaybe counter $ M.lookup id vars
    when (idx == counter) $ put (M.insert id counter vars, counter + 1)
    result <- compileExp e
    let gap = if idx `elem` varIdx then "_" else " "
    return $ result ++ "istore" ++ gap ++ show idx ++ "\n"
compileStmt (SExp e) = do
    result <- compileExp e
    return $ "getstatic java/lang/System/out Ljava/io/PrintStream;\n" -- FIXME!!!
            ++ result 
            ++ "invokevirtual java/io/PrintStream/println(I)V\n"

compileProgram :: [Stmt] -> JVMMonad String
compileProgram [] = return ""
compileProgram (x:xs) = do
    result1 <- compileStmt x
    result2 <- compileProgram xs
    return $ result1 ++ result2

compile :: Program -> IO String
compile (Prog stmts) = do
    let (compiledCode, _) = runState (compileProgram stmts) emptyState
    fillCode compiledCode <$> getProgName -- FIXME!!!!!!!!!!!!!

fillCode :: String -> String -> String
fillCode code progName = ".class public " ++ progName ++ "\n"
    ++ ".super  java/lang/Object\n"
    ++ ".method public <init>()V\n"
    ++ "aload_0\n"
    ++ "invokespecial java/lang/Object/<init>()V\n"
    ++ "return\n"
    ++ ".end method\n"
    ++ ".method public static main([Ljava/lang/String;)V\n"
    ++ ".limit locals 1000\n" -- FIXME!!!!!!!!!!!!!!!!
    ++ ".limit stack 1000\n" -- FIXME!!!!!!!!!!!!!!!!
    ++ code
    ++ "return\n"
    ++ ".end method\n"

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
