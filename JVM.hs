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

getArithmeticExpStackSize :: Exp -> Exp -> StackInfo -> StackInfo -- FIXME (names)!!!!!!!!!!!
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
getExpStackSize (ExpAdd e1 e2) = getArithmeticExpStackSize e2 e1
getExpStackSize (ExpSub e1 e2) = getArithmeticExpStackSize e1 e2
getExpStackSize (ExpMul e1 e2) = getArithmeticExpStackSize e1 e2
getExpStackSize (ExpDiv e1 e2) = getArithmeticExpStackSize e1 e2
getExpStackSize (ExpLit _) = getConstExpStackSize
getExpStackSize (ExpVar _) = getConstExpStackSize

getStmtStackSize :: Stmt -> StackInfo
getStmtStackSize (SAss _ e) = getExpStackSize e (0, 0)
getStmtStackSize (SExp e) = getExpStackSize e (0, 2)

getStackLimit :: [Stmt] -> Int
getStackLimit stmts = maximum (foldl (\acc (_, maxSize) -> acc ++ [maxSize]) [] (map getStmtStackSize stmts))

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
    return $ result
            ++ "getstatic java/lang/System/out Ljava/io/PrintStream;\n"
            ++ "swap\n" 
            ++ "invokevirtual java/io/PrintStream/println(I)V\n"

compileProgram :: [Stmt] -> JVMMonad String
compileProgram [] = return ""
compileProgram (x:xs) = do
    result1 <- compileStmt x
    result2 <- compileProgram xs
    return $ result1 ++ result2

compile :: Program -> IO String
compile (Prog stmts) = do
    let stackLimit = getStackLimit stmts
    let (compiledCode, (vars, _)) = runState (compileProgram stmts) emptyState
    let localsLimit = M.size vars + 1
    fillCode compiledCode localsLimit stackLimit <$> getProgName -- FIXME!!!!!!!!!!!!!

fillCode :: String -> Int -> Int -> String -> String -- FIXME (name)!!!!!!!!!!!
fillCode code localsLimit stackLimit progName = 
    ".class public " ++ progName ++ "\n"
    ++ ".super  java/lang/Object\n"
    ++ ".method public <init>()V\n"
    ++ "aload_0\n"
    ++ "invokespecial java/lang/Object/<init>()V\n"
    ++ "return\n"
    ++ ".end method\n"
    ++ ".method public static main([Ljava/lang/String;)V\n"
    ++ ".limit locals " ++ show localsLimit ++ "\n"
    ++ ".limit stack " ++ show stackLimit ++ "\n"
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
