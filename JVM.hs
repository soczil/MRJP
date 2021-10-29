module Main where

import System.IO (readFile)
import System.Environment (getArgs, getProgName)

import Control.Monad.State

import qualified Data.Map as M

import Instant.Par
import Instant.ErrM
import Instant.Abs

type JVMState = (M.Map Ident Int, Int)
type JVMMonad = State JVMState

constIdx = [0, 1, 2, 3, 4, 5]
varIdx = [0, 1, 2, 3]

emptyState :: JVMState
emptyState = (M.empty, 1)

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
compileStmt (SAss id e) = do -- FIXME!!!!!!!!!!!!!
    (vars, counter) <- get
    put (M.insert id counter vars, counter + 1)
    result <- compileExp e
    let gap = if counter `elem` varIdx then "_" else " "
    return $ result ++ "istore" ++ gap ++ show counter ++ "\n"
compileStmt (SExp e) = do
    result <- compileExp e
    return $ result ++ "invokevirtual java/io/PrintStream/println(I)V\n"

compileProgram :: [Stmt] -> JVMMonad String
compileProgram [] = return ""
compileProgram (x:xs) = do
    result1 <- compileStmt x
    result2 <- compileProgram xs
    return $ result1 ++ result2

compile :: Program -> IO String
compile (Prog stmts) = do
    let (compiledCode, _) = runState (compileProgram stmts) emptyState
    fillCode compiledCode <$> getProgName

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
    ++ "getstatic java/lang/System/out Ljava/io/PrintStream;\n"
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
