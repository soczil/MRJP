module LLVM where

import System.IO (readFile, writeFile, hPutStrLn, stderr)
import System.Environment (getArgs)

import Control.Monad.State

import qualified Data.Set as S

import Instant.Par
import Instant.ErrM
import Instant.Abs

type LLVMState = (S.Set Ident, Int)
type LLVMMonad = State LLVMState

emptyState :: LLVMState
emptyState = (S.empty, 1)

getFreeRegister :: LLVMMonad String
getFreeRegister = do
    (vars, counter) <- get
    put (vars, counter + 1)
    return $ "%t" ++ show counter

register :: Int -> String
register n = "%t" ++ show n

loadInstr :: String -> Ident -> String
loadInstr register (Ident id) = register ++ " = load i32, i32* %" ++ id ++ "\n"

arithmeticInstr :: String -> String -> String -> String -> String
arithmeticInstr register spot1 spot2 opCode = 
    register ++ " = " ++ opCode ++ " i32 " ++ spot1 ++ ", " ++ spot2 ++ "\n"

compileArithmeticExp :: Exp -> Exp -> String -> LLVMMonad (String, String)
compileArithmeticExp e1 e2 opCode = do
    (result1, spot1) <- compileExp e1
    (result2, spot2) <- compileExp e2
    register <- getFreeRegister
    let instr = arithmeticInstr register spot1 spot2 opCode
    return (result1 ++ result2 ++ instr, register)

compileExp :: Exp -> LLVMMonad (String, String)
compileExp (ExpAdd e1 e2) = compileArithmeticExp e1 e2 "add"
compileExp (ExpSub e1 e2) = compileArithmeticExp e1 e2 "sub"
compileExp (ExpMul e1 e2) = compileArithmeticExp e1 e2 "mul"
compileExp (ExpDiv e1 e2) = compileArithmeticExp e1 e2 "udiv"
compileExp (ExpLit n) = return ("", show n)
compileExp (ExpVar id) = do
    register <- getFreeRegister
    return (loadInstr register id, register)

compileStmt :: Stmt -> LLVMMonad String
compileStmt (SAss id e) = undefined
compileStmt (SExp e) = do
    (result, _) <- compileExp e
    return result

compileProgram :: [Stmt] -> LLVMMonad String
compileProgram stmts = do
    result <- mapM compileStmt stmts
    return $ concat result

compile :: Program -> IO String
compile (Prog stmts) = do
    let (compiledCode, _) = runState (compileProgram stmts) emptyState
    return compiledCode

runCompiler :: String -> IO ()
runCompiler filePath = do
    instantProgram <- readFile filePath
    case pProgram (myLexer instantProgram) of
        Ok prog -> do
            result <- compile prog
            putStr result
        Bad msg -> hPutStrLn stderr $ "Error: " ++ msg

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Error: enter the path to the input file"
        (file:_) -> runCompiler file

