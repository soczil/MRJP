module InscJVM where

import System.IO (readFile, writeFile, hPutStrLn, stderr)
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeBaseName, takeDirectory)
import System.Process (system)
import System.Exit (ExitCode(ExitSuccess))

import Control.Monad.State

import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Instant.Par
import Instant.ErrM
import Instant.Abs

type StackInfo = (Int, Int)
type JVMState = (M.Map Ident Int, Int)
type JVMMonad = State JVMState

emptyState :: JVMState
emptyState = (M.empty, 1)

getArithmeticExpStackSize :: Exp -> Exp -> StackInfo -> StackInfo
getArithmeticExpStackSize e1 e2 (current, maxSize) = do
    let (current1, maxSize1) = getExpStackSize e1 (current, maxSize)
    let newMaxSize = max maxSize maxSize1
    let (current2, maxSize2) = getExpStackSize e2 (current1, newMaxSize)
    (current2 - 1, max newMaxSize maxSize2)

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
getStackLimit [] = 0
getStackLimit stmts = 
    maximum (foldl (\acc (_, maxSize) -> acc ++ [maxSize]) [] (map getStmtStackSize stmts))

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
            | n <= 5 = "iconst_"
            | n <= 127 = "bipush "
            | n <= 32767 = "sipush "
            | otherwise = "ldc "
compileExp (ExpVar id) = do
    (vars, _) <- get
    let varNum = vars M.! id
    let gap = if varNum <= 3 then "_" else " " 
    return $ "iload" ++ gap ++ show varNum ++ "\n"

compileStmt :: Stmt -> JVMMonad String
compileStmt (SAss id e) = do
    (vars, counter) <- get
    let idx = fromMaybe counter $ M.lookup id vars
    when (idx == counter) $ put (M.insert id counter vars, counter + 1)
    result <- compileExp e
    let gap = if idx <= 3 then "_" else " "
    return $ result ++ "istore" ++ gap ++ show idx ++ "\n"
compileStmt (SExp e) = do
    result <- compileExp e
    return $ result
            ++ "getstatic java/lang/System/out Ljava/io/PrintStream;\n"
            ++ "swap\n" 
            ++ "invokevirtual java/io/PrintStream/println(I)V\n"

compileProgram :: [Stmt] -> JVMMonad String
compileProgram stmts = do
    result <- mapM compileStmt stmts
    return $ concat result

compile :: Program -> String -> IO String
compile (Prog stmts) className = do
    let stackLimit = getStackLimit stmts
    let (compiledCode, (vars, _)) = runState (compileProgram stmts) emptyState
    let localsLimit = M.size vars + 1
    return $ completeCode compiledCode localsLimit stackLimit className

completeCode :: String -> Int -> Int -> String -> String
completeCode code localsLimit stackLimit progName = 
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

runCompiler :: String -> IO ()
runCompiler filePath = do
    instantProgram <- readFile filePath
    case pProgram (myLexer instantProgram) of
        Ok prog -> do
            result <- compile prog $ takeBaseName filePath
            let jasminFilePath = dropExtension filePath ++ ".j"
            writeFile jasminFilePath result
            ExitSuccess <- system $ 
                "java -jar lib/jasmin.jar -d " 
                ++ takeDirectory filePath 
                ++ " " ++ jasminFilePath
            return ()
        Bad msg -> hPutStrLn stderr $ "Error: " ++ msg

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Error: enter the path to the input file"
        (file:_) -> runCompiler file
