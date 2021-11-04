module InscLLVM where

import System.IO (readFile, writeFile, hPutStrLn, stderr)
import System.Environment (getArgs)
import System.FilePath (dropExtension) 
import System.Process (system)
import System.Exit (ExitCode(ExitSuccess))

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

loadInstr :: String -> Ident -> String
loadInstr register (Ident id) = register ++ " = load i32, i32* %" ++ id ++ "\n"

arithmeticInstr :: String -> String -> String -> String -> String
arithmeticInstr register spot1 spot2 opCode = 
    register ++ " = " ++ opCode ++ " i32 " ++ spot1 ++ ", " ++ spot2 ++ "\n"

allocInstr :: Ident -> String
allocInstr (Ident id) = "%" ++ id ++ " = alloca i32\n"

storeInstr :: String -> Ident -> String
storeInstr register (Ident id) =
    "store i32 " ++ register ++ ", i32* %" ++ id ++ "\n"

printInstr :: String -> String
printInstr spot = "call void @printInt(i32 " ++ spot ++ ")\n"

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
compileStmt (SAss id e) = do
    (vars, counter) <- get
    (result, spot) <- compileExp e
    when (S.notMember id vars) $ do
        put (S.insert id vars, counter)
    let resultWithAlloc = if S.notMember id vars
            then result ++ allocInstr id
            else result
    return $ resultWithAlloc ++ storeInstr spot id
compileStmt (SExp e) = do
    (result, spot) <- compileExp e
    return $ result ++ printInstr spot

compileProgram :: [Stmt] -> LLVMMonad String
compileProgram stmts = do
    result <- mapM compileStmt stmts
    return $ concat result

compile :: Program -> IO String
compile (Prog stmts) = do
    let (compiledCode, _) = runState (compileProgram stmts) emptyState
    return $ completeCode compiledCode

completeCode :: String -> String
completeCode code = 
    "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n"
    ++ "declare i32 @printf(i8*, ...)\n"
    ++ "define void @printInt(i32 %x) {\n"
    ++ "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n"
    ++ "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n"
    ++ "ret void\n"
    ++ "}\n"
    ++ "define i32 @main() {\n"
    ++ code
    ++ "ret i32 0\n"
    ++ "}\n"

runCompiler :: String -> IO ()
runCompiler filePath = do
    instantProgram <- readFile filePath
    case pProgram (myLexer instantProgram) of
        Ok prog -> do
            result <- compile prog
            let llFilePath = dropExtension filePath ++ ".ll"
            let bcFilePath = dropExtension filePath ++ ".bc"
            writeFile llFilePath result
            ExitSuccess <- system $ 
                "llvm-as " ++ llFilePath ++ " -o " ++ bcFilePath
            return ()
        Bad msg -> hPutStrLn stderr $ "Error: " ++ msg

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Error: enter the path to the input file"
        (file:_) -> runCompiler file
