module Compiler (compile) where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as M

import Text.Printf

import Latte.Abs

import Errors

type CMPEnv = M.Map Ident Type
type CMPExcept = ExceptT CMPError IO
type CMPState = (CMPEnv, Int)
type CMPMonad = StateT CMPState CMPExcept

emptyState :: CMPState
emptyState = undefined

getFreeRegister :: CMPMonad String
getFreeRegister = do
    (vars, counter) <- get
    put (vars, counter + 1)
    return $ "%" ++ show counter

getVarType :: Ident -> CMPMonad Type
getVarType id = do
    (vars, _) <- get
    return $ vars M.! id

toLLVMType :: Type -> String
toLLVMType (Int _) = "i32"
toLLVMType (Str _) = "i8*"
toLLVMType (Bool _) = "i1"
toLLVMType (Void _) = "void"

loadInstr :: String -> Type -> Ident -> String
loadInstr reg t (Ident id) = do
    let llvmType = toLLVMType t
    return printf "%s = load %s, %s* \\%%s\n" reg llvmType llvmType id

arithmeticInstr :: String -> Type -> String -> String -> String -> String
arithmeticInstr reg t opCode = 
    printf "%s = %s %s %s, %s\n" reg opCode (toLLVMType t)

compileBlock :: Block -> CMPMonad ()
compileBlock (Block _ stmts) = mapM_ compileStmt stmts

compileStmt :: Stmt -> CMPMonad ()
compileStmt (Empty _) = return ()
compileStmt (BStmt _ block) = compileBlock block
compileStmt (Decl p t itms) = undefined
compileStmt (Ass p id e) = undefined
compileStmt (Incr p id) = undefined
compileStmt (Decr p id) = undefined
compileStmt (Ret p e) = undefined
compileStmt (VRet p) = undefined
compileStmt (Cond p e stmt) = undefined
compileStmt (CondElse p e stmt1 stmt2) = undefined
compileStmt (While p e stmt) = undefined
compileStmt (SExp p e) = undefined

compileAddOpExpr :: AddOp -> Type -> String -> String -> CMPMonad (String, String, Type)
compileAddOpExpr (Plus _) t s1 s2 = do
    reg <- getFreeRegister
    let instr = arithmeticInstr reg t "add" s1 s2
    return (instr, reg, t)

-- TODO: zamienic (String, String) na cos bardziej sensownego (czytelnego) (w calym kodzie!!!)
compileExpr :: Expr -> CMPMonad (String, String, Type)
compileExpr (EVar _ id) = do
    t <- getVarType id
    reg <- getFreeRegister
    return (loadInstr reg t id, reg, t)
compileExpr (ELitInt _ n) = return ("", show n, Int BNFC'NoPosition)
compileExpr (ELitTrue p) = return ("", "true", Bool BNFC'NoPosition)
compileExpr (ELitFalse p) = return ("", "false", Bool BNFC'NoPosition)
compileExpr (EApp p id exprs) = undefined
compileExpr (EString p s) = undefined
compileExpr (Neg p e) = undefined
compileExpr (Not p e) = undefined
compileExpr (EMul p e1 op e2) = undefined
compileExpr (EAdd p e1 op e2) = do
    (res1, spot1, t) <- compileExpr e1
    (res2, spot2, _) <- compileExpr e2
    (result, reg, _) <- compileAddOpExpr op t spot1 spot2
    return ("", "", Bool BNFC'NoPosition)
compileExpr (ERel p e1 op e2) = undefined
compileExpr (EAnd p e1 e2) = undefined
compileExpr (EOr p e1 e2) = undefined

compileEveryTopFun :: [TopDef] -> CMPMonad ()
compileEveryTopFun = undefined

compile :: Program -> IO ()
compile (Program _ fundefs) = do
    let runS = runStateT (compileEveryTopFun fundefs) emptyState
    result <- runExceptT runS
    return ()
