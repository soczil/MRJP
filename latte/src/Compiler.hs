module Compiler (compile) where

import Control.Monad.Except
import Control.Monad.State

import Data.Maybe
import Data.List
import qualified Data.Map as M

import Text.Printf

import Latte.Abs

import Errors

type CMPEnv = M.Map Ident Type
type CMPExcept = ExceptT CMPError IO
type CMPState = (CMPEnv, Int)
type CMPMonad = StateT CMPState CMPExcept
type ExprRet = (String, String, Type)

emptyState :: CMPState
emptyState = (M.empty, 1)

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
    return printf "%s = load %s, %s* %%%s\n" reg llvmType llvmType id

-- TODO: wymyslic lepsza nazwe
basicInstr :: String -> Type -> String -> String -> String -> String
basicInstr reg t opCode = 
    printf "%s = %s %s %s, %s\n" reg opCode (toLLVMType t)

compileBlock :: Block -> CMPMonad String
compileBlock (Block _ stmts) = do
    result <- mapM compileStmt stmts
    return $ concat result

compileStmt :: Stmt -> CMPMonad String
compileStmt (Empty _) = return ""
compileStmt (BStmt _ block) = compileBlock block
compileStmt (Decl p t itms) = undefined
compileStmt (Ass p id e) = undefined
compileStmt (Incr p id) = undefined
compileStmt (Decr p id) = undefined
compileStmt (Ret _ e) = do
    (result, reg, t) <- compileExpr e
    return $ result ++ printf "ret %s %s\n" (toLLVMType t) reg
compileStmt (VRet _) = return "ret void\n"
compileStmt (Cond p e stmt) = undefined
compileStmt (CondElse p e stmt1 stmt2) = undefined
compileStmt (While p e stmt) = undefined
compileStmt (SExp p e) = undefined

getAddOpCode :: AddOp -> String
getAddOpCode (Plus _) = "add"
getAddOpCode _ = "sub"

getMulOpCode :: MulOp -> String
getMulOpCode (Times _) = "mul"
getMulOpCode (Div _) = "sdiv" -- TODO: moze udiv
getMulOpCode _ = "mod" -- TODO: to raczej nie jest mod

getRelOpCodeAux :: RelOp -> String
getRelOpCodeAux (LTH _) = "slt"
getRelOpCodeAux (LE _) = "sle"
getRelOpCodeAux (GTH _) = "sgt"
getRelOpCodeAux (GE _) = "sge"
getRelOpCodeAux (EQU _) = "eq"
getRelOpCodeAux (NE _) = "ne"

getRelOpCode :: RelOp -> String
getRelOpCode op = "icmp " ++ getRelOpCodeAux op

compileArithmeticExpr :: String -> Expr -> Expr -> Maybe Type -> CMPMonad ExprRet
compileArithmeticExpr opCode e1 e2 retType = do
    (res1, spot1, t) <- compileExpr e1
    (res2, spot2, _) <- compileExpr e2
    reg <- getFreeRegister
    let instr = basicInstr reg t opCode spot1 spot2
    let result = res1 ++ res2 ++ instr
    return (result, reg, fromMaybe t retType)

-- TODO: to bedzie dzikie (labelki itp)
compileBoolExpr :: String -> Expr -> Expr -> CMPMonad ExprRet
compileBoolExpr opCode e1 e2 = do
    (res1, spot1, t) <- compileExpr e1
    (res2, spot2, _) <- compileExpr e2
    reg <- getFreeRegister
    let instr = basicInstr reg t opCode spot1 spot2
    let result = res1 ++ res2 ++ instr
    return (result, reg, Bool BNFC'NoPosition)

-- TODO: zamienic (String, String) na cos bardziej sensownego (czytelnego) (w calym kodzie!!!)
compileExpr :: Expr -> CMPMonad ExprRet
compileExpr (EVar _ id) = do
    t <- getVarType id
    reg <- getFreeRegister
    return (loadInstr reg t id, reg, t)
compileExpr (ELitInt _ n) = return ("", show n, Int BNFC'NoPosition)
compileExpr (ELitTrue _) = return ("", "true", Bool BNFC'NoPosition)
compileExpr (ELitFalse _) = return ("", "false", Bool BNFC'NoPosition)
compileExpr (EApp p id exprs) = undefined
compileExpr (EString p s) = undefined
compileExpr (Neg p e) = undefined
compileExpr (Not p e) = undefined
compileExpr (EMul _ e1 op e2) =
    compileArithmeticExpr (getMulOpCode op) e1 e2 Nothing
compileExpr (EAdd _ e1 op e2) =
    compileArithmeticExpr (getAddOpCode op) e1 e2 Nothing -- TODO: optymalizacja?
compileExpr (ERel _ e1 op e2) =
    compileArithmeticExpr (getRelOpCode op) e1 e2 $ Just (Bool BNFC'NoPosition)
compileExpr (EAnd _ e1 e2) = compileBoolExpr "and" e1 e2
compileExpr (EOr _ e1 e2) = compileBoolExpr "or" e1 e2

compileArg :: Arg -> String
compileArg (Arg _ t (Ident id)) = printf "%s %%%s" (toLLVMType t) id

compileTopFun :: TopDef -> CMPMonad String
compileTopFun (FnDef _ t (Ident id) args block) = do
    compiledCode <- compileBlock block
    let compiledArgs = intercalate ", " $ map compileArg args
    return $ printf "define %s @%s(%s) {\n%s}\n\n"
        (toLLVMType t) id compiledArgs compiledCode

compileEveryTopFun :: [TopDef] -> CMPMonad String
compileEveryTopFun fundefs = do
    result <- mapM compileTopFun fundefs
    return $ concat result

compile :: Program -> IO String
compile (Program _ fundefs) = do
    let runS = runStateT (compileEveryTopFun fundefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return ""
        Right (compiledCode, _) -> return compiledCode
