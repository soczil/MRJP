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
type CMPState = (CMPEnv, Int, Int, String)
type CMPMonad = StateT CMPState CMPExcept
type ExprRet = (String, String, Type)

emptyState :: CMPState
emptyState = (M.empty, 1, 1, "")

getFreeRegister :: CMPMonad String
getFreeRegister = do
    (vars, regCounter, strCounter, globals) <- get
    put (vars, regCounter + 1, strCounter, globals)
    return $ "%" ++ show regCounter

varToEnv :: Ident -> Type -> CMPMonad ()
varToEnv id t = do
    (env, regCounter, strCounter, globals) <- get
    put (M.insert id t env, regCounter, strCounter, globals)

getVarType :: Ident -> CMPMonad Type
getVarType id = do
    (vars, _, _, _) <- get
    return $ vars M.! id

toLLVMType :: Type -> String
toLLVMType (Int _) = "i32"
toLLVMType (Str _) = "i8*"
toLLVMType (Bool _) = "i1"
toLLVMType (Void _) = "void"

loadInstr :: String -> Type -> Ident -> String
loadInstr reg t (Ident id) = do
    let llvmType = toLLVMType t
    printf "%s = load %s, %s* %%%s\n" reg llvmType llvmType id

allocInstr :: Ident -> Type -> String
allocInstr (Ident id) t = printf "%%%s = alloca %s\n" id $ toLLVMType t

storeInstr :: String -> Ident -> Type -> String
storeInstr reg (Ident id) t = do
    let llvmType = toLLVMType t
    printf "store %s %s, %s* %%%s\n" llvmType reg llvmType id

-- TODO: wymyslic lepsza nazwe
basicInstr :: String -> Type -> String -> String -> String -> String
basicInstr reg t opCode = 
    printf "%s = %s %s %s, %s\n" reg opCode (toLLVMType t)

compileBlock :: Block -> CMPMonad String
compileBlock (Block _ stmts) = do
    result <- mapM compileStmt stmts
    return $ concat result

getDefaultValueExpr :: Type -> Expr
getDefaultValueExpr (Int _) = ELitInt BNFC'NoPosition 0
getDefaultValueExpr (Str _) = EString BNFC'NoPosition ""
getDefaultValueExpr (Bool _) = ELitFalse BNFC'NoPosition

compileItem :: Type -> Item -> CMPMonad String
compileItem t (NoInit p id) = compileItem t (Init p id (getDefaultValueExpr t))
compileItem t (Init _ id e) = do
    (result, spot, _) <- compileExpr e
    varToEnv id t
    return $ result ++ allocInstr id t ++ storeInstr spot id t

compileIncrDecrStmt :: Ident -> AddOp -> CMPMonad String
compileIncrDecrStmt id op = do
    let p = BNFC'NoPosition
    compileStmt (Ass p id (EAdd p (EVar p id) op (ELitInt p 1)))

compileStmt :: Stmt -> CMPMonad String
compileStmt (Empty _) = return ""
compileStmt (BStmt _ block) = compileBlock block
compileStmt (Decl _ t itms) = do
    result <- mapM (compileItem t) itms
    return $ concat result
compileStmt (Ass _ id e) = do
    (result, spot, t) <- compileExpr e
    return $ result ++ storeInstr spot id t
compileStmt (Incr _ id) = compileIncrDecrStmt id $ Plus BNFC'NoPosition
compileStmt (Decr _ id) = compileIncrDecrStmt id $ Minus BNFC'NoPosition
compileStmt (Ret _ e) = do
    (result, reg, t) <- compileExpr e
    return $ result ++ printf "ret %s %s\n" (toLLVMType t) reg
compileStmt (VRet _) = return "ret void\n"
compileStmt (Cond p e stmt) = undefined
compileStmt (CondElse p e stmt1 stmt2) = undefined
compileStmt (While p e stmt) = undefined
compileStmt (SExp _ e) = do
    (result, _, _) <- compileExpr e
    return result

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

getAppPrefix :: Type -> CMPMonad (String, String)
getAppPrefix (Void _) = return ("", "")
getAppPrefix _ = do
    reg <- getFreeRegister
    return (printf "%s = " reg, reg)

-- TODO: zamienic (String, String) na cos bardziej sensownego (czytelnego) (w calym kodzie!!!)
compileExpr :: Expr -> CMPMonad ExprRet
compileExpr (EVar _ id) = do
    t <- getVarType id
    reg <- getFreeRegister
    return (loadInstr reg t id, reg, t)
compileExpr (ELitInt _ n) = return ("", show n, Int BNFC'NoPosition)
compileExpr (ELitTrue _) = return ("", "true", Bool BNFC'NoPosition)
compileExpr (ELitFalse _) = return ("", "false", Bool BNFC'NoPosition)
compileExpr (EApp _ (Ident id) exprs) = do
    t <- getVarType (Ident id)
    compiledExprs <- mapM compileExpr exprs
    let result = concatMap (\(result, _, _) -> result) compiledExprs
    let args = intercalate ", " $ foldr (\(_, reg, t) acc -> 
            (toLLVMType t ++ " " ++ reg):acc) [] compiledExprs
    (prefix, reg) <- getAppPrefix t
    let instr = prefix ++ printf "call %s @%s(%s)\n" (toLLVMType t) id args
    return (result ++ instr, reg, t)
compileExpr (EString p s) = do
    (env, regCounter, strCounter, globals) <- get
    let strLen = length s + 1
    let newGlobals = globals 
            ++ printf "@.str%d = private constant [%d x i8] c\"%s\00\"\n" 
            strCounter strLen s
    put (env, regCounter, strCounter + 1, newGlobals)
    reg <- getFreeRegister
    return (
        printf "%s = bitcast [%d x i8]* @.str%d to i8*\n" reg strLen strCounter,
        reg, 
        Str BNFC'NoPosition
        )
compileExpr (Neg _ e) = do
    let newExpr = ELitInt BNFC'NoPosition 0
    let opCode = getAddOpCode $ Minus BNFC'NoPosition
    compileArithmeticExpr opCode newExpr e Nothing
compileExpr (Not _ e) = do
    let newExpr = ELitFalse BNFC'NoPosition
    let opCode = getRelOpCode $ EQU BNFC'NoPosition
    compileArithmeticExpr opCode newExpr e Nothing
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

funToEnv :: TopDef -> CMPMonad ()
funToEnv (FnDef _ t id _ _) = when (id /= Ident "main") $ varToEnv id t

funArgsToEnv :: [Arg] -> CMPMonad ()
funArgsToEnv = mapM_ (\(Arg _ t id) -> varToEnv id t)

compileTopFun :: TopDef -> CMPMonad String
compileTopFun (FnDef _ t (Ident id) args block) = do
    compiledCode <- compileBlock block
    let compiledArgs = intercalate ", " $ map compileArg args
    return $ printf "define %s @%s(%s) {\n%s}\n\n"
        (toLLVMType t) id compiledArgs compiledCode

compileEveryTopFun :: [TopDef] -> CMPMonad String
compileEveryTopFun fundefs = do
    mapM_ funToEnv fundefs
    result <- mapM compileTopFun fundefs
    return $ concat result

compile :: Program -> IO String
compile (Program _ fundefs) = do
    let runS = runStateT (compileEveryTopFun fundefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return ""
        Right (compiledCode, (_, _, _, globals)) -> 
            return $ globals ++ compiledCode
