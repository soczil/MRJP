module Compiler (compile) where

-- TODO: czy ja tego wgl potrzebuje????
import Control.Monad.Except
import Control.Monad.State

import Data.Maybe
import Data.List
import qualified Data.Map as M

import Text.Printf

import Latte.Abs

import Errors

type CMPEnv = M.Map Ident CMPInf
type CMPExcept = ExceptT CMPError IO -- TODO: no chyba sie nie przyda
type CMPState = (CMPEnv, Int, Int, Int, String)
type CMPMonad = StateT CMPState CMPExcept
type ExprRet = (String, String, Type)

data CMPInf = VarInf (Type, String)
            | FunInf Type

emptyState :: CMPState
emptyState = (M.empty, 1, 1, 0, "")

getFreeLabel :: CMPMonad String
getFreeLabel = do
    (vars, regCounter, strCounter, lblCounter, globals) <- get
    put (vars, regCounter, strCounter, lblCounter + 1, globals)
    return $ "L" ++ show lblCounter

getFreeRegister :: CMPMonad String
getFreeRegister = do
    (vars, regCounter, strCounter, lblCounter, globals) <- get
    put (vars, regCounter + 1, strCounter, lblCounter, globals)
    return $ "%" ++ show regCounter

varToEnv :: Ident -> Type -> String -> CMPMonad ()
varToEnv id t loc = do
    (env, regCounter, strCounter, lblCounter, globals) <- get
    put (M.insert id (VarInf (t, loc)) env, regCounter, strCounter, lblCounter, globals)

getCMPInf :: Ident -> CMPMonad (Type, String)
getCMPInf id = do
    (vars, _, _, _, _) <- get
    case vars M.! id of
        VarInf inf -> return inf
        FunInf t -> return (t, "")

toLLVMType :: Type -> String
toLLVMType (Int _) = "i32"
toLLVMType (Str _) = "i8*"
toLLVMType (Bool _) = "i1"
toLLVMType (Void _) = "void"

loadInstr :: String -> Type -> String -> String
loadInstr reg t loc = do
    let llvmType = toLLVMType t
    printf "%s = load %s, %s* %s\n" reg llvmType llvmType loc

allocInstr :: String -> Type -> String
allocInstr loc t = printf "%s = alloca %s\n" loc $ toLLVMType t

storeInstr :: String -> String -> Type -> String
storeInstr reg loc t = do
    let llvmType = toLLVMType t
    printf "store %s %s, %s* %s\n" llvmType reg llvmType loc

-- TODO: wymyslic lepsza nazwe
basicInstr :: String -> Type -> String -> String -> String -> String
basicInstr reg t opCode = 
    printf "%s = %s %s %s, %s\n" reg opCode (toLLVMType t)

brInstrC :: String -> String -> String -> String
brInstrC = printf "br i1 %s, label %%%s, label %%%s\n"

brInstrU :: String -> String
brInstrU = printf "br label %%%s\n"

printLabel :: String -> String
printLabel = printf "%s:\n"

callInstr :: String -> Type -> String -> [ExprRet] -> String
callInstr reg t fun args = do
    let compiledArgs = intercalate ", " $ foldr (\(_, reg, argType) acc -> 
            (toLLVMType argType ++ " " ++ reg):acc) [] args
    printf "%s = call %s @%s(%s)\n" reg (toLLVMType t) fun compiledArgs

compileBlockNewEnv :: Block -> CMPMonad String
compileBlockNewEnv block = do
    (oldEnv, _, _, _, _) <- get
    result <- compileBlock block
    (_, regCounter, strCounter, lblCounter, globals) <- get
    put (oldEnv, regCounter, strCounter, lblCounter, globals)
    return result

compileBlock :: Block -> CMPMonad String
compileBlock (Block _ stmts) = do
    result <- mapM compileStmt stmts
    return $ concat result

getDefaultValueExpr :: Type -> Expr
getDefaultValueExpr (Int _) = ELitInt BNFC'NoPosition 0
getDefaultValueExpr (Str _) = EString BNFC'NoPosition ""
getDefaultValueExpr (Bool _) = ELitFalse BNFC'NoPosition

getDefaultReturn :: Type -> CMPMonad String
getDefaultReturn (Int _) = return "ret i32 0\n"
getDefaultReturn (Str p) = compileStmt (Ret p (EString p "")) -- TODO: jeden pusty ziomek lepszy niz to lol
getDefaultReturn (Bool _) = return "ret i1 false\n"
getDefaultReturn (Void _) = return "ret void\n"

compileItem :: Type -> Item -> CMPMonad String
compileItem t (NoInit p id) = compileItem t (Init p id (getDefaultValueExpr t))
compileItem t (Init _ id e) = do
    (result, spot, _) <- compileExpr e
    loc <- getFreeRegister
    varToEnv id t loc
    return $ result ++ allocInstr loc t ++ storeInstr spot loc t

compileIncrDecrStmt :: Ident -> AddOp -> CMPMonad String
compileIncrDecrStmt id op = do
    let p = BNFC'NoPosition
    compileStmt (Ass p id (EAdd p (EVar p id) op (ELitInt p 1)))

compileStmt :: Stmt -> CMPMonad String
compileStmt (Empty _) = return ""
compileStmt (BStmt _ block) = compileBlockNewEnv block
compileStmt (Decl _ t itms) = do
    result <- mapM (compileItem t) itms
    return $ concat result
compileStmt (Ass _ id e) = do
    (_, loc) <- getCMPInf id
    (result, spot, t) <- compileExpr e
    return $ result ++ storeInstr spot loc t
compileStmt (Incr _ id) = compileIncrDecrStmt id $ Plus BNFC'NoPosition
compileStmt (Decr _ id) = compileIncrDecrStmt id $ Minus BNFC'NoPosition
compileStmt (Ret _ e) = do
    (result, reg, t) <- compileExpr e
    _ <- getFreeRegister
    return $ result ++ printf "ret %s %s\n" (toLLVMType t) reg
compileStmt (VRet _) = do
    _ <- getFreeRegister
    return "ret void\n"
compileStmt (Cond _ e stmt) = do
    (result, spot, _) <- compileExpr e
    label <- getFreeLabel
    compiledStmt <- compileStmt stmt
    return $ result
        ++ brInstrC spot (label ++ "then") (label ++ "end")
        ++ printLabel (label ++ "then")
        ++ compiledStmt
        ++ brInstrU (label ++ "end")
        ++ printLabel (label ++ "end")
compileStmt (CondElse _ e stmt1 stmt2) = do
    (result, spot, _) <- compileExpr e
    label <- getFreeLabel
    compiledStmt1 <- compileStmt stmt1
    compiledStmt2 <- compileStmt stmt2
    return $ result
        ++ brInstrC spot (label ++ "then") (label ++ "else")
        ++ printLabel (label ++ "then")
        ++ compiledStmt1
        ++ brInstrU (label ++ "end")
        ++ printLabel (label ++ "else")
        ++ compiledStmt2
        ++ brInstrU (label ++ "end")
        ++ printLabel (label ++ "end")
compileStmt (While p e stmt) = do
    label <- getFreeLabel
    (result, spot, _) <- compileExpr e
    compiledStmt <- compileStmt stmt
    return $ brInstrU (label ++ "cond")
        ++ printLabel (label ++ "cond")
        ++ result
        ++ brInstrC spot (label ++ "body") (label ++ "end")
        ++ printLabel (label ++ "body")
        ++ compiledStmt
        ++ brInstrU (label ++ "cond")
        ++ printLabel (label ++ "end")
compileStmt (SExp _ e) = do
    (result, _, _) <- compileExpr e
    return result

getAddOpCode :: AddOp -> String
getAddOpCode (Plus _) = "add"
getAddOpCode _ = "sub"

getMulOpCode :: MulOp -> String
getMulOpCode (Times _) = "mul"
getMulOpCode (Div _) = "sdiv" -- TODO: moze udiv
getMulOpCode _ = "srem"

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

compileAddExpr :: AddOp -> Expr -> Expr -> CMPMonad ExprRet
compileAddExpr (Minus p) e1 e2 = 
    compileArithmeticExpr "sub" e1 e2 Nothing
compileAddExpr _ e1 e2 = do
    let opCode = "add"
    (res1, spot1, t) <- compileExpr e1
    (res2, spot2, _) <- compileExpr e2
    case t of
        Str _ -> do
            let p = BNFC'NoPosition
            reg1 <- getFreeRegister
            reg2 <- getFreeRegister
            reg3 <- getFreeRegister
            reg4 <- getFreeRegister
            reg5 <- getFreeRegister
            reg6 <- getFreeRegister
            reg7 <- getFreeRegister
            let instr = callInstr reg1 (Int p) "strlen" [("", spot1, t)]
                    ++ callInstr reg2 (Int p) "strlen" [("", spot2, t)]
                    ++ basicInstr reg3 (Int p) opCode reg1 "1"
                    ++ basicInstr reg4 (Int p) opCode reg3 reg2
                    ++ callInstr reg5 t "malloc" [("", reg4, Int p)]
                    ++ callInstr reg6 t "strcpy" [("", reg5, t), ("", spot1, t)]
                    ++ callInstr reg7 t "strcat" [("", reg6, t), ("", spot2, t)]
            return (res1 ++ res2 ++ instr, reg7, t)
        _ -> do
            reg <- getFreeRegister
            let instr = basicInstr reg t opCode spot1 spot2
            let result = res1 ++ res2 ++ instr
            return (result, reg, t)

compileBoolExpr :: Expr -> Expr -> Bool -> CMPMonad ExprRet
compileBoolExpr e1 e2 isAnd = do
    let (ifTrue, ifFalse) = if isAnd then ("next", "end") else ("end", "next")
    loc <- getFreeRegister
    label <- getFreeLabel
    (res1, spot1, t) <- compileExpr e1
    (res2, spot2, _) <- compileExpr e2
    reg <- getFreeRegister
    let code = allocInstr loc t
            ++ res1
            ++ storeInstr spot1 loc t 
            ++ brInstrC spot1 (label ++ ifTrue) (label ++ ifFalse)
            ++ printLabel (label ++ "next")
            ++ res2
            ++ storeInstr spot2 loc t
            ++ brInstrU (label ++ "end")
            ++ printLabel (label ++ "end")
            ++ loadInstr reg t loc
    return (code, reg, t)

getAppPrefix :: Type -> CMPMonad (String, String)
getAppPrefix (Void _) = return ("", "")
getAppPrefix _ = do
    reg <- getFreeRegister
    return (printf "%s = " reg, reg)

compileExpr :: Expr -> CMPMonad ExprRet
compileExpr (EVar _ id) = do
    (t, loc) <- getCMPInf id
    reg <- getFreeRegister
    return (loadInstr reg t loc, reg, t)
compileExpr (ELitInt _ n) = return ("", show n, Int BNFC'NoPosition)
compileExpr (ELitTrue _) = return ("", "true", Bool BNFC'NoPosition)
compileExpr (ELitFalse _) = return ("", "false", Bool BNFC'NoPosition)
compileExpr (EApp _ (Ident id) exprs) = do
    (t, _) <- getCMPInf (Ident id)
    compiledExprs <- mapM compileExpr exprs
    let result = concatMap (\(result, _, _) -> result) compiledExprs
    let args = intercalate ", " $ foldr (\(_, reg, t) acc -> 
            (toLLVMType t ++ " " ++ reg):acc) [] compiledExprs
    (prefix, reg) <- getAppPrefix t
    let instr = prefix ++ printf "call %s @%s(%s)\n" (toLLVMType t) id args
    return (result ++ instr, reg, t)
compileExpr (EString _ s) = do
    (env, regCounter, strCounter, lblCounter, globals) <- get
    let strLen = length s + 1
    let newGlobals = globals 
            ++ printf "@.str%d = private constant [%d x i8] c\"%s\00\"\n" 
            strCounter strLen s
    put (env, regCounter, strCounter + 1, lblCounter, newGlobals)
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
compileExpr (EAdd _ e1 op e2) = compileAddExpr op e1 e2 -- TODO: optymalizacja?
compileExpr (ERel _ e1 op e2) =
    compileArithmeticExpr (getRelOpCode op) e1 e2 $ Just (Bool BNFC'NoPosition)
compileExpr (EAnd _ e1 e2) = compileBoolExpr e1 e2 True
compileExpr (EOr _ e1 e2) = compileBoolExpr e1 e2 False

compileArg :: Arg -> String
compileArg (Arg _ t (Ident id)) = printf "%s %%%s" (toLLVMType t) id

funToEnv :: TopDef -> CMPMonad ()
funToEnv (FnDef _ t id _ _) = when (id /= Ident "main") $ varToEnv id t ""

funArgsToEnv :: [Arg] -> CMPMonad String
funArgsToEnv args = do
    result <- mapM funArgToEnv args
    return $ concat result

funArgToEnv :: Arg -> CMPMonad String
funArgToEnv (Arg _ t (Ident id)) = do
    loc <- getFreeRegister
    varToEnv (Ident id) t loc
    return $ allocInstr loc t ++ storeInstr ("%" ++ id) loc t

compileTopFun :: CMPEnv -> TopDef -> CMPMonad String
compileTopFun initialEnv (FnDef _ t (Ident id) args block) = do
    (_, _, strCounter, _, globals) <- get
    put (initialEnv, 1, strCounter, 0, globals)
    argsDecls <- funArgsToEnv args
    compiledCode <- compileBlock block
    let compiledArgs = intercalate ", " $ map compileArg args
    ret <- getDefaultReturn t
    (_, _, _, _, globals) <- get
    return $ printf "define %s @%s(%s) {\n%s\n%s\n%s}\n\n"
        (toLLVMType t) id compiledArgs argsDecls compiledCode ret

predefinedFuns :: [(Ident, CMPInf)]
predefinedFuns = [
    (Ident "printInt", FunInf (Void BNFC'NoPosition)),
    (Ident "printString", FunInf (Void BNFC'NoPosition)),
    (Ident "error", FunInf (Void BNFC'NoPosition)),
    (Ident "readInt", FunInf (Int BNFC'NoPosition)),
    (Ident "readString", FunInf (Str BNFC'NoPosition))
    ]

completeCode :: String -> String -> String
completeCode globals code = "declare void @printInt(i32)\n"
    ++ "declare void @printString(i8*)\n"
    ++ "declare void @error()\n"
    ++ "declare i32 @readInt()\n"
    ++ "declare i8* @readString()\n"
    ++ "declare i32 @strlen(i8*)\n"
    ++ "declare i8* @malloc(i32)\n"
    ++ "declare i8* @strcpy(i8*, i8*)\n"
    ++ "declare i8* @strcat(i8*, i8*)\n\n"
    -- ++ "@.str0 = private constant [1 x i8] c\"\00\"\n"
    ++ globals ++ "\n"
    ++ code

compileEveryTopFun :: [TopDef] -> CMPMonad String
compileEveryTopFun fundefs = do
    mapM_ funToEnv fundefs
    (env, _, _, _, _) <- get
    let initialEnv = M.union env $ M.fromList predefinedFuns
    result <- mapM (compileTopFun initialEnv) fundefs
    return $ concat result

compile :: Program -> IO String
compile (Program _ fundefs) = do
    let runS = runStateT (compileEveryTopFun fundefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return ""
        Right (compiledCode, (_, _, _, _, globals)) -> 
            return $ completeCode globals compiledCode
