module Compiler (compile) where

import Control.Monad.State

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Map as M

import Text.Printf (printf)

import Latte.Abs

type Loc = Int
type Reg = String
type CMPEnv = M.Map Ident CMPInf
type CMPStore = M.Map Loc Reg
type CMPState = (CMPEnv, CMPStore, Int, Int, Int, Int, String, String)
type CMPMonad = State CMPState
type ExprRet = (String, String, Type)

data CMPInf = VarInf (Type, Loc)
            | FunInf Type

emptyState :: CMPState
emptyState = (M.empty, M.empty, 1, 1, 1, 0, "", "")

getFreeLabel :: CMPMonad String
getFreeLabel = do
    (env, store, locCounter, regCounter, strCounter, lblCounter, _, globals) <- get
    let currentLabel = "L" ++ show lblCounter
    put (env, store, locCounter, regCounter, strCounter, lblCounter + 1, currentLabel, globals)
    return currentLabel

getCurrentLabel :: CMPMonad String
getCurrentLabel = do
    (_, _, _, _, _, _, label, _) <- get
    return label

getFreeRegister :: CMPMonad Reg
getFreeRegister = do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals) <- get
    put (env, store, locCounter, regCounter + 1, strCounter, lblCounter, label, globals)
    return $ "%" ++ show regCounter

varToEnv :: Ident -> Type -> Reg -> CMPMonad ()
varToEnv id t reg = do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals) <- get
    put (
        M.insert id (VarInf (t, locCounter)) env, 
        M.insert locCounter reg store, 
        locCounter + 1, regCounter, strCounter, lblCounter, label, globals)

getVarStoreInf :: Ident -> CMPMonad (Type, Reg)
getVarStoreInf id = do
    (env, store, _, _, _, _, _, _) <- get
    case env M.! id of
        VarInf (t, loc) -> do
            return (t, store M.! loc)
        FunInf t -> return (t, "")

getVarInf :: Ident -> CMPMonad (Type, Loc)
getVarInf id = do
    (env, _, _, _, _, _, _, _) <- get
    case env M.! id of
        VarInf inf -> return inf
        FunInf t -> return (t, 0)

updateVarReg :: Ident -> Reg -> CMPMonad ()
updateVarReg id reg = do
    (_, loc) <- getVarInf id
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals) <- get
    let updatedStore = M.insert loc reg store
    put (env, updatedStore, locCounter, regCounter, strCounter, lblCounter, label, globals)

updateRegCounter :: Int -> CMPMonad ()
updateRegCounter counter = do
    (env, store, locCounter, _, strCounter, lblCounter, currentLabel, globals) <- get
    put (env, store, locCounter, counter, strCounter, lblCounter, currentLabel, globals)

toLLVMType :: Type -> String
toLLVMType (Int _) = "i32"
toLLVMType (Str _) = "i8*"
toLLVMType (Bool _) = "i1"
toLLVMType (Void _) = "void"

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
    (oldEnv, _, _, _, _, _, _, _) <- get
    result <- compileBlock block
    (_, store, locCounter, regCounter, strCounter, lblCounter, label, globals) <- get
    put (oldEnv, store, locCounter, regCounter, strCounter, lblCounter, label, globals)
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
getDefaultReturn (Str p) = compileStmt (Ret p (EString p ""))
getDefaultReturn (Bool _) = return "ret i1 false\n"
getDefaultReturn (Void _) = return "ret void\n"

compileItem :: Type -> Item -> CMPMonad String
compileItem t (NoInit p id) = compileItem t (Init p id (getDefaultValueExpr t))
compileItem t (Init _ id e) = do
    (result, spot, _) <- compileExpr e
    varToEnv id t spot
    return result

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
    (result, spot, _) <- compileExpr e
    updateVarReg id spot
    return result
compileStmt (Incr _ id) = compileIncrDecrStmt id $ Plus BNFC'NoPosition
compileStmt (Decr _ id) = compileIncrDecrStmt id $ Minus BNFC'NoPosition
compileStmt (Ret _ e) = do
    (result, reg, t) <- compileExpr e
    _ <- getFreeRegister
    return $ result ++ printf "ret %s %s\n" (toLLVMType t) reg
compileStmt (VRet _) = do
    _ <- getFreeRegister
    return "ret void\n"
compileStmt (Cond p e stmt) = compileStmt (CondElse p e stmt (Empty p))
compileStmt (CondElse _ e stmt1 stmt2) = do
    label <- getFreeLabel
    let (preLabel, thenLabel, elseLabel, endLabel) =
            (label ++ "pre", label ++ "then", label ++ "else", label ++ "end")
    (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals) <- get
    (result, spot, _) <- compileExpr e
    compiledStmt1 <- compileStmt stmt1
    (_, thenStore, _, _, _, _, _, _) <- get
    compiledStmt2 <- compileStmt stmt2
    (_, elseStore, _, _, _, _, _, _) <- get

    let vars = foldr (\(id, inf) acc ->
            case inf of
                VarInf _ -> id:acc
                FunInf _ -> acc) [] $ M.assocs env

    phiOptions <- mapM (getPhiOptionForCond store thenStore elseStore thenLabel elseLabel) vars
    return $ brInstrU preLabel
        ++ printLabel preLabel
        ++ result
        ++ brInstrC spot thenLabel elseLabel
        ++ printLabel thenLabel
        ++ compiledStmt1
        ++ brInstrU endLabel
        ++ printLabel elseLabel
        ++ compiledStmt2
        ++ brInstrU endLabel
        ++ printLabel endLabel
        ++ concat phiOptions
compileStmt (While _ e stmt) = do
    label <- getFreeLabel
    let (preLabel, condLabel, bodyLabel, endLabel) =
            (label ++ "pre", label ++ "cond", label ++ "body", label ++ "end")
    (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals) <- get
    compileStmt stmt
    (_, newStore, _, _, _, _, _, _) <- get
    put (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals)

    let vars = foldr (\(id, inf) acc ->
            case inf of
                VarInf _ -> id:acc
                FunInf _ -> acc) [] $ M.assocs env

    phiInstrCount <- mapM (isPhiOption store newStore) vars
    let phiInstrNum = sum phiInstrCount

    updateRegCounter $ regCounter + phiInstrNum
    compileExpr e
    compileStmt stmt
    (_, newStore, _, _, _, _, _, _) <- get
    put (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals)

    phiOptions <- mapM (getPhiOptionForWhile store newStore preLabel bodyLabel) vars
    let phiCode = foldl (\acc (code, _, _) -> acc ++ code) "" phiOptions
    let phiVars = foldr (\(code, id, reg) acc -> 
            if code /= "" 
                then(id, reg):acc
                else acc) [] phiOptions

    (result, spot, _) <- compileExpr e
    compiledStmt <- compileStmt stmt
    updateVarsRegs phiVars

    return $ brInstrU preLabel
        ++ printLabel preLabel
        ++ brInstrU condLabel
        ++ printLabel condLabel
        ++ phiCode
        ++ result
        ++ brInstrC spot bodyLabel endLabel
        ++ printLabel bodyLabel
        ++ compiledStmt
        ++ brInstrU condLabel
        ++ printLabel endLabel
compileStmt (SExp _ e) = do
    (result, _, _) <- compileExpr e
    return result

getPhiOptionForCond :: CMPStore -> CMPStore -> CMPStore -> String -> String -> Ident -> CMPMonad String
getPhiOptionForCond store thenStore elseStore thenLabel elseLabel id = do
    (t, loc) <- getVarInf id
    let oldReg = store M.! loc
    let thenReg = thenStore M.! loc
    let elseReg = elseStore M.! loc
    if thenReg /= elseReg
        then do
            reg <- getFreeRegister
            updateVarReg id reg
            return $ phiInstr reg t [(thenReg, thenLabel), (elseReg, elseLabel)]
        else if oldReg /= thenReg 
            then do
                reg <- getFreeRegister
                updateVarReg id reg
                return $ phiInstr reg t [(thenReg, thenLabel), (oldReg, elseLabel)]
            else if oldReg /= elseReg
                then do
                    reg <- getFreeRegister
                    updateVarReg id reg
                    return $ phiInstr reg t [(oldReg, thenLabel), (elseReg, elseLabel)]
                else return ""

updateVarsRegs :: [(Ident, Reg)] -> CMPMonad ()
updateVarsRegs [] = return ()
updateVarsRegs ((id, reg):rest) = do
    updateVarReg id reg
    updateVarsRegs rest

isPhiOption :: CMPStore -> CMPStore -> Ident -> CMPMonad Int
isPhiOption oldStore newStore id = do
    (t, loc) <- getVarInf id
    if oldStore M.! loc /= newStore M.! loc
        then return 1
        else return 0

getPhiOptionForWhile :: CMPStore -> CMPStore -> String -> String -> Ident -> CMPMonad (String, Ident, Reg)
getPhiOptionForWhile oldStore newStore oldLabel newLabel id = do
    (t, loc) <- getVarInf id
    let oldReg = oldStore M.! loc
    let newReg = newStore M.! loc
    if oldReg /= newReg
        then do
            reg <- getFreeRegister
            updateVarReg id reg
            return (phiInstr reg t [(oldReg, oldLabel), (newReg, newLabel)], id, reg)
        else return ("", id, "")

getAddOpCode :: AddOp -> String
getAddOpCode (Plus _) = "add"
getAddOpCode _ = "sub"

getMulOpCode :: MulOp -> String
getMulOpCode (Times _) = "mul"
getMulOpCode (Div _) = "sdiv"
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

phiInstr :: String -> Type -> [(String, String)] -> String
phiInstr reg t options = do
    let compiledOptions = intercalate ", " $ foldr (\(val, inedge) acc -> 
            ("[" ++ val ++ ", %" ++ inedge ++ "]"):acc) [] options
    printf "%s = phi %s %s\n" reg (toLLVMType t) compiledOptions

compileBoolExpr :: Expr -> Expr -> Bool -> CMPMonad ExprRet
compileBoolExpr e1 e2 isAnd = do
    let (ifTrue, ifFalse) = 
            if isAnd then ("second", "end") else ("end", "second")
    label <- getFreeLabel
    (res1, spot1, t) <- compileExpr e1
    currentLabel <- getCurrentLabel
    let predecessor1 = if currentLabel == label 
        then label ++ "first" else currentLabel ++ "end"
    (res2, spot2, _) <- compileExpr e2
    currentLabel <- getCurrentLabel
    let predecessor2 = if currentLabel == label 
        then label ++ "second" else currentLabel ++ "end"
    reg <- getFreeRegister
    let code = brInstrU (label ++ "first")
            ++ printLabel (label ++ "first")
            ++ res1
            ++ brInstrC spot1 (label ++ ifTrue) (label ++ ifFalse)
            ++ printLabel (label ++ "second")
            ++ res2
            ++ brInstrU (label ++ "end")
            ++ printLabel (label ++ "end")
            ++ phiInstr reg t [(spot1, predecessor1), (spot2, predecessor2)]
    return (code, reg, t)

getAppPrefix :: Type -> CMPMonad (String, String)
getAppPrefix (Void _) = return ("", "")
getAppPrefix _ = do
    reg <- getFreeRegister
    return (printf "%s = " reg, reg)

emptyString :: CMPMonad ExprRet
emptyString = do
    reg <- getFreeRegister
    return (
        printf "%s = bitcast [1 x i8]* @.str0 to i8*\n" reg,
        reg, 
        Str BNFC'NoPosition
        )

compileExpr :: Expr -> CMPMonad ExprRet
compileExpr (EVar _ id) = do
    (t, reg) <- getVarStoreInf id
    return ("", reg, t)
compileExpr (ELitInt _ n) = return ("", show n, Int BNFC'NoPosition)
compileExpr (ELitTrue _) = return ("", "true", Bool BNFC'NoPosition)
compileExpr (ELitFalse _) = return ("", "false", Bool BNFC'NoPosition)
compileExpr (EApp _ (Ident id) exprs) = do
    (t, _) <- getVarStoreInf (Ident id)
    compiledExprs <- mapM compileExpr exprs
    let result = concatMap (\(result, _, _) -> result) compiledExprs
    let args = intercalate ", " $ foldr (\(_, reg, t) acc -> 
            (toLLVMType t ++ " " ++ reg):acc) [] compiledExprs
    (prefix, reg) <- getAppPrefix t
    let instr = prefix ++ printf "call %s @%s(%s)\n" (toLLVMType t) id args
    return (result ++ instr, reg, t)
compileExpr (EString _ s) = if s == "" then emptyString else do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals) <- get
    let strLen = length s + 1
    let newGlobals = globals 
            ++ printf "@.str%d = private constant [%d x i8] c\"%s\00\"\n" 
            strCounter strLen s
    put (env, store, locCounter, regCounter, strCounter + 1, lblCounter, label, newGlobals)
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
compileExpr (EAdd _ e1 op e2) = compileAddExpr op e1 e2
compileExpr (ERel _ e1 op e2) =
    compileArithmeticExpr (getRelOpCode op) e1 e2 $ Just (Bool BNFC'NoPosition)
compileExpr (EAnd _ e1 e2) = compileBoolExpr e1 e2 True
compileExpr (EOr _ e1 e2) = compileBoolExpr e1 e2 False

compileArg :: Arg -> String
compileArg (Arg _ t (Ident id)) = printf "%s %%%s" (toLLVMType t) id

funToEnv :: TopDef -> CMPMonad ()
funToEnv (FnDef _ t id _ _) = when (id /= Ident "main") $ do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals) <- get
    put (
        M.insert id (FunInf t) env, 
        store, locCounter, regCounter, strCounter, lblCounter, label, globals)

funArgToEnv :: Arg -> CMPMonad ()
funArgToEnv (Arg _ t (Ident id)) = varToEnv (Ident id) t ("%" ++ id)

compileTopFun :: CMPEnv -> TopDef -> CMPMonad String
compileTopFun initialEnv (FnDef _ t (Ident id) args block) = do
    (_, _, _, _, strCounter, _, _, globals) <- get
    put (initialEnv, M.empty, 1, 1, strCounter, 0, "entry", globals)
    mapM_ funArgToEnv args
    compiledCode <- compileBlock block
    let compiledArgs = intercalate ", " $ map compileArg args
    ret <- getDefaultReturn t
    return $ printf "define %s @%s(%s) {\n%s\n%s}\n\n"
        (toLLVMType t) id compiledArgs compiledCode ret

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
    ++ "@.str0 = private constant [1 x i8] c\"\00\"\n"
    ++ globals ++ "\n"
    ++ code

compileEveryTopFun :: [TopDef] -> CMPMonad String
compileEveryTopFun fundefs = do
    mapM_ funToEnv fundefs
    (env, _, _, _, _, _, _, _) <- get
    let initialEnv = M.union env $ M.fromList predefinedFuns
    result <- mapM (compileTopFun initialEnv) fundefs
    return $ concat result

compile :: Program -> IO String
compile (Program _ fundefs) = do
    let (code, (_, _, _, _, _, _, _, globals)) = 
            runState (compileEveryTopFun fundefs) emptyState
    return $ completeCode globals code
