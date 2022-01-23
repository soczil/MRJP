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
type CMPState = (CMPEnv, CMPStore, Int, Int, Int, Int, String, String, String)
type CMPMonad = State CMPState
type ExprRet = (String, Reg, Type)
type ClsAtrInf = M.Map Ident (Type, Int)
type ClsFunInf = M.Map Ident (Type, String)

data CMPInf = VarInf (Type, Loc)
            | FunInf Type
            | ClsInf (ClsAtrInf, ClsFunInf, String)

emptyState :: CMPState
emptyState = (M.empty, M.empty, 1, 1, 1, 0, "", "", "")

getFreeLabel :: CMPMonad String
getFreeLabel = do
    (env, store, locCounter, regCounter, strCounter, lblCounter, _, globals, currClass) <- get
    let currentLabel = "L" ++ show lblCounter
    put (env, store, locCounter, regCounter, strCounter, lblCounter + 1, currentLabel, globals, currClass)
    return currentLabel

getCurrentLabel :: CMPMonad String
getCurrentLabel = do
    (_, _, _, _, _, _, label, _, _) <- get
    return label

getFreeRegister :: CMPMonad Reg
getFreeRegister = do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass) <- get
    put (env, store, locCounter, regCounter + 1, strCounter, lblCounter, label, globals, currClass)
    return $ "%" ++ show regCounter

varToEnv :: Ident -> Type -> Reg -> CMPMonad ()
varToEnv id t reg = do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass) <- get
    put (
        M.insert id (VarInf (t, locCounter)) env, 
        M.insert locCounter reg store, 
        locCounter + 1, regCounter, strCounter, lblCounter, label, globals, currClass)

getClassInf :: Ident -> CMPMonad (ClsAtrInf, ClsFunInf, String)
getClassInf id = do
    (env, _, _, _, _, _, _, _, _) <- get
    case env M.! id of
        (ClsInf inf) -> return inf
        _ -> return (M.empty, M.empty, "")

getVarStoreInf :: Ident -> CMPMonad (Type, Reg)
getVarStoreInf id = do
    (env, store, _, _, _, _, _, _, _) <- get
    case env M.! id of
        VarInf (t, loc) -> return (t, store M.! loc)
        FunInf t -> return (t, "")

getVarInf :: Ident -> CMPMonad (Type, Loc)
getVarInf id = do
    (env, _, _, _, _, _, _, _, _) <- get
    case env M.! id of
        VarInf inf -> return inf
        FunInf t -> return (t, 0)

updateVarReg :: Ident -> Reg -> CMPMonad ()
updateVarReg id reg = do
    (_, loc) <- getVarInf id
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass) <- get
    let updatedStore = M.insert loc reg store
    put (env, updatedStore, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass)

updateRegCounter :: Int -> CMPMonad ()
updateRegCounter counter = do
    (env, store, locCounter, _, strCounter, lblCounter, currentLabel, globals, currClass) <- get
    put (env, store, locCounter, counter, strCounter, lblCounter, currentLabel, globals, currClass)

toLLVMType :: Type -> String
toLLVMType (Int _) = "i32"
toLLVMType (Str _) = "i8*"
toLLVMType (Bool _) = "i1"
toLLVMType (Void _) = "void"
toLLVMType (Array _ t) = printf "{i32, %s*}" (toLLVMType t)
toLLVMType (Class _ (Ident id)) = printf "%%class.%s*" id

toReturnType :: Type -> String
toReturnType (Array p t) = toLLVMType (Array p t) ++ "*"
toReturnType t = toLLVMType t

basicInstr :: Reg -> Type -> String -> Reg -> Reg -> String
basicInstr reg t opCode = 
    printf "%s = %s %s %s, %s\n" reg opCode (toLLVMType t)

brInstrC :: String -> String -> String -> String
brInstrC = printf "br i1 %s, label %%%s, label %%%s\n"

brInstrU :: String -> String
brInstrU = printf "br label %%%s\n"

printLabel :: String -> String
printLabel = printf "%s:\n"

callInstr :: Reg -> Type -> String -> [ExprRet] -> String
callInstr reg t fun args = do
    let compiledArgs = intercalate ", " $ foldr (\(_, reg, argType) acc -> 
            (toLLVMType argType ++ " " ++ reg):acc) [] args
    printf "%s = call %s @%s(%s)\n" reg (toLLVMType t) fun compiledArgs

compileBlockNewEnv :: Block -> CMPMonad String
compileBlockNewEnv block = do
    (oldEnv, _, _, _, _, _, _, _, _) <- get
    result <- compileBlock block
    (_, store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass) <- get
    put (oldEnv, store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass)
    return result

compileBlock :: Block -> CMPMonad String
compileBlock (Block _ stmts) = do
    result <- mapM compileStmt stmts
    return $ concat result

getDefaultValueExpr :: Type -> Expr
getDefaultValueExpr (Int _) = ELitInt BNFC'NoPosition 0
getDefaultValueExpr (Str _) = EString BNFC'NoPosition ""
getDefaultValueExpr (Bool _) = ELitFalse BNFC'NoPosition
getDefaultValueExpr (Array _ _) = ENull BNFC'NoPosition (Ident "Array")
getDefaultValueExpr (Class _ id) = ENull BNFC'NoPosition id

getDefaultReturn :: Type -> CMPMonad String
getDefaultReturn (Int _) = return "ret i32 0\n"
getDefaultReturn (Str p) = compileStmt (Ret p (EString p ""))
getDefaultReturn (Bool _) = return "ret i1 false\n"
getDefaultReturn (Void _) = return "ret void\n"
getDefaultReturn (Array _ t) = return $ printf "ret {i32, %s*}* null\n" (toLLVMType t)
getDefaultReturn (Class _ (Ident id)) = return $ printf "ret %%class.%s* null\n" id

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

isClassVar :: Ident -> CMPMonad Bool
isClassVar id = do
    (env, _, _, _, _, _, _, _, _) <- get
    case M.lookup id env of
        Nothing -> return True
        _ -> return False

compileStmt :: Stmt -> CMPMonad String
compileStmt (Empty _) = return ""
compileStmt (BStmt _ block) = compileBlockNewEnv block
compileStmt (Decl _ t itms) = do
    result <- mapM (compileItem t) itms
    return $ concat result
compileStmt (Ass _ id e) = do
    (result, spot, _) <- compileExpr e
    classVar <- isClassVar id
    if classVar
        then do
            (_, _, _, _, _, _, _, _, currClass) <- get
            (clsAtrInf, _, _) <- getClassInf (Ident currClass)
            let (t, fld) = clsAtrInf M.! id
            ptr <- getFreeRegister
            return $ result
                ++ getelementptrInstr ptr currClass "%this" (show fld)
                ++ printf "store %s %s, %s* %s\n" (toLLVMType t) spot (toLLVMType t) ptr
        else do
            updateVarReg id spot
            return result
compileStmt (ArrAss _ id e1 e2) = do
    (result1, idx, _) <- compileExpr e1
    (result2, val, _) <- compileExpr e2
    (arrType, classReg) <- getVarStoreInf id
    let (Array _ t) = arrType
    arrPtr <- getFreeRegister
    arr <- getFreeRegister
    element <- getFreeRegister
    return $ result1 ++ result2
        ++ printf "%s = getelementptr %s, %s* %s, i32 0, i32 1\n"
            arrPtr (toLLVMType arrType) (toLLVMType arrType) classReg
        ++ printf "%s = load %s*, %s** %s\n" arr (toLLVMType t) (toLLVMType t) arrPtr
        ++ printf "%s = getelementptr %s, %s* %s, %s %s\n"
            element (toLLVMType t) (toLLVMType t) arr (toLLVMType t) idx
        ++ printf "store %s %s, %s* %s\n" (toLLVMType t) val (toLLVMType t) element
compileStmt (AtrAss _ id fld e) = do
    (result, spot, _) <- compileExpr e
    (t, reg) <- getVarStoreInf id
    let (Class _ (Ident cls)) = t
    (clsAtrInf, _, _) <- getClassInf (Ident cls)
    let (fldType, fldCount) = clsAtrInf M.! fld
    newReg <- getFreeRegister
    return $ result
        ++ printf "%s = getelementptr inbounds %%class.%s, %%class.%s* %s, i32 0, i32 %d\n" newReg cls cls reg fldCount 
        ++ printf "store %s %s, %s* %s\n" (toLLVMType fldType) spot (toLLVMType fldType) newReg
compileStmt (Incr _ id) = compileIncrDecrStmt id $ Plus BNFC'NoPosition
compileStmt (Decr _ id) = compileIncrDecrStmt id $ Minus BNFC'NoPosition
compileStmt (Ret _ e) = do
    (result, reg, t) <- compileExpr e
    _ <- getFreeRegister
    return $ result ++ printf "ret %s %s\n" (toReturnType t) reg
compileStmt (VRet _) = do
    _ <- getFreeRegister
    return "ret void\n"
compileStmt (Cond p e stmt) = compileStmt (CondElse p e stmt (Empty p))
compileStmt (CondElse _ e stmt1 stmt2) = do
    label <- getFreeLabel
    let (preLabel, thenLabel, elseLabel, endLabel) =
            (label ++ "pre", label ++ "then", label ++ "else", label ++ "end")
    (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals, currClass) <- get
    (result, spot, _) <- compileExpr e
    compiledStmt1 <- compileStmt stmt1
    (_, thenStore, _, _, _, _, _, _, _) <- get
    compiledStmt2 <- compileStmt stmt2
    (_, elseStore, _, _, _, _, _, _, _) <- get

    let vars = foldr (\(id, inf) acc ->
            case inf of
                VarInf _ -> id:acc
                _ -> acc) [] $ M.assocs env

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
    (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals, currClass) <- get
    compileStmt stmt
    (_, newStore, _, _, _, _, _, _, _) <- get
    put (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals, currClass)

    let vars = foldr (\(id, inf) acc ->
            case inf of
                VarInf _ -> id:acc
                _ -> acc) [] $ M.assocs env

    phiInstrCount <- mapM (isPhiOption store newStore) vars
    let phiInstrNum = sum phiInstrCount

    updateRegCounter $ regCounter + phiInstrNum
    compileExpr e
    compileStmt stmt
    (_, newStore, _, _, _, _, _, _, _) <- get
    put (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals, currClass)

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
compileStmt (ForEach _ t varId arrId stmt) = do
    let p = BNFC'NoPosition
    let iterator = "arr.iterate"
    let lengthExpr = EClsRead p arrId (Ident "length")
    let condExpr = ERel p (EVar p (Ident iterator)) (LTH p) lengthExpr
    let body = [
            Ass p varId (EArrRead p arrId (EVar p (Ident iterator))), 
            stmt, 
            Incr p (Ident iterator)
            ]
    (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals, currClass) <- get
    compiledVarDecl <- compileItem t (NoInit p varId)
    compiledIteratorDecl <- compileItem (Int p) (Init p (Ident iterator) (ELitInt p 0))
    compiledStmt <- compileStmt (While p condExpr (BStmt p (Block p body))) 
    (_, _, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals, currClass) <- get
    put (env, store, locCounter, regCounter, strCounter, lblCounter, currentLabel, globals, currClass)
    return $ compiledVarDecl
        ++ compiledIteratorDecl
        ++ compiledStmt
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

phiInstr :: Reg -> Type -> [(Reg, String)] -> String
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

getAppPrefix :: Type -> CMPMonad (String, Reg)
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

getelementptrInstr :: Reg -> String -> String -> String -> String
getelementptrInstr reg cls = 
    printf "%s = getelementptr inbounds %%class.%s, %%class.%s* %s, i32 0, i32 %s\n" reg cls cls

allocaInstr :: Reg -> String -> String
allocaInstr = printf "%s = alloca %%class.%s\n"

storeInstr :: Type -> Reg -> Reg -> String
storeInstr t reg1 reg2 = do
    let llvmType = toLLVMType t
    printf "store %s %s, %s* %s\n" llvmType reg1 llvmType reg2

getClassId :: Type -> String
getClassId (Class _ (Ident id)) = id
getClassId (Array _ _) = "Array"

calculateTypeSize :: Type -> CMPMonad (String, Reg)
calculateTypeSize t = do
    let llvmType = toLLVMType t
    p <- getFreeRegister
    resultReg <- getFreeRegister
    return (
        printf "%s = getelementptr %s, %s* null, i32 1\n" p llvmType llvmType
        ++ printf "%s = ptrtoint %s* %s to i32\n" resultReg llvmType p,
        resultReg)

bitcastInstr :: Reg -> Type -> Reg -> Type -> String
bitcastInstr result t1 reg t2 =
    printf "%s = bitcast %s %s to %s*\n" result (toLLVMType t1) reg (toLLVMType t2)

compileArrayLengthExpr :: Ident -> CMPMonad ExprRet
compileArrayLengthExpr id = do
    (arrType, classReg) <- getVarStoreInf id
    lenPtr <- getFreeRegister
    len <- getFreeRegister
    return (
        printf "%s = getelementptr %s, %s* %s, i32 0, i32 0\n"
            lenPtr (toLLVMType arrType) (toLLVMType arrType) classReg
        ++ printf "%s = load i32, i32* %s\n" len lenPtr,
        len, Int BNFC'NoPosition)

getVarStoreClassInf :: Ident -> CMPMonad ExprRet
getVarStoreClassInf id = case id of
    (Ident "self") -> do
        (_, _, _, _, _, _, _, _, currClass) <- get
        return ("", "%this", Class BNFC'NoPosition (Ident currClass))
    _ -> do
        classVar <- isClassVar id
        if classVar
            then do
                (_, _, _, _, _, _, _, _, currClass) <- get
                (clsAtrInf, _, _) <- getClassInf (Ident currClass)
                let (t, fld) = clsAtrInf M.! id
                ptr <- getFreeRegister
                val <- getFreeRegister
                return (getelementptrInstr ptr currClass "%this" (show fld)
                    ++ printf "%s = load %s, %s* %s\n" val (toLLVMType t) (toLLVMType t) ptr,
                    val, t)
            else do
                (classType, classReg) <- getVarStoreInf id
                return ("", classReg, classType)

compileExpr :: Expr -> CMPMonad ExprRet
compileExpr (EVar _ id) = do
    classVar <- isClassVar id
    if classVar
        then do
            (_, _, _, _, _, _, _, _, currClass) <- get
            (clsAtrInf, _, _) <- getClassInf (Ident currClass)
            let (t, fld) = clsAtrInf M.! id
            ptr <- getFreeRegister
            val <- getFreeRegister
            return (getelementptrInstr ptr currClass "%this" (show fld)
                ++ printf "%s = load %s, %s* %s\n" val (toLLVMType t) (toLLVMType t) ptr,
                val, t)
        else do
            (t, reg) <- getVarStoreInf id
            return ("", reg, t)
compileExpr (ELitInt _ n) = return ("", show n, Int BNFC'NoPosition)
compileExpr (ELitTrue _) = return ("", "true", Bool BNFC'NoPosition)
compileExpr (ELitFalse _) = return ("", "false", Bool BNFC'NoPosition)
compileExpr (ENull _ id) = return ("", "null", Class BNFC'NoPosition id)
compileExpr (EApp _ (Ident id) exprs) = do
    (t, _) <- getVarStoreInf (Ident id)
    compiledExprs <- mapM compileExpr exprs
    let result = concatMap (\(result, _, _) -> result) compiledExprs
    let args = intercalate ", " $ foldr (\(_, reg, t) acc -> 
            (toReturnType t ++ " " ++ reg):acc) [] compiledExprs
    (prefix, reg) <- getAppPrefix t
    let instr = prefix ++ printf "call %s @%s(%s)\n" (toReturnType t) id args
    return (result ++ instr, reg, t)
compileExpr (EArrRead _ id e) = do
    (result, idx, _) <- compileExpr e
    (arrType, classReg) <- getVarStoreInf id
    let (Array _ t) = arrType
    arrPtr <- getFreeRegister
    arr <- getFreeRegister
    elementPtr <- getFreeRegister
    element <- getFreeRegister
    return (result
        ++ printf "%s = getelementptr %s, %s* %s, i32 0, i32 1\n"
            arrPtr (toLLVMType arrType) (toLLVMType arrType) classReg
        ++ printf "%s = load %s*, %s** %s\n" arr (toLLVMType t) (toLLVMType t) arrPtr
        ++ printf "%s = getelementptr %s, %s* %s, %s %s\n" 
            elementPtr (toLLVMType t) (toLLVMType t) arr (toLLVMType t) idx
        ++ printf "%s = load %s, %s* %s\n" element (toLLVMType t) (toLLVMType t) elementPtr,
        element, t)
compileExpr (EArrNew p t e) = do
    let arrType = Array p t
    (result1, classSize) <- calculateTypeSize arrType
    classReg <- getFreeRegister
    classResult <- getFreeRegister
    (result2, len, _) <- compileExpr e
    (result3, typeSize) <- calculateTypeSize t
    arrSize <- getFreeRegister
    arrReg <- getFreeRegister
    arrResult <- getFreeRegister
    lenPtr <- getFreeRegister
    arrPtr <- getFreeRegister
    return (
        result1
        ++ callInstr classReg (Str p) "malloc" [("", classSize, Int p)]
        ++ bitcastInstr classResult (Str p) classReg arrType
        ++ result2 ++ result3
        ++ basicInstr arrSize (Int p) "mul" typeSize len
        ++ callInstr arrReg (Str p) "malloc" [("", arrSize, Int p)]
        ++ bitcastInstr arrResult (Str p) arrReg t
        ++ printf "%s = getelementptr %s, %s* %s, i32 0, i32 0\n" 
            lenPtr (toLLVMType arrType) (toLLVMType arrType) classResult
        ++ printf "store i32 %s, i32* %s\n" len lenPtr
        ++ printf "%s = getelementptr %s, %s* %s, i32 0, i32 1\n" 
            arrPtr (toLLVMType arrType) (toLLVMType arrType) classResult
        ++ printf "store %s* %s, %s** %s\n" (toLLVMType t) arrResult (toLLVMType t) arrPtr,
        classResult, arrType)
compileExpr (EClsRead _ id fld) = do
    (t, reg) <- getVarStoreInf id
    case t of
        (Array _ _) -> compileArrayLengthExpr id
        _ -> do
            let cls = getClassId t
            (clsAtrInf, _, _) <- getClassInf (Ident cls)
            let (fldType, fldCount) = clsAtrInf M.! fld
            ptrReg <- getFreeRegister
            resultReg <- getFreeRegister
            return (
                getelementptrInstr ptrReg cls reg (show fldCount)
                ++ printf "%s = load %s, %s* %s\n" 
                    resultReg (toLLVMType fldType) (toLLVMType fldType) ptrReg,
                resultReg, fldType)
compileExpr (EClsApp _ (Ident id) (Ident fld) exprs) = do
    (compiledCode, classReg, classType) <- getVarStoreClassInf (Ident id)
    let cls = getClassId classType
    let funId = cls ++ "." ++ fld
    (t, _) <- getVarStoreInf (Ident funId)
    compiledExprs <- mapM compileExpr exprs
    let result = concatMap (\(result, _, _) -> result) compiledExprs
    let args = intercalate ", " $ foldr (\(_, reg, t) acc -> 
            (toReturnType t ++ " " ++ reg):acc) [] compiledExprs
    let space = if null args then "" else ", "
    (prefix, reg) <- getAppPrefix t
    let instr = prefix ++ printf "call %s @%s(%s)\n" (toReturnType t) funId 
            (toReturnType classType ++ " " ++ classReg ++ space ++ args)
    return (compiledCode ++ result ++ instr, reg, t)
compileExpr (ENewCls _ (Ident id)) = do
    p <- getFreeRegister
    s <- getFreeRegister
    ptr <- getFreeRegister
    reg <- getFreeRegister
    return (
        printf "%s = getelementptr %%class.%s, %%class.%s* null, i32 1\n" p id id
        ++ printf "%s = ptrtoint %%class.%s* %s to i32\n" s id p
        ++ printf "%s = call i8* @malloc(i32 %s)\n" ptr s
        ++ printf "%s = bitcast i8* %s to %%class.%s*\n" reg ptr id
        ++ printf "call void @%s.default.constructor(%%class.%s* %s)\n" id id reg,
        reg, Class BNFC'NoPosition (Ident id))
compileExpr (EString _ s) = if s == "" then emptyString else do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass) <- get
    let strLen = length s + 1
    let newGlobals = globals 
            ++ printf "@.str%d = private constant [%d x i8] c\"%s\00\"\n" 
            strCounter strLen s
    put (env, store, locCounter, regCounter, strCounter + 1, lblCounter, label, newGlobals, currClass)
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
compileArg (Arg _ t (Ident id)) = printf "%s %%%s" (toReturnType t) id

compileAtr :: Field -> String
compileAtr (ClsAtr _ t _) = toLLVMType t

clsFunToEnv :: Ident -> Field -> CMPMonad ()
clsFunToEnv (Ident cls) (ClsFun p t (Ident id) args block) = do
    let funId = cls ++ "." ++ id
    topDefToEnv (FnDef p t (Ident funId) args block)

topDefToEnv :: TopDef -> CMPMonad ()
topDefToEnv (FnDef _ t id _ _) = when (id /= Ident "main") $ do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass) <- get
    put (
        M.insert id (FunInf t) env, 
        store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass)
topDefToEnv (ClsDef _ (Ident id) flds) = do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass) <- get
    let (atrs, funs) = foldl (\(atrList, funList) fld -> case fld of
            (ClsAtr p t id) -> (atrList ++ [ClsAtr p t id], funList)
            fun -> (atrList, funList ++ [fun])) ([], []) flds
    let compiledAtrs = intercalate ", " $ map compileAtr atrs
    let (atrList, _) = foldl (\(atrL, c) (ClsAtr _ atrType atrId) -> 
            ((atrId, (atrType, c)):atrL, c + 1)) ([], 0) atrs
    let funList = foldl (\acc (ClsFun _ funType funId _ _) ->
            (funId, (funType, id)):acc) [] funs
    put (
        M.insert (Ident id) (ClsInf (M.fromList atrList, M.fromList funList, compiledAtrs)) env,
        store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass)
    mapM_ (clsFunToEnv (Ident id)) funs
topDefToEnv (ClsDefExt _ (Ident clsId) inhId flds) = do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass) <- get
    (inhClsAtrInf, inhClsFunInf, compiledInhAtrs) <- getClassInf inhId
    let (atrs, funs) = foldl (\(atrList, funList) fld -> case fld of
            (ClsAtr p t id) -> (atrList ++ [ClsAtr p t id], funList)
            fun -> (atrList, funList ++ [fun])) ([], []) flds
    let compiledAtrs = compiledInhAtrs ++ ", " ++ intercalate ", " (map compileAtr atrs)
    let (atrList, _) = foldl (\(atrL, c) (ClsAtr _ atrType atrId) ->
            ((atrId, (atrType, c)):atrL, c + 1)) ([], M.size inhClsAtrInf) atrs
    let funList = foldl (\acc (ClsFun _ funType funId _ _) ->
            (funId, (funType, clsId)):acc) [] funs
    let clsAtrInf = M.union (M.fromList atrList) inhClsAtrInf
    let clsFunInf = M.union (M.fromList funList) inhClsFunInf
    put (
        M.insert (Ident clsId) (ClsInf (clsAtrInf, clsFunInf, compiledAtrs)) env,
        store, locCounter, regCounter, strCounter, lblCounter, label, globals, currClass)
    mapM_ (clsFunToEnv (Ident clsId)) funs

funArgToEnv :: Arg -> CMPMonad ()
funArgToEnv (Arg _ t (Ident id)) = varToEnv (Ident id) t ("%" ++ id)

compileClsFun :: CMPEnv -> Ident -> Field -> CMPMonad String
compileClsFun initialEnv (Ident cls) (ClsFun p t (Ident id) args block) = do
    let funId = cls ++ "." ++ id
    let newArgs = Arg p (Class p (Ident cls)) (Ident "this"):args
    compileTopDef initialEnv (FnDef p t (Ident funId) newArgs block)

compileConstructor :: CMPEnv -> Ident -> CMPMonad String
compileConstructor initialEnv (Ident id) = do
    (clsAtrInf, _, _) <- getClassInf (Ident id)
    let funId = id ++ ".default.constructor"
    let p = BNFC'NoPosition
    let args = [Arg p (Class p (Ident id)) (Ident "this")]
    let stmts = foldl (\acc (atrId, (atrType, _)) -> 
            Ass p atrId (getDefaultValueExpr atrType):acc)
            [] (M.toList clsAtrInf)
    let block = Block p stmts
    compileTopDef initialEnv (FnDef p (Void p) (Ident funId) args block)

compileTopDef :: CMPEnv -> TopDef -> CMPMonad String
compileTopDef initialEnv (FnDef _ t (Ident id) args block) = do
    (_, _, _, _, strCounter, _, _, globals, currClass) <- get
    put (initialEnv, M.empty, 1, 1, strCounter, 0, "entry", globals, currClass)
    mapM_ funArgToEnv args
    compiledCode <- compileBlock block
    let compiledArgs = intercalate ", " $ map compileArg args
    ret <- getDefaultReturn t
    return $ printf "define %s @%s(%s) {\n%s\n%s}\n\n"
        (toReturnType t) id compiledArgs compiledCode ret
compileTopDef initialEnv (ClsDef _ (Ident id) flds) = do
    (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals, _) <- get
    let funs = foldl (\acc fld -> case fld of
            ClsAtr {} -> acc
            fun -> acc ++ [fun]) [] flds
    (_, _, compiledAtrs) <- getClassInf (Ident id)
    let newGlobals = globals ++ printf "%%class.%s = type { %s }\n" id compiledAtrs
    put (env, store, locCounter, regCounter, strCounter, lblCounter, label, newGlobals, id)
    constructor <- compileConstructor initialEnv (Ident id)
    result <- mapM (compileClsFun initialEnv (Ident id)) funs
    return $ constructor ++ concat result
compileTopDef initialEnv (ClsDefExt p clsId _ flds) = 
    compileTopDef initialEnv (ClsDef p clsId flds)
    -- (env, store, locCounter, regCounter, strCounter, lblCounter, label, globals, _) <- get
    -- let (atrs, funs) = foldl (\(atrList, funList) fld -> case fld of
    --         (ClsAtr p t id) -> (atrList ++ [ClsAtr p t id], funList)
    --         fun -> (atrList, funList ++ [fun])) ([], []) flds
    -- (_, _, compiledAtrs) <- getClassInf (Ident clsId)
    -- let newGlobals = globals ++ printf "%%class.%s = type { %s }\n" clsId compiledAtrs
    -- put (env, store, locCounter, regCounter, strCounter, lblCounter, label, newGlobals, clsId)
    -- constructor <- compileConstructor initialEnv (Ident clsId)
    -- result <- mapM (compileClsFun initialEnv (Ident clsId)) funs
    -- return $ constructor ++ concat result

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

compileEveryTopDef :: [TopDef] -> CMPMonad String
compileEveryTopDef topdefs = do
    mapM_ topDefToEnv topdefs
    (env, _, _, _, _, _, _, _, _) <- get
    let initialEnv = M.union env $ M.fromList predefinedFuns
    result <- mapM (compileTopDef initialEnv) topdefs
    return $ concat result

compile :: Program -> IO String
compile (Program _ topdefs) = do
    let (code, (_, _, _, _, _, _, _, globals, _)) = 
            runState (compileEveryTopDef topdefs) emptyState
    return $ completeCode globals code
