module Typechecker (check) where

import Control.Monad.Except
import Control.Monad.State

import Latte.Abs

import qualified Data.Map as M
import qualified Data.Set as S

import Errors

type TCEnv = M.Map Ident TCInf
type TCUsedVars = S.Set Ident
type TCExcept = ExceptT TCError IO
type TCState = (TCEnv, TCUsedVars, Type, Bool, String)
type TCMonad = StateT TCState TCExcept
type ClsAtrInf = M.Map Ident Type
type ClsFunInf = M.Map Ident (Type, String)

data TCInf = VarInf Type
           | FunInf (Type, [Type])
           | ClsInf (ClsAtrInf, ClsFunInf)
    deriving Eq

data CondExprVal = CondTrue | CondFalse | CondUndefined deriving (Eq)

emptyState :: TCState
emptyState = (M.empty, S.empty, Void BNFC'NoPosition, False, "")

checkBlock :: Block -> TCMonad ()
checkBlock (Block _ stmts) = mapM_ checkStmt stmts

checkBlockNewEnv :: Block -> TCMonad ()
checkBlockNewEnv block = do
    (oldEnv, oldUsedVars, oldRetType, oldRet, oldCurrClass) <- get
    put (oldEnv, S.fromAscList (M.keys oldEnv), oldRetType, oldRet, oldCurrClass)
    checkBlock block
    newRet <- getRet
    put (oldEnv, oldUsedVars, oldRetType, newRet, oldCurrClass)

varToEnv :: Ident -> Type -> BNFC'Position -> TCMonad ()
varToEnv id t p = do
    (env, usedVars, retType, ret, currClass) <- get
    when (M.member id env && S.notMember id usedVars) $ throwError $ VarAlreadyDeclared id p
    put (M.insert id (VarInf t) env, S.delete id usedVars, retType, ret, currClass)

checkItem :: Type -> Item -> TCMonad ()
checkItem t (NoInit p id) = varToEnv id t p
checkItem t (Init p id expr) = do
    assertExprType expr t p
    varToEnv id t p

checkRetType :: Type -> BNFC'Position -> TCMonad ()
checkRetType t p = do
    (_, _, retType, _, _) <- get
    unless (checkType t retType) $ throwError $ WrongRetType t retType p

condExprCheck :: Expr -> CondExprVal
condExprCheck (ELitTrue _) = CondTrue
condExprCheck (ELitFalse _) = CondFalse
condExprCheck _ = CondUndefined

updateRet :: Bool -> TCMonad ()
updateRet ret = do
    (env, usedVars, retType, _, currClass) <- get
    put (env, usedVars, retType, ret, currClass)

getRet :: TCMonad Bool
getRet = do
    (_, _, _, ret, _) <- get
    return ret

checkStmt :: Stmt -> TCMonad ()
checkStmt (Empty _) = return ()
checkStmt (BStmt _ block) = checkBlockNewEnv block
checkStmt (Decl _ t itms) = mapM_ (checkItem t) itms
checkStmt (Ass p id e) = do
    t <- getVarType id p
    assertExprType e t p
checkStmt (ArrAss p id e1 e2) = do
    assertArray id p
    arrType <- getVarType id p
    assertExprType e1 (Int p) p
    elemType <- checkExpr e2
    unless (checkType (Array p elemType) arrType) 
        $ throwError $ WrongArrElemType id arrType elemType p
checkStmt (AtrAss p lhs fld e) = do
    idType <- checkExpr lhs
    let clsId = getClsId idType
    (clsAtrInf, _) <- getClsInf clsId p
    case M.lookup fld clsAtrInf of
        Just t -> assertExprType e t p
        Nothing -> throwError $ FieldNotInClass fld p
checkStmt (Incr p id) = do
    actual <- getVarType id p
    assertType actual (Int p) p
checkStmt (Decr p id) = do
    actual <- getVarType id p
    assertType actual (Int p) p
checkStmt (Ret p e) = do
    t <- checkExpr e
    checkRetType t p
    updateRet True
checkStmt (VRet p) = checkRetType (Void p) p
checkStmt (Cond p e stmt) = do
    assertExprType e (Bool p) p
    let condExprVal = condExprCheck e 
    unless (condExprVal == CondFalse) $ checkStmt stmt
    unless (condExprVal == CondTrue) $ updateRet False
checkStmt (CondElse p e stmt1 stmt2) = do
    assertExprType e (Bool p) p
    case condExprCheck e of
        CondTrue -> checkStmt stmt1
        CondFalse -> checkStmt stmt2
        CondUndefined -> do
            checkStmt stmt1
            ret1 <- getRet
            updateRet False
            checkStmt stmt2
            ret2 <- getRet
            unless (ret1 && ret2) $ updateRet False
checkStmt (While p e stmt) = do
    assertExprType e (Bool p) p
    checkStmt stmt
checkStmt (ForEach p t varId arrId stmt) = do
    assertArray arrId p
    arrType <- getVarType arrId p
    unless (checkType (Array p t) arrType) 
        $ throwError $ WrongArrElemType arrId arrType t p
    (env, usedVars, retType, ret, currClass) <- get
    put (M.insert varId (VarInf t) env, usedVars, retType, ret, currClass)
    checkStmt stmt
    put (env, usedVars, retType, ret, currClass)
checkStmt (SExp _ e) = void $ checkExpr e

assertArray :: Ident -> BNFC'Position -> TCMonad ()
assertArray id p = do
    t <- getVarType id p
    case t of
        (Array _ _) -> return ()
        _ -> throwError $ NotAnArray id p

checkType :: Type -> Type -> Bool
checkType (Int _) (Int _) = True
checkType (Str _) (Str _) = True
checkType (Bool _) (Bool _) = True
checkType (Void _) (Void _) = True
checkType (Array _ t1) (Array _ t2) = checkType t1 t2
checkType (Class _ _) (Class _ _) = True
checkType _ _ = False

assertType :: Type -> Type -> BNFC'Position -> TCMonad ()
assertType actual expected p =
    unless (checkType actual expected) $ throwError $ WrongType actual expected p 

assertExprType :: Expr -> Type -> BNFC'Position -> TCMonad ()
assertExprType e t p = do
    actualType <- checkExpr e
    assertType actualType t p

getInf :: Ident -> BNFC'Position -> TCMonad TCInf
getInf id p = do
    (env, _, _, _, _) <- get
    case M.lookup id env of
        Nothing -> throwError $ VarNotDeclared id p
        Just inf -> return inf

posFromType :: Type -> BNFC'Position
posFromType (Int p) = p
posFromType (Str p) = p
posFromType (Bool p) = p
posFromType (Void p) = p
posFromType (Array p _) = p

checkOpType :: Expr -> Expr -> Type -> TCMonad Type
checkOpType e1 e2 t = do
    let p = posFromType t
    assertExprType e1 t p
    assertExprType e2 t p
    return t

checkPrefixOpType :: Expr -> Type -> TCMonad Type
checkPrefixOpType e t = do
    assertExprType e t $ posFromType t
    return t

getFunInf :: Ident -> BNFC'Position -> TCMonad TCInf
getFunInf id p = do
    inf <- getInf id p
    case inf of
        (FunInf (_, _)) -> return inf
        _ -> throwError $ NotAFunction id p

checkFunArg :: BNFC'Position -> (Type, Expr) -> TCMonad ()
checkFunArg p (t, e) = do
    exprType <- checkExpr e
    unless (checkType t exprType) $ throwError $ WrongArgType exprType t p

getVarType :: Ident -> BNFC'Position -> TCMonad Type
getVarType id p = do
    (env, _, _, _, currClass) <- get
    case M.lookup id env of
        Just inf -> do
            case inf of
                (VarInf t) -> return t
                _ -> throwError $ NotAVariable id p
        Nothing -> do
            (clsAtrInf, _) <- getClsInf (Ident currClass) p
            case M.lookup id clsAtrInf of
                Just t -> return t
                Nothing -> throwError $ VarNotDeclared id p

getClsInf :: Ident -> BNFC'Position -> TCMonad (ClsAtrInf, ClsFunInf)
getClsInf id p = do
    inf <- getInf id p
    case inf of
        (ClsInf inf) -> return inf
        _ -> throwError $ NotAClass id p

getClsId :: Type -> Ident
getClsId (Class _ id) = id

checkExpr :: Expr -> TCMonad Type
checkExpr (EVar p id) = do
    (env, _, _, _, currClass) <- get
    case M.lookup id env of
        Just _ -> getVarType id p
        Nothing -> do
            (clsAtrInf, _) <- getClsInf (Ident currClass) p
            case M.lookup id clsAtrInf of
                Just t -> return t
                Nothing -> throwError $ VarNotDeclared id p
checkExpr (ELitInt p _) = return $ Int p
checkExpr (ELitTrue p) = return $ Bool p
checkExpr (ELitFalse p) = return $ Bool p
checkExpr (ENull p id) = return $ Class p id
checkExpr (EApp p id exprs) = do
    (FunInf (t, argTypes)) <- getFunInf id p
    let argLen = length argTypes
    let exprLen = length exprs
    when (argLen /= exprLen) $ throwError $ WrongArgsNumber id exprLen argLen p
    let argTypesAndExprs = zip argTypes exprs
    mapM_ (checkFunArg p) argTypesAndExprs
    return t
checkExpr (EArrRead p id e) = do
    assertArray id p
    (Array _ t) <- getVarType id p
    assertExprType e (Int p) p
    return t
checkExpr (EArrNew p t e) = do
    assertExprType e (Int p) p
    return (Array p t)
checkExpr (EClsRead p e fld) = do
    t <- checkExpr e
    case t of
        (Array p t) -> do
            unless (fld == Ident "length") $ throwError $ NotArrayAtr fld p
            return (Int p)
        _ -> do
            let clsId = getClsId t
            (clsAtrInf, _) <- getClsInf clsId p
            case M.lookup fld clsAtrInf of
                Just t -> return t
                Nothing -> throwError $ FieldNotInClass fld p
checkExpr (EClsApp p e (Ident funId) exprs) = do
    case e of
        (EVar _ (Ident "self")) -> do
            (_, _, _, _, currClass) <- get
            (_, clsFunInf) <- getClsInf (Ident currClass) p
            case M.lookup (Ident funId) clsFunInf of
                Just (t, _) -> return t
                Nothing -> throwError $ FieldNotInClass (Ident funId) p
        _ -> do
            t <- checkExpr e
            let (Ident clsId) = getClsId t
            (_, clsFunInf) <- getClsInf (Ident clsId) p
            case M.lookup (Ident funId) clsFunInf of
                Just (_, parent) -> do
                    let funName = parent ++ "." ++ funId
                    checkExpr (EApp p (Ident funName) exprs)
                Nothing -> throwError $ FieldNotInClass (Ident funId) p
checkExpr (ENewCls p id) = do
    return $ Class p id
checkExpr (EString p _) = return $ Str p
checkExpr (Neg p e) = checkPrefixOpType e $ Int p
checkExpr (Not p e) = checkPrefixOpType e $ Bool p
checkExpr (EMul p e1 _ e2) = checkOpType e1 e2 $ Int p
checkExpr (EAdd p e1 op e2) = do
    case op of
        Plus _ -> do
            t <- checkExpr e1
            if checkType t (Int p) || checkType t (Str p)
                then do
                    assertExprType e2 t p
                    return t
                else throwError $ AddOpError t p
        _ -> checkOpType e1 e2 $ Int p
checkExpr (ERel p e1 _ e2) = do
    t <- checkExpr e1
    assertExprType e2 t p
    return $ Bool p
checkExpr (EAnd p e1 e2) = checkOpType e1 e2 $ Bool p
checkExpr (EOr p e1 e2) = checkOpType e1 e2 $ Bool p

clsFunToEnv :: Ident -> Field -> TCMonad ()
clsFunToEnv (Ident cls) (ClsFun p t (Ident id) args block) = do
    let funId = cls ++ "." ++ id
    topDefToEnv (FnDef p t (Ident funId) args block)

topDefToEnv :: TopDef -> TCMonad ()
topDefToEnv (FnDef p t id args _) = do
    (env, usedVars, retType, ret, currClass) <- get
    case M.lookup id env of
        Nothing -> do
            let argTypeList = foldr (\(Arg _ t _) acc -> t : acc) [] args
            put (M.insert id (FunInf (t, argTypeList)) env, usedVars, retType, ret, currClass)
        Just _ -> throwError $ FunAlreadyDeclared id p
topDefToEnv (ClsDef p (Ident id) flds) = do
    (env, usedVars, retType, ret, currClass) <- get
    let (atrList, funs) = foldr (\fld (atrL, funL) -> case fld of
            (ClsAtr p atrType atrId) -> ((atrId, atrType) : atrL, funL)
            fun -> (atrL, fun : funL)) ([], []) flds
    let funList = foldr (\(ClsFun _ t funId _ _) acc -> (funId, (t, id)) : acc) [] funs
    put (M.insert (Ident id) (ClsInf (M.fromList atrList, M.fromList funList)) env, 
        usedVars, retType, ret, id)
    mapM_ (clsFunToEnv (Ident id)) funs
topDefToEnv (ClsDefExt p (Ident id) inh flds) = do
    (env, usedVars, retType, ret, currClass) <- get
    let (atrList, funs) = foldr (\fld (atrL, funL) -> case fld of
            (ClsAtr p atrType atrId) -> ((atrId, atrType) : atrL, funL)
            fun -> (atrL, fun : funL)) ([], []) flds
    let funList = foldr (\(ClsFun _ t funId _ _) acc -> (funId, (t, id)) : acc) [] funs
    (inhClsAtrInf, inhClsFunInf) <- getClsInf inh p
    let clsAtrInf = M.union (M.fromList atrList) inhClsAtrInf
    let clsFunInf = M.union (M.fromList funList) inhClsFunInf
    put (M.insert (Ident id) (ClsInf (clsAtrInf, clsFunInf)) env, 
        usedVars, retType, ret, id)
    mapM_ (clsFunToEnv (Ident id)) funs

funArgsToEnv :: [Arg] -> TCMonad ()
funArgsToEnv = mapM_ (\(Arg p argType argId) -> varToEnv argId argType p)

hasFunRet :: Type -> TCMonad Bool
hasFunRet (Void _) = return True
hasFunRet _ = getRet

checkClsFun :: TCEnv -> Ident -> Field -> TCMonad ()
checkClsFun initialEnv (Ident cls) (ClsFun p t (Ident id) args block) = do
    let funId = cls ++ "." ++ id
    checkTopDef initialEnv (FnDef p t (Ident funId) args block)

checkTopDef :: TCEnv -> TopDef -> TCMonad ()
checkTopDef initialEnv (FnDef p t id args block) = do
    (_, _, _, _, currClass) <- get
    put (initialEnv, S.empty, t, False, currClass)
    funArgsToEnv args
    checkBlock block
    funRet <- hasFunRet t
    unless funRet $ throwError $ NoReturn id p
checkTopDef initialEnv (ClsDef p (Ident id) flds) = do
    (env, usedVars, retType, ret, _) <- get
    put (env, usedVars, retType, ret, id)
    let funs = foldr (\fld acc -> case fld of
            ClsAtr {} -> acc
            fun -> fun : acc) [] flds
    mapM_ (checkClsFun initialEnv (Ident id)) funs
checkTopDef initialEnv (ClsDefExt p id _ flds) =
    checkTopDef initialEnv (ClsDef p id flds)

predefinedFuns :: [(Ident, TCInf)]
predefinedFuns = [
    (Ident "printInt", FunInf (Void BNFC'NoPosition, [Int BNFC'NoPosition])),
    (Ident "printString", FunInf (Void BNFC'NoPosition, [Str BNFC'NoPosition])),
    (Ident "error", FunInf (Void BNFC'NoPosition, [])),
    (Ident "readInt", FunInf (Int BNFC'NoPosition, [])),
    (Ident "readString", FunInf (Str BNFC'NoPosition, []))
    ]

checkMain :: TCMonad ()
checkMain = do
    let id = Ident "main"
    (env, _, _, _, _) <- get
    case M.lookup id env of
        Nothing -> throwError NoMainFunction
        Just (FunInf (t, argTypes)) -> do
            unless (checkType t (Int BNFC'NoPosition)) $ throwError $ WrongMainType t
            unless (null argTypes) $ throwError $ WrongArgsNumber id (length argTypes) 0 (posFromType $ head argTypes)

checkEveryTopDef :: [TopDef] -> TCMonad ()
checkEveryTopDef topdefs = do
    mapM_ topDefToEnv topdefs
    checkMain
    (env, _, _, _, _) <- get
    let initialEnv = M.union env $ M.fromList predefinedFuns
    mapM_ (checkTopDef $ M.delete (Ident "main") initialEnv) topdefs

check :: Program -> IO (String, Bool)
check (Program _ topdefs) = do
    let runS = runStateT (checkEveryTopDef topdefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return ("ERROR\n" ++ errMsg err, True)
        Right _ -> return ("OK", False)
