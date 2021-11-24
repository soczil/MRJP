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
type TCState = (TCEnv, TCUsedVars, Type)
type TCMonad = StateT TCState TCExcept

data TCInf = VarInf Type
           | FunInf (Type, [Type]) 
    deriving Eq

emptyState :: TCState
emptyState = (M.empty, S.empty, Void BNFC'NoPosition)

checkBlock :: Block -> TCMonad ()
checkBlock (Block _ stmts) = mapM_ checkStmt stmts

checkBlockNewEnv :: Block -> TCMonad ()
checkBlockNewEnv block = do
    (oldEnv, oldUsedVars, oldRetType) <- get
    put (oldEnv, S.fromAscList (M.keys oldEnv), oldRetType)
    checkBlock block
    put (oldEnv, oldUsedVars, oldRetType)

varToEnv :: Ident -> Type -> BNFC'Position -> TCMonad ()
varToEnv id t p = do
    (env, usedVars, retType) <- get
    when (M.member id env && S.notMember id usedVars) $ throwError $ VarAlreadyDeclared id p
    put (M.insert id (VarInf t) env, S.delete id usedVars, retType)

checkItem :: Type -> Item -> TCMonad ()
checkItem t (NoInit p id) = varToEnv id t p
checkItem t (Init p id expr) = do
    assertExprType expr t p
    varToEnv id t p

checkRetType :: Type -> BNFC'Position -> TCMonad ()
checkRetType t p = do
    (_, _, retType) <- get
    unless (checkType t retType) $ throwError $ WrongRetType t retType p

checkStmt :: Stmt -> TCMonad ()
checkStmt (Empty _) = return ()
checkStmt (BStmt _ block) = checkBlockNewEnv block
checkStmt (Decl _ t itms) = mapM_ (checkItem t) itms
checkStmt (Ass p id e) = do
    t <- getVarType id p
    assertExprType e t p
checkStmt (Incr p id) = do
    actual <- getVarType id p
    assertType actual (Int p) p
checkStmt (Decr p id) = do
    actual <- getVarType id p
    assertType actual (Int p) p
checkStmt (Ret p e) = do
    t <- checkExpr e
    checkRetType t p
checkStmt (VRet p) = checkRetType (Void p) p
checkStmt (Cond p e stmt) = do
    assertExprType e (Bool p) p
    checkStmt stmt
checkStmt (CondElse p e stmt1 stmt2) = do
    assertExprType e (Bool p) p
    checkStmt stmt1
    checkStmt stmt2
checkStmt (While p e stmt) = do
    assertExprType e (Bool p) p
    checkStmt stmt
checkStmt (SExp _ e) = void $ checkExpr e

checkType :: Type -> Type -> Bool
checkType (Int _) (Int _) = True
checkType (Str _) (Str _) = True
checkType (Bool _) (Bool _) = True
checkType (Void _) (Void _) = True
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
    (env, _, _) <- get
    case M.lookup id env of
        Nothing -> throwError $ VarNotDeclared id p
        Just inf -> return inf

posFromType :: Type -> BNFC'Position
posFromType (Int p) = p
posFromType (Str p) = p
posFromType (Bool p) = p
posFromType (Void p) = p

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
    inf <- getInf id p
    case inf of
        (VarInf t) -> return t
        _ -> throwError $ NotAVariable id p

checkExpr :: Expr -> TCMonad Type
checkExpr (EVar p id) = getVarType id p
checkExpr (ELitInt p _) = return $ Int p
checkExpr (ELitTrue p) = return $ Bool p
checkExpr (ELitFalse p) = return $ Bool p
checkExpr (EApp p id exprs) = do
    (FunInf (t, argTypes)) <- getFunInf id p
    let argLen = length argTypes
    let exprLen = length exprs
    when (argLen /= exprLen) $ throwError $ WrongArgsNumber id exprLen argLen p
    let argTypesAndExprs = zip argTypes exprs
    mapM_ (checkFunArg p) argTypesAndExprs
    return t
checkExpr (EString p _) = return $ Str p
checkExpr (Neg p e) = checkPrefixOpType e $ Int p
checkExpr (Not p e) = checkPrefixOpType e $ Bool p
checkExpr (EMul p e1 _ e2) = checkOpType e1 e2 $ Int p
checkExpr (EAdd p e1 _ e2) = checkOpType e1 e2 $ Int p
checkExpr (ERel p e1 _ e2) = do
    t <- checkExpr e1
    assertExprType e2 t p
    return $ Bool p
checkExpr (EAnd p e1 e2) = checkOpType e1 e2 $ Bool p
checkExpr (EOr p e1 e2) = checkOpType e1 e2 $ Bool p

funToEnv :: TopDef -> TCMonad ()
funToEnv (FnDef p t id args _) = do
    (env, usedVars, retType) <- get
    case M.lookup id env of
        Nothing -> do
            let argTypeList = foldr (\(Arg _ t _) acc -> t : acc) [] args
            put (M.insert id (FunInf (t, argTypeList)) env, usedVars, retType)
        Just _ -> throwError $ FunAlreadyDeclared id p

checkTopFun :: TopDef -> TCMonad ()
checkTopFun = undefined

checkEveryTopFun :: [TopDef] -> TCMonad ()
checkEveryTopFun fundefs = do
    mapM_ funToEnv fundefs
    mapM_ checkTopFun fundefs

check :: Program -> IO (String, Bool)
check (Program _ fundefs) = do
    let runS = runStateT (checkEveryTopFun fundefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return (errMsg err, True)
        Right _ -> return ("", False)
