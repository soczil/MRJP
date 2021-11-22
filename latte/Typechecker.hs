module Typechecker (check) where

import Control.Monad.Except
import Control.Monad.State

import Latte.Abs

import qualified Data.Map as M
import qualified Data.Set as S

type TCEnv = M.Map Ident TCInf
type TCOldVars = S.Set Ident
type TCExcept = ExceptT TCError IO
type TCState = (TCEnv, TCOldVars, Type)
type TCMonad = StateT TCState TCExcept

data TCInf = VarInf Type
           | FunInf (Type, [Type]) 
    deriving Eq

data TCError = NoMainFunction
             | FunAlreadyDeclared Ident BNFC'Position
             | VarNotDeclared Ident BNFC'Position
             | IsNotVar Ident BNFC'Position
             | WrongType Type Type BNFC'Position

-- ============================ ERROR =================================

errMsgPref :: BNFC'Position -> String
errMsgPref p = case p of
    Nothing -> "Static Error: "
    Just (l, _) -> "Static Error at line " ++ show l ++ ": "

showId :: Ident -> String
showId (Ident id) = "[" ++ id ++ "]"

errMsg :: TCError -> String
errMsg (FunAlreadyDeclared id p) = errMsgPref p ++
    "Function " ++ showId id ++ " already declared"

-- ===================================================================

emptyState :: TCState
emptyState = (M.empty, S.empty, Void BNFC'NoPosition)

checkStmt :: Stmt -> TCMonad ()
checkStmt (Empty _) = return ()
checkStmt (BStmt p block) = undefined
checkStmt (Decl p t itms) = undefined
checkStmt (Ass p id e) = undefined
checkStmt (Incr p id) = undefined
checkStmt (Decr p id) = undefined
checkStmt (Ret p e) = undefined
checkStmt (VRet p) = undefined
checkStmt (Cond p e block) = undefined
checkStmt (CondElse p e block1 block2) = undefined
checkStmt (While p e block) = undefined
checkStmt (SExp p e) = undefined

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

checkExpr :: Expr -> TCMonad Type
checkExpr (EVar p id) = do
    inf <- getInf id p
    case inf of
        (VarInf t) -> return t
        (FunInf _) -> throwError $ IsNotVar id p
checkExpr (ELitInt p _) = return $ Int p
checkExpr (ELitTrue p) = return $ Bool p
checkExpr (ELitFalse p) = return $ Bool p
checkExpr (EApp p id exprs) = undefined
checkExpr (EString p _) = return $ Str p
checkExpr (Neg p e) = checkPrefixOpType e (Int p)
checkExpr (Not p e) = checkPrefixOpType e (Bool p)
checkExpr (EMul p e1 _ e2) = checkOpType e1 e2 (Int p)
checkExpr (EAdd p e1 _ e2) = checkOpType e1 e2 (Int p)
checkExpr (ERel p e1 op e2) = undefined
checkExpr (EAnd p e1 e2) = checkOpType e1 e2 (Bool p)
checkExpr (EOr p e1 e2) = checkOpType e1 e2 (Bool p)

funToEnv :: TopDef -> TCMonad ()
funToEnv (FnDef p t id args _) = do
    (env, oldVars, retType) <- get
    case M.lookup id env of
        Nothing -> do
            let argTypeList = foldr (\(Arg _ t _) acc -> t : acc) [] args
            put (M.insert id (FunInf (t, argTypeList)) env, oldVars, retType)
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
