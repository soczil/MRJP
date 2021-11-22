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

checkExpr :: Expr -> TCMonad Type
checkExpr (EVar p id) = undefined
checkExpr (ELitInt p x) = undefined
checkExpr (ELitTrue p) = undefined
checkExpr (ELitFalse p) = undefined
checkExpr (EApp p id exprs) = undefined
checkExpr (EString p str) = undefined
checkExpr (Neg p e) = undefined
checkExpr (Not p e) = undefined
checkExpr (EMul p e1 op e2) = undefined
checkExpr (EAdd p e1 op e2) = undefined
checkExpr (ERel p e1 op e2) = undefined
checkExpr (EAnd p e1 e2) = undefined
checkExpr (EOr p e1 e2) = undefined

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
