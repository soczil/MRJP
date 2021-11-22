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
