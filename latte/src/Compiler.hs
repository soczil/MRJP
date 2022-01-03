module Compiler (compile) where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Set as S

import Latte.Abs

import Errors

type CMPExcept = ExceptT CMPError IO
type CMPState = (S.Set Ident, Int)
type CMPMonad = StateT CMPState CMPExcept

emptyState :: CMPState
emptyState = undefined

compileExpr :: Expr -> CMPMonad Type
compileExpr (EVar p id) = undefined
compileExpr (ELitInt p n) = undefined
compileExpr (ELitTrue p) = undefined
compileExpr (ELitFalse p) = undefined
compileExpr (EApp p id exprs) = undefined
compileExpr (EString p s) = undefined
compileExpr (Neg p e) = undefined
compileExpr (Not p e) = undefined
compileExpr (EMul p e1 op e2) = undefined
compileExpr (EAdd p e1 op e2) = undefined
compileExpr (ERel p e1 op e2) = undefined
compileExpr (EAnd p e1 e2) = undefined
compileExpr (EOr p e1 e2) = undefined

compileEveryTopFun :: [TopDef] -> CMPMonad ()
compileEveryTopFun = undefined

compile :: Program -> IO ()
compile (Program _ fundefs) = do
    let runS = runStateT (compileEveryTopFun fundefs) emptyState
    result <- runExceptT runS
    return ()
