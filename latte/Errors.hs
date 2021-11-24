module Errors where

import Latte.Abs

data TCError = FunAlreadyDeclared Ident BNFC'Position
             | VarAlreadyDeclared Ident BNFC'Position
             | VarNotDeclared Ident BNFC'Position
             | NotAVar Ident BNFC'Position
             | NotAFunction Ident BNFC'Position
             | WrongType Type Type BNFC'Position
             | WrongArgsNumber Ident Int Int BNFC'Position
             | WrongArgType Type Type BNFC'Position
             | WrongRetType Type Type BNFC'Position

errMsgPref :: BNFC'Position -> String
errMsgPref p = case p of
    Nothing -> "Static Error: "
    Just (l, _) -> "Static Error at line " ++ show l ++ ": "

showId :: Ident -> String
showId (Ident id) = "[" ++ id ++ "]"

errMsg :: TCError -> String
errMsg (FunAlreadyDeclared id p) = errMsgPref p ++
    "Function " ++ showId id ++ " is already declared"
errMsg (VarAlreadyDeclared id p) = errMsgPref p ++
    "Variable " ++ showId id ++ " is already declared"
errMsg (VarNotDeclared id p) = errMsgPref p ++
    "Variable/Function " ++ showId id ++ " is not declared"
errMsg (NotAVar id p) = errMsgPref p ++
    showId id ++ " is not a variable"
errMsg (NotAFunction id p) = errMsgPref p ++
    showId id ++ " is not a function"
errMsg (WrongType act exp p) =
    "Wrong type - found " ++ show act ++ " instead of " ++ show exp
