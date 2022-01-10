module Errors where

import Latte.Abs

import Text.Printf (printf)

data TCError = FunAlreadyDeclared Ident BNFC'Position
             | VarAlreadyDeclared Ident BNFC'Position
             | VarNotDeclared Ident BNFC'Position
             | NotAVariable Ident BNFC'Position
             | NotAFunction Ident BNFC'Position
             | WrongType Type Type BNFC'Position
             | WrongArgsNumber Ident Int Int BNFC'Position
             | WrongArgType Type Type BNFC'Position
             | WrongRetType Type Type BNFC'Position
             | NoReturn Ident BNFC'Position
             | AddOpError Type BNFC'Position
             | NoMainFunction
             | WrongMainType Type

errMsgPref :: BNFC'Position -> String
errMsgPref p = case p of
    Nothing -> "Static Error: "
    Just (l, _) -> printf "Static Error at line %d: " l

showId :: Ident -> String
showId (Ident id) = "[" ++ id ++ "]"

showT :: Type -> String
showT (Int _) = "int"
showT (Str _) = "string"
showT (Bool _) = "boolean"
showT (Void _) = "void"

errMsg :: TCError -> String
errMsg (FunAlreadyDeclared id p) = errMsgPref p ++
    printf "function %s is already declared" (showId id)
errMsg (VarAlreadyDeclared id p) = errMsgPref p ++
    printf "variable %s is already declared" (showId id)
errMsg (VarNotDeclared id p) = errMsgPref p ++
    printf "%s is not declared" (showId id)
errMsg (NotAVariable id p) = errMsgPref p ++
    printf "%s is not a variable" (showId id)
errMsg (NotAFunction id p) = errMsgPref p ++
    printf "%s is not a function" (showId id)
errMsg (WrongType act exp p) = errMsgPref p ++
    printf "wrong type - found %s instead of %s" (showT act) (showT exp)
errMsg (WrongArgsNumber id act exp p) = errMsgPref p ++
    printf "function %s takes %d arguments instead of %d" (showId id) exp act
errMsg (WrongArgType act exp p) = errMsgPref p ++
    printf "found argument type %s instead of %s" (showT act) (showT exp)
errMsg (WrongRetType act exp p) = errMsgPref p ++
    printf "found return type %s instead of %s" (showT act) (showT exp)
errMsg (NoReturn id p) = errMsgPref p ++
    printf "there exists a path where non-void function %s does not return any value" (showId id)
errMsg (AddOpError t p) = errMsgPref p ++
    printf "add operation cannot be performed on %s type" (showT t)
errMsg NoMainFunction = errMsgPref BNFC'NoPosition ++
    "no main function"
errMsg (WrongMainType t) = errMsgPref BNFC'NoPosition ++
    printf "function main is type %s instead of int" (showT t)
