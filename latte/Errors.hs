module Errors where

import Latte.Abs

import Text.Printf

data TCError = FunAlreadyDeclared Ident BNFC'Position
             | VarAlreadyDeclared Ident BNFC'Position
             | VarNotDeclared Ident BNFC'Position
             | NotAVariable Ident BNFC'Position
             | NotAFunction Ident BNFC'Position
             | WrongType Type Type BNFC'Position
             | WrongArgsNumber Ident Int Int BNFC'Position
             | WrongArgType Type Type BNFC'Position
             | WrongRetType Type Type BNFC'Position

errMsgPref :: BNFC'Position -> String
errMsgPref p = case p of
    Nothing -> "Static Error: "
    Just (l, _) -> printf "Static Error at line %d:" (show l)

showId :: Ident -> String
showId (Ident id) = "[" ++ id ++ "]"

errMsg :: TCError -> String
errMsg (FunAlreadyDeclared id p) = errMsgPref p ++
    printf "Function %s is already declared" (showId id)
errMsg (VarAlreadyDeclared id p) = errMsgPref p ++
    printf "Variable %s is already declared" (showId id)
errMsg (VarNotDeclared id p) = errMsgPref p ++
    printf "%s is not declared" (showId id)
errMsg (NotAVariable id p) = errMsgPref p ++
    printf "%s is not a variable" (showId id)
errMsg (NotAFunction id p) = errMsgPref p ++
    printf "%s is not a function" (showId id)
errMsg (WrongType act exp p) = errMsgPref p ++
    printf "Wrong type - found %s instead of %s" (show act) (show exp)
errMsg (WrongArgsNumber id act exp p) = errMsgPref p ++
    printf "Function %s takes %d arguments instead of %d" (showId id) exp act
errMsg (WrongArgType act exp p) = errMsgPref p ++
    printf "Found argument type %s instead of %s" (show act) (show exp)
errMsg (WrongRetType act exp p) = errMsgPref p ++
    printf "Found return type %s instead of %s" (show act) (show exp)
