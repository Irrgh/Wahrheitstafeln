module Types exposing (..)


type StackElement
    = Formula Formula
    | Token BoolToken


type Formula
    = Const Bool
    | Var String
    | And Formula Formula
    | Or Formula Formula
    | Not Formula


type BoolToken
    = CONST Bool
    | VAR String
    | AND
    | OR
    | NOT
    | OPEN
    | CLOSE


example : List BoolToken
example =
    [ OPEN, VAR "x", OR, OPEN, OPEN, NOT, VAR "y", CLOSE, AND, VAR "z", CLOSE, CLOSE ]
