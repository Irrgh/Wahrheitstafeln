module Types exposing (..)


type BooleanFormula
    = Const Bool
    | Var String
    | And BooleanFormula BooleanFormula
    | Or BooleanFormula BooleanFormula
    | Not BooleanFormula


type BoolToken
    = CONST Bool
    | VAR String
    | AND
    | OR
    | NOT
    | OPEN
    | CLOSE



-- example encoding 'x3 || True' in reverse Polish notation


example : List BoolToken
example =
    [ OPEN, VAR "x", OR, OPEN, OPEN, NOT, VAR "y", CLOSE, AND, VAR "z", CLOSE, CLOSE ]
