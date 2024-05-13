module BooleanFormulaParser exposing (..)

import Result exposing (Result)
import Types exposing (..)





parseStep : BoolToken -> List StackElement -> List StackElement
parseStep token stack =
    case ( token, stack ) of
        ( CLOSE, (Formula f2) :: (Token AND) :: (Formula f1) :: (Token OPEN) :: restStack ) ->
            Formula (And f1 f2) :: restStack

        ( CLOSE, (Formula f2) :: (Token OR) :: (Formula f1) :: (Token OPEN) :: restStack ) ->
            Formula (Or f1 f2) :: restStack

        ( CLOSE, (Formula f) :: (Token NOT) :: (Token OPEN) :: restStack ) ->
            Formula (Not f) :: restStack

        ( CONST value, _ ) ->
            Formula (Const value) :: stack

        ( VAR name, _ ) ->
            Formula (Var name) :: stack

        ( _, _ ) ->
            Token token :: stack


parse : List BoolToken -> Result (List StackElement) Formula
parse list =
    let
        parseResult : List StackElement
        parseResult =
            List.foldl parseStep [] list
    in
    case parseResult of
        [ Formula f ] ->
            Result.Ok f

        _ ->
            Result.Err parseResult
