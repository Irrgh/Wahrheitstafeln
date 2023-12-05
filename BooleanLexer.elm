module BooleanLexer exposing (lex,machine)
import BetterMealy as Mealy exposing (..)
import Types exposing (..)


type State 
    = V
    


type Output
    = Val
    | Not 
    | And
    | Or
    | Op   -- [
    | Cl   -- ] 
    | Ign  -- Ignores whitespace and \n , \r






machine: BetterMealy State Output
machine =
    {
    startState = V,
    transition = transition,
    acceptingStates = [V]
    }



toToken: (String, Output) -> Maybe BoolToken
toToken (val, out) =
    case out of
        Val ->
            if (val == "true" || val == "1") then
                Just (CONST True)
            else if (val == "false" || val == "0") then
                Just (CONST False)
            else 
                Just (VAR val)
        Not ->
            Just NOT
        Or ->
            Just OR
        And ->
            Just AND
        Op ->
            Just OPEN
        Cl ->
            Just CLOSE
        Ign ->
            Nothing


repeationFix : List (String, Output) -> List (String, Output)
repeationFix list =
    let
        helper : (String, Output) -> List (String, Output) -> List (String, Output)
        helper (str, token) ls =
            case token of
                Val ->
                    ls ++ [(str, token)]
                Ign -> 
                    ls ++ [(str,token)]
                Op ->
                    ls ++ List.repeat (String.length str) ("(",token)
                Cl ->
                    ls ++ List.repeat (String.length str) (")",token)
                Not -> 
                    ls ++ List.repeat (String.length str) ("!",token)
                And -> 
                    ls ++ List.repeat (String.length str) ("&",token)
                Or -> 
                    ls ++ List.repeat (String.length str) ("|",token)  
    in
    List.foldr (helper) [] list




lex : String -> List (BoolToken)
lex str =
    List.filterMap (toToken) (repeationFix (Mealy.parse machine str))






transition : State -> Char -> (State, Output)
transition state char =
    case (state, char) of 
        (V, ' ') ->
            (V, Ign)
        (V, '\n') ->
            (V, Ign)
        (V, '\r') ->
            (V, Ign)
        (V, '!') ->
            (V, Not)
        (V, '|') ->
            (V, Or)
        (V, '&') -> 
            (V, And)
        (V, '(') ->
            (V, Op)
        (V, ')') ->
            (V, Cl)
        (V, _) ->
            (V, Val)
        
