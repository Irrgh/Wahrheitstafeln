module BooleanLexer exposing (..)
import MealyMachine as Mealy exposing (..)
import Types exposing (..)


type State 
    = S
    | V
    | O
    


type Output
    = Val
    | In   
    | Op   -- [
    | Cl   -- ] 
    | Ign  -- Ignores whitespace and \n , \r






machine: Mealy State Output
machine =
    {
    startState = S,
    transition = transition,
    acceptingStates = [S]
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
        In ->
            if (val == "||") then
                Just (OR)
            else if (val == "&&") then
                Just (AND)
            else if (val == "!") then
                Just (NOT)
            else
                Nothing
        Op ->
            Just OPEN
        Cl ->
            Just CLOSE
        Ign ->
            Nothing


bracketFix : List (String, Output) -> List (String, Output)
bracketFix list =
    let
        helper : (String, Output) -> List (String, Output) -> List (String, Output)
        helper (str, token) ls =
            case token of
                Op ->
                    ls ++ List.repeat (String.length str) (str,token) 
                Cl ->
                    ls ++ List.repeat (String.length str) (str,token) 
                _ ->
                    ls ++ [(str, token)]
    in
    List.foldl (helper) [] list




lex : String -> List (BoolToken)
lex str =
    List.filterMap (toToken) (bracketFix (Mealy.parseAndMerge machine str))






transition : State -> Char -> (State, Output)
transition state char =
    case (state, char) of 
        (S, ' ') ->
            (S, Ign)
        (S, '\n') ->
            (S, Ign)
        (S, '\r') ->
            (S, Ign)
        (S, '|') ->
            (O, In)
        (S, '&') ->
            (O, In)
        (S, '!') ->
            (O, In)
        (S, '(') ->
            (S, Op)
        (S, ')') ->
            (S, Cl)
        (S, _) ->
            (V, Val)
        (O, '&') ->
            (S, In)
        (O, '|') ->
            (S, In)
        (O, ' ') ->
            (S, Ign)
        (O, '\n') ->
            (S, Ign)
        (O, '\r') ->
            (S, Ign)
        (O, '(') ->
            (S, Op)
        (O, ')') ->
            (S, Op)
        (O, _) ->
            (V, Val)
        (V, ' ') ->
            (S, Ign)
        (V, '\n') ->
            (S, Ign)
        (V, '\r') ->
            (S, Ign)
        (V, '!') ->
            (S, In)
        (V, '|') ->
            (O, In)
        (V, '&') -> 
            (O, In)
        (V, '(') ->
            (S, Op)
        (V, ')') ->
            (S, Cl)
        (V, _) ->
            (V, Val)
        
