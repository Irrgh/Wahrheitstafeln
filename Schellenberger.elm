module Schellenberger exposing (..)

import Html exposing (Html)
import Types exposing (..)
import Html.Attributes
import BooleanFormulas exposing (toString)



{--
    Inverse Polish Parser with ()
--}
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



parseStep : BoolToken -> List StackElement -> List StackElement
parseStep token stack =
    case ( token, stack ) of
        ( CLOSE, (Token AND) :: (Formula f2) :: (Formula f1) :: (Token OPEN) :: restStack ) ->
            Formula (And f1 f2) :: restStack

        ( CLOSE, (Token OR) :: (Formula f2) :: (Formula f1) :: (Token OPEN) :: restStack ) ->
            Formula (Or f1 f2) :: restStack

        ( CLOSE, (Token NOT) :: (Formula f) :: (Token OPEN) :: restStack ) ->
            Formula (Not f) :: restStack

        ( CONST value, _ ) ->
            Formula (Const value) :: stack

        ( VAR name, _ ) ->
            Formula (Var name) :: stack

        ( _, _ ) ->
            Token token :: stack








boolTreeToString : Formula -> String
boolTreeToString tree =
    case tree of
        And _ _ ->
            "∧"
        Or _ _ ->
            "∨"
        Not  _ ->
            "¬"
        Const val ->
            boolToString val
        Var var ->
            var


{--
    Takes the first elements from parsing result
--}
extractResult : Result (List StackElement) Formula -> Formula
extractResult res =
    let 
    
        filter : StackElement -> Maybe Formula
        filter stack =
            case stack of
                Formula form ->
                    Just form
                _ ->
                    Nothing
    
    in
    case res of
            Err stack ->
                case List.filterMap (filter) stack of
                    [] ->
                        Var "Smth went wrong while parsing"
                    a :: _ ->
                        a
            Ok formula ->
                formula

{--
    Takes all partial Formulas from parsing result
--}
extractStack : Result (List StackElement) Formula -> List Formula
extractStack res =
    let
        filter : StackElement -> Maybe Formula
        filter stack =
            case stack of
                Formula form ->
                    Just form
                Token _ ->
                    Nothing
    in
    case res of
        Err stack ->
            let 
                
                filtered = List.filterMap (filter) stack
                
            in
            case filtered of
                [] ->
                    [Var "Smth went wrong while parsing"]
                _ :: _ ->
                    filtered     
        Ok formula ->
                [formula]
            











boolToString : Bool -> String
boolToString b =
    if b then   
        "T"
    else 
        "F"







viewTruthTable : List ( List ( String, Bool ), Formula ) -> (Html msg)
viewTruthTable table =
    let


        inputOfRow : (List (String,Bool), Formula) -> List (String, Bool)
        inputOfRow (input, _) =
            input 


        heading : List (String, Bool) -> Html msg
        heading input =
            Html.tr [] ((List.map (\f -> Html.th [] [Html.text (Tuple.first f)]) input) ++ [Html.th [] [Html.text "Result"]])


        rowToHtml : (List (String,Bool), Formula) -> Html msg
        rowToHtml (input, res) =
            case input of
                [] ->
                    Html.tr [] [Html.div [] [Html.text "All variables are disabled (Probably)"]]
                _ ->
                    let

                        permutation : List (Bool)
                        permutation = List.map (Tuple.second) input
                    in
                    Html.tr [] ((List.map (\f -> Html.td [] [Html.text (boolToString f)]) permutation) ++ [Html.td [] [Html.text (toString res)]])


    in
    case table of
        [] ->
            Html.text "table has no rows"
        a :: _ ->
            Html.table [Html.Attributes.class "table table-striped"] [
                Html.thead [] [
                    heading (inputOfRow a)
                ],
                Html.tbody [] 
                    (List.map (rowToHtml) table)
            ]
        