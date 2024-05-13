module Assignments exposing (..)


getAllAssignments : List String -> List (List ( String, Bool ))
getAllAssignments variables =
    case variables of
        [] ->
            [ [] ]

        x :: rest ->
            let
                subTable : List (List ( String, Bool ))
                subTable =
                    getAllAssignments rest
            in
            List.map ((::) ( x, False )) subTable
                ++ List.map ((::) ( x, True )) subTable
