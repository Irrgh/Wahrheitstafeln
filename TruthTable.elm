module TruthTable exposing (..)

import Assignments
import BooleanFormulas exposing (..)
import Dict
import Types exposing (..)


type alias TruthTableEntry =
    List ( String, Bool, Formula )


type alias TruthTable =
    List TruthTableEntry


generateTruthTable : List String -> Formula -> List ( List ( String, Bool ), Formula )
generateTruthTable variables formula =
    let
        assignemnts : List (List ( String, Bool ))
        assignemnts =
            Assignments.getAllAssignments variables

        eval : List ( String, Bool ) -> Formula
        eval list =
            BooleanFormulas.evaluate formula (Dict.fromList list)
    in
    List.map (\alpha -> ( alpha, eval alpha )) assignemnts
