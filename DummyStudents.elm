module DummyStudents exposing (evalBooleanFormula, parseBooleanFormula, computeTruthTable, name)

import Types exposing (..)
import Dict exposing (..)



name = "Max Musterman"




{--
    Evaluates a BooleanFormular based on a Dictionary of input values

    this is what Dictionary in Elm looks like : Dict.fromList [("a",False),("b",True),("c",False)]

    if you ever feels like there is probably already a solution for what you are trying to do check the offical Elm
    documentation : https://package.elm-lang.org/packages/elm/core/latest/


    I NEED THIS FOR THE WEBSITE TO WORK
--}
evalBooleanFormula : Dict String Bool -> BooleanFormula -> Bool
evalBooleanFormula values formula =
    False




{--
Recursily perfoms ALL parsing steps

should stop whenever some kind of error occurs within parseStep

    HELPER FUNCTION FOR PARSING
--}
parseBooleanFormula : List BoolToken -> List BooleanFormula -> (List BooleanFormula, Maybe String)
parseBooleanFormula inputList initial =
    ([Var "This is a dummy"], Nothing)




{--
Uses the first element of the input list and the partially parsed stack to perform a SINGLE parsing step (more info VorlesungsSkript??)

Without any errors occuring last part of output should always be Nothing

Error that can happen :
    - Stackunderflow : while parsing you try to pop more elements from stack than there are actually contained
    - Input Terminated : inputList == []

--}
parseStep : List (BoolToken) -> List (BooleanFormula) -> (List BoolToken, List BooleanFormula, Maybe String)  
parseStep inputList stack =
    (inputList, stack, Just "Nothing was done here")




{--
Returns all on/off permutations for a list a variable name

["a","b"]  -> [[True,True], [True,False],[False,True],[False,False]]

the size of this list should always be:  2^(List.length variables)

--}
createAllAssignments : List String -> List (List Bool)
createAllAssignments variables =
    [[]]







{-- 
Returns an alphabeticaly sorted List of variable name for all "Var" contained in a BooleanFormula
Duplicates should only be listed **once**

[Not (And (Var "c") (Or (Var "b") (Var "a")))]   ->   ["a","b","c"]


** sorting only serves to make the final table easier to read
--}
variablesInBoolFormula : BooleanFormula -> List String
variablesInBoolFormula tree =
    []





{--
Returns a list of variable names followed but a list of permutations and the result

@see createAllAssignments - permutations
@see variablesInBoolFormula - variable names
@see evalBooleanFormula - result

--}
computeTruthTable : BooleanFormula -> ( List String, List ( List Bool, Bool ) )
computeTruthTable tree =
    ([], [])