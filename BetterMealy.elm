module BetterMealy exposing (parse,BetterMealy,deltaHat)
import MealyMachine exposing (Mealy)


type alias BetterMealy state outputAlphabet =
    { startState : state
    , transition : state -> Char -> (state, outputAlphabet)
    , acceptingStates : List (state)
    }



{--
    Calculates the end state of the Mealy machine
--}
deltaHat : Mealy state outputAlphabet -> state -> List Char -> state
deltaHat machine startState chars =
    let
        helper : Char -> state -> state         
        helper c s =
            Tuple.first (machine.transition s c)


    in
    List.foldl helper startState chars 




parse : Mealy state outputAlphabet -> String -> List (String, outputAlphabet)
parse machine inputStr =
    let
        chars : List Char
        chars = String.toList inputStr    


        lambdaHat : Char -> List (state, outputAlphabet) -> List (state, outputAlphabet)
        lambdaHat char init =
            case init of
                [] ->
                    [machine.transition machine.startState char] 
                (state, _) :: _ ->
                    machine.transition state char :: init


        charsAndOutput : List (Char, outputAlphabet)
        charsAndOutput =
            List.map2 (Tuple.pair) chars (List.reverse (List.map (Tuple.second) (List.foldl (lambdaHat) [] chars)))


        charToString = (\(c, s) -> (String.fromChar c, s))

        condensor : (Char, outputAlphabet) -> List (String, outputAlphabet) -> List (String, outputAlphabet)
        condensor el init =
            let
              (s,o) = charToString el
            in
            case init of
                [] ->
                    [(s,o)]
                (str, out) :: rest ->
                    if out == o then
                        (str ++ s, o) :: rest
                    else
                        (s,o) :: init

    in
    Debug.log (Debug.toString charsAndOutput) List.reverse (List.foldl (condensor) [] charsAndOutput)


