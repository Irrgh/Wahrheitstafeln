module Schellenberger exposing (..)

import Html exposing (Html)
import Svg exposing (Svg)
import Types exposing (..)
import ParseTree exposing (..)
import BooleanLexer exposing (lex)
import Html.Attributes
import Svg.Attributes
import Html.Events






booleanFormulaLexer : String -> List BoolToken
booleanFormulaLexer inputString =
    lex inputString



boolTreeToString : BooleanFormula -> String
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
    da können Sie zum Beispiel als Funktion 'computeSvgAttributes' eine Funktion übergeben, die jedem Blatt 
    per Svg.Events.onClick einen Eventhandler anfügt, bei inneren Knoten die Attributliste aber leerlässt.  


    Wahrscheinlich müssen Sie noch allerhand Zusatzinfo übergeben, zum Beispiel das Rechteck auf dem Svg-Bereich, 
        wo der Baum hingezeichnet werden soll und dann noch einen Skalierfaktor etc. 
--}
--- den Truth Table in einen Html table umwandeln


extractResult : (List BooleanFormula, Maybe String) -> BooleanFormula
extractResult (tree, _) =
    case tree of
        [] ->
            Var "Smth went wrong / why the heck is the formular empty??"
        a :: rest ->
            if List.length tree > 1 then
                extractResult (rest, Nothing)
            else
                a 





boolToString : Bool -> String
boolToString b =
    if b then   
        "T"
    else 
        "F"




viewTruthTable : (Bool -> String) -> ( List String, List ( List Bool, Bool ) ) -> (Html msg)
viewTruthTable boolRender (vars, truthTable) =
    let

        
        
        genRow : (List String) -> Html msg
        genRow list =
            Html.tr [] (List.map (\f-> Html.td [] [Html.text f]) list)


        makeStringList : (List Bool, Bool) -> List (String)
        makeStringList (list, res) =
            (List.map (boolRender) (list ++ [res]))


    in
    Html.table [Html.Attributes.class "table table-striped overflow-y-scroll"]
        [Html.thead [] 
            (List.map (\f-> Html.th [] [Html.text f]) (vars ++ ["Result:"]))
        ,Html.tbody [] (List.map (genRow) (List.map (makeStringList) truthTable))
        ]
    



{--
 Ein Wörtchen zur Repräsentierung des Truth Tables. 

 Zuerst würde man ja meinen, ein Truth Table sei als Typ eine 

    List (Assignment, Bool)

  und ein 'Assignment' sei einfach 
    
   (String -> Bool)

  Daher wäre ein Truth Table insgesamt eine 

     List ( (String -> Bool), Bool)


  Das Problem entsteht jetzt, wenn Sie zwei Wahrheitstabellen vergleichen. 
  Ein schmutziges Geheimnis von Elm ist nämlich, dass Funktionen doch nicht 
  zu 100% sich wie normale Werte verhalten. Sie können sie nämlich nicht vergleichen. 
  Probieren Sie's aus:

> f = (\s -> s == "hello")
> g = (\s -> s == String.toLower "HeLLo")
> f == g 
 !@#$#@@#$%$%^&*&*()(*())(*&^%^$%@##!@#@#$%$#@#$%^$#$%)




 ]



--}
