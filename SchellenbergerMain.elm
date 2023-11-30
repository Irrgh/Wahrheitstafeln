module SchellenbergerMain exposing (..)

import Types exposing (..)
import Browser exposing (..)
import Browser.Dom exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlAtt exposing (..)
import Html.Events as Event exposing (..)
import ParseTree exposing (ParseTree(..))
import Students as Students exposing (..)
import Schellenberger exposing (..)
import ZoomSvg exposing (..)
import Svg.Attributes as SvgAtt exposing (..)
import Svg as Svg exposing (..)
import Dict exposing (..)
import SvgAuxiliary exposing (viewboxLeft, viewboxTop, viewboxWidth, viewboxHeight)
import Browser.Events



{--
 Hier kommt die ganze App rein, wo man den Ausdruck eingeben kann 
 (z.B. umgekehrt polnisch), dann die Parsebäume angezeigt werden 
 (oder eine Fehlermeldung), wo man die Blätter rumklicken kann 
 und auch automatisch die Wahrheitstabelle berechnet wird.

 Eventuell könnte man die Zeile in der Wahrheitstabelle, 
 die der gerade angeklickten Belegung entspricht, 
 farblich markieren.
--}

type Msg 
    = InputChanged String
    | DrawCanvasMessage ZoomSvg.Message
    | ValueToggle String
    | Resize Float Float



type alias Model =
    { 
    inputStr : String,
    parseResult : (List BooleanFormula, Maybe String),
    parseTrees : List (BooleanFormula),
    truthTable : Html Msg,
    drawCanvas : ZoomSvg Msg,
    assignments : Dict String Bool,
    viewportWidth : Float,
    viewportHeight : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }



recordMessage : Msg -> Bool
recordMessage message =
    case message of
        _ ->
            False



main =
    Browser.element {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\w h -> Resize (toFloat w)  (toFloat h))
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ({
    inputStr = "",
    parseResult = ([], Nothing),
    parseTrees = [],
    truthTable = Html.text "no table generated yet :(",
    drawCanvas = ZoomSvg.makeZoomableSvgCanvas DrawCanvasMessage viewboxLeft viewboxTop viewboxWidth viewboxHeight viewboxWidth viewboxHeight,
    assignments = Dict.empty,
    viewportWidth = 0,
    viewportHeight = 0
    }
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg) 
update msg model =
    case msg of
        InputChanged str ->
            let 
                res : (List (BooleanFormula), Maybe String)
                res = parseBooleanFormula (booleanFormulaLexer str) []
            
                extracted : BooleanFormula
                extracted = extractResult res

                help : (List (BooleanFormula), Maybe String) -> List (BooleanFormula)
                help (trees, _) =
                    trees


                table : Html Msg
                table = (viewTruthTable boolToString (computeTruthTable extracted))


            in
            ({model | inputStr = str ,parseResult = res, parseTrees = help res, truthTable = table}, Cmd.none)
        DrawCanvasMessage childMessage ->
            ( { model | drawCanvas = ZoomSvg.update childMessage model.drawCanvas }, Cmd.none)
        ValueToggle str ->
            let

                maybeInvert : Maybe Bool -> Maybe Bool
                maybeInvert maybe =
                    case maybe of
                        Just a ->
                            Just (not a)
                        _ ->
                            Nothing


                newAssignments : Dict String Bool
                newAssignments = 
                    if Dict.member str model.assignments then
                        Dict.update str (maybeInvert) model.assignments
                    else
                        Dict.insert str True model.assignments

            in
            (Debug.log "assignment" {model | assignments = newAssignments}, Cmd.none)
        Resize width height ->
            ({ model | viewportWidth = width, viewportHeight = height}, Cmd.none)
        


css path =
    Html.node "link" [ HtmlAtt.rel "stylesheet", HtmlAtt.href path ] []



genStartPositions : Position -> Float ->Int -> List (Position)
genStartPositions pos oldWidth length =
    let
        spacing = oldWidth / toFloat (length + 1)

        newY = pos.y + 40

        leftBorder = oldWidth / 2

        helper : Int -> List Position -> List Position
        helper _ ls =
            case ls of
                [] ->
                    [{x = pos.x - leftBorder + spacing, y = newY}]
                a :: rest ->
                    {a | x = a.x + spacing} :: ls


    in
    if length == 1 then
        [{pos | x = pos.x , y = newY}]
    else
        List.foldl (helper) [] (List.range 1 length)




view : Model -> Html Msg
view model =
    let 
        rootPos : Position
        rootPos = {x = model.drawCanvas.width / 2, y = model.drawCanvas.height / 4}


        startPos : List (Position)
        startPos = genStartPositions rootPos 400 (List.length model.parseTrees)

      
    in
    div [HtmlAtt.class "container-fluid" ]
        [css "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css",
        css "style.css",
        h1 [] [Html.text "Boolean Formulas Programming Project"],
        
        div [HtmlAtt.class "row h-12"] [
            div [HtmlAtt.class "col-sm-6"] [
                p [] [
                    label [] [Html.text "inverse polish"],
                    input [HtmlAtt.type_ "radio", HtmlAtt.name "parseMode"] [],
                    label [] [Html.text "infix"],
                    input [HtmlAtt.type_ "radio", HtmlAtt.name "parseMode"] [],

                    textarea [Event.onInput InputChanged, HtmlAtt.placeholder "Input Boolean Formular here:" ] []
                ],
                p [] [
                    h3 [] [Html.text "Raw Parsing Results:"],
                    pre [] [Html.text (Debug.toString model.parseResult)]
                ],


                p [] [
                    h3 [] [Html.text "Parsed Boolean Formulars:"],
                    pre [] [Html.text (Debug.toString model.parseTrees)]
                ],
                p [] [
                    h3 [] [Html.text "Generated Truthtable:"],
                    div [HtmlAtt.class "table-container"] [
                        model.truthTable
                    ]
                ]
                
                
            ]
            ,
            div [HtmlAtt.class "col-sm-6"] [
                div [HtmlAtt.style "border" "solid black 1pt" ] [
                ZoomSvg.view
                    model.drawCanvas
                    []
                    (List.concat (List.map2 (treeToSvg model.drawCanvas model.assignments 400) startPos model.parseTrees))
                ]
            ]
        ]
        
    ]









-- depth should start at 
treeToSvg : ZoomSvg Msg ->Dict String Bool -> Float -> Position -> BooleanFormula -> List (Svg Msg)
treeToSvg canvas assigned width rootPos tree =
    let

        resultToColor: Bool -> String
        resultToColor res =
            if res then
                "green"
            else
                "red"

        drawNode : Position -> String -> Bool -> Bool -> List (Svg (Msg))
        drawNode pos str res events =    
            [
            Svg.circle [
                r  (String.fromFloat (10.0 * canvas.zoom)),
                cx (String.fromFloat pos.x),
                cy (String.fromFloat pos.y),
                stroke "black",
                SvgAtt.strokeWidth (String.fromFloat (1.5 * canvas.zoom)),
                fill (resultToColor res),
                onClick (ValueToggle str)
            ] [],
            Svg.line [
                


            ] [],
            Svg.text_ [
                x (String.fromFloat pos.x),
                y (String.fromFloat (pos.y - (20.0 * canvas.zoom))),
                fontSize (String.fromFloat (18.0 * canvas.zoom)),
                textAnchor "middle"
                

            ] [Svg.text str]
             ]


        withEvents : Bool -> List (Svg Msg)
        withEvents event =
            drawNode rootPos (boolTreeToString tree) (Students.evalBooleanFormula assigned tree) event

    in 
    case tree of
        And child1 child2 ->
            let
                newRoots : List Position
                newRoots = genStartPositions rootPos width 2

                newWidth = width / toFloat 2
            in
            List.concat (List.append [(withEvents False)] (List.map2 (treeToSvg canvas assigned newWidth) newRoots [child1,child2]))
        Or child1 child2 ->
            let
                newRoots : List Position
                newRoots = genStartPositions rootPos width 2

                newWidth = width / toFloat 2
            in
            List.concat (List.append [(withEvents False)] (List.map2 (treeToSvg canvas assigned newWidth) newRoots [child1,child2]))
        Not child1 ->
            let
                newRoots : List Position
                newRoots = genStartPositions rootPos width 1

                newWidth = width / toFloat 1
            in
            List.concat (List.append [(withEvents False)] (List.map2 (treeToSvg canvas assigned newWidth) newRoots [child1]))
        Var _ ->
            withEvents True
        Const _ ->
            withEvents False

    