module SchellenbergerMain exposing (..)

import Types exposing (..)
import Browser exposing (..)
import Browser.Dom exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlAtt exposing (..)
import Html.Events as Event exposing (..)
import Schellenberger exposing (..)
import ZoomSvg exposing (..)
import Svg.Attributes as SvgAtt exposing (..)
import Svg as Svg exposing (..)
import Dict exposing (..)
import SvgAuxiliary exposing (viewboxLeft, viewboxTop, viewboxWidth, viewboxHeight)
import Browser.Events
import BooleanFormulaParser as BooleanFormulaParser exposing (..)
import BooleanLexer exposing (lex)
import Assignments as Assignments exposing (..)
import TruthTable  as TruthTable exposing (..)
import BooleanFormulas as BooleanFormulas exposing (..)





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
    | ValueCycle String
    | Resize Float Float
    | ParserChange (List BoolToken -> Result (List StackElement) Formula)



type alias Model =
    { 
    inputStr : String,
    parseResult : Result (List StackElement) Formula,
    parseTrees : List (Formula),
    truthTable : Html Msg,
    drawCanvas : ZoomSvg Msg,
    assignments : Dict String Assignment,
    viewportWidth : Float,
    viewportHeight : Float,
    parser : List BoolToken -> Result (List StackElement) Formula
    }


type alias Position =
    { x : Float
    , y : Float
    }

type Assignment 
    = T
    | F
    | D




infixParser : List BoolToken -> Result (List StackElement) Formula
infixParser tokens =
    BooleanFormulaParser.parse tokens


inversePolishParser : List BoolToken -> Result (List StackElement) Formula
inversePolishParser tokens= 
    Schellenberger.parse tokens








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
    parseResult = Result.Err [],
    parseTrees = [],
    truthTable = Html.text "no table generated yet :(",
    drawCanvas = ZoomSvg.makeZoomableSvgCanvas DrawCanvasMessage viewboxLeft viewboxTop viewboxWidth viewboxHeight viewboxWidth viewboxHeight,
    assignments = Dict.empty,
    viewportWidth = 0,
    viewportHeight = 0,
    parser = BooleanFormulaParser.parse
    }
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg) 
update msg model =
    case msg of
        InputChanged str ->
            let 
                res : Result (List StackElement) Formula
                res = model.parser (lex str)
            
                extracted : Formula
                extracted = extractResult res

                mapHelp : (String, Assignment) -> Maybe String
                mapHelp (s, ass) =
                    case ass of
                        D ->
                            Nothing
                        _ ->
                            Just s

                varList : List String
                varList = List.filterMap (mapHelp) (Dict.toList model.assignments)

                table : Html Msg
                table = (viewTruthTable (TruthTable.generateTruthTable varList extracted ))


            in
            ({model | inputStr = str ,parseResult = res, parseTrees = extractStack res, truthTable = table}, Cmd.none)
        DrawCanvasMessage childMessage ->
            ( { model | drawCanvas = ZoomSvg.update childMessage model.drawCanvas }, Cmd.none)
        ValueCycle str ->
            let

                maybeCycle : Maybe Assignment -> Maybe Assignment
                maybeCycle maybe =
                    case maybe of
                        Just T ->
                            Just F
                        Just F ->
                            Just D
                        Just D ->
                            Just T
                        _ ->
                            Nothing


                newAssignments : Dict String Assignment
                newAssignments = 
                    if Dict.member str model.assignments then
                        Dict.update str (maybeCycle) model.assignments
                    else
                        Dict.insert str T model.assignments


                mapHelp : (String, Assignment) -> Maybe String
                mapHelp (s, ass) =
                    case ass of
                        D ->
                            Nothing
                        _ ->
                            Just s

                varList : List String
                varList = List.filterMap (mapHelp) (Dict.toList newAssignments)


                table : Html Msg
                table = (viewTruthTable (TruthTable.generateTruthTable varList (extractResult model.parseResult) ))

            in
            ({model | assignments = newAssignments , truthTable = table}, Cmd.none)
        Resize width height ->
            ({ model | viewportWidth = width, viewportHeight = height}, Cmd.none)
        ParserChange newParser ->
            let 
    
                res : Result (List StackElement) Formula
                res = newParser (lex model.inputStr)
            
                extracted : Formula
                extracted = extractResult res

                mapHelp : (String, Assignment) -> Maybe String
                mapHelp (s, ass) =
                    case ass of
                        D ->
                            Nothing
                        _ ->
                            Just s

                varList : List String
                varList = List.filterMap (mapHelp) (Dict.toList model.assignments)

                table : Html Msg
                table = (viewTruthTable (TruthTable.generateTruthTable varList extracted ))  
            
            in
            
            
            ({model | parser = newParser , parseTrees = extractStack res, parseResult = res, truthTable = table}, Cmd.none)
        







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
                    input [HtmlAtt.type_ "radio", HtmlAtt.name "parseMode", onClick (ParserChange inversePolishParser)] [],
                    label [] [Html.text "infix"],
                    input [HtmlAtt.type_ "radio", HtmlAtt.name "parseMode", onClick (ParserChange infixParser)] [],

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
                div [HtmlAtt.style "border" "solid black 1pt"] [
                ZoomSvg.view
                    model.drawCanvas
                    []
                    (List.concat (List.map2 (treeToSvg model.drawCanvas model.assignments 400) startPos model.parseTrees))
                ]
            ]
        ]
        
    ]









-- depth should start at 
treeToSvg : ZoomSvg Msg ->Dict String Assignment -> Float -> Position -> Formula -> List (Svg Msg)
treeToSvg canvas assigned width rootPos tree =
    let

        resultToColor: Formula -> String
        resultToColor res =
            case res of
                Const b ->
                    if b then
                        "green"
                    else
                        "red"
                _ ->
                    "grey"


        properAssignment : Dict String Bool
        properAssignment =
            let
                filter k v = 
                    case v of
                        D ->
                            False
                        _ ->
                            True
                        
                map k v =
                    case v of
                        T ->
                            True
                        _ ->
                            False


            in
            Dict.map (map) (Dict.filter (filter) assigned)


        line : Position -> Position -> (Svg Msg)
        line root child =
            Svg.line [
                x1 (String.fromFloat root.x),
                y1 (String.fromFloat root.y),
                x2 (String.fromFloat child.x),
                y2 (String.fromFloat child.y),
                stroke "black",
                SvgAtt.strokeWidth (String.fromFloat (1.5 * canvas.zoom))
            ] []
                



        drawNode : Position -> String -> Formula -> Bool -> List (Svg (Msg))
        drawNode pos str res events =    
            let
            
                
                circleAttributes event =
                    if event then
                        [
                        r  (String.fromFloat (10.0 * canvas.zoom)),
                        cx (String.fromFloat pos.x),
                        cy (String.fromFloat pos.y),
                        stroke "black",
                        SvgAtt.strokeWidth (String.fromFloat (1.5 * canvas.zoom)),
                        fill (resultToColor res),
                        onClick (ValueCycle str)
                        ]
                    else
                        [
                        r  (String.fromFloat (10.0 * canvas.zoom)),
                        cx (String.fromFloat pos.x),
                        cy (String.fromFloat pos.y),
                        stroke "black",
                        SvgAtt.strokeWidth (String.fromFloat (1.5 * canvas.zoom)),
                        fill (resultToColor res)
                        ]
            
            in
            
            
            
            
            [
            Svg.circle (circleAttributes events) [],
            Svg.line [
                


            ] [],
            Svg.text_ [
                x (String.fromFloat (pos.x + (10.0 * canvas.zoom))),
                y (String.fromFloat (pos.y - (20.0 * canvas.zoom))),
                fontSize (String.fromFloat (18.0 * canvas.zoom)),
                textAnchor "middle",
                stroke "rgb(107, 196, 255)",
                fill "rgb(107, 196, 255)"
                

            ] [Svg.text str]
             ]

        



        withEvents : Bool -> List (Svg Msg)
        withEvents event =
            drawNode rootPos (boolTreeToString tree) (BooleanFormulas.evaluate tree properAssignment) event

    in 
    case tree of
        And child1 child2 ->
            let
                newRoots : List Position
                newRoots = genStartPositions rootPos width 2

                newWidth = width / toFloat 2
            in
            List.concat (List.append [(List.map (line rootPos) newRoots) ++ (withEvents False)] (List.map2 (treeToSvg canvas assigned newWidth) newRoots [child1,child2]))
        Or child1 child2 ->
            let
                newRoots : List Position
                newRoots = genStartPositions rootPos width 2

                newWidth = width / toFloat 2
            in
            List.concat (List.append [(List.map (line rootPos) newRoots) ++ (withEvents False)] (List.map2 (treeToSvg canvas assigned newWidth) newRoots [child1,child2]))
        Not child1 ->
            let
                newRoots : List Position
                newRoots = genStartPositions rootPos width 1

                newWidth = width / toFloat 1


            in
            List.concat (List.append [(List.map (line rootPos) newRoots) ++ (withEvents False)] (List.map2 (treeToSvg canvas assigned newWidth) newRoots [child1])) 
        Var _ ->
            withEvents True
        Const _ ->
            withEvents False

    