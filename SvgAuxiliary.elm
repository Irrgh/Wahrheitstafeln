module SvgAuxiliary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes


type alias Box =
    { left : Float
    , right : Float
    , top : Float
    , bottom : Float
    }


type alias ColorScheme =
    { lines : String, dotStroke : String, dotFill : String }


originalColorScheme =
    { lines = "gray", dotStroke = "brown", dotFill = "cornsilk" }


errorColorScheme =
    { lines = "red", dotStroke = "red", dotFill = "white" }


highlightFreeVariablesScheme =
    { lines = originalColorScheme.lines, dotStroke = "red", dotFill = "orange" }


editSubTreeColorScheme =
    { lines = "purple", dotStroke = "blue", dotFill = "lightblue" }


selectedSubTreeColorScheme =
    { lines = "brown", dotStroke = "brown", dotFill = "red" }


equalToSelectedSubTreeColorScheme =
    { lines = "orange", dotStroke = "red", dotFill = "orange" }


moveCanvasStepSize =
    20


circleRadius =
    10


viewboxLeft =
    0


viewboxTop =
    0


viewboxWidth =
    width


viewboxHeight =
    height


width =
    600


height =
    600


rootBox : Box
rootBox =
    { left = viewboxLeft + 10, right = viewboxLeft + width - 10, top = viewboxTop + 30, bottom = viewboxTop + viewboxHeight - 20 }


topPadding =
    20


leftPadding =
    40


rightPadding =
    40


bottomPadding =
    20



{--x_decay_factor =
    0--}


y_decay_factor =
    0.9


strokeWidth =
    1


createLine : List (Svg.Attribute msg) -> ( Float, Float ) -> ( Float, Float ) -> Svg msg
createLine otherSvgAttributes ( x1, y1 ) ( x2, y2 ) =
    Svg.line
        ([ Svg.Attributes.x1 (String.fromFloat x1)
         , Svg.Attributes.y1 (String.fromFloat y1)
         , Svg.Attributes.x2 (String.fromFloat x2)
         , Svg.Attributes.y2 (String.fromFloat y2)
         ]
            ++ otherSvgAttributes
        )
        []


createDot : List (Svg.Attribute msg) -> ( Float, Float ) -> Float -> Svg msg
createDot otherSvgAttributes ( x, y ) radius =
    Svg.circle
        ([ Svg.Attributes.cx (String.fromFloat x)
         , Svg.Attributes.cy (String.fromFloat y)
         , Svg.Attributes.r (String.fromFloat radius)
         ]
            ++ otherSvgAttributes
        )
        []


createRect : List (Svg.Attribute msg) -> ( Float, Float ) -> Float -> Float -> Svg msg
createRect otherSvgAttributes ( x, y ) w h =
    Svg.rect
        ([ Svg.Attributes.x (String.fromFloat (x - w / 2))
         , Svg.Attributes.y (String.fromFloat (y - h / h))
         , Svg.Attributes.width (String.fromFloat w)
         , Svg.Attributes.height (String.fromFloat h)
         ]
            ++ otherSvgAttributes
        )
        []


createSquare : List (Svg.Attribute msg) -> ( Float, Float ) -> Float -> Svg msg
createSquare otherSvgAttributes ( x, y ) radius =
    -- radius is in Svg coordinates (absolute)
    Svg.rect
        ([ Svg.Attributes.x (String.fromFloat (x - radius))
         , Svg.Attributes.y (String.fromFloat (x - radius))
         , Svg.Attributes.width (String.fromFloat (2 * radius))
         , Svg.Attributes.height (String.fromFloat (2 * radius))
         ]
            ++ otherSvgAttributes
        )
        []


createSvgLabel : ( Float, Float ) -> String -> Float -> Svg msg
createSvgLabel ( x, y ) content fontsize =
    let
        xString =
            String.fromFloat (x + 0.6 * fontsize)

        yString =
            String.fromFloat (y - 0.6 * fontsize)

        --}
    in
    Svg.text_
        ([ Svg.Attributes.x xString
         , Svg.Attributes.y yString
         , Svg.Attributes.fontSize (String.fromFloat fontsize)
         ]
         --   ++ otherSvgAttributes
        )
        [ Svg.text content ]


css path =
    Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href path ] []


linkBootstrap =
    css "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
