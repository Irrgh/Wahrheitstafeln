module ZoomSvg exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Round
import Svg
import Svg.Attributes as Satt
import Svg.Events


type alias ZoomSvg msg =
    { left : Float
    , top : Float
    , height : Float
    , width : Float
    , currentMouseData : Maybe Mouse.Event
    , topLeftCornerWhenDraggingStarted : ( Float, Float )
    , mouseDataWhenDraggingStarted : Maybe Mouse.Event
    , parentMessage : Message -> msg
    , pixelWidth : Float
    , pixelHeight : Float
    , zoom : Float -- small means we have zoomed in a lot
    }


type Message
    = MouseUpOnCanvas Mouse.Event
    | MouseDownOnCanvas Mouse.Event
    | MouseMovedOnCanvas Mouse.Event
    | ChangeZoom Float


zoomInStep =
    0.8


zoomOutStep =
    1.25


update : Message -> ZoomSvg msg -> ZoomSvg msg
update message zoomSvg =
    case message of
        ChangeZoom step ->
            { zoomSvg
                | zoom = zoomSvg.zoom * step
                , left = zoomSvg.left + (1 - step) / 2 * zoomSvg.width
                , top = zoomSvg.top + (1 - step) / 2 * zoomSvg.height
                , width = step * zoomSvg.width
                , height = step * zoomSvg.height
            }

        MouseDownOnCanvas data ->
            { zoomSvg
                | mouseDataWhenDraggingStarted = Just data
                , topLeftCornerWhenDraggingStarted = ( zoomSvg.left, zoomSvg.top )
            }

        MouseUpOnCanvas data ->
            { zoomSvg | mouseDataWhenDraggingStarted = Nothing }

        MouseMovedOnCanvas data ->
            case zoomSvg.mouseDataWhenDraggingStarted of
                Nothing ->
                    { zoomSvg | currentMouseData = Just data }

                Just oldData ->
                    -- now we have to drag
                    let
                        ( old_corner_x, old_corner_y ) =
                            zoomSvg.topLeftCornerWhenDraggingStarted

                        ( old_mouse_x, old_mouse_y ) =
                            oldData.offsetPos

                        ( mouse_x, mouse_y ) =
                            data.offsetPos

                        new_x =
                            old_corner_x - (mouse_x - old_mouse_x) * zoomSvg.zoom

                        new_y =
                            old_corner_y - (mouse_y - old_mouse_y) * zoomSvg.zoom
                    in
                    { zoomSvg
                        | currentMouseData = Just data
                        , left = new_x
                        , top = new_y
                    }


view : ZoomSvg msg -> List (Html.Attribute msg) -> List (Svg.Svg msg) -> Html msg
view zoomSvg attributesFromParent objects =
    let
        localAttributes =
            [ Satt.viewBox (viewBoxToString -1 zoomSvg)
            , Satt.width (String.fromFloat zoomSvg.pixelWidth)
            , Satt.height (String.fromFloat zoomSvg.pixelHeight)
            , Mouse.onMove MouseMovedOnCanvas
            , Mouse.onDown MouseDownOnCanvas
            , Mouse.onUp MouseUpOnCanvas
            ]

        ( left, top ) =
            ( zoomSvg.left, zoomSvg.top )

        zoom =
            zoomSvg.zoom

        length =
            30 * zoom

        plus_x =
            left + length

        plus_y =
            top + length

        minus_x =
            left + length

        minus_y =
            top + length * 2.5

        rect1 =
            Svg.rect
                [ Satt.x (String.fromFloat plus_x)
                , Satt.y (String.fromFloat plus_y)
                , Satt.width (String.fromFloat length)
                , Satt.height (String.fromFloat length)
                , Satt.strokeWidth (String.fromFloat zoom)
                , Satt.stroke "black"
                , Satt.fill "rgba(255,255,255,0.1)"
                , Svg.Events.onClick (ChangeZoom zoomInStep)
                ]
                []

        rect2 =
            Svg.rect
                [ Satt.x (String.fromFloat minus_x)
                , Satt.y (String.fromFloat minus_y)
                , Satt.width (String.fromFloat length)
                , Satt.height (String.fromFloat length)
                , Satt.strokeWidth (String.fromFloat zoom)
                , Satt.stroke "black"
                , Satt.fill "rgba(255,255,255,0.1)"
                , Svg.Events.onClick (ChangeZoom zoomOutStep)
                ]
                []

        margin =
            3 * zoom

        line1 =
            Svg.line
                [ Satt.x1 (String.fromFloat (plus_x + margin))
                , Satt.y1 (String.fromFloat (plus_y + length / 2))
                , Satt.x2 (String.fromFloat (plus_x + length - margin))
                , Satt.y2 (String.fromFloat (plus_y + length / 2))
                , Satt.strokeWidth (String.fromFloat zoom)
                , Satt.stroke "black"
                ]
                []

        line2 =
            Svg.line
                [ Satt.y1 (String.fromFloat (plus_y + margin))
                , Satt.x1 (String.fromFloat (plus_x + length / 2))
                , Satt.y2 (String.fromFloat (plus_y + length - margin))
                , Satt.x2 (String.fromFloat (plus_x + length / 2))
                , Satt.strokeWidth (String.fromFloat zoom)
                , Satt.stroke "black"
                ]
                []

        line3 =
            Svg.line
                [ Satt.x1 (String.fromFloat (minus_x + margin))
                , Satt.y1 (String.fromFloat (minus_y + length / 2))
                , Satt.x2 (String.fromFloat (minus_x + length - margin))
                , Satt.y2 (String.fromFloat (minus_y + length / 2))
                , Satt.strokeWidth (String.fromFloat zoom)
                , Satt.stroke "black"
                ]
                []
    in
    Html.div []
        [ Svg.svg
            (List.map (Html.Attributes.map zoomSvg.parentMessage) localAttributes ++ attributesFromParent)
            (objects ++ List.map (Html.map zoomSvg.parentMessage) [ line1, line2, line3, rect1, rect2 ])
        ]


makeZoomableSvgCanvas : (Message -> msg) -> Float -> Float -> Float -> Float -> Float -> Float -> ZoomSvg msg
makeZoomableSvgCanvas parentMessage left top width height pixelWidth pixelHeight =
    let
        zoom_x =
            width / pixelWidth

        zoom_y =
            height / pixelHeight

        asdfjaskldjf =
            Debug.log "svg left" left

        asdfjaskldjfasfa =
            Debug.log "svg top" top
    in
    { left = left
    , top = top
    , height = height
    , width = width
    , currentMouseData = Nothing
    , topLeftCornerWhenDraggingStarted = ( left, top )
    , mouseDataWhenDraggingStarted = Nothing
    , parentMessage = parentMessage
    , pixelWidth = pixelWidth
    , pixelHeight = pixelHeight
    , zoom = max zoom_x zoom_y
    }


viewBoxToString : Int -> ZoomSvg msg -> String
viewBoxToString decimals zoomSvg =
    let
        ( x, y ) =
            ( zoomSvg.left, zoomSvg.top )

        w =
            zoomSvg.width

        h =
            zoomSvg.height

        z =
            zoomSvg.zoom

        {--
        coords =
            [ x + (1 - z) / 2 * w
            , y + (1 - z) / 2 * h
            , w * zoomSvg.zoom
            , h * zoomSvg.zoom
            ]
            --}
        coords =
            [ x, y, w, h ]
    in
    if decimals >= 0 then
        List.map (Round.round decimals) coords |> String.join " "

    else
        List.map String.fromFloat coords |> String.join " "
