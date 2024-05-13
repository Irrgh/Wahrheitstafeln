module BooleanFormulas exposing (toString, evaluate)

import Dict exposing (Dict)
import Types exposing (..)


toString : Formula -> String
toString f =
    case f of
        Const True ->
            "1"

        Const False ->
            "0"

        Var x ->
            x

        And f1 f2 ->
            "(" ++ toString f1 ++ " & " ++ toString f2 ++ ")"

        Or f1 f2 ->
            "(" ++ toString f1 ++ " | " ++ toString f2 ++ ")"

        Not g ->
            "(!" ++ toString g ++ ")"


evaluate : Formula -> Dict String Bool -> Formula
evaluate f alpha =
    case f of
        Const b ->
            Const b

        Var x ->
            case Dict.get x alpha of
                Just b ->
                    Const b

                Nothing ->
                    Var x

        And f1 f2 ->
            let
                g1 : Formula
                g1 =
                    evaluate f1 alpha

                g2 : Formula
                g2 =
                    evaluate f2 alpha
            in
            case ( g1, g2 ) of
                ( Const True, _ ) ->
                    g2

                ( _, Const True ) ->
                    g1

                ( Const False, _ ) ->
                    Const False

                ( _, Const False ) ->
                    Const False

                _ ->
                    And g1 g2

        Or f1 f2 ->
            let
                g1 : Formula
                g1 =
                    evaluate f1 alpha

                g2 : Formula
                g2 =
                    evaluate f2 alpha
            in
            case ( g1, g2 ) of
                ( Const False, _ ) ->
                    g2

                ( _, Const False ) ->
                    g1

                ( Const True, _ ) ->
                    Const True

                ( _, Const True ) ->
                    Const True

                _ ->
                    Or g1 g2

        Not g ->
            let
                h : Formula
                h =
                    evaluate g alpha
            in
            case h of
                Const False ->
                    Const True

                Const True ->
                    Const False

                _ ->
                    Not h
