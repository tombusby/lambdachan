module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import List
import Palette
import Thread


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    ()


init : Model
init =
    ()


type alias Msg =
    ()


update : Msg -> Model -> Model
update _ _ =
    ()


header : Element Msg
header =
    column [ padding 10, width fill ]
        [ image [ centerX, width (fill |> maximum 300) ]
            { src = "assets/img/banner.png"
            , description = "banner"
            }
        , el [ centerX ] <|
            text "λchan - FP Imageboard"
        , el [ paddingXY 0 10 ] none
        ]


view : Model -> Html Msg
view _ =
    layout
        [ Background.color Palette.background
        , Font.color Palette.text
        , Font.family
            [ Font.typeface "Tahoma"
            , Font.sansSerif
            ]
        , padding 10
        ]
    <|
        column [ centerX, width fill ] <|
            [ header
            , Thread.renderThread Thread.examples
            ]
