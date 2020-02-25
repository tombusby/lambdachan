module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Html exposing (Html)


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
        [ image [ centerX, padding 10, width (fill |> maximum 300) ]
            { src = "assets/img/banner.png"
            , description = "banner"
            }
        , el [ centerX ] <| text " λchan - A 4chan-style imageboard implemented with FP"
        ]


view : Model -> Html Msg
view _ =
    layout [ Background.color (rgb255 255 255 238) ] <|
        column [ centerX ] [ header ]
