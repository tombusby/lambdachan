module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
        [ image [ centerX, width (fill |> maximum 300) ]
            { src = "assets/img/banner.png"
            , description = "banner"
            }
        , el [ centerX ] <|
            text "λchan - FP Imageboard"
        ]


commentBox : Element Msg
commentBox =
    column
        [ alignLeft
        , Font.size 14
        , spacing 10
        , padding 20
        , Background.color <| rgb255 240 224 214
        , Border.color <| rgb255 217 191 183
        , Border.solid
        , Border.width 1
        ]
        [ row [ spacing 5 ]
            [ el [ Font.color <| rgb255 17 119 67, Font.heavy ] <| text "Anonymous"
            , text "(ID: 2423424)"
            , text "25/02/20(Tue)20:34:37 No.1234567890"
            , el [ Font.color <| rgb255 0 0 238 ] <| text "▶"
            , row [ Font.color <| rgb255 0 0 238, Font.underline ]
                [ link []
                    { url = "#"
                    , label = text ">>34234324"
                    }
                ]
            ]
        , row
            [ spacing 10 ]
            [ text "blah blah shitpost" ]
        ]


view : Model -> Html Msg
view _ =
    layout
        [ Background.color <| rgb255 255 255 238
        , Font.color <| Element.rgb255 180 0 0
        , Font.family
            [ Font.typeface "Tahoma"
            , Font.sansSerif
            ]
        , padding 10
        ]
    <|
        column [ centerX, width fill ]
            [ header
            , row [ padding 10 ] []
            , commentBox
            ]
