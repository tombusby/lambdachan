module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Palette


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


commentBox : String -> Maybe String -> Element Msg
commentBox commentText murl =
    el [ paddingXY 0 5 ] <|
        column
            [ alignLeft
            , Font.size 14
            , spacing 10
            , padding 20
            , width
                (shrink
                    |> maximum 900
                )
            , Background.color Palette.commentBoxBG
            , Border.color Palette.commentBoxBorder
            , Border.solid
            , Border.width 1
            ]
            [ row [ spacing 5 ]
                [ el [ Font.color Palette.green, Font.heavy ] <|
                    text "Anonymous"
                , text "(ID: 2423424)"
                , text "25/02/20(Tue)20:34:37 No.1234567890"
                , el [ Font.color Palette.blue ] <| text "▶"
                , row [ Font.color Palette.blue, Font.underline ]
                    [ link []
                        { url = "#"
                        , label = text ">>34234324"
                        }
                    ]
                ]
            , row
                [ paddingXY 0 10 ]
                [ paragraph []
                    [ case murl of
                        Nothing ->
                            none

                        Just url ->
                            image
                                [ alignLeft
                                , width (fill |> maximum 150)
                                , padding 5
                                ]
                                { src = url
                                , description = "banner"
                                }
                    , text commentText
                    ]
                ]
            ]


veryLongMessage : String
veryLongMessage =
    """
    blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg
    blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg
    blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg
    blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg
    blah blah shitpost dfgdfgdfgfdsgdsg sdfdsfd dsfgsdfgsdfgsdfgsfd
    dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdf
    dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg dfgdgfdsgs
    dgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgs
    dfgsfdg dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsd
    gsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg
    dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg
    dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg
    dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg
    dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsdgsdgf"""


view : Model -> Html Msg
view _ =
    layout
        [ Background.color Palette.beige
        , Font.color Palette.maroon
        , Font.family
            [ Font.typeface "Tahoma"
            , Font.sansSerif
            ]
        , padding 10
        ]
    <|
        column [ centerX, width fill ]
            [ header
            , commentBox "blah blah shitpost" Nothing
            , commentBox veryLongMessage <| Just "assets/img/banner.png"
            ]
