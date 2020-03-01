module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Http
import Palette
import Task
import Thread
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { timezone : Time.Zone
    , mthread : Maybe Thread.Thread
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { timezone = Time.utc
      , mthread = Nothing
      }
    , Cmd.batch
        [ Task.perform SetTimezone Time.here
        , getThread
        ]
    )


getThread : Cmd Msg
getThread =
    Http.get
        { url = "/assets/thread.json"
        , expect = Http.expectJson GotThread Thread.threadDecoder
        }


type Msg
    = SetTimezone Time.Zone
    | GotThread (Result Http.Error Thread.Thread)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTimezone zone ->
            ( { model | timezone = zone }, Cmd.none )

        GotThread result ->
            let
                r =
                    Debug.log "result" result
            in
            case r of
                Ok thread ->
                    ( { model | mthread = Just (Debug.log "thread" thread) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


header : Element Msg
header =
    column [ padding 10, width fill ]
        [ image [ centerX, width (fill |> maximum 300) ]
            { src = "/assets/img/banner.png"
            , description = "banner"
            }
        , el [ centerX ] <|
            text "λchan - FP Imageboard"
        , el [ paddingXY 0 10 ] none
        ]


view : Model -> Html Msg
view model =
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
            , case model.mthread of
                Nothing ->
                    none

                Just thread ->
                    Thread.renderThread model.timezone thread
            ]
