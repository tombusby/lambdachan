module Main exposing (main)

import Browser
import Html exposing (Html, text)


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


view : Model -> Html Msg
view _ =
    text "Hello World!"
