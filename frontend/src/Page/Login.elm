module Page.Login exposing (Model, Msg(..), init, update, view)

import Api
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Types exposing (Session)


type alias Model =
    { username : String
    , password : String
    , submitting : Bool
    , error : Maybe String
    }


type Msg
    = SetUsername String
    | SetPassword String
    | Submit
    | LoginResult (Result Http.Error Session)


init : Model
init =
    { username = ""
    , password = ""
    , submitting = False
    , error = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername s ->
            ( { model | username = s }, Cmd.none )

        SetPassword s ->
            ( { model | password = s }, Cmd.none )

        Submit ->
            ( { model | submitting = True, error = Nothing }
            , Api.login model.username model.password LoginResult
            )

        LoginResult (Ok _) ->
            -- handled in Main.elm
            ( model, Cmd.none )

        LoginResult (Err err) ->
            ( { model
                | submitting = False
                , error = Just (Api.httpErrorToString err)
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "login-page" ]
        [ h2 [] [ text "Login" ]
        , case model.error of
            Just err ->
                div [ class "error-msg" ] [ text err ]

            Nothing ->
                text ""
        , Html.form [ onSubmit Submit, class "login-form" ]
            [ table [ class "post-form-table" ]
                [ tbody []
                    [ tr []
                        [ td [] [ label [] [ text "Username" ] ]
                        , td []
                            [ input
                                [ type_ "text"
                                , value model.username
                                , onInput SetUsername
                                , placeholder "Username"
                                ]
                                []
                            ]
                        ]
                    , tr []
                        [ td [] [ label [] [ text "Password" ] ]
                        , td []
                            [ input
                                [ type_ "password"
                                , value model.password
                                , onInput SetPassword
                                , placeholder "Password"
                                ]
                                []
                            ]
                        ]
                    , tr []
                        [ td [] []
                        , td []
                            [ button
                                [ type_ "submit"
                                , disabled model.submitting
                                ]
                                [ text
                                    (if model.submitting then
                                        "Logging in…"

                                     else
                                        "Login"
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
