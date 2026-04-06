module View.PostForm exposing
    ( Model
    , Msg(..)
    , init
    , toPostBody
    , toThreadBody
    , update
    , view
    , viewWithSubject
    )

import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Task
import Types exposing (CreatePostBody, CreateThreadBody, ImageUpload)


type alias Model =
    { authorName : String
    , subject : String
    , content : String
    , image : Maybe ImageUpload
    , submitting : Bool
    }


init : Model
init =
    { authorName = "Anonymous"
    , subject = ""
    , content = ""
    , image = Nothing
    , submitting = False
    }


type Msg
    = SetAuthorName String
    | SetSubject String
    | SetContent String
    | PickFile
    | FileSelected File
    | FileLoaded String String String  -- dataUrl filename mimeType
    | ClearImage
    | SetSubmitting Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAuthorName s ->
            ( { model | authorName = s }, Cmd.none )

        SetSubject s ->
            ( { model | subject = s }, Cmd.none )

        SetContent s ->
            ( { model | content = s }, Cmd.none )

        PickFile ->
            ( model, Select.file [ "image/*" ] FileSelected )

        FileSelected file ->
            ( model
            , Task.perform
                (\dataUrl ->
                    FileLoaded dataUrl (File.name file) (File.mime file)
                )
                (File.toUrl file)
            )

        FileLoaded dataUrl filename mimeType ->
            let
                base64 =
                    -- strip "data:<mime>;base64," prefix
                    case String.split "," dataUrl of
                        _ :: b64 :: _ ->
                            b64

                        _ ->
                            dataUrl
            in
            ( { model
                | image =
                    Just
                        { data_ = base64
                        , filename = filename
                        , mimeType = mimeType
                        }
              }
            , Cmd.none
            )

        ClearImage ->
            ( { model | image = Nothing }, Cmd.none )

        SetSubmitting b ->
            ( { model | submitting = b }, Cmd.none )


toPostBody : Model -> CreatePostBody
toPostBody model =
    { content = model.content
    , authorName = model.authorName
    , image = model.image
    }


toThreadBody : Model -> CreateThreadBody
toThreadBody model =
    { subject =
        if String.isEmpty model.subject then
            Nothing

        else
            Just model.subject
    , content = model.content
    , authorName = model.authorName
    , image = model.image
    }


-- | Render a reply form (no subject field).
view :
    { model : Model
    , onMsg : Msg -> msg
    , onSubmit : msg
    , submitLabel : String
    }
    -> Html msg
view cfg =
    formBody cfg False


-- | Render a new-thread form (includes subject field).
viewWithSubject :
    { model : Model
    , onMsg : Msg -> msg
    , onSubmit : msg
    , submitLabel : String
    }
    -> Html msg
viewWithSubject cfg =
    formBody cfg True


formBody :
    { model : Model
    , onMsg : Msg -> msg
    , onSubmit : msg
    , submitLabel : String
    }
    -> Bool
    -> Html msg
formBody { model, onMsg, onSubmit, submitLabel } withSubject =
    Html.form
        [ class "post-form"
        , Html.Events.onSubmit onSubmit
        ]
        [ table [ class "post-form-table" ]
            [ tbody []
                (List.concat
                    [ [ tr []
                            [ td [] [ label [] [ text "Name" ] ]
                            , td []
                                [ input
                                    [ type_ "text"
                                    , value model.authorName
                                    , onInput (onMsg << SetAuthorName)
                                    , placeholder "Anonymous"
                                    ]
                                    []
                                ]
                            ]
                      ]
                    , if withSubject then
                        [ tr []
                            [ td [] [ label [] [ text "Subject" ] ]
                            , td []
                                [ input
                                    [ type_ "text"
                                    , value model.subject
                                    , onInput (onMsg << SetSubject)
                                    , placeholder "Subject"
                                    ]
                                    []
                                ]
                            ]
                        ]

                      else
                        []
                    , [ tr []
                            [ td [] [ label [] [ text "Comment" ] ]
                            , td []
                                [ textarea
                                    [ rows 4
                                    , value model.content
                                    , onInput (onMsg << SetContent)
                                    ]
                                    []
                                ]
                            ]
                      , tr []
                            [ td [] [ label [] [ text "File" ] ]
                            , td []
                                [ case model.image of
                                    Nothing ->
                                        button
                                            [ type_ "button"
                                            , onClick (onMsg PickFile)
                                            ]
                                            [ text "Choose image…" ]

                                    Just img ->
                                        span []
                                            [ text img.filename
                                            , text " "
                                            , button
                                                [ type_ "button"
                                                , onClick (onMsg ClearImage)
                                                ]
                                                [ text "✕" ]
                                            ]
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
                                            "Posting…"

                                         else
                                            submitLabel
                                        )
                                    ]
                                ]
                            ]
                      ]
                    ]
                )
            ]
        ]
