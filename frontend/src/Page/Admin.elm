module Page.Admin exposing (Model, Msg, init, update, view)

import Api
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, selected, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit, targetValue)
import Http
import Json.Decode
import Types exposing (Board, Session, User, UserRole(..))


type alias Model =
    { users : List User
    , error : Maybe String
    , newBoardName : String
    , newBoardTitle : String
    , newBoardDescription : String
    , boardSubmitting : Bool
    , newUsername : String
    , newPassword : String
    , newRole : UserRole
    , submitting : Bool
    , deleteTarget : Maybe Int
    , assignUserId : Maybe Int
    , assignBoard : String
    }


type Msg
    = GotUsers (Result Http.Error (List User))
    | SetNewBoardName String
    | SetNewBoardTitle String
    | SetNewBoardDescription String
    | SubmitCreateBoard
    | BoardCreated (Result Http.Error Board)
    | SetNewUsername String
    | SetNewPassword String
    | SetNewRole UserRole
    | SubmitCreateUser
    | UserCreated (Result Http.Error User)
    | AskDeleteUser Int
    | CancelDelete
    | ConfirmDeleteUser
    | UserDeleted (Result Http.Error ())
    | StartAssign Int
    | SetAssignBoard String
    | SubmitAssign
    | AssignDone (Result Http.Error ())
    | RevokeBoard Int String
    | RevokeDone (Result Http.Error ())


init : Session -> ( Model, Cmd Msg )
init session =
    ( { users = []
      , error = Nothing
      , newBoardName = ""
      , newBoardTitle = ""
      , newBoardDescription = ""
      , boardSubmitting = False
      , newUsername = ""
      , newPassword = ""
      , newRole = ModeratorRole
      , submitting = False
      , deleteTarget = Nothing
      , assignUserId = Nothing
      , assignBoard = ""
      }
    , Api.adminListUsers session GotUsers
    )


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        GotUsers (Ok users) ->
            ( { model | users = users, error = Nothing }, Cmd.none )

        GotUsers (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )

        SetNewBoardName s ->
            ( { model | newBoardName = s }, Cmd.none )

        SetNewBoardTitle s ->
            ( { model | newBoardTitle = s }, Cmd.none )

        SetNewBoardDescription s ->
            ( { model | newBoardDescription = s }, Cmd.none )

        SubmitCreateBoard ->
            ( { model | boardSubmitting = True }
            , Api.createBoard model.newBoardName model.newBoardTitle model.newBoardDescription session BoardCreated
            )

        BoardCreated (Ok _) ->
            ( { model
                | boardSubmitting = False
                , newBoardName = ""
                , newBoardTitle = ""
                , newBoardDescription = ""
              }
            , Cmd.none
            )

        BoardCreated (Err err) ->
            ( { model | boardSubmitting = False, error = Just (Api.httpErrorToString err) }, Cmd.none )

        SetNewUsername s ->
            ( { model | newUsername = s }, Cmd.none )

        SetNewPassword s ->
            ( { model | newPassword = s }, Cmd.none )

        SetNewRole role ->
            ( { model | newRole = role }, Cmd.none )

        SubmitCreateUser ->
            ( { model | submitting = True }
            , Api.adminCreateUser model.newUsername model.newPassword model.newRole session UserCreated
            )

        UserCreated (Ok _) ->
            ( { model
                | submitting = False
                , newUsername = ""
                , newPassword = ""
              }
            , Api.adminListUsers session GotUsers
            )

        UserCreated (Err err) ->
            ( { model | submitting = False, error = Just (Api.httpErrorToString err) }, Cmd.none )

        AskDeleteUser userId ->
            ( { model | deleteTarget = Just userId }, Cmd.none )

        CancelDelete ->
            ( { model | deleteTarget = Nothing }, Cmd.none )

        ConfirmDeleteUser ->
            case model.deleteTarget of
                Just userId ->
                    ( { model | deleteTarget = Nothing }
                    , Api.adminDeleteUser userId session UserDeleted
                    )

                Nothing ->
                    ( model, Cmd.none )

        UserDeleted (Ok ()) ->
            ( model, Api.adminListUsers session GotUsers )

        UserDeleted (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )

        StartAssign userId ->
            ( { model | assignUserId = Just userId, assignBoard = "" }, Cmd.none )

        SetAssignBoard s ->
            ( { model | assignBoard = s }, Cmd.none )

        SubmitAssign ->
            case model.assignUserId of
                Just userId ->
                    ( model
                    , Api.adminAssignMod userId model.assignBoard session AssignDone
                    )

                Nothing ->
                    ( model, Cmd.none )

        AssignDone (Ok ()) ->
            ( { model | assignUserId = Nothing, assignBoard = "" }
            , Api.adminListUsers session GotUsers
            )

        AssignDone (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )

        RevokeBoard userId boardName ->
            ( model, Api.adminRevokeMod userId boardName session RevokeDone )

        RevokeDone (Ok ()) ->
            ( model, Api.adminListUsers session GotUsers )

        RevokeDone (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "admin-page" ]
        [ h2 [] [ text "Admin panel" ]
        , case model.error of
            Just err ->
                div [ class "error-msg" ] [ text err ]

            Nothing ->
                text ""
        , h3 [] [ text "Create board" ]
        , Html.form [ onSubmit SubmitCreateBoard, class "create-board-form" ]
            [ table [ class "post-form-table" ]
                [ tbody []
                    [ tr []
                        [ td [] [ label [] [ text "Name" ] ]
                        , td []
                            [ input
                                [ type_ "text"
                                , value model.newBoardName
                                , onInput SetNewBoardName
                                , placeholder "e.g. g"
                                ]
                                []
                            ]
                        ]
                    , tr []
                        [ td [] [ label [] [ text "Title" ] ]
                        , td []
                            [ input
                                [ type_ "text"
                                , value model.newBoardTitle
                                , onInput SetNewBoardTitle
                                , placeholder "e.g. Technology"
                                ]
                                []
                            ]
                        ]
                    , tr []
                        [ td [] [ label [] [ text "Description" ] ]
                        , td []
                            [ input
                                [ type_ "text"
                                , value model.newBoardDescription
                                , onInput SetNewBoardDescription
                                , placeholder "e.g. Technology discussion"
                                ]
                                []
                            ]
                        ]
                    , tr []
                        [ td [] []
                        , td []
                            [ button
                                [ type_ "submit"
                                , disabled model.boardSubmitting
                                ]
                                [ text
                                    (if model.boardSubmitting then
                                        "Creating…"

                                     else
                                        "Create"
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , h3 [] [ text "Users" ]
        , table [ class "user-table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "ID" ]
                    , th [] [ text "Username" ]
                    , th [] [ text "Role" ]
                    , th [] [ text "Boards" ]
                    , th [] [ text "Actions" ]
                    ]
                ]
            , tbody []
                (List.map (userRow model) model.users)
            ]
        , h3 [] [ text "Create user" ]
        , Html.form [ onSubmit SubmitCreateUser, class "create-user-form" ]
            [ table [ class "post-form-table" ]
                [ tbody []
                    [ tr []
                        [ td [] [ label [] [ text "Username" ] ]
                        , td []
                            [ input
                                [ type_ "text"
                                , value model.newUsername
                                , onInput SetNewUsername
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
                                , value model.newPassword
                                , onInput SetNewPassword
                                , placeholder "Password"
                                ]
                                []
                            ]
                        ]
                    , tr []
                        [ td [] [ label [] [ text "Role" ] ]
                        , td []
                            [ select
                                [ on "change"
                                    (Json.Decode.map
                                        (\s ->
                                            if s == "admin" then
                                                SetNewRole AdminRole

                                            else
                                                SetNewRole ModeratorRole
                                        )
                                        targetValue
                                    )
                                ]
                                [ option
                                    [ value "moderator"
                                    , selected (model.newRole == ModeratorRole)
                                    ]
                                    [ text "Moderator" ]
                                , option
                                    [ value "admin"
                                    , selected (model.newRole == AdminRole)
                                    ]
                                    [ text "Admin" ]
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
                                        "Creating…"

                                     else
                                        "Create"
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , case model.deleteTarget of
            Just _ ->
                div [ class "modal-overlay" ]
                    [ div [ class "modal-box" ]
                        [ p [] [ text "Delete this user?" ]
                        , button [ onClick ConfirmDeleteUser, class "btn-danger" ] [ text "Delete" ]
                        , text " "
                        , button [ onClick CancelDelete ] [ text "Cancel" ]
                        ]
                    ]

            Nothing ->
                text ""
        ]


userRow : Model -> User -> Html Msg
userRow model user =
    tr []
        [ td [] [ text (String.fromInt user.id) ]
        , td [] [ text user.username ]
        , td []
            [ text
                (case user.role of
                    AdminRole ->
                        "admin"

                    ModeratorRole ->
                        "moderator"
                )
            ]
        , td []
            (List.intersperse (text ", ")
                (List.map
                    (\board ->
                        span []
                            [ text board
                            , text " "
                            , button
                                [ onClick (RevokeBoard user.id board)
                                , class "mod-delete-btn"
                                ]
                                [ text "✕" ]
                            ]
                    )
                    user.modBoards
                )
                ++ (if model.assignUserId == Just user.id then
                        [ text " "
                        , input
                            [ type_ "text"
                            , value model.assignBoard
                            , onInput SetAssignBoard
                            , placeholder "board name"
                            ]
                            []
                        , button [ onClick SubmitAssign ] [ text "Assign" ]
                        , button [ onClick (StartAssign -1) ] [ text "✕" ]
                        ]

                    else
                        [ text " "
                        , button [ onClick (StartAssign user.id) ] [ text "[+board]" ]
                        ]
                   )
            )
        , td []
            [ button [ onClick (AskDeleteUser user.id), class "mod-delete-btn" ] [ text "[Delete]" ] ]
        ]


