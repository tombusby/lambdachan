module Page.Catalog exposing (Model, Msg, init, update, view)

import Api
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Session as Sess
import Types exposing (BoardCatalog, Session, ThreadSummary)
import View.Modal as Modal
import View.Post as Post
import View.PostForm as PostForm


type alias Model =
    { boardName : String
    , catalog : Maybe BoardCatalog
    , notFound : Bool
    , error : Maybe String
    , showNewThread : Bool
    , postForm : PostForm.Model
    , submitting : Bool
    , deleteTarget : Maybe Int
    }


type Msg
    = GotCatalog (Result Http.Error BoardCatalog)
    | ToggleNewThread
    | FormMsg PostForm.Msg
    | SubmitThread
    | ThreadCreated (Result Http.Error ThreadSummary)
    | AskDeleteThread Int
    | CancelDelete
    | ConfirmDeleteThread
    | ThreadDeleted (Result Http.Error ())
    | ToggleSticky Int Bool
    | StickySet (Result Http.Error ())
    | ToggleLocked Int Bool
    | LockSet (Result Http.Error ())


init : String -> ( Model, Cmd Msg )
init boardName =
    ( { boardName = boardName
      , catalog = Nothing
      , notFound = False
      , error = Nothing
      , showNewThread = False
      , postForm = PostForm.init
      , submitting = False
      , deleteTarget = Nothing
      }
    , Api.getBoard boardName GotCatalog
    )


update : Maybe Session -> Msg -> Model -> ( Model, Cmd Msg )
update maybeSession msg model =
    case msg of
        GotCatalog (Ok catalog) ->
            ( { model | catalog = Just catalog, error = Nothing }, Cmd.none )

        GotCatalog (Err (Http.BadStatus 404)) ->
            ( { model | notFound = True }, Cmd.none )

        GotCatalog (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )

        ToggleNewThread ->
            ( { model | showNewThread = not model.showNewThread }, Cmd.none )

        FormMsg subMsg ->
            let
                ( newForm, cmd ) =
                    PostForm.update subMsg model.postForm
            in
            ( { model | postForm = newForm }, Cmd.map FormMsg cmd )

        SubmitThread ->
            ( { model | submitting = True }
            , Api.createThread model.boardName (PostForm.toThreadBody model.postForm) maybeSession ThreadCreated
            )

        ThreadCreated (Ok _) ->
            ( { model
                | submitting = False
                , showNewThread = False
                , postForm = PostForm.init
              }
            , Api.getBoard model.boardName GotCatalog
            )

        ThreadCreated (Err err) ->
            ( { model | submitting = False, error = Just (Api.httpErrorToString err) }, Cmd.none )

        AskDeleteThread threadId ->
            ( { model | deleteTarget = Just threadId }, Cmd.none )

        CancelDelete ->
            ( { model | deleteTarget = Nothing }, Cmd.none )

        ConfirmDeleteThread ->
            case ( model.deleteTarget, maybeSession ) of
                ( Just threadId, Just session ) ->
                    ( { model | deleteTarget = Nothing }
                    , Api.deleteThread model.boardName threadId session ThreadDeleted
                    )

                _ ->
                    ( { model | deleteTarget = Nothing }, Cmd.none )

        ThreadDeleted (Ok ()) ->
            ( model, Api.getBoard model.boardName GotCatalog )

        ThreadDeleted (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )

        ToggleSticky threadId val ->
            case maybeSession of
                Just session ->
                    ( model, Api.setSticky model.boardName threadId val session StickySet )

                Nothing ->
                    ( model, Cmd.none )

        StickySet (Ok ()) ->
            ( model, Api.getBoard model.boardName GotCatalog )

        StickySet (Err _) ->
            ( model, Cmd.none )

        ToggleLocked threadId val ->
            case maybeSession of
                Just session ->
                    ( model, Api.setLocked model.boardName threadId val session LockSet )

                Nothing ->
                    ( model, Cmd.none )

        LockSet (Ok ()) ->
            ( model, Api.getBoard model.boardName GotCatalog )

        LockSet (Err _) ->
            ( model, Cmd.none )


view : Maybe Session -> Model -> Html Msg
view maybeSession model =
    let
        canMod =
            Maybe.map Sess.isMod maybeSession |> Maybe.withDefault False
    in
    div [ class "catalog-page" ]
        [ if model.notFound then
            div [ class "not-found" ]
                [ h1 [] [ text "404" ]
                , p [] [ text ("There is no board /" ++ model.boardName ++ "/.") ]
                , p [] [ a [ href "/" ] [ text "← Return to board list" ] ]
                ]

          else
          case model.catalog of
            Nothing ->
                div [ class "loading" ] [ text "Loading…" ]

            Just catalog ->
                div []
                    [ div [ class "board-header" ]
                        [ h1 [] [ text ("/" ++ catalog.board.name ++ "/ — " ++ catalog.board.title) ]
                        , p [ class "board-desc" ] [ text catalog.board.description ]
                        ]
                    , div [ class "catalog-controls" ]
                        [ button [ onClick ToggleNewThread ]
                            [ text
                                (if model.showNewThread then
                                    "▲ Hide form"

                                 else
                                    "[ Start a new thread ]"
                                )
                            ]
                        ]
                    , if model.showNewThread then
                        PostForm.viewWithSubject
                            { model = model.postForm
                            , onMsg = FormMsg
                            , onSubmit = SubmitThread
                            , submitLabel = "Post"
                            }

                      else
                        text ""
                    , case model.error of
                        Just err ->
                            div [ class "error-msg" ] [ text err ]

                        Nothing ->
                            text ""
                    , div [ class "thread-list" ]
                        (List.map (threadCard catalog.board.name canMod) catalog.threads)
                    ]
        , Modal.view
            { visible = model.deleteTarget /= Nothing
            , message = "Delete this thread and all its replies?"
            , onConfirm = ConfirmDeleteThread
            , onCancel = CancelDelete
            }
        ]


threadCard : String -> Bool -> ThreadSummary -> Html Msg
threadCard boardName canMod thread =
    div [ class "thread-card" ]
        [ div [ class "thread-card-header" ]
            [ a [ href ("/" ++ boardName ++ "/" ++ String.fromInt thread.id) ]
                [ text
                    (case thread.subject of
                        Just s ->
                            s

                        Nothing ->
                            "No subject"
                    )
                ]
            , text (" — " ++ String.fromInt thread.postCount ++ " posts")
            , if thread.isSticky then
                span [ class "badge badge-sticky" ] [ text " [Sticky]" ]

              else
                text ""
            , if thread.isLocked then
                span [ class "badge badge-locked" ] [ text " [Locked]" ]

              else
                text ""
            , if canMod then
                span [ class "mod-controls" ]
                    [ text " "
                    , button [ onClick (AskDeleteThread thread.id), class "mod-delete-btn" ] [ text "[Del]" ]
                    , text " "
                    , button [ onClick (ToggleSticky thread.id (not thread.isSticky)) ]
                        [ text (if thread.isSticky then "[Unsticky]" else "[Sticky]") ]
                    , text " "
                    , button [ onClick (ToggleLocked thread.id (not thread.isLocked)) ]
                        [ text (if thread.isLocked then "[Unlock]" else "[Lock]") ]
                    ]

              else
                text ""
            ]
        , Post.viewCompact { post = thread.opPost }
        ]
