module Page.Thread exposing (Model, Msg, init, update, view)

import Api
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Session as Sess
import Task
import Time exposing (Zone)
import Types exposing (Post, Session, ThreadDetail)
import View.Modal as Modal
import View.Post as Post
import View.PostForm as PostForm


type alias Model =
    { boardName : String
    , threadId : Int
    , detail : Maybe ThreadDetail
    , error : Maybe String
    , zone : Zone
    , showReplyForm : Bool
    , postForm : PostForm.Model
    , submitting : Bool
    , deletePostTarget : Maybe Int
    , deleteThreadConfirm : Bool
    }


type Msg
    = GotThread (Result Http.Error ThreadDetail)
    | GotZone Zone
    | ToggleReplyForm
    | FormMsg PostForm.Msg
    | SubmitPost
    | PostCreated (Result Http.Error Post)
    | AskDeletePost Int
    | CancelDelete
    | ConfirmDeletePost
    | PostDeleted (Result Http.Error ())
    | AskDeleteThread
    | ConfirmDeleteThread
    | ThreadDeleted (Result Http.Error ())
    | ToggleSticky Bool
    | StickySet (Result Http.Error ())
    | ToggleLocked Bool
    | LockSet (Result Http.Error ())


init : String -> Int -> ( Model, Cmd Msg )
init boardName threadId =
    ( { boardName = boardName
      , threadId = threadId
      , detail = Nothing
      , error = Nothing
      , zone = Time.utc
      , showReplyForm = False
      , postForm = PostForm.init
      , submitting = False
      , deletePostTarget = Nothing
      , deleteThreadConfirm = False
      }
    , Cmd.batch
        [ Api.getThread boardName threadId GotThread
        , Task.perform GotZone Time.here
        ]
    )


update : Maybe Session -> Msg -> Model -> ( Model, Cmd Msg )
update maybeSession msg model =
    case msg of
        GotThread (Ok detail) ->
            ( { model | detail = Just detail, error = Nothing }, Cmd.none )

        GotThread (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        ToggleReplyForm ->
            ( { model | showReplyForm = not model.showReplyForm }, Cmd.none )

        FormMsg subMsg ->
            let
                ( newForm, cmd ) =
                    PostForm.update subMsg model.postForm
            in
            ( { model | postForm = newForm }, Cmd.map FormMsg cmd )

        SubmitPost ->
            ( { model | submitting = True }
            , Api.createPost model.boardName model.threadId (PostForm.toPostBody model.postForm) maybeSession PostCreated
            )

        PostCreated (Ok _) ->
            ( { model
                | submitting = False
                , showReplyForm = False
                , postForm = PostForm.init
              }
            , Api.getThread model.boardName model.threadId GotThread
            )

        PostCreated (Err err) ->
            ( { model | submitting = False, error = Just (Api.httpErrorToString err) }, Cmd.none )

        AskDeletePost postId ->
            ( { model | deletePostTarget = Just postId }, Cmd.none )

        CancelDelete ->
            ( { model | deletePostTarget = Nothing, deleteThreadConfirm = False }, Cmd.none )

        ConfirmDeletePost ->
            case ( model.deletePostTarget, maybeSession ) of
                ( Just postId, Just session ) ->
                    ( { model | deletePostTarget = Nothing }
                    , Api.deletePost model.boardName model.threadId postId session PostDeleted
                    )

                _ ->
                    ( { model | deletePostTarget = Nothing }, Cmd.none )

        PostDeleted (Ok ()) ->
            ( model, Api.getThread model.boardName model.threadId GotThread )

        PostDeleted (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )

        AskDeleteThread ->
            ( { model | deleteThreadConfirm = True }, Cmd.none )

        ConfirmDeleteThread ->
            case maybeSession of
                Just session ->
                    ( { model | deleteThreadConfirm = False }
                    , Api.deleteThread model.boardName model.threadId session ThreadDeleted
                    )

                Nothing ->
                    ( { model | deleteThreadConfirm = False }, Cmd.none )

        ThreadDeleted (Ok ()) ->
            -- Navigate back to catalog
            ( model, Api.getThread model.boardName model.threadId GotThread )

        ThreadDeleted (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )

        ToggleSticky val ->
            case maybeSession of
                Just session ->
                    ( model, Api.setSticky model.boardName model.threadId val session StickySet )

                Nothing ->
                    ( model, Cmd.none )

        StickySet (Ok ()) ->
            ( model, Api.getThread model.boardName model.threadId GotThread )

        StickySet (Err _) ->
            ( model, Cmd.none )

        ToggleLocked val ->
            case maybeSession of
                Just session ->
                    ( model, Api.setLocked model.boardName model.threadId val session LockSet )

                Nothing ->
                    ( model, Cmd.none )

        LockSet (Ok ()) ->
            ( model, Api.getThread model.boardName model.threadId GotThread )

        LockSet (Err _) ->
            ( model, Cmd.none )


view : Maybe Session -> Model -> Html Msg
view maybeSession model =
    let
        canMod =
            Maybe.map Sess.isMod maybeSession |> Maybe.withDefault False
    in
    div [ class "thread-page" ]
        [ case model.error of
            Just err ->
                div [ class "error-msg" ] [ text err ]

            Nothing ->
                text ""
        , case model.detail of
            Nothing ->
                div [ class "loading" ] [ text "Loading…" ]

            Just detail ->
                let
                    thread =
                        detail.thread

                    isLocked =
                        thread.isLocked
                in
                div []
                    [ div [ class "board-header" ]
                        [ h1 []
                            [ a [ href ("/b/" ++ model.boardName) ]
                                [ text ("/" ++ model.boardName ++ "/") ]
                            , text (" — " ++ Maybe.withDefault "No subject" thread.subject)
                            ]
                        , if canMod then
                            div [ class "mod-controls" ]
                                [ button [ onClick AskDeleteThread, class "mod-delete-btn" ] [ text "[Delete Thread]" ]
                                , text " "
                                , button [ onClick (ToggleSticky (not thread.isSticky)) ]
                                    [ text (if thread.isSticky then "[Unsticky]" else "[Sticky]") ]
                                , text " "
                                , button [ onClick (ToggleLocked (not thread.isLocked)) ]
                                    [ text (if thread.isLocked then "[Unlock]" else "[Lock]") ]
                                ]

                          else
                            text ""
                        ]
                    , div [ class "post-list" ]
                        (List.map
                            (\p ->
                                Post.view
                                    { post = p
                                    , zone = model.zone
                                    , modActions =
                                        if canMod then
                                            Just AskDeletePost

                                        else
                                            Nothing
                                    }
                            )
                            detail.posts
                        )
                    , if isLocked then
                        div [ class "locked-notice" ] [ text "This thread is locked." ]

                      else
                        div [ class "reply-controls" ]
                            [ button [ onClick ToggleReplyForm ]
                                [ text
                                    (if model.showReplyForm then
                                        "▲ Hide reply form"

                                     else
                                        "[ Post a reply ]"
                                    )
                                ]
                            , if model.showReplyForm then
                                PostForm.view
                                    { model = model.postForm
                                    , onMsg = FormMsg
                                    , onSubmit = SubmitPost
                                    , submitLabel = "Post"
                                    }

                              else
                                text ""
                            ]
                    ]
        , Modal.view
            { visible = model.deletePostTarget /= Nothing
            , message = "Delete this post?"
            , onConfirm = ConfirmDeletePost
            , onCancel = CancelDelete
            }
        , Modal.view
            { visible = model.deleteThreadConfirm
            , message = "Delete this thread and all its replies?"
            , onConfirm = ConfirmDeleteThread
            , onCancel = CancelDelete
            }
        ]
