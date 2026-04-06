module Main exposing (main)

import Api
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Json.Decode as D
import Page.Admin as Admin
import Page.BoardList as BoardList
import Page.Catalog as Catalog
import Page.Login as Login
import Page.Thread as Thread
import Route exposing (Route(..))
import Session
import Types exposing (Board, Session, sessionDecoder)
import Url exposing (Url)
import View.Nav as NavView


-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

type PageModel
    = BoardListModel BoardList.Model
    | CatalogModel Catalog.Model
    | ThreadModel Thread.Model
    | LoginModel Login.Model
    | AdminModel Admin.Model
    | NotFound


type alias Model =
    { key : Nav.Key
    , session : Maybe Session
    , page : PageModel
    , notice : Maybe String
    , boards : List Board
    }


-- ---------------------------------------------------------------------------
-- Init
-- ---------------------------------------------------------------------------

init : Maybe String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        maybeSession =
            flags
                |> Maybe.andThen
                    (\s ->
                        case D.decodeString sessionDecoder s of
                            Ok sess ->
                                Just sess

                            Err _ ->
                                Nothing
                    )

        ( pageModel, pageCmd ) =
            routeToPage maybeSession (Route.fromUrl url)
    in
    ( { key = key
      , session = maybeSession
      , page = pageModel
      , notice = Nothing
      , boards = []
      }
    , Cmd.batch
        [ Cmd.map PageMsg pageCmd
        , Api.getBoards GotBoards
        ]
    )


-- ---------------------------------------------------------------------------
-- Msg
-- ---------------------------------------------------------------------------

type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest
    | PageMsg PageMsg
    | GotBoards (Result Http.Error (List Board))
    | Logout
    | LogoutDone (Result Http.Error ())
    | DismissNotice


type PageMsg
    = BoardListMsg BoardList.Msg
    | CatalogMsg Catalog.Msg
    | ThreadMsg Thread.Msg
    | LoginMsg Login.Msg
    | AdminMsg Admin.Msg


-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (External url) ->
            ( model, Nav.load url )

        UrlChanged url ->
            let
                ( pageModel, pageCmd ) =
                    routeToPage model.session (Route.fromUrl url)
            in
            ( { model | page = pageModel, notice = Nothing }
            , Cmd.map PageMsg pageCmd
            )

        GotBoards (Ok boards) ->
            ( { model | boards = boards }, Cmd.none )

        GotBoards (Err _) ->
            ( model, Cmd.none )

        Logout ->
            case model.session of
                Just session ->
                    ( model, Api.logout session LogoutDone )

                Nothing ->
                    ( model, Cmd.none )

        LogoutDone _ ->
            ( { model | session = Nothing, notice = Nothing }
            , Session.clearSession
            )

        DismissNotice ->
            ( { model | notice = Nothing }, Cmd.none )

        PageMsg pageMsg ->
            updatePage pageMsg model


updatePage : PageMsg -> Model -> ( Model, Cmd Msg )
updatePage pageMsg model =
    case ( pageMsg, model.page ) of
        ( BoardListMsg subMsg, BoardListModel subModel ) ->
            let
                ( newSubModel, cmd ) =
                    BoardList.update subMsg subModel
            in
            ( { model | page = BoardListModel newSubModel }
            , Cmd.map (PageMsg << BoardListMsg) cmd
            )

        ( CatalogMsg subMsg, CatalogModel subModel ) ->
            let
                ( newSubModel, cmd ) =
                    Catalog.update model.session subMsg subModel
            in
            ( { model | page = CatalogModel newSubModel }
            , Cmd.map (PageMsg << CatalogMsg) cmd
            )

        ( ThreadMsg subMsg, ThreadModel subModel ) ->
            let
                ( newSubModel, cmd ) =
                    Thread.update model.session subMsg subModel
            in
            ( { model | page = ThreadModel newSubModel }
            , Cmd.map (PageMsg << ThreadMsg) cmd
            )

        ( LoginMsg (Login.LoginResult (Ok session)), LoginModel _ ) ->
            ( { model | session = Just session, notice = Nothing }
            , Cmd.batch
                [ Session.saveSession session
                , Nav.pushUrl model.key "/"
                ]
            )

        ( LoginMsg subMsg, LoginModel subModel ) ->
            let
                ( newSubModel, cmd ) =
                    Login.update subMsg subModel
            in
            ( { model | page = LoginModel newSubModel }
            , Cmd.map (PageMsg << LoginMsg) cmd
            )

        ( AdminMsg subMsg, AdminModel subModel ) ->
            case model.session of
                Just session ->
                    let
                        ( newSubModel, cmd ) =
                            Admin.update session subMsg subModel
                    in
                    ( { model | page = AdminModel newSubModel }
                    , Cmd.map (PageMsg << AdminMsg) cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


-- ---------------------------------------------------------------------------
-- Routing helper
-- ---------------------------------------------------------------------------

routeToPage : Maybe Session -> Maybe Route -> ( PageModel, Cmd PageMsg )
routeToPage maybeSession maybeRoute =
    case maybeRoute of
        Nothing ->
            ( NotFound, Cmd.none )

        Just BoardList ->
            let
                ( m, cmd ) =
                    BoardList.init
            in
            ( BoardListModel m, Cmd.map BoardListMsg cmd )

        Just (Catalog boardName) ->
            let
                ( m, cmd ) =
                    Catalog.init boardName
            in
            ( CatalogModel m, Cmd.map CatalogMsg cmd )

        Just (Thread boardName threadId) ->
            let
                ( m, cmd ) =
                    Thread.init boardName threadId
            in
            ( ThreadModel m, Cmd.map ThreadMsg cmd )

        Just Login ->
            ( LoginModel Login.init, Cmd.none )

        Just Admin ->
            case maybeSession of
                Just session ->
                    let
                        ( m, cmd ) =
                            Admin.init session
                    in
                    ( AdminModel m, Cmd.map AdminMsg cmd )

                Nothing ->
                    ( NotFound, Cmd.none )


-- ---------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------

view : Model -> Document Msg
view model =
    { title = "λchan"
    , body =
        [ NavView.view
                { session = model.session
                , boards = model.boards
                , notice = model.notice
                , onLogout = Logout
                }
        , div [ class "page-content" ]
            [ viewPage model ]
        , footer [ class "site-footer" ]
            [ text "λchan — powered by Servant and Elm" ]
        ]
    }


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        NotFound ->
            div [ class "not-found" ] [ text "404 — page not found" ]

        BoardListModel subModel ->
            Html.map (PageMsg << BoardListMsg) (BoardList.view model.session subModel)

        CatalogModel subModel ->
            Html.map (PageMsg << CatalogMsg) (Catalog.view model.session subModel)

        ThreadModel subModel ->
            Html.map (PageMsg << ThreadMsg) (Thread.view model.session subModel)

        LoginModel subModel ->
            Html.map (PageMsg << LoginMsg) (Login.view subModel)

        AdminModel subModel ->
            Html.map (PageMsg << AdminMsg) (Admin.view subModel)


-- ---------------------------------------------------------------------------
-- Subscriptions
-- ---------------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
