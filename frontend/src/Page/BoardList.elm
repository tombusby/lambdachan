module Page.BoardList exposing (Model, Msg, init, update, view)

import Api
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Route
import Types exposing (Board, Session)


type alias Model =
    { boards : List Board
    , error : Maybe String
    }


type Msg
    = GotBoards (Result Http.Error (List Board))


init : ( Model, Cmd Msg )
init =
    ( { boards = [], error = Nothing }
    , Api.getBoards GotBoards
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBoards (Ok boards) ->
            ( { model | boards = boards, error = Nothing }, Cmd.none )

        GotBoards (Err err) ->
            ( { model | error = Just (Api.httpErrorToString err) }, Cmd.none )


view : Maybe Session -> Model -> Html Msg
view maybeSession model =
    div [ class "board-list-page" ]
        [ div [ class "page-banner" ]
            [ h1 [] [ text "λchan" ]
            , p [] [ text "Welcome. Choose a board below." ]
            ]
        , case model.error of
            Just err ->
                div [ class "error-msg" ] [ text err ]

            Nothing ->
                text ""
        , table [ class "board-list-table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Board" ]
                    , th [] [ text "Title" ]
                    , th [] [ text "Description" ]
                    ]
                ]
            , tbody []
                (List.map (boardRow maybeSession) model.boards)
            ]
        ]


boardRow : Maybe Session -> Board -> Html Msg
boardRow _ board =
    tr []
        [ td []
            [ a [ Route.href (Route.Catalog board.name) ]
                [ text ("/" ++ board.name ++ "/") ]
            ]
        , td [] [ text board.title ]
        , td [] [ text board.description ]
        ]
