module View.Nav exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events
import Types exposing (Board, Session, UserRole(..))


view :
    { session : Maybe Session
    , boards : List Board
    , notice : Maybe String
    , onLogout : msg
    }
    -> Html msg
view { session, boards, notice, onLogout } =
    div []
        [ div [ class "nav-bar" ]
            [ span [ class "nav-logo" ]
                [ a [ href "/" ] [ text "λchan" ] ]
            , span [ class "nav-boards" ]
                (List.intersperse (text " / ")
                    (List.map boardLink boards)
                )
            , span [ class "nav-auth" ]
                (authControls session onLogout)
            ]
        , case notice of
            Just msg ->
                div [ class "notice-bar" ] [ text msg ]

            Nothing ->
                text ""
        ]


boardLink : Board -> Html msg
boardLink board =
    a [ href ("/" ++ board.name) ]
        [ text ("/" ++ board.name ++ "/") ]


authControls : Maybe Session -> msg -> List (Html msg)
authControls maybeSession onLogout =
    case maybeSession of
        Nothing ->
            [ a [ href "/login" ] [ text "[Login]" ] ]

        Just session ->
            List.concat
                [ if session.user.role == AdminRole then
                    [ a [ href "/admin" ] [ text "[Admin]" ]
                    , text " "
                    ]

                  else
                    []
                , [ span [] [ text session.user.username ]
                  , text " "
                  , button
                        [ Html.Events.onClick onLogout
                        , class "nav-logout"
                        ]
                        [ text "[Logout]" ]
                  ]
                ]


