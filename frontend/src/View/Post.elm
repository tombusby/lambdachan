module View.Post exposing (view, viewCompact)

import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Html.Events
import Iso8601
import Time exposing (Posix, Zone)
import Types exposing (Post, Session)
import View.Image as Image


-- | Full post rendering (used in thread view).
view :
    { post : Post
    , zone : Zone
    , modActions : Maybe (Int -> msg)  -- Nothing = no mod controls
    }
    -> Html msg
view { post, zone, modActions } =
    div
        [ id ("p" ++ String.fromInt post.id)
        , class "post"
        ]
        [ div [ class "post-header" ]
            [ span [ class "post-author" ] [ text post.authorName ]
            , case post.tripcode of
                Just tc ->
                    span [ class "post-tripcode" ] [ text (" " ++ tc) ]

                Nothing ->
                    text ""
            , text " "
            , span [ class "post-date" ] [ text (formatTime zone post.createdAt) ]
            , text " "
            , span [ class "post-num" ]
                [ text ("No." ++ String.fromInt post.id) ]
            , case modActions of
                Just onDelete ->
                    span
                        [ class "mod-action"
                        , style "margin-left" "8px"
                        ]
                        [ deleteButton (onDelete post.id) ]

                Nothing ->
                    text ""
            ]
        , case post.imageData of
            Just b64 ->
                Image.thumbnail
                    (Maybe.withDefault "image/png" post.imageMimeType)
                    b64
                    (Maybe.withDefault "image" post.imageName)

            Nothing ->
                text ""
        , div [ class "post-content" ] [ text post.content ]
        ]


-- | Compact OP preview (used in catalog).
viewCompact :
    { post : Post
    , zone : Zone
    }
    -> Html msg
viewCompact { post, zone } =
    div [ class "post post-compact" ]
        [ case post.imageData of
            Just b64 ->
                Image.thumbnail
                    (Maybe.withDefault "image/png" post.imageMimeType)
                    b64
                    (Maybe.withDefault "image" post.imageName)

            Nothing ->
                text ""
        , div [ class "post-content" ]
            [ text (truncate 200 post.content) ]
        ]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

deleteButton : msg -> Html msg
deleteButton onDelete =
    Html.button
        [ Html.Events.onClick onDelete
        , class "mod-delete-btn"
        ]
        [ text "[Delete]" ]


formatTime : Zone -> Posix -> String
formatTime zone posix =
    let
        y  = String.fromInt (Time.toYear zone posix)
        mo = padTwo (monthToInt (Time.toMonth zone posix))
        d  = padTwo (Time.toDay zone posix)
        h  = padTwo (Time.toHour zone posix)
        mi = padTwo (Time.toMinute zone posix)
        s  = padTwo (Time.toSecond zone posix)
    in
    y ++ "/" ++ mo ++ "/" ++ d ++ "(" ++ dayOfWeek zone posix ++ ")" ++ h ++ ":" ++ mi ++ ":" ++ s


padTwo : Int -> String
padTwo n =
    if n < 10 then
        "0" ++ String.fromInt n
    else
        String.fromInt n


truncate : Int -> String -> String
truncate maxLen s =
    if String.length s <= maxLen then
        s
    else
        String.left maxLen s ++ "…"


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan -> 1
        Time.Feb -> 2
        Time.Mar -> 3
        Time.Apr -> 4
        Time.May -> 5
        Time.Jun -> 6
        Time.Jul -> 7
        Time.Aug -> 8
        Time.Sep -> 9
        Time.Oct -> 10
        Time.Nov -> 11
        Time.Dec -> 12


dayOfWeek : Zone -> Posix -> String
dayOfWeek zone posix =
    case Time.toWeekday zone posix of
        Time.Mon -> "Mon"
        Time.Tue -> "Tue"
        Time.Wed -> "Wed"
        Time.Thu -> "Thu"
        Time.Fri -> "Fri"
        Time.Sat -> "Sat"
        Time.Sun -> "Sun"
