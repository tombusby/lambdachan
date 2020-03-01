module Thread exposing
    ( Thread
    , commentDecoder
    , renderThread
    , threadDecoder
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Json.Decode as Decode
    exposing
        ( Decoder
        , field
        , int
        , list
        , nullable
        , string
        )
import Palette
import Time
import Time.Format as TimeFormat


type alias Post a =
    { a
        | id : Int
        , time : Time.Posix
        , commentText : String
    }


type alias Thread =
    Post
        { picURL : String
        , threadTitle : String
        , replies : List Comment
        }


type alias Comment =
    Post
        { mpicURL : Maybe String
        }


renderThread : Time.Zone -> Thread -> Element msg
renderThread timezone thread =
    column [ Font.size 14, spacing 10 ] <|
        [ renderOP timezone thread
        , el [ paddingXY 0 5 ] none
        ]
            ++ List.map (renderCommentBox timezone) thread.replies


renderPostTime : Time.Zone -> Post a -> Element msg
renderPostTime timezone { time } =
    let
        formatString =
            "padDay/Month/Year(Weekday)padHour:padMinute:padSecond"
    in
    text << TimeFormat.format timezone formatString <| Time.posixToMillis time


renderInfoText : Time.Zone -> Maybe String -> Post a -> Element msg
renderInfoText timezone mtitle post =
    column []
        [ row [ spacing 5, width fill ]
            [ case mtitle of
                Nothing ->
                    none

                Just title ->
                    el [ Font.color Palette.threadTitle, Font.heavy ] <|
                        text title
            , el [ Font.color Palette.author, Font.heavy ] <|
                text "Anonymous"
            , renderPostTime timezone post
            , text <| "No:" ++ String.fromInt post.id
            , el [ Font.color Palette.links ] <| text "▶"

            -- TODO: Wire this so that isn't a placeholder
            , link [ Font.color Palette.links, Font.underline ]
                { url = "#"
                , label = text ">>34234324"
                }
            ]
        , row [ paddingXY 0 5 ] []
        ]


renderOP : Time.Zone -> Thread -> Element msg
renderOP timezone ({ picURL, threadTitle, commentText } as op) =
    paragraph []
        [ image
            [ alignLeft
            , width (fill |> maximum 200)
            , padding 5
            ]
            { src = picURL
            , description = "OP image"
            }
        , renderInfoText timezone (Just threadTitle) op
        , paragraph [] [ text commentText ]
        ]


renderCommentBox : Time.Zone -> Comment -> Element msg
renderCommentBox timezone ({ mpicURL, commentText } as reply) =
    row
        [ alignLeft
        , spacing 10
        , padding 20
        , width shrink
        , Background.color Palette.commentBoxBG
        , Border.color Palette.commentBoxBorder
        , Border.solid
        , Border.width 1
        ]
        [ paragraph []
            [ case mpicURL of
                Nothing ->
                    none

                Just url ->
                    image
                        [ alignLeft
                        , width (fill |> maximum 150)
                        , padding 5
                        ]
                        { src = url
                        , description = "thread image"
                        }
            , renderInfoText timezone Nothing reply
            , paragraph [] [ text commentText ]
            ]
        ]


threadDecoder : Decoder Thread
threadDecoder =
    let
        helper id time commentText picURL threadTitle replies =
            { id = id
            , time = time
            , commentText = commentText
            , picURL = picURL
            , threadTitle = threadTitle
            , replies = replies
            }
    in
    Decode.map6 helper
        (field "id" int)
        (Decode.map Time.millisToPosix <| field "time" int)
        (field "commentText" string)
        (field "picURL" string)
        (field "threadTitle" string)
        (field "replies" <| list commentDecoder)


commentDecoder : Decoder Comment
commentDecoder =
    let
        helper id time commentText mpicURL =
            { id = id
            , time = time
            , commentText = commentText
            , mpicURL = mpicURL
            }
    in
    Decode.map4 helper
        (field "id" int)
        (Decode.map Time.millisToPosix <| field "time" int)
        (field "commentText" string)
        (field "mpicURL" <| nullable string)
