module Thread exposing
    ( Comment
    , Thread
    , examples
    , renderThread
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Palette


type alias Thread =
    { picURL : String
    , id : Int
    , threadTitle : String
    , commentText : String
    , replies : List Comment
    }


type alias Comment =
    { mpicURL : Maybe String
    , commentText : String
    }


renderThread : Thread -> Element msg
renderThread thread =
    column [ Font.size 14, spacing 10 ] <|
        [ renderOP thread
        , el [ paddingXY 0 5 ] none
        ]
            ++ List.map renderCommentBox thread.replies


renderInfoText : Maybe String -> Element msg
renderInfoText mtitle =
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
            , text "(ID: 2423424)"
            , text "25/02/20(Tue)20:34:37 No.1234567890"
            , el [ Font.color Palette.links ] <| text "▶"
            , link [ Font.color Palette.links, Font.underline ]
                { url = "#"
                , label = text ">>34234324"
                }
            ]
        , row [ paddingXY 0 5 ] []
        ]


renderOP : Thread -> Element msg
renderOP { picURL, threadTitle, commentText } =
    paragraph []
        [ image
            [ alignLeft
            , width (fill |> maximum 200)
            , padding 5
            ]
            { src = picURL
            , description = "OP image"
            }
        , renderInfoText <| Just threadTitle
        , paragraph [] [ text commentText ]
        ]


renderCommentBox : Comment -> Element msg
renderCommentBox { mpicURL, commentText } =
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
            , renderInfoText Nothing
            , paragraph [] [ text commentText ]
            ]
        ]


veryLongCommentText : String
veryLongCommentText =
    """
    blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg
    blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg
    blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg
    blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg
    blah blah shitpost dfgdfgdfgfdsgdsg sdfdsfd dsfgsdfgsdfgsdfgsfd
    dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdf
    dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg dfgdgfdsgs
    dgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgs
    dfgsfdg dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsd
    gsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg
    dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg
    dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg
    dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg
    dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsdgsdgf"""


examples : Thread
examples =
    { picURL = "assets/img/banner.png"
    , id = 12343245
    , threadTitle = "Aw shit it's the coronavirus"
    , commentText = veryLongCommentText ++ veryLongCommentText
    , replies =
        [ { mpicURL = Nothing
          , commentText = "blah blah shitpost"
          }
        , { mpicURL = Just "assets/img/banner.png"
          , commentText = veryLongCommentText
          }
        , { mpicURL = Just "assets/img/banner.png"
          , commentText = "blah blah shitpost"
          }
        ]
    }
