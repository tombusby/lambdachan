module Thread exposing
    ( Comment
    , Thread
    , examples
    , renderCommentBox
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
    , commentText : String
    , replies : List Comment
    }


type alias Comment =
    { mpicURL : Maybe String
    , commentText : String
    }


renderThread : Thread -> List (Element msg)
renderThread thread =
    List.map renderCommentBox thread.replies


renderCommentBox : Comment -> Element msg
renderCommentBox { mpicURL, commentText } =
    el [ paddingXY 0 5 ] <|
        column
            [ alignLeft
            , Font.size 14
            , spacing 10
            , padding 20
            , width shrink
            , Background.color Palette.commentBoxBG
            , Border.color Palette.commentBoxBorder
            , Border.solid
            , Border.width 1
            ]
            [ row [ spacing 5 ]
                [ el [ Font.color Palette.green, Font.heavy ] <|
                    text "Anonymous"
                , text "(ID: 2423424)"
                , text "25/02/20(Tue)20:34:37 No.1234567890"
                , el [ Font.color Palette.blue ] <| text "▶"
                , row [ Font.color Palette.blue, Font.underline ]
                    [ link []
                        { url = "#"
                        , label = text ">>34234324"
                        }
                    ]
                ]
            , row
                [ paddingXY 0 10 ]
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
                                , description = "banner"
                                }
                    , text commentText
                    ]
                ]
            ]


examples : Thread
examples =
    { picURL = "assets/img/banner.png"
    , commentText = "This is the OP"
    , id = 12343245
    , replies =
        [ { mpicURL = Nothing
          , commentText = "blah blah shitpost"
          }
        , { mpicURL = Just "assets/img/banner.png"
          , commentText = """
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
          }
        ]
    }
