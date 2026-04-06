module View.Image exposing (thumbnail, fullImage)

import Html exposing (Html, a, img)
import Html.Attributes exposing (alt, href, src, style, target)


dataUrl : String -> String -> String
dataUrl mimeType base64 =
    "data:" ++ mimeType ++ ";base64," ++ base64


thumbnail : String -> String -> String -> Html msg
thumbnail mimeType base64 filename =
    a
        [ href (dataUrl mimeType base64)
        , target "_blank"
        , style "display" "block"
        , style "margin-bottom" "4px"
        ]
        [ img
            [ src (dataUrl mimeType base64)
            , alt filename
            , style "max-width" "150px"
            , style "max-height" "150px"
            , style "border" "1px solid #aaa"
            , style "cursor" "pointer"
            ]
            []
        ]


fullImage : String -> String -> String -> Html msg
fullImage mimeType base64 filename =
    img
        [ src (dataUrl mimeType base64)
        , alt filename
        , style "max-width" "100%"
        ]
        []
