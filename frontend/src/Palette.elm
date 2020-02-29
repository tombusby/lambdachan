module Palette exposing
    ( author
    , background
    , commentBoxBG
    , commentBoxBorder
    , links
    , text
    , threadTitle
    )

import Element exposing (Color, rgb255)


text : Color
text =
    rgb255 180 0 0


threadTitle : Color
threadTitle =
    rgb255 204 17 5


author : Color
author =
    rgb255 17 119 67


links : Color
links =
    rgb255 0 0 238


background : Color
background =
    rgb255 255 255 238


commentBoxBG : Color
commentBoxBG =
    rgb255 240 224 214


commentBoxBorder : Color
commentBoxBorder =
    rgb255 217 191 183
