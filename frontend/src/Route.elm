module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , pushUrl
    , toString
    )

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, s, string, top)


type Route
    = BoardList
    | Catalog String
    | Thread String Int
    | Login
    | Admin


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map BoardList top
        , Parser.map Login (s "login")
        , Parser.map Admin (s "admin")
        , Parser.map Thread (string </> int)
        , Parser.map Catalog string
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


toString : Route -> String
toString route =
    case route of
        BoardList ->
            "/"

        Catalog board ->
            "/" ++ board

        Thread board threadId ->
            "/" ++ board ++ "/" ++ String.fromInt threadId

        Login ->
            "/login"

        Admin ->
            "/admin"


href : Route -> Attribute msg
href route =
    Attr.href (toString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (toString route)
