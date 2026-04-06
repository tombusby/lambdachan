module Types exposing
    ( Board
    , BoardCatalog
    , CreatePostBody
    , CreateThreadBody
    , ImageUpload
    , Post
    , Session
    , ThreadDetail
    , ThreadSummary
    , User
    , UserRole(..)
    , boardDecoder
    , boardCatalogDecoder
    , postDecoder
    , sessionDecoder
    , sessionEncoder
    , threadDetailDecoder
    , threadSummaryDecoder
    , userDecoder
    , userRoleDecoder
    , userRoleEncoder
    )

import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Time exposing (Posix)


-- ---------------------------------------------------------------------------
-- Core types
-- ---------------------------------------------------------------------------

type alias Board =
    { id : Int
    , name : String
    , title : String
    , description : String
    , createdAt : Posix
    }

type alias Post =
    { id : Int
    , authorName : String
    , tripcode : Maybe String
    , content : String
    , imageData : Maybe String
    , imageName : Maybe String
    , imageMimeType : Maybe String
    , createdAt : Posix
    }

type alias ThreadSummary =
    { id : Int
    , subject : Maybe String
    , bumpedAt : Posix
    , createdAt : Posix
    , isLocked : Bool
    , isSticky : Bool
    , postCount : Int
    , opPost : Post
    }

type alias ThreadDetail =
    { thread : ThreadSummary
    , posts : List Post
    }

type alias BoardCatalog =
    { board : Board
    , threads : List ThreadSummary
    }

type UserRole
    = AdminRole
    | ModeratorRole

type alias User =
    { id : Int
    , username : String
    , role : UserRole
    , modBoards : List String
    }

type alias Session =
    { token : String
    , user : User
    }

-- ---------------------------------------------------------------------------
-- Request body types
-- ---------------------------------------------------------------------------

type alias ImageUpload =
    { data_ : String
    , filename : String
    , mimeType : String
    }

type alias CreateThreadBody =
    { subject : Maybe String
    , content : String
    , authorName : String
    , image : Maybe ImageUpload
    }

type alias CreatePostBody =
    { content : String
    , authorName : String
    , image : Maybe ImageUpload
    }

-- ---------------------------------------------------------------------------
-- Decoders
-- ---------------------------------------------------------------------------

boardDecoder : Decoder Board
boardDecoder =
    D.map5 Board
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "title" D.string)
        (D.field "description" D.string)
        (D.field "createdAt" Iso8601.decoder)

postDecoder : Decoder Post
postDecoder =
    D.map8 Post
        (D.field "id" D.int)
        (D.field "authorName" D.string)
        (D.field "tripcode" (D.nullable D.string))
        (D.field "content" D.string)
        (D.field "imageData" (D.nullable D.string))
        (D.field "imageName" (D.nullable D.string))
        (D.field "imageMimeType" (D.nullable D.string))
        (D.field "createdAt" Iso8601.decoder)

threadSummaryDecoder : Decoder ThreadSummary
threadSummaryDecoder =
    D.map8 ThreadSummary
        (D.field "id" D.int)
        (D.field "subject" (D.nullable D.string))
        (D.field "bumpedAt" Iso8601.decoder)
        (D.field "createdAt" Iso8601.decoder)
        (D.field "isLocked" D.bool)
        (D.field "isSticky" D.bool)
        (D.field "postCount" D.int)
        (D.field "opPost" postDecoder)

threadDetailDecoder : Decoder ThreadDetail
threadDetailDecoder =
    D.map2 ThreadDetail
        (D.field "thread" threadSummaryDecoder)
        (D.field "posts" (D.list postDecoder))

boardCatalogDecoder : Decoder BoardCatalog
boardCatalogDecoder =
    D.map2 BoardCatalog
        (D.field "board" boardDecoder)
        (D.field "threads" (D.list threadSummaryDecoder))

userRoleDecoder : Decoder UserRole
userRoleDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "admin" ->
                        D.succeed AdminRole

                    "moderator" ->
                        D.succeed ModeratorRole

                    _ ->
                        D.fail ("Unknown role: " ++ s)
            )

userDecoder : Decoder User
userDecoder =
    D.map4 User
        (D.field "id" D.int)
        (D.field "username" D.string)
        (D.field "role" userRoleDecoder)
        (D.field "modBoards" (D.list D.string))

sessionDecoder : Decoder Session
sessionDecoder =
    D.map2 Session
        (D.field "token" D.string)
        (D.field "user" userDecoder)

-- ---------------------------------------------------------------------------
-- Encoders
-- ---------------------------------------------------------------------------

userRoleEncoder : UserRole -> E.Value
userRoleEncoder role =
    case role of
        AdminRole ->
            E.string "admin"

        ModeratorRole ->
            E.string "moderator"

sessionEncoder : Session -> E.Value
sessionEncoder s =
    E.object
        [ ( "token", E.string s.token )
        , ( "user"
          , E.object
                [ ( "id", E.int s.user.id )
                , ( "username", E.string s.user.username )
                , ( "role", userRoleEncoder s.user.role )
                , ( "modBoards", E.list E.string s.user.modBoards )
                ]
          )
        ]

imageUploadEncoder : ImageUpload -> E.Value
imageUploadEncoder img =
    E.object
        [ ( "data", E.string img.data_ )
        , ( "filename", E.string img.filename )
        , ( "mimeType", E.string img.mimeType )
        ]
