module Api exposing
    ( adminAssignMod
    , adminCreateUser
    , adminDeleteUser
    , adminListUsers
    , adminRevokeMod
    , createBoard
    , createPost
    , createThread
    , deletePost
    , deleteThread
    , getBoard
    , getBoards
    , getThread
    , httpErrorToString
    , login
    , logout
    , setLocked
    , setSticky
    )

import Http
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Types exposing (..)


apiBase : String
apiBase =
    "/api"


authHeader : Maybe Session -> List Http.Header
authHeader maybeSession =
    case maybeSession of
        Nothing ->
            []

        Just s ->
            [ Http.header "Authorization" ("Bearer " ++ s.token) ]


-- ---------------------------------------------------------------------------
-- Boards
-- ---------------------------------------------------------------------------

getBoards : (Result Http.Error (List Board) -> msg) -> Cmd msg
getBoards toMsg =
    Http.get
        { url = apiBase ++ "/boards"
        , expect = Http.expectJson toMsg (D.list boardDecoder)
        }


getBoard : String -> (Result Http.Error BoardCatalog -> msg) -> Cmd msg
getBoard boardName toMsg =
    Http.get
        { url = apiBase ++ "/boards/" ++ boardName
        , expect = Http.expectJson toMsg boardCatalogDecoder
        }


createBoard : String -> String -> String -> Session -> (Result Http.Error Board -> msg) -> Cmd msg
createBoard name title description session toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/boards"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "name", E.string name )
                    , ( "title", E.string title )
                    , ( "description", E.string description )
                    ]
                )
        , expect = Http.expectJson toMsg boardDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


-- ---------------------------------------------------------------------------
-- Threads
-- ---------------------------------------------------------------------------

getThread : String -> Int -> (Result Http.Error ThreadDetail -> msg) -> Cmd msg
getThread boardName threadId toMsg =
    Http.get
        { url = apiBase ++ "/boards/" ++ boardName ++ "/threads/" ++ String.fromInt threadId
        , expect = Http.expectJson toMsg threadDetailDecoder
        }


createThread : String -> CreateThreadBody -> Maybe Session -> (Result Http.Error ThreadSummary -> msg) -> Cmd msg
createThread boardName body maybeSession toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader maybeSession
        , url = apiBase ++ "/boards/" ++ boardName ++ "/threads"
        , body = Http.jsonBody (encodeCreateThread body)
        , expect = Http.expectJson toMsg threadResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteThread : String -> Int -> Session -> (Result Http.Error () -> msg) -> Cmd msg
deleteThread boardName threadId session toMsg =
    Http.request
        { method = "DELETE"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/boards/" ++ boardName ++ "/threads/" ++ String.fromInt threadId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


setSticky : String -> Int -> Bool -> Session -> (Result Http.Error () -> msg) -> Cmd msg
setSticky boardName threadId value session toMsg =
    Http.request
        { method = "PATCH"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/boards/" ++ boardName ++ "/threads/" ++ String.fromInt threadId ++ "/sticky"
        , body = Http.jsonBody (E.object [ ( "value", E.bool value ) ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


setLocked : String -> Int -> Bool -> Session -> (Result Http.Error () -> msg) -> Cmd msg
setLocked boardName threadId value session toMsg =
    Http.request
        { method = "PATCH"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/boards/" ++ boardName ++ "/threads/" ++ String.fromInt threadId ++ "/lock"
        , body = Http.jsonBody (E.object [ ( "value", E.bool value ) ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


-- ---------------------------------------------------------------------------
-- Posts
-- ---------------------------------------------------------------------------

createPost : String -> Int -> CreatePostBody -> Maybe Session -> (Result Http.Error Post -> msg) -> Cmd msg
createPost boardName threadId body maybeSession toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader maybeSession
        , url = apiBase ++ "/boards/" ++ boardName ++ "/threads/" ++ String.fromInt threadId ++ "/posts"
        , body = Http.jsonBody (encodeCreatePost body)
        , expect = Http.expectJson toMsg postDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deletePost : String -> Int -> Int -> Session -> (Result Http.Error () -> msg) -> Cmd msg
deletePost boardName threadId postId session toMsg =
    Http.request
        { method = "DELETE"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/boards/" ++ boardName ++ "/threads/" ++ String.fromInt threadId ++ "/posts/" ++ String.fromInt postId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


-- ---------------------------------------------------------------------------
-- Auth
-- ---------------------------------------------------------------------------

login : String -> String -> (Result Http.Error Session -> msg) -> Cmd msg
login username password toMsg =
    Http.post
        { url = apiBase ++ "/auth/login"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "username", E.string username )
                    , ( "password", E.string password )
                    ]
                )
        , expect = Http.expectJson toMsg sessionDecoder
        }


logout : Session -> (Result Http.Error () -> msg) -> Cmd msg
logout session toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/auth/logout"
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


-- ---------------------------------------------------------------------------
-- Admin
-- ---------------------------------------------------------------------------

adminListUsers : Session -> (Result Http.Error (List User) -> msg) -> Cmd msg
adminListUsers session toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/admin/users"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (D.list userDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


adminCreateUser : String -> String -> UserRole -> Session -> (Result Http.Error User -> msg) -> Cmd msg
adminCreateUser username password role session toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/admin/users"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "username", E.string username )
                    , ( "password", E.string password )
                    , ( "role", userRoleEncoder role )
                    ]
                )
        , expect = Http.expectJson toMsg userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


adminDeleteUser : Int -> Session -> (Result Http.Error () -> msg) -> Cmd msg
adminDeleteUser userId session toMsg =
    Http.request
        { method = "DELETE"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/admin/users/" ++ String.fromInt userId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


adminAssignMod : Int -> String -> Session -> (Result Http.Error () -> msg) -> Cmd msg
adminAssignMod userId boardName session toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/admin/users/" ++ String.fromInt userId ++ "/boards/" ++ boardName
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


adminRevokeMod : Int -> String -> Session -> (Result Http.Error () -> msg) -> Cmd msg
adminRevokeMod userId boardName session toMsg =
    Http.request
        { method = "DELETE"
        , headers = authHeader (Just session)
        , url = apiBase ++ "/admin/users/" ++ String.fromInt userId ++ "/boards/" ++ boardName
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


-- ---------------------------------------------------------------------------
-- Encoders
-- ---------------------------------------------------------------------------

encodeImageUpload : ImageUpload -> E.Value
encodeImageUpload img =
    E.object
        [ ( "data", E.string img.data_ )
        , ( "filename", E.string img.filename )
        , ( "mimeType", E.string img.mimeType )
        ]


encodeCreateThread : CreateThreadBody -> E.Value
encodeCreateThread body =
    E.object
        [ ( "subject", encodeMaybe E.string body.subject )
        , ( "content", E.string body.content )
        , ( "authorName", E.string body.authorName )
        , ( "image", encodeMaybe encodeImageUpload body.image )
        ]


encodeCreatePost : CreatePostBody -> E.Value
encodeCreatePost body =
    E.object
        [ ( "content", E.string body.content )
        , ( "authorName", E.string body.authorName )
        , ( "image", encodeMaybe encodeImageUpload body.image )
        ]


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe enc maybeVal =
    case maybeVal of
        Nothing ->
            E.null

        Just val ->
            enc val


-- The createThread endpoint returns a ThreadResponse (id, boardName, subject, createdAt, opPost)
-- We decode it into a ThreadSummary-compatible shape for simplicity.
threadResponseDecoder : D.Decoder ThreadSummary
threadResponseDecoder =
    D.map8 ThreadSummary
        (D.field "id" D.int)
        (D.field "subject" (D.nullable D.string))
        (D.field "createdAt" Iso8601.decoder)
        (D.field "createdAt" Iso8601.decoder)
        (D.succeed False)
        (D.succeed False)
        (D.succeed 1)
        (D.field "opPost" postDecoder)


-- ---------------------------------------------------------------------------
-- Error helper
-- ---------------------------------------------------------------------------

httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Could not reach the server"

        Http.BadStatus 401 ->
            "Not authenticated (session may have expired)"

        Http.BadStatus 403 ->
            "Not authorised"

        Http.BadStatus 404 ->
            "Not found"

        Http.BadStatus 400 ->
            "Bad request"

        Http.BadStatus code ->
            "Server error (" ++ String.fromInt code ++ ")"

        Http.BadBody msg ->
            "Unexpected response: " ++ msg


