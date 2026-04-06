port module Session exposing
    ( clearSession
    , isAdmin
    , isMod
    , saveSession
    , storeSession
    )

import Json.Encode as E
import Types exposing (Session, UserRole(..), sessionEncoder)


port storeSession : Maybe String -> Cmd msg


clearSession : Cmd msg
clearSession =
    storeSession Nothing


saveSession : Session -> Cmd msg
saveSession session =
    storeSession (Just (E.encode 0 (sessionEncoder session)))


isAdmin : Session -> Bool
isAdmin session =
    session.user.role == AdminRole


isMod : Session -> Bool
isMod session =
    session.user.role == ModeratorRole || session.user.role == AdminRole
