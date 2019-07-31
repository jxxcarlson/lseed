module Msg exposing
    ( AppMode(..)
    , BackendMsg(..)
    , FrontendMsg(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ValidationState(..)
    )

import Browser exposing (UrlRequest(..))
import Lamdera.Types exposing (ClientId, WsError)
import Time exposing (Posix)
import Url exposing (Url)
import User exposing (User, Username)
import Vote exposing (VoteCount)


type ToBackend
    = NoOpToBackend
    | ClientJoin
    | ClearClients
    | RequestUsers
    | SignInUser String String
    | SendSignUpInfo String String String
    | SendChangePasswordInfo User.Username String String
    | BECastVote String String


type ToFrontend
    = NoOpToFrontend
    | ClientTimeoutReceived ClientId
    | SendMessage String
    | ValidateUser (Maybe User)
    | SendUserList (List User)
    | VoteCounttoFE VoteCount
    | ClientCountToFE Int


type BackendMsg
    = NoOpBackendMsg
    | SentToFrontendResult ClientId (Result WsError ())


type FrontendMsg
    = FENoop
      -- Admin
    | SendUsers
    | Clear
      -- App
    | ClientTimedOut ClientId
    | SetAppMode AppMode
    | SentToBackendResult (Result WsError ())
    | TimeChange Posix
      -- VOTE
    | CastVote String
      -- User
    | GotUserName String
    | GotPassword String
    | GotNewPassword1 String
    | GotNewPassword2 String
    | ChangePassword
    | GotEmail String
    | SignIn
    | SignUp
    | SignOut
      -- Url
    | ChangeUrl Url
    | ClickLink UrlRequest


type AppMode
    = UserValidation ValidationState
    | Admin
    | Voting



-- NOTES


type ValidationState
    = SignInState
    | SignUpState
    | ChangePasswordState
