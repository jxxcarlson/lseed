module Backend exposing (Model, app, userList)

import Dict exposing (Dict)
import Frontend
import Lamdera.Backend
import Lamdera.Types exposing (..)
import Maybe.Extra
import Msg exposing (..)
import Set exposing (Set)
import TestData exposing (passwordDict)
import User exposing (PasswordDict, User, UserDict, UserInfo, Username)
import Vote exposing (VoteCount)


app =
    Lamdera.Backend.application
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }



--
-- MODEL
--


type alias Model =
    { passwordDict : PasswordDict
    , userDict : UserDict
    , voteCount : VoteCount
    , clients : Set ClientId
    }


init : ( Model, Cmd BackendMsg )
init =
    ( { passwordDict = TestData.passwordDict
      , userDict = TestData.userDict
      , voteCount = TestData.voteCount
      , clients = Set.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        -- Our sendToFrontend Cmd has completed
        SentToFrontendResult clientId result ->
            case result of
                Ok () ->
                    ( model, Cmd.none )

                Err _ ->
                    -- Message was not delivered successfully, it timed out.
                    -- Let's consider that client dead, and remove it from the set of clients.
                    if Set.member clientId model.clients then
                        let
                            newClients =
                                Set.remove clientId model.clients
                        in
                        ( { model | clients = newClients }, broadcast newClients (ClientTimeoutReceived clientId) )

                    else
                        -- we've already registered this user as disconnected.
                        -- if we didn't do this extra check, this naive timeout implementation would send duplicate ClientTimeoutReceived msgs for each message the leaving client didn't receive, until the first of those messages timed out.
                        -- A better implementation would send heartbeats to all clients and use that to detect dead clients, but that requires periodic timers, which the backend doesn't support yet.
                        ( model, Cmd.none )


updateFromFrontend : ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        RequestUsers ->
            ( model, sendToFrontend clientId (SendUserList (userList model.userDict)) )

        ClearClients ->
            ( { model | clients = Set.empty }, broadcast Set.empty (ClientCountToFE 0) )

        SignInUser username password ->
            case User.validateUser model.passwordDict username password of
                True ->
                    let
                        newClients =
                            Set.insert clientId model.clients
                    in
                    ( { model | clients = newClients }
                    , Cmd.batch
                        [ sendToFrontend clientId <| ValidateUser (User.fromDict model.userDict username)
                        , sendToFrontend clientId (VoteCounttoFE model.voteCount)
                        , broadcast newClients (ClientCountToFE <| Set.size newClients)
                        ]
                    )

                False ->
                    ( { model | clients = removeClient clientId model.clients }
                    , sendToFrontend clientId <| ValidateUser Nothing
                    )

        SignoutUser maybeUser ->
            case maybeUser of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    let
                        newClients =
                            removeClient clientId model.clients
                    in
                    ( { model | clients = newClients }
                    , broadcast newClients (ClientCountToFE <| Set.size newClients)
                    )

        SendChangePasswordInfo username password newPassword ->
            case User.validateUser model.passwordDict username password of
                True ->
                    let
                        passwordUpdater =
                            Maybe.map (\ep -> User.encrypt newPassword)

                        newPasswordDict =
                            Dict.update username passwordUpdater model.passwordDict
                    in
                    ( { model | passwordDict = newPasswordDict }, sendToFrontend clientId <| SendMessage "Password changed" )

                False ->
                    ( model, sendToFrontend clientId <| SendMessage "Could not change password" )

        SendSignUpInfo username password email ->
            case User.add username password email ( model.passwordDict, model.userDict ) of
                Ok ( newPasswordDict, newUserDict ) ->
                    ( { model
                        | userDict = newUserDict
                        , passwordDict = newPasswordDict
                        , clients = Set.insert clientId model.clients
                      }
                    , sendToFrontend clientId <| ValidateUser (User.fromDict newUserDict username)
                    )

                Err str ->
                    ( { model | clients = removeClient clientId model.clients }, sendToFrontend clientId <| ValidateUser Nothing )

        BECastVote username candidate ->
            let
                newVoteCount =
                    Vote.cast candidate model.voteCount
            in
            ( { model
                | userDict = User.enterUserAsVoted username model.userDict
                , voteCount = newVoteCount
              }
            , Cmd.batch
                [ broadcast model.clients (VoteCounttoFE newVoteCount)
                , broadcast model.clients (ClientCountToFE <| Set.size model.clients)
                ]
            )

        ClientJoin ->
            ( model, Cmd.none )


broadcast clients msg =
    clients
        |> Set.toList
        |> List.map (\clientId -> sendToFrontend clientId msg)
        |> Cmd.batch


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.Backend.sendToFrontend 1000 clientId (\_ -> NoOpBackendMsg) msg



--
-- HELPERS
--


removeClient : ClientId -> Set ClientId -> Set ClientId
removeClient clientId clients =
    case Set.member clientId clients of
        True ->
            Set.remove clientId clients

        False ->
            clients


userList : UserDict -> List User
userList userDict =
    List.map (User.fromDict userDict) (Dict.keys userDict)
        |> Maybe.Extra.values
