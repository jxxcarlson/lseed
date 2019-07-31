module Frontend exposing (Model, app)

import Array exposing (map)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html exposing (Html, time)
import Html.Attributes as HA
import Lamdera.Frontend as Frontend
import Lamdera.Types exposing (..)
import Msg exposing (AppMode(..), BackendMsg(..), FrontendMsg(..), ToBackend(..), ToFrontend(..), ValidationState(..))
import Style
import Task
import TestData exposing (..)
import Time exposing (Posix)
import Url exposing (Url)
import User exposing (User, Username)
import Utility
import Vote exposing (VoteCount)


app =
    Frontend.application
        { init = \_ _ -> init
        , onUrlRequest = ClickLink
        , onUrlChange = ChangeUrl
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Lamdera Vote Tally"
                , body = [ view model ]
                }
        }



--
-- MODEL
--


type alias Model =
    { appMode : AppMode
    , message : String
    , counter : Int
    , currentTime : Posix
    , clientCount : Int

    -- VOTE
    , voteCount : VoteCount

    -- USER
    , currentUser : Maybe User
    , username : String
    , password : String
    , newPassword1 : String
    , newPassword2 : String
    , email : String
    , signInMessage : String
    , userList : List User
    }



--
-- INIT
--
--
-- CONFIG
--


config =
    { timeoutInMs = 5 * 1000
    , panelHeight = 550
    , panelWidth = 450
    , blurb = "Vote for your favorite inventor"
    }


initialModel =
    { message = "App started"
    , appMode = UserValidation SignInState
    , currentTime = Time.millisToPosix 0
    , counter = 0
    , clientCount = 0

    -- VOTE
    , voteCount = Dict.empty
    , -- USER
      currentUser = Nothing
    , username = ""
    , password = ""
    , newPassword1 = ""
    , newPassword2 = ""
    , email = ""
    , signInMessage = "Please sign in"
    , userList = []
    }


init : ( Model, Cmd FrontendMsg )
init =
    ( initialModel, sendToBackend ClientJoin )


sendToBackend : ToBackend -> Cmd FrontendMsg
sendToBackend =
    Frontend.sendToBackend config.timeoutInMs SentToBackendResult


subscriptions model =
    Time.every 1000 TimeChange



--
-- UPDATE
--


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ClientTimeoutReceived clientId ->
            ( { model | message = "Client time out" }, Cmd.none )

        SendMessage str ->
            ( { model | signInMessage = str }, Cmd.none )

        SendUserList userList ->
            ( { model | userList = userList }, Cmd.none )

        ValidateUser currentUser ->
            case currentUser of
                Nothing ->
                    ( { model | currentUser = Nothing, signInMessage = "Incorrect password/username" }, Cmd.none )

                Just user ->
                    ( { model
                        | currentUser = Just user
                        , message = "Signed in as " ++ user.username
                        , appMode = Voting
                      }
                    , Cmd.none
                    )

        VoteCounttoFE voteCount ->
            ( { model | voteCount = voteCount, message = "Vote count: " ++ String.fromInt (Vote.total voteCount) }, Cmd.none )

        ClientCountToFE clientCount ->
            ( { model | clientCount = clientCount }, Cmd.none )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        FENoop ->
            ( model, Cmd.none )

        -- APP
        ClientTimedOut _ ->
            ( { model | message = "Client timed out" }, Cmd.none )

        TimeChange t ->
            ( { model | currentTime = t }, Cmd.none )

        -- ADMIN
        SendUsers ->
            case currentUserIsAdmin model of
                False ->
                    ( model, Cmd.none )

                True ->
                    ( model
                    , sendToBackend RequestUsers
                    )

        Clear ->
            case currentUserIsAdmin model of
                False ->
                    ( model, Cmd.none )

                True ->
                    ( model
                    , sendToBackend ClearClients
                    )

        -- VOTE
        CastVote candidate ->
            case model.currentUser of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | currentUser = Just (User.castVote user) }
                    , sendToBackend (BECastVote user.username candidate)
                    )

        -- BACKEND
        SentToBackendResult result ->
            ( model, Cmd.none )

        -- URL (NOT USED)
        ChangeUrl url ->
            ( model, Cmd.none )

        ClickLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Cmd.none )

                External url ->
                    ( model, Cmd.none )

        -- UI
        -- USER
        GotUserName str ->
            ( { model | username = str }, Cmd.none )

        GotPassword str ->
            ( { model | password = str }, Cmd.none )

        GotNewPassword1 str ->
            ( { model | newPassword1 = str }, Cmd.none )

        GotNewPassword2 str ->
            ( { model | newPassword2 = str }, Cmd.none )

        ChangePassword ->
            case User.validateChangePassword model.newPassword1 model.newPassword2 of
                [] ->
                    case model.currentUser of
                        Nothing ->
                            ( { model | signInMessage = "No user signed in" }, Cmd.none )

                        Just user ->
                            ( { model | signInMessage = "OK" }
                            , sendToBackend (SendChangePasswordInfo user.username model.password model.newPassword1)
                            )

                errorList ->
                    ( { model | signInMessage = String.join ", " errorList }, Cmd.none )

        GotEmail str ->
            ( { model | email = str }, Cmd.none )

        SignIn ->
            ( initialModel
            , sendToBackend (SignInUser model.username model.password)
            )

        SignUp ->
            let
                signUpErrors =
                    User.validateSignUpInfo model.username model.password model.email
            in
            case List.length signUpErrors > 0 of
                True ->
                    ( { model | signInMessage = String.join ", " signUpErrors }, Cmd.none )

                False ->
                    ( initialModel, sendToBackend (SendSignUpInfo model.username model.password model.email) )

        SignOut ->
            ( initialModel, sendToBackend (SignoutUser model.currentUser) )

        SetAppMode appMode ->
            ( { model | appMode = appMode }, appModeCmd appMode )


appModeCmd : AppMode -> Cmd FrontendMsg
appModeCmd mode =
    case mode of
        Admin ->
            sendToBackend RequestUsers

        _ ->
            Cmd.none



-- NOtE
--
-- VIEW
--


view : Model -> Html FrontendMsg
view model =
    Element.layout
        [ Font.family
            [ Font.typeface "Arial"
            , Font.sansSerif
            ]
        ]
        (mainView model)


mainView : Model -> Element FrontendMsg
mainView model =
    column [ height fill ]
        [ Html.node "link" [ HA.rel "stylesheet", HA.href "mystyle.css" ] [] |> Element.html
        , header model
        , case model.appMode of
            UserValidation _ ->
                userValidationView model

            Admin ->
                adminView model

            Voting ->
                votingView model
        , footer model
        ]



--
-- FOOTER
--


footer model =
    row
        [ spacing 24
        , Font.size 12
        , width fill
        , Background.color Style.charcoal
        , Font.color Style.white
        , paddingXY 8 8
        ]
        [ el [] (text model.message), el [] (text <| "Voters online: " ++ String.fromInt model.clientCount) ]



--
-- USER VIEW
--


userValidationView : Model -> Element FrontendMsg
userValidationView model =
    case model.currentUser of
        Nothing ->
            noUserView model

        Just user ->
            signedInUserView model user


noUserView : Model -> Element FrontendMsg
noUserView model =
    row [ spacing 12 ]
        [ noUserLHS model
        , noUserRHS model
        ]


noUserLHS model =
    column Style.mainColumnX
        [ el [ Font.size 18, Font.bold, paddingXY 0 12 ] (text "Welcome!")
        , inputUserName model
        , inputPassword model
        , showIf (model.appMode == UserValidation SignUpState) (inputEmail model)
        , showIf (model.appMode == UserValidation SignUpState) (el [ Font.size 12 ] (text "A real email address is only needed for password recovery in real production."))
        , row [ spacing 12, paddingXY 0 12 ]
            [ showIf (model.appMode == UserValidation SignInState) (signInButton model)
            , row [ spacing 12 ]
                [ signUpButton model
                , showIf (model.appMode == UserValidation SignUpState) (cancelSignUpButton model)
                ]
            ]
        , el [ Font.size 12 ] (text model.signInMessage)
        ]


noUserRHS model =
    column [ padding 40, spacing 18, Font.size 16 ]
        [ el [ Font.bold, Font.size 24 ]
            (text "Lamdera Voter")
        , image
            [ width (px 200) ]
            { src = "https://cdn.imgbin.com/13/12/6/imgbin-general-election-ballot-box-voting-title-box-J0juQk8iyrbFQVdZUPbS3TGCe.jpg"
            , description = "Ballot box"
            }
        , Element.paragraph []
            [ el [ Font.bold ] (text "Lamdera Vote Tally ")
            , text "is a demo of the capabilities of Lamdera. "
            , text "It has a basic user authentication and admin system. "
            , text "Since the app is quite simple, it can be used as a 'seed' for other apps. "
            , text "Just rip out the voter part and replace it with your code. "
            ]
        , Element.paragraph []
            [ el [ Font.bold ] (text "Note. ")
            , text "The authentication system is insecure.  You must replace User.encrypt by something better."
            ]
        ]


signedInUserView : Model -> User -> Element FrontendMsg
signedInUserView model user =
    column Style.mainColumnX
        [ el [] (text <| "Signed in as " ++ user.username)
        , signOutButton model
        , showIf (model.appMode == UserValidation ChangePasswordState) (passwordPanel model)
        , row [ spacing 12 ]
            [ changePasswordButton model
            , showIf (model.appMode == UserValidation ChangePasswordState) (cancelChangePasswordButton model)
            ]
        , adminStatus model
        ]


passwordPanel model =
    column [ spacing 12, paddingXY 0 18 ]
        [ inputCurrentPassword model
        , inputNewPassword1 model
        , inputNewPassword2 model
        , el [ Font.size 12 ] (text model.signInMessage)
        ]


inputCurrentPassword model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPassword
        , text = model.password
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (text "Old password: ")
        }


inputNewPassword1 model =
    Input.newPassword (Style.inputStyle 200)
        { onChange = GotNewPassword1
        , show = False
        , text = model.newPassword1
        , placeholder = Nothing

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (text "New password: ")
        }


inputNewPassword2 model =
    Input.newPassword (Style.inputStyle 200)
        { onChange = GotNewPassword2
        , text = model.newPassword2
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (text "Password again: ")
        }


changePasswordButton : Model -> Element FrontendMsg
changePasswordButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation ChangePasswordState ->
                    Just ChangePassword

                _ ->
                    Just <| SetAppMode (UserValidation ChangePasswordState)
        , label = Element.text "Change password"
        }


adminStatus : Model -> Element FrontendMsg
adminStatus model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    el [ Font.size 12 ] (text "Admin")


inputUserName model =
    Input.text (Style.inputStyle 200)
        { onChange = GotUserName
        , text = model.username
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (text "Username")
        }


inputEmail model =
    Input.text (Style.inputStyle 200)
        { onChange = GotEmail
        , text = model.email
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (text "Email")
        }


inputPassword model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPassword
        , text = model.password
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (text "Password")
        }


signInButton : Model -> Element FrontendMsg
signInButton model =
    Input.button Style.headerButton
        { onPress = Just SignIn
        , label = Element.text "Sign in"
        }


signUpButton : Model -> Element FrontendMsg
signUpButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation SignUpState ->
                    Just SignUp

                _ ->
                    Just (SetAppMode (UserValidation SignUpState))
        , label = Element.text "Sign Up"
        }


cancelSignUpButton : Model -> Element FrontendMsg
cancelSignUpButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation SignUpState ->
                    Just (SetAppMode (UserValidation SignInState))

                _ ->
                    Just FENoop
        , label = Element.text "Cancel"
        }


cancelChangePasswordButton : Model -> Element FrontendMsg
cancelChangePasswordButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation ChangePasswordState ->
                    Just (SetAppMode (UserValidation SignInState))

                _ ->
                    Just FENoop
        , label = Element.text "Cancel"
        }


signOutButton : Model -> Element FrontendMsg
signOutButton model =
    Input.button Style.headerButton
        { onPress = Just SignOut
        , label = Element.text "Sign out"
        }



--
-- HEADER
--


header : Model -> Element FrontendMsg
header model =
    row
        [ width fill
        , paddingXY 40 8
        , Background.color Style.charcoal
        , spacing 12
        ]
        [ showIf (currentUserIsAdmin model) (adminModeButton model)
        , userValidationModeButton model
        , hideIf (model.currentUser == Nothing) (votingModeButton model)
        , el [ centerX, Font.size 18, Font.color Style.white ] (text <| appTitle model)
        ]


votingModeButton : Model -> Element FrontendMsg
votingModeButton model =
    Input.button ((Style.select <| model.appMode == Voting) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode Voting)
        , label = Element.text "Voting"
        }


currentUserIsAdmin : Model -> Bool
currentUserIsAdmin model =
    case model.currentUser of
        Nothing ->
            False

        Just user ->
            user.admin


appTitle : Model -> String
appTitle model =
    case model.currentUser of
        Nothing ->
            "Lamdera Vote"

        Just user ->
            "Voter: " ++ user.username


userValidationModeButton : Model -> Element FrontendMsg
userValidationModeButton model =
    Input.button ((Style.select <| model.appMode == UserValidation SignInState) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode (UserValidation SignInState))
        , label = Element.text "User"
        }


adminModeButton : Model -> Element FrontendMsg
adminModeButton model =
    Input.button ((Style.select <| model.appMode == Admin) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode Admin)
        , label = Element.text "Admin"
        }



--
-- LOG ROW
--


masterView : Model -> Element FrontendMsg
masterView model =
    column Style.mainColumnX
        [ text "VIEW"
        ]



-- VIEW HELPERS
--


showIf : Bool -> Element FrontendMsg -> Element FrontendMsg
showIf bit element =
    if bit then
        element

    else
        Element.none


hideIf : Bool -> Element FrontendMsg -> Element FrontendMsg
hideIf bit element =
    if not bit then
        element

    else
        Element.none


showOne : Bool -> String -> String -> String
showOne bit str1 str2 =
    case bit of
        True ->
            str1

        False ->
            str2



--
-- VOTING VIEW
--


votingView : Model -> Element FrontendMsg
votingView model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            column (Style.mainColumnX ++ [ padding 40 ])
                [ el [ Font.size 24, Font.bold, centerX ] (text <| "Vote count")
                , el [ Font.size 18, Font.italic, centerX ] (text <| config.blurb)
                , showIf user.voted (el [ Font.size 18, Font.italic, centerX ] (text <| "Thankyou for voting!"))
                , indexedTable
                    [ spacing 12, Font.size 18, paddingXY 0 12, height (px 400), scrollbarY ]
                    { data = model.voteCount |> Dict.toList |> List.sortBy (\( candidate, votes ) -> candidate)
                    , columns =
                        [ { header = el [ Font.bold ] (text "")
                          , width = px 40
                          , view = \k _ -> el [ Font.size 18 ] (text <| String.fromInt <| k + 1)
                          }
                        , { header = el [ Font.bold ] (text "Candidate")
                          , width = px 200
                          , view = \k ( candidate, votes ) -> castVoteButton model candidate
                          }
                        , { header = row [ width (px 60) ] [ el [ alignRight, Font.size 18, Font.bold ] (text "Votes") ]
                          , width = px 60
                          , view = \k ( candidate, votes ) -> row [ width (px 60) ] [ el [ alignRight ] (text (String.fromInt votes)) ]
                          }
                        ]
                    }
                ]


castVoteButton : Model -> String -> Element FrontendMsg
castVoteButton model candidate =
    case model.currentUser of
        Nothing ->
            el [] (text "INVALID")

        Just user ->
            case user.voted of
                True ->
                    el [ Font.size 18 ] (text candidate)

                False ->
                    Input.button Style.headerButton
                        { onPress =
                            Just (CastVote candidate)
                        , label = Element.text candidate
                        }



--
-- ADMIN VIEW
--


adminView : Model -> Element FrontendMsg
adminView model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    adminView_ model user


adminView_ : Model -> User -> Element FrontendMsg
adminView_ model user =
    column Style.mainColumnX
        [ el [ Font.size 14 ] (text <| "Admin: " ++ user.username)
        , indexedTable
            [ spacing 4, Font.size 12, paddingXY 0 12, height (px 300), scrollbarY ]
            { data = model.userList
            , columns =
                [ { header = el [ Font.bold ] (text "k")
                  , width = px 40
                  , view = \k usr -> el [ Font.size 12 ] (text <| String.fromInt <| k + 1)
                  }
                , { header = el [ Font.bold ] (text "Username")
                  , width = px 80
                  , view = \k usr -> el [ Font.size 12 ] (text usr.username)
                  }
                , { header = el [ Font.bold ] (text "Email")
                  , width = px 200
                  , view = \k usr -> el [ Font.size 12 ] (text usr.email)
                  }
                ]
            }
        , row [] [ clearClientsButton model ]
        ]


clearClientsButton model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    Input.button
                        Style.headerButton
                        { onPress = Just Clear
                        , label = text "Clear"
                        }



--
-- UPDATE HELPERS
--
--
-- END
--
