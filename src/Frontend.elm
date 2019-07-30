module Frontend exposing (Model, app)

--
-- import Main exposing (UserNotes)
-- import Svg.Attributes exposing (k1)
-- exposing (..)
-- import Date exposing (Date)

import Array exposing (map)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import DateTime
import Debounce exposing (Debounce)
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
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (User, Username)
import Utility


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
                { title = "Lamdera Notes"
                , body = [ view model ]
                }
        }



--
-- TYPES
--
--
-- MODEL
--


type alias Model =
    { input : String
    , appMode : AppMode
    , message : String
    , counter : Int
    , currentTime : Posix

    -- USER
    , currentUser : Maybe User
    , username : String
    , password : String
    , newPassword1 : String
    , newPassword2 : String
    , email : String
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
    }


initialModel =
    { input = "App started"
    , message = "Please sign in"
    , appMode = UserValidation SignInState
    , currentTime = Time.millisToPosix 0
    , counter = 0

    -- ADMIN
    , -- USER
      currentUser = Nothing
    , username = ""
    , password = ""
    , newPassword1 = ""
    , newPassword2 = ""
    , email = ""
    , userList = []
    }


init : ( Model, Cmd FrontendMsg )
init =
    ( initialModel, sendToBackend config.timeoutInMs SentToBackendResult ClientJoin )


sendToBackend : Milliseconds -> (Result WsError () -> FrontendMsg) -> ToBackend -> Cmd FrontendMsg
sendToBackend =
    Frontend.sendToBackend


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

        SendMessage str ->
            ( { model | message = str }, Cmd.none )

        SendUserList userList ->
            ( { model | userList = userList }, Cmd.none )

        SendValidatedUser currentUser ->
            case currentUser of
                Nothing ->
                    ( { model | currentUser = Nothing, message = "Incorrect password/username" }, Cmd.none )

                Just user ->
                    ( { model | currentUser = Just user }
                    , Cmd.none
                    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        FENoop ->
            ( model, Cmd.none )

        TimeChange t ->
            ( { model | currentTime = t }, Cmd.none )

        -- ADMIN
        SendUsers ->
            case currentUserIsAdmin model of
                False ->
                    ( model, Cmd.none )

                True ->
                    ( model
                    , sendToBackend config.timeoutInMs SentToBackendResult RequestUsers
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
                            ( { model | message = "No user signed in" }, Cmd.none )

                        Just user ->
                            ( { model | message = "OK" }
                            , sendToBackend config.timeoutInMs SentToBackendResult (SendChangePasswordInfo user.username model.password model.newPassword1)
                            )

                errorList ->
                    ( { model | message = String.join ", " errorList }, Cmd.none )

        GotEmail str ->
            ( { model | email = str }, Cmd.none )

        SignIn ->
            ( initialModel, sendToBackend config.timeoutInMs SentToBackendResult (SendSignInInfo model.username model.password) )

        SignUp ->
            let
                signUpErrors =
                    User.validateSignUpInfo model.username model.password model.email
            in
            case List.length signUpErrors > 0 of
                True ->
                    ( { model | message = String.join ", " signUpErrors }, Cmd.none )

                False ->
                    ( initialModel, sendToBackend config.timeoutInMs SentToBackendResult (SendSignUpInfo model.username model.password model.email) )

        SignOut ->
            ( initialModel, Cmd.none )

        SetAppMode appMode ->
            ( { model | appMode = appMode }, Cmd.none )



-- NOtE
--
-- VIEW
--


view : Model -> Html FrontendMsg
view model =
    Element.layout [] (mainView model)


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
        , footer model
        ]



--
-- FOOTER
--


footer model =
    case model.currentUser of
        Nothing ->
            blankFooter

        Just _ ->
            activeFooter model


blankFooter =
    row [ width fill, spacing 18, Background.color Style.charcoal, paddingXY 8 18 ] []


activeFooter model =
    row [ width fill, spacing 18, Background.color Style.charcoal, paddingXY 8 18 ] []



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
        , el [ Font.size 12 ] (text model.message)
        ]


noUserRHS model =
    column [ padding 40, spacing 18, Font.size 16 ]
        [ el [ Font.bold, Font.size 24 ]
            (text "Screenshot of app")
        , image
            [ width (px config.panelWidth) ]
            { src = "http://noteimages.s3.amazonaws.com/jim_images/notes-screen.png"
            , description = "screenshot of app"
            }
        , Element.paragraph []
            [ el [ Font.bold ] (text "Features. ")
            , text "Searchable note repository. Supports Markdown. Filter notes by title, tags, full text. "
            , text "Active links to the most-used tags. Notes are automatically saved every 0.5 second."
            ]
        , Element.paragraph []
            [ el [ Font.bold ] (text "Coming soon. ")
            , text "Filter by date, options to sort note list; export."
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
        , el [ Font.size 12 ] (text model.message)
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
        , el [ centerX, Font.size 18, Font.color Style.white ] (text <| appTitle model)
        ]


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
            "Seed App"

        Just user ->
            user.username ++ ": Seed App"


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
        ]



--
-- UPDATE HELPERS
--
--
-- END
--
