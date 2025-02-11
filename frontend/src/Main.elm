module Main exposing (main)

import AddAgent
import Browser exposing (Document)
import Browser.Navigation as Nav
import ChoosePlan
import Dashboard
import Debug
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as Decode exposing (Decoder)
import Login
import Settings
import Signup
import TempLanding
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s, string)


type alias VerificationResponse =
    { success : Bool
    , redirectUrl : String
    , session : String
    , email : String
    , orgSlug : String
    }


type alias SessionResponse =
    { valid : Bool
    , session : String
    , email : String
    }


verificationDecoder : Decoder VerificationResponse
verificationDecoder =
    Decode.map5 VerificationResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "redirectUrl" Decode.string)
        (Decode.field "session" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "orgSlug" Decode.string)


sessionDecoder : Decoder SessionResponse
sessionDecoder =
    Decode.map3 SessionResponse
        (Decode.field "valid" Decode.bool)
        (Decode.field "session" Decode.string)
        (Decode.field "email" Decode.string)


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : Page
    , session : SessionState
    }


type SessionState
    = Unknown -- Initial state
    | Verified String -- Has valid session
    | NoSession -- Definitely no valid session


type Page
    = NotFoundPage
    | LoginPage Login.Model
    | DashboardPage Dashboard.Model
    | TempLandingPage TempLanding.Model
    | SettingsPage Settings.Model
    | Signup Signup.Model
    | ChoosePlanPage ChoosePlan.Model
    | AddAgentsPage AddAgent.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | LoginMsg Login.Msg
    | DashboardMsg Dashboard.Msg
    | TempLandingMsg TempLanding.Msg
    | SettingsMsg Settings.Msg
    | SignupMsg Signup.Msg
    | ChoosePlanMsg ChoosePlan.Msg
    | AddAgentsMsg AddAgent.Msg
    | GotVerification (Result Http.Error VerificationResponse)
    | GotSession (Result Http.Error SessionResponse)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { key = key
            , url = url
            , page = NotFoundPage
            , session = Unknown
            }

        route =
            Parser.parse routeParser url
    in
    case route of
        Just (Public _) ->
            -- Don't verify session for public routes
            changeRouteTo url model

        _ ->
            -- Verify session for protected routes or unknown routes
            ( model
            , verifySession
            )


verifySession : Cmd Msg
verifySession =
    Http.get
        { url = "/api/auth/session"
        , expect = Http.expectJson GotSession sessionDecoder
        }


type Route
    = Public PublicRoute
    | Protected ProtectedRoute
    | NotFound


type PublicRoute
    = LoginRoute
    | VerifyRoute String String -- orgSlug token
    | SignupRoute


type ProtectedRoute
    = TempLandingRoute
    | DashboardRoute
    | SettingsRoute Bool
    | ChoosePlanRoute
    | AddAgentsRoute Bool -- Add setup flag like Settings


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ -- Public routes
          map (Public LoginRoute) (s "login")
        , map (\slug token -> Public (VerifyRoute slug token))
            (s "auth" </> s "verify" </> string </> string)
        , map (Public SignupRoute) (s "signup")

        -- Protected routes
        , map (Protected TempLandingRoute) (s "templanding")
        , map (Protected DashboardRoute) (s "dashboard")
        , map (Protected (SettingsRoute False)) (s "settings")
        , map (Protected (SettingsRoute True)) (s "settings" </> s "setup")
        , map (Protected ChoosePlanRoute) (s "choose-plan")
        , map (Protected (AddAgentsRoute False)) (s "add-agents")
        , map (Protected (AddAgentsRoute True)) (s "add-agents" </> s "setup")
        ]


changeRouteTo : Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        _ =
            Debug.log "Changing route to" (Url.toString url)

        _ =
            Debug.log "Session state" model.session
    in
    case Parser.parse routeParser url of
        Nothing ->
            ( { model | page = NotFoundPage }
            , Cmd.none
            )

        Just route ->
            case route of
                Public publicRoute ->
                    -- Public routes are always accessible
                    case publicRoute of
                        LoginRoute ->
                            case model.session of
                                Verified _ ->
                                    -- Already logged in, show logged-in view
                                    let
                                        ( pageModel, pageCmd ) =
                                            Login.init True
                                    in
                                    ( { model | page = LoginPage pageModel }
                                    , Cmd.map LoginMsg pageCmd
                                    )

                                _ ->
                                    let
                                        ( pageModel, pageCmd ) =
                                            Login.init False
                                    in
                                    ( { model | page = LoginPage pageModel }
                                    , Cmd.map LoginMsg pageCmd
                                    )

                        SignupRoute ->
                            -- Always allow access to signup, regardless of session state
                            let
                                _ =
                                    Debug.log "Signing up"

                                ( signupModel, signupCmd ) =
                                    Signup.init
                            in
                            ( { model | page = Signup signupModel }
                            , Cmd.map SignupMsg signupCmd
                            )

                        VerifyRoute orgSlug token ->
                            ( model
                            , Http.get
                                { url = "/api/auth/verify/" ++ orgSlug ++ "/" ++ token
                                , expect = Http.expectJson GotVerification verificationDecoder
                                }
                            )

                Protected protectedRoute ->
                    case model.session of
                        Verified _ ->
                            -- Show protected page
                            showProtectedRoute protectedRoute model

                        Unknown ->
                            -- Keep current page and verify session
                            ( model
                            , verifySession
                            )

                        NoSession ->
                            -- Redirect to login
                            ( model
                            , Nav.replaceUrl model.key "/login"
                            )

                NotFound ->
                    ( { model | page = NotFoundPage }
                    , Cmd.none
                    )


showProtectedRoute : ProtectedRoute -> Model -> ( Model, Cmd Msg )
showProtectedRoute route model =
    case route of
        TempLandingRoute ->
            let
                ( pageModel, pageCmd ) =
                    TempLanding.init ()
            in
            ( { model | page = TempLandingPage pageModel }
            , Cmd.map TempLandingMsg pageCmd
            )

        DashboardRoute ->
            let
                ( pageModel, pageCmd ) =
                    Dashboard.init ()
            in
            ( { model | page = DashboardPage pageModel }
            , Cmd.map DashboardMsg pageCmd
            )

        SettingsRoute isSetup ->
            let
                ( pageModel, pageCmd ) =
                    Settings.init
                        { isSetup = isSetup
                        , key = model.key
                        }
            in
            ( { model | page = SettingsPage pageModel }
            , Cmd.map SettingsMsg pageCmd
            )

        ChoosePlanRoute ->
            case model.session of
                Verified session ->
                    let
                        ( pageModel, pageCmd ) =
                            ChoosePlan.init "test-org" session model.key
                    in
                    ( { model | page = ChoosePlanPage pageModel }
                    , Cmd.map ChoosePlanMsg pageCmd
                    )

                _ ->
                    ( model, Nav.pushUrl model.key "/login" )

        AddAgentsRoute isSetup ->
            case model.session of
                Verified session ->
                    let
                        ( pageModel, pageCmd ) =
                            AddAgent.init
                                { isSetup = isSetup
                                , key = model.key
                                }
                    in
                    ( { model | page = AddAgentsPage pageModel }
                    , Cmd.map AddAgentsMsg pageCmd
                    )

                _ ->
                    ( model, Nav.pushUrl model.key "/login" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            let
                _ =
                    Debug.log "URL changed to" (Url.toString url)
            in
            changeRouteTo url { model | url = url }

        LoginMsg subMsg ->
            case model.page of
                LoginPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Login.update subMsg pageModel
                    in
                    ( { model | page = LoginPage newPageModel }
                    , Cmd.map LoginMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        DashboardMsg subMsg ->
            case model.page of
                DashboardPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Dashboard.update subMsg pageModel
                    in
                    ( { model | page = DashboardPage newPageModel }
                    , Cmd.map DashboardMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        TempLandingMsg subMsg ->
            case model.page of
                TempLandingPage pageModel ->
                    case subMsg of
                        TempLanding.NavigateTo path ->
                            ( model
                            , Nav.pushUrl model.key path
                            )

                _ ->
                    ( model, Cmd.none )

        SettingsMsg subMsg ->
            case model.page of
                SettingsPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Settings.update subMsg pageModel
                    in
                    ( { model | page = SettingsPage newPageModel }
                    , Cmd.map SettingsMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        SignupMsg subMsg ->
            case model.page of
                Signup signupModel ->
                    let
                        ( newSignupModel, newCmd ) =
                            Signup.update subMsg signupModel
                    in
                    ( { model | page = Signup newSignupModel }
                    , Cmd.map SignupMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ChoosePlanMsg subMsg ->
            case model.page of
                ChoosePlanPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            ChoosePlan.update subMsg pageModel
                    in
                    ( { model | page = ChoosePlanPage newPageModel }
                    , Cmd.map ChoosePlanMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        AddAgentsMsg subMsg ->
            case model.page of
                AddAgentsPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            AddAgent.update subMsg pageModel
                    in
                    ( { model | page = AddAgentsPage newPageModel }
                    , Cmd.map AddAgentsMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        GotVerification result ->
            case result of
                Ok response ->
                    if response.success then
                        let
                            ( choosePlanModel, choosePlanCmd ) =
                                ChoosePlan.init response.orgSlug response.session model.key

                            newModel =
                                { model
                                    | session = Verified response.session
                                    , page = ChoosePlanPage choosePlanModel
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ Nav.replaceUrl model.key response.redirectUrl
                            , Cmd.map ChoosePlanMsg choosePlanCmd
                            ]
                        )

                    else
                        ( model, Nav.pushUrl model.key "/login" )

                Err error ->
                    ( model, Nav.pushUrl model.key "/login" )

        GotSession result ->
            case result of
                Ok response ->
                    if response.valid then
                        let
                            _ =
                                Debug.log "Session verified" response

                            newModel =
                                { model | session = Verified response.session }
                        in
                        -- Re-evaluate current route with verified session
                        changeRouteTo model.url newModel

                    else
                        let
                            _ =
                                Debug.log "Session invalid" response

                            newModel =
                                { model | session = NoSession }
                        in
                        -- For public routes, don't redirect on invalid session
                        case Parser.parse routeParser model.url of
                            Just (Public _) ->
                                changeRouteTo model.url newModel

                            _ ->
                                ( newModel
                                , Nav.replaceUrl model.key "/login"
                                )

                Err error ->
                    let
                        _ =
                            Debug.log "Session verification error" error

                        newModel =
                            { model | session = NoSession }
                    in
                    -- For public routes, don't redirect on session error
                    case Parser.parse routeParser model.url of
                        Just (Public _) ->
                            changeRouteTo model.url newModel

                        _ ->
                            ( newModel
                            , Nav.replaceUrl model.key "/login"
                            )


view : Model -> Document Msg
view model =
    case model.page of
        NotFoundPage ->
            { title = "Not Found"
            , body =
                [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center" ]
                    [ div [ class "text-center" ]
                        [ h1 [ class "text-4xl font-bold text-gray-900" ]
                            [ text "Page not found" ]
                        ]
                    ]
                ]
            }

        LoginPage pageModel ->
            { title = "Login"
            , body = List.map (Html.map LoginMsg) (Login.view pageModel).body
            }

        DashboardPage pageModel ->
            { title = "Dashboard"
            , body = [ Html.map DashboardMsg (Dashboard.view pageModel) ]
            }

        TempLandingPage pageModel ->
            { title = "Welcome"
            , body = List.map (Html.map TempLandingMsg) (TempLanding.view pageModel).body
            }

        SettingsPage pageModel ->
            { title = "Settings"
            , body = List.map (Html.map SettingsMsg) (Settings.view pageModel).body
            }

        Signup signupModel ->
            { title = "Create Organization"
            , body = List.map (Html.map SignupMsg) (Signup.view signupModel).body
            }

        ChoosePlanPage pageModel ->
            { title = "Choose Plan"
            , body = List.map (Html.map ChoosePlanMsg) (ChoosePlan.view pageModel).body
            }

        AddAgentsPage pageModel ->
            { title = "Add Agent"
            , body = List.map (Html.map AddAgentsMsg) (AddAgent.view pageModel).body
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        LoginPage pageModel ->
            Sub.map LoginMsg (Login.subscriptions pageModel)

        DashboardPage pageModel ->
            Sub.map DashboardMsg (Dashboard.subscriptions pageModel)

        TempLandingPage pageModel ->
            Sub.map TempLandingMsg (TempLanding.subscriptions pageModel)

        SettingsPage pageModel ->
            Sub.map SettingsMsg (Settings.subscriptions pageModel)

        Signup signupModel ->
            Sub.map SignupMsg (Signup.subscriptions signupModel)

        ChoosePlanPage pageModel ->
            Sub.map ChoosePlanMsg (ChoosePlan.subscriptions pageModel)

        AddAgentsPage pageModel ->
            Sub.map AddAgentsMsg (AddAgent.subscriptions pageModel)

        NotFoundPage ->
            Sub.none
