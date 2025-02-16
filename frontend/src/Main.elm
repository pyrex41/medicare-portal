module Main exposing (main)

import AddAgent
import Browser exposing (Document)
import Browser.Navigation as Nav
import ChoosePlan
import Dashboard
import Debug
import Home
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as Decode exposing (Decoder)
import Login
import Profile
import Settings
import Signup
import TempLanding
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s, string, top)


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
    , organizationSlug : String
    , firstName : String
    , lastName : String
    , id : String
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
    Decode.map7 SessionResponse
        (Decode.field "valid" Decode.bool)
        (Decode.field "session" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "organizationSlug" Decode.string)
        (Decode.field "first_name" Decode.string)
        (Decode.field "last_name" Decode.string)
        (Decode.field "id" (Decode.map String.fromInt Decode.int))


type Role
    = AdminOnly
    | AdminAgent
    | AgentOnly


type alias User =
    { id : String
    , email : String
    , role : Role
    , organizationSlug : String
    , firstName : String
    , lastName : String
    }


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : Page
    , session : SessionState
    , currentUser : Maybe User
    , isSetup : Bool
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
    | ProfilePage Profile.Model
    | LoadingPage
    | HomePage Home.Model


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
    | ProfileMsg Profile.Msg
    | HomeMsg Home.Msg


type alias Flags =
    { initialSession : Maybe String }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialSession =
            case flags.initialSession of
                Just session ->
                    Verified session

                Nothing ->
                    Unknown

        model =
            { key = key
            , url = url
            , page = LoadingPage
            , session = initialSession
            , currentUser = Nothing
            , isSetup = False
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
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = "/api/auth/session"
        , body = Http.emptyBody
        , expect = Http.expectJson GotSession sessionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type Route
    = Public PublicRoute
    | Protected ProtectedRoute
    | NotFound


type PublicRoute
    = LoginRoute
    | VerifyRoute String String -- orgSlug token
    | SignupRoute
    | HomeRoute


type ProtectedRoute
    = TempLandingRoute
    | DashboardRoute
    | SettingsRoute Bool
    | ChoosePlanRoute
    | AddAgentsRoute Bool -- Add setup flag like Settings
    | ProfileRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map (Public HomeRoute) top
        , -- Public routes
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
        , map (Protected ProfileRoute) (s "profile")
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
                    case publicRoute of
                        HomeRoute ->
                            let
                                ( homeModel, homeCmd ) =
                                    Home.init ()
                            in
                            ( { model | page = HomePage homeModel }
                            , Cmd.map HomeMsg homeCmd
                            )

                        LoginRoute ->
                            case model.session of
                                Verified _ ->
                                    -- If already verified, replace URL with dashboard
                                    ( model
                                    , Nav.replaceUrl model.key "/dashboard"
                                    )

                                Unknown ->
                                    -- Show loading while we verify
                                    ( { model | page = LoadingPage }
                                    , verifySession
                                    )

                                NoSession ->
                                    -- Show login form
                                    let
                                        ( pageModel, pageCmd ) =
                                            Login.init False
                                    in
                                    ( { model | page = LoginPage pageModel }
                                    , Cmd.map LoginMsg pageCmd
                                    )

                        SignupRoute ->
                            let
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
                        Unknown ->
                            ( model, verifySession )

                        Verified _ ->
                            showProtectedRoute protectedRoute model

                        NoSession ->
                            ( model, Nav.replaceUrl model.key "/login" )

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
                    Dashboard.init
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
                        , currentUser =
                            model.currentUser
                                |> Maybe.map
                                    (\user ->
                                        { id = user.id
                                        , email = user.email
                                        , role = roleToString user.role
                                        }
                                    )
                        }
            in
            ( { model | page = SettingsPage pageModel }
            , Cmd.map SettingsMsg pageCmd
            )

        ChoosePlanRoute ->
            case ( model.session, model.currentUser ) of
                ( Verified session, Just user ) ->
                    let
                        ( pageModel, pageCmd ) =
                            ChoosePlan.init user.organizationSlug session model.key
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

        ProfileRoute ->
            let
                ( pageModel, pageCmd ) =
                    Profile.init ()
            in
            ( { model | page = ProfilePage pageModel }
            , Cmd.map ProfileMsg pageCmd
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Parser.parse routeParser url of
                        Just (Public _) ->
                            -- For public routes, just navigate directly
                            ( model
                            , Nav.pushUrl model.key (Url.toString url)
                            )

                        _ ->
                            -- For protected routes, verify session first if needed
                            if model.session == Unknown then
                                ( { model | page = LoadingPage }
                                , Cmd.batch
                                    [ verifySession
                                    , Nav.replaceUrl model.key (Url.toString url)
                                    ]
                                )

                            else
                                ( model
                                , Nav.pushUrl model.key (Url.toString url)
                                )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            case Parser.parse routeParser url of
                Just (Public _) ->
                    -- Don't verify session for public routes
                    changeRouteTo url model

                _ ->
                    if model.session == Unknown then
                        ( { model | url = url }
                        , verifySession
                        )

                    else
                        changeRouteTo url model

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
            case ( model.page, subMsg ) of
                ( DashboardPage pageModel, dashboardMsg ) ->
                    let
                        ( newPageModel, newCmd ) =
                            Dashboard.update dashboardMsg pageModel
                    in
                    ( { model | page = DashboardPage newPageModel }
                    , case dashboardMsg of
                        Dashboard.ViewProfile ->
                            Nav.pushUrl model.key "/profile"

                        Dashboard.ViewSettings ->
                            Nav.pushUrl model.key "/settings"

                        _ ->
                            Cmd.map DashboardMsg newCmd
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
                            _ =
                                Debug.log "Got verification response" response

                            ( choosePlanModel, choosePlanCmd ) =
                                ChoosePlan.init response.orgSlug response.session model.key

                            newModel =
                                { model
                                    | session = Verified response.session
                                    , page = ChoosePlanPage choosePlanModel
                                    , currentUser =
                                        Just
                                            { id = ""
                                            , email = response.email
                                            , role = AdminOnly
                                            , organizationSlug = response.orgSlug
                                            , firstName = ""
                                            , lastName = ""
                                            }
                                    , isSetup = True
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
                                { model
                                    | session = Verified response.session
                                    , currentUser =
                                        Just
                                            { id = response.id
                                            , email = response.email
                                            , role = AdminOnly
                                            , organizationSlug = response.organizationSlug
                                            , firstName = response.firstName
                                            , lastName = response.lastName
                                            }
                                    , isSetup = model.isSetup
                                }
                        in
                        -- If we're verifying from login attempt, replace URL with dashboard
                        if model.page == LoadingPage then
                            ( newModel
                            , Nav.replaceUrl model.key "/dashboard"
                            )

                        else
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

        ProfileMsg subMsg ->
            case model.page of
                ProfilePage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Profile.update subMsg pageModel
                    in
                    ( { model | page = ProfilePage newPageModel }
                    , case subMsg of
                        Profile.NavigateTo path ->
                            Nav.pushUrl model.key path

                        _ ->
                            Cmd.map ProfileMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        HomeMsg subMsg ->
            case model.page of
                HomePage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Home.update subMsg pageModel
                    in
                    ( { model | page = HomePage newPageModel }
                    , Cmd.map HomeMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    case model.page of
        LoadingPage ->
            { title = "Loading..."
            , body =
                [ div [ class "min-h-screen bg-gray-50 flex items-center justify-center" ]
                    [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-blue-500 border-t-transparent" ] []
                    ]
                ]
            }

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
            { title = "Choose Plan - Medicare Max"
            , body = List.map (Html.map ChoosePlanMsg) (ChoosePlan.view pageModel).body
            }

        AddAgentsPage pageModel ->
            { title = "Add Agent"
            , body = List.map (Html.map AddAgentsMsg) (AddAgent.view pageModel).body
            }

        ProfilePage pageModel ->
            { title = "Profile"
            , body = List.map (Html.map ProfileMsg) (Profile.view pageModel).body
            }

        HomePage pageModel ->
            { title = "Medicare Max"
            , body = List.map (Html.map HomeMsg) (Home.view pageModel).body
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        LoadingPage ->
            Sub.none

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

        ProfilePage pageModel ->
            Sub.map ProfileMsg (Profile.subscriptions pageModel)

        HomePage pageModel ->
            Sub.map HomeMsg (Home.subscriptions pageModel)

        NotFoundPage ->
            Sub.none


type RouteAccess
    = PublicOnly -- Login, etc
    | RequiresAuth -- Dashboard etc
    | AdminRouteOnly -- Agents, certain settings
    | SetupOnly -- Special setup flow


getRouteAccess : Route -> RouteAccess
getRouteAccess route =
    case route of
        Public LoginRoute ->
            PublicOnly

        Public (VerifyRoute _ _) ->
            PublicOnly

        Public SignupRoute ->
            PublicOnly

        Public HomeRoute ->
            PublicOnly

        Protected TempLandingRoute ->
            PublicOnly

        Protected DashboardRoute ->
            RequiresAuth

        Protected (SettingsRoute False) ->
            RequiresAuth

        Protected (SettingsRoute True) ->
            SetupOnly

        Protected ChoosePlanRoute ->
            SetupOnly

        Protected (AddAgentsRoute False) ->
            AdminRouteOnly

        Protected (AddAgentsRoute True) ->
            SetupOnly

        Protected ProfileRoute ->
            RequiresAuth

        NotFound ->
            PublicOnly


roleDecoder : Decoder Role
roleDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "admin" ->
                        Decode.succeed AdminOnly

                    "admin_agent" ->
                        Decode.succeed AdminAgent

                    "agent" ->
                        Decode.succeed AgentOnly

                    _ ->
                        Decode.fail ("Invalid role: " ++ str)
            )


roleToString : Role -> String
roleToString role =
    case role of
        AdminOnly ->
            "admin"

        AdminAgent ->
            "admin_agent"

        AgentOnly ->
            "agent"


userDecoder : Decoder User
userDecoder =
    Decode.map6 User
        (Decode.field "id" (Decode.map String.fromInt Decode.int))
        (Decode.field "email" Decode.string)
        (Decode.field "role" roleDecoder)
        (Decode.field "organization_name" Decode.string)
        (Decode.field "first_name" Decode.string)
        (Decode.field "last_name" Decode.string)
