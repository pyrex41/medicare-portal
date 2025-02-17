module Main exposing (main)

import AddAgent
import BrandSettings
import Browser exposing (Document)
import Browser.Navigation as Nav
import ChoosePlan
import Dashboard
import Debug
import Home
import Html exposing (Html, button, div, h1, img, nav, p, text)
import Html.Attributes exposing (alt, class, href, src)
import Html.Events exposing (onClick)
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
    , intendedDestination : Maybe String
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
    | BrandSettingsPage BrandSettings.Model
    | AddAgentsPage AddAgent.Model
    | ProfilePage Profile.Model
    | LoadingPage
    | HomePage Home.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | InternalLinkClicked String
    | UrlChanged Url
    | LoginMsg Login.Msg
    | DashboardMsg Dashboard.Msg
    | TempLandingMsg TempLanding.Msg
    | SettingsMsg Settings.Msg
    | SignupMsg Signup.Msg
    | ChoosePlanMsg ChoosePlan.Msg
    | BrandSettingsMsg BrandSettings.Msg
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
            , intendedDestination = Nothing
            }
    in
    ( model, Cmd.none )
        |> updatePage url


type Route
    = HomeRoute
    | LoginRoute
    | DashboardRoute
    | SettingsRoute
    | ProfileRoute
    | BrandSettingsRoute
    | SignupRoute
    | ChoosePlanRoute
    | AddAgentsRoute
    | TempLandingRoute
    | NotFoundRoute


type RouteTypes
    = Protected
    | Public
    | Setup


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map HomeRoute top
        , map LoginRoute (s "login")
        , map DashboardRoute (s "dashboard")
        , map SettingsRoute (s "settings")
        , map ProfileRoute (s "profile")
        , map BrandSettingsRoute (s "brand-settings")
        , map SignupRoute (s "signup")
        , map ChoosePlanRoute (s "choose-plan")
        , map AddAgentsRoute (s "add-agents")
        , map TempLandingRoute (s "templanding")
        ]


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

        InternalLinkClicked frag ->
            ( model, Nav.pushUrl model.key frag )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )
                |> updatePage url

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
                        updatePage model.url ( newModel, Cmd.none )

                    else
                        let
                            newModel =
                                { model | session = NoSession }
                        in
                        updatePage model.url ( newModel, Cmd.none )

                Err error ->
                    let
                        newModel =
                            { model | session = NoSession }
                    in
                    updatePage model.url ( newModel, Cmd.none )

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

        BrandSettingsMsg subMsg ->
            case model.page of
                BrandSettingsPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            BrandSettings.update subMsg pageModel
                    in
                    ( { model | page = BrandSettingsPage newPageModel }
                    , Cmd.map BrandSettingsMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        viewPage =
            case model.page of
                NotFoundPage ->
                    viewNotFound

                LoginPage loginModel ->
                    let
                        loginView =
                            Login.view loginModel
                    in
                    { title = loginView.title
                    , body = List.map (Html.map LoginMsg) loginView.body
                    }

                DashboardPage dashboardModel ->
                    { title = "Dashboard"
                    , body = [ viewWithNav model (Html.map DashboardMsg (Dashboard.view dashboardModel)) ]
                    }

                TempLandingPage landingModel ->
                    let
                        landingView =
                            TempLanding.view landingModel
                    in
                    { title = landingView.title
                    , body = [ viewWithNav model (Html.map TempLandingMsg (div [] landingView.body)) ]
                    }

                SettingsPage settingsModel ->
                    let
                        settingsView =
                            Settings.view settingsModel
                    in
                    { title = settingsView.title
                    , body = [ viewWithNav model (Html.map SettingsMsg (div [] settingsView.body)) ]
                    }

                Signup signupModel ->
                    let
                        signupView =
                            Signup.view signupModel
                    in
                    { title = signupView.title
                    , body = List.map (Html.map SignupMsg) signupView.body
                    }

                ChoosePlanPage choosePlanModel ->
                    let
                        choosePlanView =
                            ChoosePlan.view choosePlanModel
                    in
                    { title = choosePlanView.title
                    , body = [ viewWithNav model (Html.map ChoosePlanMsg (div [] choosePlanView.body)) ]
                    }

                BrandSettingsPage brandSettingsModel ->
                    let
                        brandSettingsView =
                            BrandSettings.view brandSettingsModel
                    in
                    { title = brandSettingsView.title
                    , body = [ viewWithNav model (Html.map BrandSettingsMsg (div [] brandSettingsView.body)) ]
                    }

                AddAgentsPage addAgentModel ->
                    let
                        addAgentView =
                            AddAgent.view addAgentModel
                    in
                    { title = addAgentView.title
                    , body = [ viewWithNav model (Html.map AddAgentsMsg (div [] addAgentView.body)) ]
                    }

                ProfilePage profileModel ->
                    let
                        profileView =
                            Profile.view profileModel
                    in
                    { title = profileView.title
                    , body = [ viewWithNav model (Html.map ProfileMsg (div [] profileView.body)) ]
                    }

                LoadingPage ->
                    { title = "Loading..."
                    , body = [ viewLoading ]
                    }

                HomePage homeModel ->
                    let
                        homeView =
                            Home.view homeModel
                    in
                    { title = homeView.title
                    , body = List.map (Html.map HomeMsg) homeView.body
                    }
    in
    viewPage


viewWithNav : Model -> Html Msg -> Html Msg
viewWithNav model content =
    div []
        [ viewNavHeader model
        , content
        ]


viewNavHeader : Model -> Html Msg
viewNavHeader model =
    nav [ class "bg-white border-b border-gray-200" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex justify-between h-16" ]
                [ div [ class "flex" ]
                    [ div [ class "shrink-0 flex items-center" ]
                        [ button
                            [ onClick (InternalLinkClicked "/") ]
                            [ img
                                [ src "/images/medicare-max-logo.png"
                                , class "h-8 w-auto"
                                , alt "Medicare Max logo"
                                ]
                                []
                            ]
                        ]
                    ]
                , div [ class "flex items-center space-x-4" ]
                    [ button
                        [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:text-purple-600 transition-colors duration-200"
                        , onClick (InternalLinkClicked "/dashboard")
                        ]
                        [ text "Dashboard" ]
                    , button
                        [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:text-purple-600 transition-colors duration-200"
                        , onClick (InternalLinkClicked "/brand-settings")
                        ]
                        [ text "Brand Settings" ]
                    , button
                        [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:text-purple-600 transition-colors duration-200"
                        , onClick (InternalLinkClicked "/profile")
                        ]
                        [ text "Profile" ]
                    , --if isAdminOrAdminAgent model.currentUser then
                      div [ class "flex items-center space-x-4" ]
                        [ button
                            [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:text-purple-600 transition-colors duration-200"
                            , onClick (InternalLinkClicked "/settings")
                            ]
                            [ text "Organization Settings" ]
                        ]

                    -- else
                    -- text ""
                    ]
                ]
            ]
        ]


isAdminOrAdminAgent : Maybe User -> Bool
isAdminOrAdminAgent maybeUser =
    case maybeUser of
        Just user ->
            user.role == AdminOnly || user.role == AdminAgent

        Nothing ->
            False


viewNotFound : Browser.Document msg
viewNotFound =
    { title = "Not Found"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex flex-col items-center justify-center" ]
            [ h1 [ class "text-4xl font-bold text-gray-900 mb-4" ]
                [ text "404 - Page Not Found" ]
            , p [ class "text-gray-600" ]
                [ text "The page you're looking for doesn't exist." ]
            ]
        ]
    }


viewLoading : Html msg
viewLoading =
    div [ class "min-h-screen bg-gray-50 flex items-center justify-center" ]
        [ div [ class "animate-spin rounded-full h-8 w-8 border-2 border-purple-500 border-t-transparent" ] []
        ]


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

        BrandSettingsPage pageModel ->
            Sub.map BrandSettingsMsg (BrandSettings.subscriptions pageModel)

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
        HomeRoute ->
            PublicOnly

        LoginRoute ->
            PublicOnly

        DashboardRoute ->
            RequiresAuth

        SettingsRoute ->
            RequiresAuth

        ProfileRoute ->
            RequiresAuth

        BrandSettingsRoute ->
            RequiresAuth

        SignupRoute ->
            PublicOnly

        ChoosePlanRoute ->
            RequiresAuth

        AddAgentsRoute ->
            AdminRouteOnly

        TempLandingRoute ->
            PublicOnly

        NotFoundRoute ->
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


updatePage : Url -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePage url ( model, cmd ) =
    case Parser.parse routeParser url of
        Just route ->
            case route of
                HomeRoute ->
                    let
                        ( homeModel, homeCmd ) =
                            Home.init ()
                    in
                    ( { model | page = HomePage homeModel }
                    , Cmd.batch [ cmd, Cmd.map HomeMsg homeCmd ]
                    )

                LoginRoute ->
                    let
                        ( loginModel, loginCmd ) =
                            Login.init False
                    in
                    ( { model | page = LoginPage loginModel }
                    , Cmd.batch [ cmd, Cmd.map LoginMsg loginCmd ]
                    )

                DashboardRoute ->
                    let
                        ( dashboardModel, dashboardCmd ) =
                            Dashboard.init
                    in
                    ( { model | page = DashboardPage dashboardModel }
                    , Cmd.batch [ cmd, Cmd.map DashboardMsg dashboardCmd ]
                    )

                SettingsRoute ->
                    let
                        ( settingsModel, settingsCmd ) =
                            Settings.init
                                { isSetup = False
                                , key = model.key
                                , currentUser = Nothing
                                }
                    in
                    ( { model | page = SettingsPage settingsModel }
                    , Cmd.batch [ cmd, Cmd.map SettingsMsg settingsCmd ]
                    )

                ProfileRoute ->
                    let
                        ( profileModel, profileCmd ) =
                            Profile.init ()
                    in
                    ( { model | page = ProfilePage profileModel }
                    , Cmd.batch [ cmd, Cmd.map ProfileMsg profileCmd ]
                    )

                BrandSettingsRoute ->
                    let
                        ( brandSettingsModel, brandSettingsCmd ) =
                            BrandSettings.init
                                { key = model.key
                                , session = ""
                                , orgSlug = ""
                                , isSetup = False
                                }
                    in
                    ( { model | page = BrandSettingsPage brandSettingsModel }
                    , Cmd.batch [ cmd, Cmd.map BrandSettingsMsg brandSettingsCmd ]
                    )

                SignupRoute ->
                    let
                        ( signupModel, signupCmd ) =
                            Signup.init
                    in
                    ( { model | page = Signup signupModel }
                    , Cmd.batch [ cmd, Cmd.map SignupMsg signupCmd ]
                    )

                ChoosePlanRoute ->
                    let
                        ( choosePlanModel, choosePlanCmd ) =
                            ChoosePlan.init "" "" model.key
                    in
                    ( { model | page = ChoosePlanPage choosePlanModel }
                    , Cmd.batch [ cmd, Cmd.map ChoosePlanMsg choosePlanCmd ]
                    )

                AddAgentsRoute ->
                    let
                        ( addAgentsModel, addAgentsCmd ) =
                            AddAgent.init
                                { isSetup = False
                                , key = model.key
                                }
                    in
                    ( { model | page = AddAgentsPage addAgentsModel }
                    , Cmd.batch [ cmd, Cmd.map AddAgentsMsg addAgentsCmd ]
                    )

                TempLandingRoute ->
                    let
                        ( tempLandingModel, tempLandingCmd ) =
                            TempLanding.init ()
                    in
                    ( { model | page = TempLandingPage tempLandingModel }
                    , Cmd.batch [ cmd, Cmd.map TempLandingMsg tempLandingCmd ]
                    )

                NotFoundRoute ->
                    ( { model | page = NotFoundPage }
                    , cmd
                    )

        Nothing ->
            ( { model | page = NotFoundPage }
            , cmd
            )
