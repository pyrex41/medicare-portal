module Main exposing (main)

import AddAgent
import BrandSettings
import Browser exposing (Document)
import Browser.Navigation as Nav
import ChoosePlan
import Contact
import Contacts
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
import Url.Parser as Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query


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
    , isAdmin : Bool
    , isAgent : Bool
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
    | ContactsPage Contacts.Model
    | TempLandingPage TempLanding.Model
    | SettingsPage Settings.Model
    | Signup Signup.Model
    | ChoosePlanPage ChoosePlan.Model
    | BrandSettingsPage BrandSettings.Model
    | AddAgentsPage AddAgent.Model
    | ProfilePage Profile.Model
    | LoadingPage
    | HomePage Home.Model
    | ContactPage Contact.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | InternalLinkClicked String
    | UrlChanged Url
    | LoginMsg Login.Msg
    | ContactsMsg Contacts.Msg
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
    | ContactMsg Contact.Msg


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
        _ =
            Debug.log "Init flags" flags

        initialSession =
            case flags.initialSession of
                Just session ->
                    let
                        _ =
                            Debug.log "Found initial session" session
                    in
                    Verified session

                Nothing ->
                    let
                        _ =
                            Debug.log "No initial session found" ()
                    in
                    Unknown

        model =
            { key = key
            , url = url
            , page = LoadingPage -- Start with loading page while we check session
            , session = initialSession
            , currentUser = Nothing
            , isSetup = False
            , intendedDestination = Nothing
            }

        _ =
            Debug.log "Checking session at URL" (Url.toString url)

        checkSession =
            Http.get
                { url = "/api/auth/session"
                , expect = Http.expectJson GotSession sessionDecoder
                }
    in
    ( model
      -- Don't call updatePage here, wait for session response
    , checkSession
    )


type Route
    = PublicRoute PublicPage
    | ProtectedRoute ProtectedPage
    | SetupRoute SetupPage
    | NotFound


type VerifyParams
    = VerifyParams String String


type PublicPage
    = HomeRoute
    | LoginRoute
    | SignupRoute
    | VerifyRoute VerifyParams


type ProtectedPage
    = ContactsRoute
    | SettingsRoute
    | ProfileRoute
    | BrandSettingsRoute
    | TempLandingRoute
    | AgentsRoute
    | ContactRoute String


type SetupPage
    = ChoosePlanRoute (Maybe SetupProgress)
    | SetupSettingsRoute (Maybe SetupProgress)
    | SetupBrandSettingsRoute (Maybe SetupProgress)
    | AddAgentsRoute (Maybe SetupProgress)


type alias SetupProgress =
    { plan : Maybe String
    , orgSettings : Bool
    , brandSettings : Bool
    }


type RouteAccess
    = Public -- No auth needed (login, home)
    | Protected -- Requires valid session
    | Setup -- Special setup flow routes


setupProgressDecoder : Query.Parser (Maybe SetupProgress)
setupProgressDecoder =
    Query.map2
        (\plan org ->
            case ( plan, org ) of
                ( Just p, Just o ) ->
                    Just
                        { plan = Just p
                        , orgSettings = o == "complete"
                        , brandSettings = False
                        }

                _ ->
                    Nothing
        )
        (Query.string "plan")
        (Query.string "org")


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map (PublicRoute HomeRoute) top
        , map (PublicRoute LoginRoute) (s "login")
        , map (PublicRoute SignupRoute) (s "signup")
        , map (\orgSlug -> \token -> PublicRoute (VerifyRoute (VerifyParams orgSlug token)))
            (s "auth" </> s "verify" </> string </> string)
        , map (ProtectedRoute ContactsRoute) (s "contacts")
        , map (ProtectedRoute SettingsRoute) (s "settings")
        , map (ProtectedRoute ProfileRoute) (s "profile")
        , map (ProtectedRoute BrandSettingsRoute) (s "brand-settings")
        , map (ProtectedRoute TempLandingRoute) (s "templanding")
        , map (ProtectedRoute AgentsRoute) (s "add-agents")
        , map (\id -> ProtectedRoute (ContactRoute id)) (s "contact" </> string)
        , map (\progress -> SetupRoute (ChoosePlanRoute progress))
            (s "choose-plan" <?> setupProgressDecoder)
        , map (\progress -> SetupRoute (SetupSettingsRoute progress))
            (s "setup" </> s "settings" <?> setupProgressDecoder)
        , map (\progress -> SetupRoute (SetupBrandSettingsRoute progress))
            (s "setup" </> s "brand-settings" <?> setupProgressDecoder)
        , map (\progress -> SetupRoute (AddAgentsRoute progress))
            (s "setup" </> s "add-agents" <?> setupProgressDecoder)
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

        ContactsMsg subMsg ->
            case model.page of
                ContactsPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Contacts.update subMsg pageModel
                    in
                    ( { model | page = ContactsPage newPageModel }
                    , Cmd.map ContactsMsg newCmd
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
                                    , currentUser =
                                        Just
                                            { id = ""
                                            , email = response.email
                                            , isAdmin = False
                                            , isAgent = False
                                            , organizationSlug = response.orgSlug
                                            , firstName = ""
                                            , lastName = ""
                                            }
                                    , isSetup = True
                                }
                        in
                        ( newModel
                        , case model.intendedDestination of
                            Just destination ->
                                Cmd.batch
                                    [ Nav.replaceUrl model.key destination
                                    , Cmd.map ChoosePlanMsg choosePlanCmd
                                    ]

                            Nothing ->
                                Cmd.batch
                                    [ Nav.replaceUrl model.key response.redirectUrl
                                    , Cmd.map ChoosePlanMsg choosePlanCmd
                                    ]
                        )

                    else
                        ( model, Nav.pushUrl model.key "/login" )

                Err error ->
                    ( model, Nav.pushUrl model.key "/login" )

        GotSession result ->
            let
                _ =
                    Debug.log "Got session response" result
            in
            case result of
                Ok response ->
                    let
                        _ =
                            Debug.log "Session response details" response
                    in
                    if response.valid then
                        let
                            _ =
                                Debug.log "Session is valid" response.session

                            -- Create the user first so we can check setup state
                            user =
                                { id = response.id
                                , email = response.email
                                , isAdmin = False
                                , isAgent = False
                                , organizationSlug = response.organizationSlug
                                , firstName = response.firstName
                                , lastName = response.lastName
                                }

                            -- Only set isSetup to True if we're in the middle of setup
                            isInSetup =
                                case Parser.parse routeParser model.url of
                                    Just (SetupRoute _) ->
                                        True

                                    _ ->
                                        False

                            newModel =
                                { model
                                    | session = Verified response.session
                                    , currentUser = Just user
                                    , isSetup = isInSetup
                                }
                        in
                        -- Now that we have session info, update the page
                        updatePage model.url ( newModel, Cmd.none )

                    else
                        let
                            _ =
                                Debug.log "Session is invalid" ()

                            newModel =
                                { model | session = NoSession }
                        in
                        -- For invalid session, update page which will handle redirects
                        updatePage model.url ( newModel, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "Session check error" error

                        newModel =
                            { model | session = NoSession }
                    in
                    -- For session error, update page which will handle redirects
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

        ContactMsg subMsg ->
            case model.page of
                ContactPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Contact.update subMsg pageModel
                    in
                    ( { model | page = ContactPage newPageModel }
                    , Cmd.map ContactMsg newCmd
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

                ContactsPage contactsModel ->
                    { title = "Contacts"
                    , body = [ viewWithNav model (Html.map ContactsMsg (Contacts.view contactsModel)) ]
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

                ContactPage contactModel ->
                    let
                        contactView =
                            Contact.view contactModel
                    in
                    { title = contactView.title
                    , body = [ viewWithNav model (Html.map ContactMsg (div [] contactView.body)) ]
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
                        , onClick (InternalLinkClicked "/contacts")
                        ]
                        [ text "Contacts" ]
                    , button
                        [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:text-purple-600 transition-colors duration-200"
                        , onClick (InternalLinkClicked "/brand-settings")
                        ]
                        [ text "Brand Settings" ]
                    , if isAdminOrAdminAgent model.currentUser then
                        button
                            [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:text-purple-600 transition-colors duration-200"
                            , onClick (InternalLinkClicked "/add-agents")
                            ]
                            [ text "Manage Agents" ]

                      else
                        text ""
                    , button
                        [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:text-purple-600 transition-colors duration-200"
                        , onClick (InternalLinkClicked "/profile")
                        ]
                        [ text "Profile" ]
                    , if isAdminOrAdminAgent model.currentUser then
                        button
                            [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:text-purple-600 transition-colors duration-200"
                            , onClick (InternalLinkClicked "/settings")
                            ]
                            [ text "Organization Settings" ]

                      else
                        text ""
                    ]
                ]
            ]
        ]


isAdminOrAdminAgent : Maybe User -> Bool
isAdminOrAdminAgent maybeUser =
    case maybeUser of
        Just user ->
            user.isAdmin

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

        ContactsPage pageModel ->
            Sub.map ContactsMsg (Contacts.subscriptions pageModel)

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

        ContactPage pageModel ->
            Sub.map ContactMsg (Contact.subscriptions pageModel)

        NotFoundPage ->
            Sub.none


getRouteAccess : Route -> RouteAccess
getRouteAccess route =
    case route of
        PublicRoute _ ->
            Public

        ProtectedRoute _ ->
            Protected

        SetupRoute _ ->
            Setup

        NotFound ->
            Public


userDecoder : Decoder User
userDecoder =
    Decode.map7 User
        (Decode.field "id" (Decode.map String.fromInt Decode.int))
        (Decode.field "email" Decode.string)
        (Decode.field "is_admin" Decode.bool)
        (Decode.field "is_agent" Decode.bool)
        (Decode.field "organization_name" Decode.string)
        (Decode.field "first_name" Decode.string)
        (Decode.field "last_name" Decode.string)


type SetupStep
    = NotStarted
    | PlanSelection
    | OrganizationSetup
    | BrandSetup
    | AgentSetup
    | Complete


getSetupStep : Model -> SetupStep
getSetupStep model =
    case model.currentUser of
        Nothing ->
            NotStarted

        Just user ->
            if not model.isSetup then
                PlanSelection

            else if not (hasOrganizationSettings user) then
                OrganizationSetup

            else if not (hasBrandSettings user) then
                BrandSetup

            else if not (hasAgents user) then
                AgentSetup

            else
                Complete


hasOrganizationSettings : User -> Bool
hasOrganizationSettings user =
    -- TODO: Add actual check for organization settings
    True


hasBrandSettings : User -> Bool
hasBrandSettings user =
    -- TODO: Add actual check for brand settings
    True


hasAgents : User -> Bool
hasAgents user =
    -- TODO: Add actual check for agents
    True


redirectToSetupStep : Model -> ( Model, Cmd Msg )
redirectToSetupStep model =
    case getSetupStep model of
        NotStarted ->
            ( model, Nav.pushUrl model.key "/login" )

        PlanSelection ->
            ( model, Nav.pushUrl model.key "/choose-plan" )

        OrganizationSetup ->
            case model.currentUser of
                Just user ->
                    ( model
                    , Nav.pushUrl model.key
                        ("/setup/settings?plan=" ++ user.organizationSlug)
                    )

                Nothing ->
                    ( model, Nav.pushUrl model.key "/setup/settings" )

        BrandSetup ->
            case model.currentUser of
                Just user ->
                    ( model
                    , Nav.pushUrl model.key
                        ("/setup/brand-settings?plan=" ++ user.organizationSlug ++ "&org=complete")
                    )

                Nothing ->
                    ( model, Nav.pushUrl model.key "/setup/brand-settings" )

        AgentSetup ->
            case model.currentUser of
                Just user ->
                    ( model
                    , Nav.pushUrl model.key
                        ("/setup/add-agents?plan=" ++ user.organizationSlug ++ "&org=complete&brand=complete")
                    )

                Nothing ->
                    ( model, Nav.pushUrl model.key "/setup/add-agents" )

        Complete ->
            ( model, Nav.pushUrl model.key "/contacts" )


shouldRedirectToLogin : Route -> Model -> Bool
shouldRedirectToLogin route model =
    case route of
        PublicRoute _ ->
            False

        NotFound ->
            False

        _ ->
            case model.session of
                Verified _ ->
                    False

                _ ->
                    True


shouldRedirectToSetup : Route -> Model -> Bool
shouldRedirectToSetup route model =
    -- Only check setup state if we're in setup mode
    if model.isSetup then
        case route of
            SetupRoute _ ->
                False

            PublicRoute _ ->
                False

            NotFound ->
                False

            ProtectedRoute _ ->
                getSetupStep model /= Complete

    else
        False



-- Not in setup mode, no need to redirect


updatePage : Url -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePage url ( model, cmd ) =
    case model.session of
        Unknown ->
            -- If we don't know session state yet, stay on loading page
            ( { model | page = LoadingPage }
            , cmd
            )

        _ ->
            -- Only process route after we know session state
            case Parser.parse routeParser url of
                Just route ->
                    if shouldRedirectToLogin route model then
                        ( { model
                            | intendedDestination = Just (Url.toString url)
                            , page = LoginPage (Login.init model.key False |> Tuple.first)
                          }
                        , Nav.pushUrl model.key "/login"
                        )

                    else if shouldRedirectToSetup route model then
                        redirectToSetupStep model

                    else
                        case route of
                            PublicRoute HomeRoute ->
                                let
                                    ( homeModel, homeCmd ) =
                                        Home.init ()
                                in
                                ( { model | page = HomePage homeModel }
                                , Cmd.batch [ cmd, Cmd.map HomeMsg homeCmd ]
                                )

                            PublicRoute LoginRoute ->
                                let
                                    ( loginModel, loginCmd ) =
                                        Login.init model.key False
                                in
                                ( { model | page = LoginPage loginModel }
                                , Cmd.batch [ cmd, Cmd.map LoginMsg loginCmd ]
                                )

                            PublicRoute (VerifyRoute (VerifyParams orgSlug token)) ->
                                let
                                    _ =
                                        Debug.log "Verifying magic link" ( orgSlug, token )

                                    verifyUrl =
                                        "/api/auth/verify/" ++ orgSlug ++ "/" ++ token

                                    verifyRequest =
                                        Http.get
                                            { url = verifyUrl
                                            , expect = Http.expectJson GotVerification verificationDecoder
                                            }
                                in
                                ( model
                                , Cmd.batch [ cmd, verifyRequest ]
                                )

                            PublicRoute SignupRoute ->
                                let
                                    ( signupModel, signupCmd ) =
                                        Signup.init
                                in
                                ( { model | page = Signup signupModel }
                                , Cmd.batch [ cmd, Cmd.map SignupMsg signupCmd ]
                                )

                            ProtectedRoute ContactsRoute ->
                                let
                                    ( contactsModel, contactsCmd ) =
                                        Contacts.init model.key
                                in
                                ( { model | page = ContactsPage contactsModel }
                                , Cmd.batch [ cmd, Cmd.map ContactsMsg contactsCmd ]
                                )

                            ProtectedRoute SettingsRoute ->
                                let
                                    ( settingsModel, settingsCmd ) =
                                        Settings.init
                                            { isSetup = True
                                            , key = model.key
                                            , currentUser =
                                                case model.currentUser of
                                                    Just user ->
                                                        Just
                                                            { id = user.id
                                                            , email = user.email
                                                            , isAdmin = user.isAdmin
                                                            , isAgent = user.isAgent
                                                            }

                                                    Nothing ->
                                                        Nothing
                                            }
                                in
                                ( { model | page = SettingsPage settingsModel }
                                , Cmd.batch [ cmd, Cmd.map SettingsMsg settingsCmd ]
                                )

                            ProtectedRoute ProfileRoute ->
                                let
                                    ( profileModel, profileCmd ) =
                                        Profile.init ()
                                in
                                ( { model | page = ProfilePage profileModel }
                                , Cmd.batch [ cmd, Cmd.map ProfileMsg profileCmd ]
                                )

                            ProtectedRoute BrandSettingsRoute ->
                                let
                                    ( brandSettingsModel, brandSettingsCmd ) =
                                        case model.currentUser of
                                            Just user ->
                                                BrandSettings.init
                                                    { key = model.key
                                                    , session =
                                                        case model.session of
                                                            Verified session ->
                                                                session

                                                            _ ->
                                                                ""
                                                    , orgSlug = user.organizationSlug
                                                    , isSetup = True
                                                    }

                                            Nothing ->
                                                BrandSettings.init
                                                    { key = model.key
                                                    , session = ""
                                                    , orgSlug = ""
                                                    , isSetup = True
                                                    }
                                in
                                ( { model | page = BrandSettingsPage brandSettingsModel }
                                , Cmd.batch [ cmd, Cmd.map BrandSettingsMsg brandSettingsCmd ]
                                )

                            ProtectedRoute TempLandingRoute ->
                                let
                                    ( tempLandingModel, tempLandingCmd ) =
                                        TempLanding.init ()
                                in
                                ( { model | page = TempLandingPage tempLandingModel }
                                , Cmd.batch [ cmd, Cmd.map TempLandingMsg tempLandingCmd ]
                                )

                            ProtectedRoute AgentsRoute ->
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

                            ProtectedRoute (ContactRoute id) ->
                                let
                                    _ =
                                        Debug.log "Handling contact route with ID" id

                                    ( contactModel, contactCmd ) =
                                        Contact.init model.key id
                                in
                                ( { model | page = ContactPage contactModel }
                                , Cmd.batch [ cmd, Cmd.map ContactMsg contactCmd ]
                                )

                            SetupRoute (ChoosePlanRoute progress) ->
                                let
                                    ( choosePlanModel, choosePlanCmd ) =
                                        case model.currentUser of
                                            Just user ->
                                                ChoosePlan.init
                                                    user.organizationSlug
                                                    (case model.session of
                                                        Verified s ->
                                                            s

                                                        _ ->
                                                            ""
                                                    )
                                                    model.key

                                            Nothing ->
                                                -- This case should not happen as we check for auth before
                                                ChoosePlan.init "" "" model.key
                                in
                                ( { model | page = ChoosePlanPage choosePlanModel }
                                , Cmd.batch [ cmd, Cmd.map ChoosePlanMsg choosePlanCmd ]
                                )

                            SetupRoute (SetupSettingsRoute progress) ->
                                let
                                    ( settingsModel, settingsCmd ) =
                                        Settings.init
                                            { isSetup = True
                                            , key = model.key
                                            , currentUser =
                                                case model.currentUser of
                                                    Just user ->
                                                        Just
                                                            { id = user.id
                                                            , email = user.email
                                                            , isAdmin = user.isAdmin
                                                            , isAgent = user.isAgent
                                                            }

                                                    Nothing ->
                                                        Nothing
                                            }
                                in
                                ( { model | page = SettingsPage settingsModel }
                                , Cmd.batch [ cmd, Cmd.map SettingsMsg settingsCmd ]
                                )

                            SetupRoute (SetupBrandSettingsRoute progress) ->
                                let
                                    ( brandSettingsModel, brandSettingsCmd ) =
                                        case model.currentUser of
                                            Just user ->
                                                BrandSettings.init
                                                    { key = model.key
                                                    , session =
                                                        case model.session of
                                                            Verified session ->
                                                                session

                                                            _ ->
                                                                ""
                                                    , orgSlug = user.organizationSlug
                                                    , isSetup = True
                                                    }

                                            Nothing ->
                                                BrandSettings.init
                                                    { key = model.key
                                                    , session = ""
                                                    , orgSlug = ""
                                                    , isSetup = True
                                                    }
                                in
                                ( { model | page = BrandSettingsPage brandSettingsModel }
                                , Cmd.batch [ cmd, Cmd.map BrandSettingsMsg brandSettingsCmd ]
                                )

                            SetupRoute (AddAgentsRoute progress) ->
                                let
                                    ( addAgentsModel, addAgentsCmd ) =
                                        AddAgent.init
                                            { isSetup = True
                                            , key = model.key
                                            }
                                in
                                ( { model | page = AddAgentsPage addAgentsModel }
                                , Cmd.batch [ cmd, Cmd.map AddAgentsMsg addAgentsCmd ]
                                )

                            NotFound ->
                                ( { model | page = NotFoundPage }
                                , cmd
                                )

                Nothing ->
                    ( { model | page = NotFoundPage }
                    , cmd
                    )
