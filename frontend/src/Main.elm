port module Main exposing (main)

import AddAgent
import Browser exposing (Document)
import Browser.Events
import Browser.Navigation as Nav
import ChangePlan
import ChoosePlan
import Compare exposing (CompareParams)
import Components.AccountStatusBanner as AccountStatusBanner
import Components.DemoModeBanner as DemoModeBanner
import Contact
import ContactUs
import Contacts
import Dashboard
import Date exposing (Date)
import Dict exposing (Dict)
import Eligibility
import Home
import Html exposing (Html, a, button, div, h1, img, nav, p, text)
import Html.Attributes exposing (alt, class, href, src)
import Html.Events exposing (onClick, stopPropagationOn)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Landing
import Login
import Logout
import MyIcon
import Onboarding
import Pricing
import Pricing2
import Process
import Profile
import Quote
import Schedule
import ScheduleMain
import SelfServiceOnboarding
import Settings
import Signup
import Stripe
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, viewBox)
import Task
import TempLanding
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query
import Waitlist
import Walkthrough



-- PORTS
-- Send a message to JavaScript to clear the session cookie


port redirectToStripe :
    String
    -> Cmd msg -- ADDED: Port for Stripe redirection


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
    , isAdmin : Bool
    , id : String
    , demoMode : Bool
    , orgCreateDate : Date
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
    Decode.succeed SessionResponse
        |> Pipeline.required "valid" Decode.bool
        |> Pipeline.required "session" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "organizationSlug" Decode.string
        |> Pipeline.custom
            (Decode.oneOf
                [ Decode.field "firstName" Decode.string
                , Decode.field "first_name" Decode.string
                ]
            )
        |> Pipeline.custom
            (Decode.oneOf
                [ Decode.field "lastName" Decode.string
                , Decode.field "last_name" Decode.string
                ]
            )
        |> Pipeline.custom
            (Decode.oneOf
                [ Decode.field "is_admin" Decode.bool
                , Decode.field "is_admin" Decode.int
                    |> Decode.map (\i -> i == 1)
                ]
            )
        |> Pipeline.required "id" (Decode.map String.fromInt Decode.int)
        |> Pipeline.custom
            (Decode.oneOf
                [ Decode.field "demo_mode" Decode.bool
                , Decode.field "demo_mode" Decode.int
                    |> Decode.map (\i -> i == 1)
                ]
            )
        |> Pipeline.required
            "orgCreateDate"
            (Decode.string
                |> Decode.andThen
                    (\s ->
                        case Date.fromIsoString s of
                            Ok date ->
                                Decode.succeed date

                            Err _ ->
                                Decode.fail "Invalid date format"
                    )
            )


type alias User =
    { id : String
    , email : String
    , isAdmin : Bool
    , isAgent : Bool
    , organizationSlug : String
    , organizationId : String
    , firstName : String
    , lastName : String
    , subscriptionTier : String
    , accountStatus : Maybe AccountStatusBanner.AccountStatusDetails
    , demoMode : Bool
    , orgCreateDate : Maybe Date
    , hasCompletedWalkthrough : Bool
    }



-- Account status types


type alias AccountStatus =
    String


type alias AccountStatusDetails =
    { status : AccountStatus
    , message : String
    , organizationId : Int
    , organizationName : String
    , organizationSlug : String
    , subscriptionTier : String
    , subscriptionStatus : String
    , agentLimit : Int
    , contactLimit : Int
    , currentAgentCount : Int
    , currentContactCount : Int
    , billingCycleEnd : Maybe String
    , paymentFailureCount : Int
    , paymentCompleted : Bool
    }


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : Page
    , session : SessionState
    , currentUser : Maybe User
    , isSetup : Bool
    , intendedDestination : Maybe String
    , showDropdown : Bool
    , showStatusBanner : Bool
    , showPaymentStatus : Bool
    , demoModeBanner : DemoModeBanner.Model
    }


type SessionState
    = Unknown -- Initial state
    | Verified String -- Has valid session
    | NoSession -- Definitely no valid session


type Page
    = NotFoundPage
    | LoginPage Login.Model
    | ContactsPage Contacts.Model
    | ContactUsPage ContactUs.Model
    | TempLandingPage TempLanding.Model
    | SettingsPage Settings.Model
    | Signup Signup.Model
    | ChoosePlanPage ChoosePlan.Model
    | ChangePlanPage ChangePlan.Model
    | AddAgentsPage AddAgent.Model
    | ProfilePage Profile.Model
    | LoadingPage
    | HomePage Home.Model
    | ContactPage Contact.Model
    | ComparePage Compare.Model
    | QuotePage Quote.Model
    | EligibilityPage Eligibility.Model
    | SchedulePage Schedule.Model
    | ScheduleMainPage ScheduleMain.Model
    | DashboardPage Dashboard.Model
    | LogoutPage Logout.Model
    | OnboardingPage Onboarding.Model
    | WalkthroughPage Walkthrough.Model
    | SelfOnboardingPage SelfServiceOnboarding.Model
    | WaitlistPage Waitlist.Model
    | LandingPage Landing.Model
    | PricingPage Pricing.Model
    | Pricing2Page Pricing2.Model
    | StripePage Stripe.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | InternalLinkClicked String
    | UrlChanged Url
    | LoginMsg Login.Msg
    | ContactsMsg Contacts.Msg
    | ContactUsMsg ContactUs.Msg
    | TempLandingMsg TempLanding.Msg
    | SettingsMsg Settings.Msg
    | SignupMsg Signup.Msg
    | ChoosePlanMsg ChoosePlan.Msg
    | ChangePlanMsg ChangePlan.Msg
    | AddAgentsMsg AddAgent.Msg
    | GotVerification (Result Http.Error VerificationResponse)
    | GotSession (Result Http.Error SessionResponse)
    | ProfileMsg Profile.Msg
    | HomeMsg Home.Msg
    | ContactMsg Contact.Msg
    | CompareMsg Compare.Msg
    | QuoteMsg Quote.Msg
    | EligibilityMsg Eligibility.Msg
    | ScheduleMsg Schedule.Msg
    | ScheduleMainMsg ScheduleMain.Msg
    | DashboardMsg Dashboard.Msg
    | NoOp
    | GotCurrentUser (Result Http.Error CurrentUserResponse)
    | OrgFinalized (Result Http.Error ())
    | LogoutMsg Logout.Msg
    | OnboardingMsg Onboarding.Msg
    | ToggleDropdown
    | CloseDropdown
    | InitiateLogout
    | GotAccountStatus (Result Http.Error AccountStatusResponse)
    | CloseStatusBanner
    | WalkthroughMsg Walkthrough.Msg
    | ShowDropdown
    | HideDropdown
    | ToggleStatusBanner
    | PerformRedirect String
    | DirectPageUpdate
    | SelfOnboardingMsg SelfServiceOnboarding.Msg
    | WaitlistMsg Waitlist.Msg
    | LandingMsg Landing.Msg
    | PricingMsg Pricing.Msg
    | Pricing2Msg Pricing2.Msg
    | StripeMsg Stripe.Msg
    | TogglePaymentStatus
    | SetSessionResponse (Result Http.Error SetSessionResponseAlias)
    | LogTrackingClickResult (Result Http.Error ())
    | DemoModeBannerMsg DemoModeBanner.Msg


type alias Flags =
    {}


type alias CompareFlags =
    { state : String
    , zip : String
    , county : String
    , gender : String
    , tobacco : Bool
    , age : Int
    , planType : String
    , currentCarrier : Maybe String
    , dateOfBirth : String
    , quoteId : Maybe String
    }


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


type alias AccountStatusResponse =
    { success : Bool
    , status : AccountStatusDetails
    }


accountStatusDecoder : Decoder AccountStatusDetails
accountStatusDecoder =
    Decode.succeed AccountStatusDetails
        |> Pipeline.required "status" Decode.string
        |> Pipeline.required "message" Decode.string
        |> Pipeline.required "organizationId" Decode.int
        |> Pipeline.required "organizationName" Decode.string
        |> Pipeline.required "organizationSlug" Decode.string
        |> Pipeline.required "subscriptionTier" Decode.string
        |> Pipeline.required "subscriptionStatus" Decode.string
        |> Pipeline.required "agentLimit" Decode.int
        |> Pipeline.required "contactLimit" Decode.int
        |> Pipeline.required "currentAgentCount" Decode.int
        |> Pipeline.required "currentContactCount" Decode.int
        |> Pipeline.optional "billingCycleEnd" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "paymentFailureCount" Decode.int
        |> Pipeline.required "paymentCompleted" Decode.bool


accountStatusResponseDecoder : Decoder AccountStatusResponse
accountStatusResponseDecoder =
    Decode.succeed AccountStatusResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "status" accountStatusDecoder


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialSession =
            Unknown

        -- Parse the initial route to determine if we're on a public page
        initialRoute =
            Parser.parse routeParser url

        -- Determine if this is a public route that can be rendered immediately
        isPublicRoute =
            case initialRoute of
                Just (PublicRoute _) ->
                    True

                _ ->
                    False

        -- Set initial page appropriately
        initialPage =
            if isPublicRoute then
                -- For public routes, we'll immediately handle this in updatePageForcePublic below
                LoadingPage

            else
                -- For protected routes, we need to wait for session verification
                LoadingPage

        -- Initialize demo mode banner
        ( demoModeBannerModel, demoModeBannerCmd ) =
            DemoModeBanner.init False Nothing

        model =
            { key = key
            , url = url
            , page = initialPage
            , session = initialSession
            , currentUser = Nothing
            , isSetup = False
            , intendedDestination = Nothing
            , showDropdown = False
            , showStatusBanner = True
            , showPaymentStatus = False
            , demoModeBanner = demoModeBannerModel
            }

        checkSession =
            Http.get
                { url = "/api/auth/session"
                , expect = Http.expectJson GotSession sessionDecoder
                }

        -- Use a very short timer for the initial direct page update for public routes
        directPageUpdate =
            Task.perform (\_ -> DirectPageUpdate) (Process.sleep 50)

        currentPath =
            url.path

        queryDict : Dict String String
        queryDict =
            getQueryDict model.url.query

        maybeUserEmail : Maybe String
        maybeUserEmail =
            Dict.get "email" queryDict

        -- Check session and also immediately try to render public routes
        cmds0 =
            case initialSession of
                Verified _ ->
                    -- If we have a session, also fetch the current user immediately
                    -- But don't use directPageUpdate for protected routes to prevent duplicate initialization
                    [ fetchCurrentUser ]

                _ ->
                    []

        -- Only add directPageUpdate for public routes to prevent duplicate initialization
        cmdsWithPageUpdate =
            if isPublicRoute then
                cmds0 ++ [ directPageUpdate ]

            else
                cmds0

        cmds1 =
            case ( currentPath, maybeUserEmail ) of
                ( "/walkthrough", Just userEmail ) ->
                    cmdsWithPageUpdate ++ [ setSession userEmail ]

                _ ->
                    cmdsWithPageUpdate

        cmds =
            cmds1
    in
    -- For public routes, immediately try to render without waiting for session
    if isPublicRoute then
        -- Try to render public route immediately
        updatePageForcePublic url ( model, Cmd.batch cmds )

    else
        -- For protected routes, wait for session verification
        ( model, Cmd.batch (cmds ++ [ checkSession ]) )


getQueryDict : Maybe String -> Dict String String
getQueryDict maybeQueryString =
    case maybeQueryString of
        Just queryString ->
            queryString
                |> String.split "&"
                |> List.filterMap
                    (\param ->
                        case String.split "=" param of
                            [ key0, value ] ->
                                Just ( key0, value )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList

        Nothing ->
            Dict.empty


type alias CompareParams =
    { quoteId : Maybe String
    , orgId : Maybe String
    , tid : Maybe String
    }


type alias CompareParamsPartial1 =
    { state : Maybe String
    , zip : Maybe String
    , county : Maybe String
    , gender : Maybe String
    }


type alias CompareParamsPartial2 =
    { tobacco : Bool
    , age : Maybe Int
    , planType : Maybe String
    , currentCarrier : Maybe String
    }


type Route
    = PublicRoute PublicPage
    | ProtectedRoute ProtectedPage
    | AdminRoute AdminPage
    | SetupRoute SetupPage
    | NotFound


type VerifyParams
    = VerifyParams String String


type PublicPage
    = HomeRoute
    | LoginRoute
    | SignupRoute
    | OnboardingRoute
    | VerifyRoute VerifyParams
    | CompareRoute CompareParams
    | QuoteRoute { quoteId : Maybe String, trackingId : Maybe String, planType : Maybe String, orgId : Maybe String }
    | EligibilityRoute ( Maybe String, Maybe String, Maybe String )
    | ScheduleRoute ( Maybe String, Maybe String, Maybe String )
    | ScheduleMainRoute
    | SelfOnboardingRoute String
    | WaitlistRoute
    | LandingRoute { quoteId : Maybe String }
    | PricingRoute
    | Pricing2Route


type ProtectedPage
    = ContactsRoute
    | ContactUsRoute
    | ProfileRoute
    | TempLandingRoute
    | ContactRoute String
    | DashboardRoute
    | ChangePlanRoute
    | StripeRoute
    | WalkthroughRoute


type AdminPage
    = SettingsRoute
    | AgentsRoute


type SetupPage
    = ChoosePlanRoute (Maybe SetupProgress)
    | SetupSettingsRoute (Maybe SetupProgress)
    | AddAgentsRoute (Maybe SetupProgress)


type alias SetupProgress =
    { plan : Maybe String
    , orgSettings : Bool
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
                        }

                _ ->
                    Nothing
        )
        (Query.string "plan")
        (Query.string "org")


compareParamsParser : Query.Parser CompareParams
compareParamsParser =
    Query.map3 CompareParams
        (Query.string "id")
        (Query.string "orgId")
        (Query.string "tid")



-- Parse the orgId


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map (PublicRoute HomeRoute) top
        , map (PublicRoute WaitlistRoute) (s "waitlist")
        , map (PublicRoute LoginRoute) (s "login")
        , map (PublicRoute SignupRoute) (s "signup")
        , map (PublicRoute OnboardingRoute) (s "onboarding")
        , map (PublicRoute PricingRoute) (s "pricing")
        , map (PublicRoute Pricing2Route) (s "pricing2")
        , map (PublicRoute ScheduleMainRoute) (s "schedule-main")
        , map (ProtectedRoute ContactUsRoute) (s "contact-us")
        , map (\orgSlug token -> PublicRoute (VerifyRoute (VerifyParams orgSlug token)))
            (s "auth" </> s "verify" </> string </> string)
        , oneOf
            [ map (\quoteId tid -> PublicRoute (CompareRoute { quoteId = Just quoteId, orgId = Nothing, tid = tid }))
                (s "compare" </> string <?> Query.string "tid")
            , map (PublicRoute << CompareRoute)
                (s "compare" <?> compareParamsParser)
            ]
        , map (\quoteId -> PublicRoute (CompareRoute { quoteId = Just quoteId, orgId = Nothing, tid = Nothing }))
            (s "compare" </> string)
        , map (PublicRoute << QuoteRoute)
            (s "quote"
                <?> Query.map4
                        (\id tid planType orgId ->
                            { quoteId = id, trackingId = tid, planType = planType, orgId = orgId }
                        )
                        (Query.string "id")
                        (Query.string "tid")
                        (Query.string "planType")
                        (Query.string "orgId")
            )
        , map (PublicRoute << EligibilityRoute)
            (s "eligibility"
                <?> Query.map3
                        (\id tid orgId ->
                            ( id, tid, orgId )
                        )
                        (Query.string "id")
                        (Query.string "tid")
                        (Query.string "orgId")
            )
        , map (PublicRoute << ScheduleRoute)
            (s "schedule"
                <?> Query.map3 (\id status tid -> ( id, status, tid ))
                        (Query.string "id")
                        (Query.string "status")
                        (Query.string "tid")
            )
        , map (\orgSlug -> PublicRoute (SelfOnboardingRoute orgSlug))
            (s "self-onboarding" </> string)
        , map (PublicRoute << LandingRoute)
            (s "landing"
                <?> Query.map (\id -> { quoteId = id })
                        (Query.string "id")
            )
        , map (ProtectedRoute ChangePlanRoute) (s "change-plan")
        , map (ProtectedRoute ContactsRoute) (s "contacts")
        , map (AdminRoute SettingsRoute) (s "settings")
        , map (ProtectedRoute ProfileRoute) (s "profile")
        , map (ProtectedRoute TempLandingRoute) (s "templanding")
        , map (ProtectedRoute WalkthroughRoute) (s "walkthrough")
        , map (ProtectedRoute StripeRoute) (s "stripe")
        , map (AdminRoute AgentsRoute) (s "add-agents")
        , map (ProtectedRoute DashboardRoute) (s "dashboard")
        , map (\id -> ProtectedRoute (ContactRoute id)) (s "contact" </> string)
        , map (\progress -> SetupRoute (ChoosePlanRoute progress))
            (s "choose-plan" <?> setupProgressDecoder)
        , map (\progress -> SetupRoute (SetupSettingsRoute progress))
            (s "setup" </> s "settings" <?> setupProgressDecoder)
        , map (\progress -> SetupRoute (AddAgentsRoute progress))
            (s "setup" </> s "add-agents" <?> setupProgressDecoder)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogTrackingClickResult result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SetSessionResponse result ->
            case result of
                Ok sessionResponse ->
                    ( model
                    , if sessionResponse.success then
                        Cmd.none

                      else
                        Nav.pushUrl model.key "/login"
                    )

                Err _ ->
                    ( model
                    , Nav.pushUrl model.key "/login"
                    )

        DirectPageUpdate ->
            -- Force updatePage even if we're in Unknown session state
            updatePageForcePublic model.url ( model, Cmd.none )

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
            ( { model
                | showDropdown = False
                , showPaymentStatus = False
              }
            , Nav.pushUrl model.key frag
            )

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

        ContactUsMsg subMsg ->
            case model.page of
                ContactUsPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            ContactUs.update subMsg pageModel
                    in
                    ( { model | page = ContactUsPage newPageModel }
                    , Cmd.map ContactUsMsg newCmd
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

        ChangePlanMsg subMsg ->
            case model.page of
                ChangePlanPage pageModel ->
                    let
                        ( updatedPageModel, updatedCmd ) =
                            ChangePlan.update subMsg pageModel
                    in
                    ( { model | page = ChangePlanPage updatedPageModel }
                    , Cmd.map ChangePlanMsg updatedCmd
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
                                ChoosePlan.init response.orgSlug response.session model.key False

                            -- Only set isSetup to True if we're being redirected to a setup route
                            isInSetup =
                                String.startsWith "/choose-plan" response.redirectUrl
                                    || String.startsWith "/setup" response.redirectUrl

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
                                            , organizationId = response.orgSlug
                                            , firstName = ""
                                            , lastName = ""
                                            , subscriptionTier = ""
                                            , accountStatus = Nothing
                                            , demoMode = False
                                            , orgCreateDate = Nothing
                                            , hasCompletedWalkthrough = False
                                            }
                                    , isSetup = isInSetup
                                    , page = LoadingPage -- Force to loading page to prevent UI flicker during redirection
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ -- Instead of direct navigation, use a message to redirect
                              case model.intendedDestination of
                                Just destination ->
                                    Task.perform PerformRedirect (Task.succeed destination)

                                Nothing ->
                                    Task.perform PerformRedirect (Task.succeed response.redirectUrl)
                            , fetchCurrentUser
                            ]
                        )

                    else
                        ( model, Nav.pushUrl model.key "/login" )

                Err error ->
                    ( model, Nav.pushUrl model.key "/login" )

        PerformRedirect url ->
            -- Navigate to the specified URL
            ( model, Nav.pushUrl model.key url )

        GotSession result ->
            case result of
                Ok response ->
                    if response.valid then
                        let
                            user =
                                { id = response.id
                                , email = response.email
                                , isAdmin = response.isAdmin
                                , isAgent = True -- We'll get this from /api/me endpoint
                                , organizationSlug = response.organizationSlug
                                , organizationId = response.organizationSlug -- Use the org slug as org ID for now
                                , firstName = response.firstName
                                , lastName = response.lastName
                                , subscriptionTier = ""
                                , accountStatus = Nothing
                                , demoMode = response.demoMode
                                , orgCreateDate = Just response.orgCreateDate
                                , hasCompletedWalkthrough = False
                                }

                            -- Initialize demo mode banner with user's demo mode state
                            ( demoModeBannerModel, demoModeBannerCmd ) =
                                DemoModeBanner.init response.demoMode (Just response.orgCreateDate)

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
                                    , demoModeBanner = demoModeBannerModel
                                }
                        in
                        -- Just update the page, which will handle fetching user data if needed
                        updatePage model.url ( newModel, Cmd.map DemoModeBannerMsg demoModeBannerCmd )

                    else
                        let
                            newModel =
                                { model | session = NoSession }
                        in
                        -- For invalid session, update page which will handle redirects
                        updatePage model.url ( newModel, Cmd.none )

                Err error ->
                    let
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
                        ( newPageModel, homeCmd ) =
                            Home.update subMsg pageModel
                    in
                    ( { model | page = HomePage newPageModel }
                    , Cmd.map HomeMsg homeCmd
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

        CompareMsg subMsg ->
            case model.page of
                ComparePage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Compare.update subMsg pageModel
                    in
                    ( { model | page = ComparePage newPageModel }
                    , Cmd.map CompareMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        QuoteMsg subMsg ->
            case model.page of
                QuotePage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Quote.update subMsg pageModel
                    in
                    ( { model | page = QuotePage newPageModel }
                    , Cmd.map QuoteMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        EligibilityMsg subMsg ->
            case model.page of
                EligibilityPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Eligibility.update subMsg pageModel
                    in
                    ( { model | page = EligibilityPage newPageModel }
                    , Cmd.map EligibilityMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ScheduleMsg subMsg ->
            case model.page of
                SchedulePage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Schedule.update subMsg pageModel
                    in
                    ( { model | page = SchedulePage newPageModel }
                    , Cmd.map ScheduleMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ScheduleMainMsg subMsg ->
            case model.page of
                ScheduleMainPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            ScheduleMain.update subMsg pageModel
                    in
                    ( { model | page = ScheduleMainPage newPageModel }
                    , Cmd.map ScheduleMainMsg newCmd
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

        WaitlistMsg subMsg ->
            case model.page of
                WaitlistPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Waitlist.update subMsg pageModel
                    in
                    ( { model | page = WaitlistPage newPageModel }
                    , Cmd.map WaitlistMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        GotCurrentUser result ->
            case result of
                Ok response ->
                    case response.user of
                        Just user ->
                            let
                                currentUser =
                                    Just
                                        { id = user.id
                                        , email = user.email
                                        , isAdmin = user.isAdmin
                                        , isAgent = user.isAgent
                                        , organizationSlug = user.organizationSlug
                                        , organizationId = user.organizationId
                                        , firstName = user.firstName
                                        , lastName = user.lastName
                                        , subscriptionTier = user.subscriptionTier
                                        , accountStatus = Nothing -- Will fetch this separately
                                        , demoMode = user.demoMode
                                        , orgCreateDate = user.orgCreateDate
                                        , hasCompletedWalkthrough = user.hasCompletedWalkthrough
                                        }

                                -- Initialize demo mode banner with the user's demo mode status
                                ( demoModeBannerModel, demoModeBannerCmd ) =
                                    DemoModeBanner.init user.demoMode user.orgCreateDate

                                newModel =
                                    { model
                                        | currentUser = currentUser
                                        , demoModeBanner = demoModeBannerModel
                                    }

                                -- Fetch account status after user is loaded
                                cmd =
                                    Cmd.batch
                                        [ Cmd.none -- fetchAccountStatus user.organizationSlug
                                        , Cmd.map DemoModeBannerMsg demoModeBannerCmd
                                        ]
                            in
                            -- Check if we were already on the right page with the right data
                            -- Only update the page if something meaningful has changed
                            case model.currentUser of
                                Just existingUser ->
                                    if existingUser.id == user.id && existingUser.organizationSlug == user.organizationSlug then
                                        -- We already have the same user, just update the model without triggering updatePage
                                        --( newModel, cmd )
                                        updatePage model.url ( newModel, cmd )

                                    else
                                        -- User has changed, update the page
                                        updatePage model.url ( newModel, cmd )

                                Nothing ->
                                    -- We didn't have a user before, update the page
                                    updatePage model.url ( newModel, cmd )

                        Nothing ->
                            -- No user data, but we should still update the page to avoid being stuck
                            updatePage model.url ( model, Cmd.none )

                Err error ->
                    -- Error retrieving user data, but we should still update the page to avoid being stuck
                    updatePage model.url ( model, Cmd.none )

        GotAccountStatus result ->
            case result of
                Ok response ->
                    if response.success then
                        -- Update user with account status
                        let
                            updatedUser =
                                model.currentUser
                                    |> Maybe.map
                                        (\user ->
                                            { user | accountStatus = Just response.status }
                                        )

                            updatedModel =
                                { model | currentUser = updatedUser }
                        in
                        -- Now that we have all data (session, user, account status), update the page
                        updatePage model.url ( updatedModel, Cmd.none )

                    else
                        ( model, Cmd.none )

                Err _ ->
                    -- Even if there's an error getting account status, we should still update the page
                    -- rather than staying on the loading screen
                    updatePage model.url ( model, Cmd.none )

        CloseStatusBanner ->
            ( { model | showStatusBanner = False }
            , Cmd.none
            )

        OrgFinalized result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                -- Navigation already happened
                Err _ ->
                    ( { model | page = LoadingPage }
                    , Nav.pushUrl model.key "/settings"
                      -- Redirect to settings on error
                    )

        LogoutMsg subMsg ->
            case model.page of
                LogoutPage logoutModel ->
                    let
                        ( newLogoutModel, logoutCmd ) =
                            Logout.update subMsg logoutModel
                    in
                    ( { model | page = LogoutPage newLogoutModel }
                    , Cmd.map LogoutMsg logoutCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleDropdown ->
            ( { model | showDropdown = not model.showDropdown }
            , Cmd.none
            )

        CloseDropdown ->
            ( { model
                | showDropdown = False
                , showPaymentStatus = False
              }
            , Cmd.none
            )

        InitiateLogout ->
            ( { model
                | session = NoSession
                , currentUser = Nothing
                , showDropdown = False
              }
            , Cmd.batch
                [ Nav.pushUrl model.key "/"
                , Http.post
                    { url = "/api/auth/logout"
                    , body = Http.emptyBody
                    , expect = Http.expectWhatever (\_ -> NoOp)
                    }
                ]
            )

        NoOp ->
            ( model, Cmd.none )

        OnboardingMsg subMsg ->
            case model.page of
                OnboardingPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Onboarding.update subMsg pageModel
                    in
                    ( { model | page = OnboardingPage newPageModel }
                    , Cmd.map OnboardingMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        WalkthroughMsg subMsg ->
            case model.page of
                WalkthroughPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Walkthrough.update subMsg pageModel
                    in
                    ( { model | page = WalkthroughPage newPageModel }
                    , Cmd.map WalkthroughMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        SelfOnboardingMsg subMsg ->
            case model.page of
                SelfOnboardingPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            SelfServiceOnboarding.update subMsg pageModel
                    in
                    ( { model | page = SelfOnboardingPage newPageModel }
                    , Cmd.map SelfOnboardingMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ShowDropdown ->
            ( { model | showDropdown = True }, Cmd.none )

        HideDropdown ->
            ( { model | showDropdown = False }, Cmd.none )

        ToggleStatusBanner ->
            ( { model | showStatusBanner = not model.showStatusBanner }, Cmd.none )

        LandingMsg subMsg ->
            case model.page of
                LandingPage landingModel ->
                    case subMsg of
                        Landing.NavigateTo path ->
                            ( model
                            , Nav.pushUrl model.key path
                            )

                _ ->
                    ( model, Cmd.none )

        PricingMsg subMsg ->
            case model.page of
                PricingPage pricingModel ->
                    let
                        ( newPricingModel, newCmd ) =
                            Pricing.update subMsg pricingModel
                    in
                    ( { model | page = PricingPage newPricingModel }
                    , Cmd.map PricingMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        Pricing2Msg subMsg ->
            case model.page of
                Pricing2Page pricing2Model ->
                    let
                        ( newPricing2Model, newCmd ) =
                            Pricing2.update subMsg pricing2Model
                    in
                    ( { model | page = Pricing2Page newPricing2Model }
                    , Cmd.map Pricing2Msg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        StripeMsg subMsg ->
            case model.page of
                StripePage stripeModel ->
                    let
                        ( newStripeModel, newCmd ) =
                            Stripe.update subMsg stripeModel
                    in
                    ( { model | page = StripePage newStripeModel }
                    , Cmd.map StripeMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        TogglePaymentStatus ->
            ( { model | showPaymentStatus = not model.showPaymentStatus }, Cmd.none )

        DemoModeBannerMsg subMsg ->
            let
                ( updatedBanner, bannerCmd ) =
                    DemoModeBanner.update subMsg model.demoModeBanner

                -- Update the user model if demo mode changed
                updatedUser =
                    if model.demoModeBanner.demoMode /= updatedBanner.demoMode then
                        model.currentUser
                            |> Maybe.map (\user -> { user | demoMode = updatedBanner.demoMode })

                    else
                        model.currentUser
            in
            ( { model
                | demoModeBanner = updatedBanner
                , currentUser = updatedUser
              }
            , Cmd.map DemoModeBannerMsg bannerCmd
            )


view : Model -> Browser.Document Msg
view model =
    let
        viewPage =
            case model.page of
                NotFoundPage ->
                    viewNotFound

                WaitlistPage waitlistModel ->
                    let
                        waitlistView =
                            Waitlist.view waitlistModel
                    in
                    { title = waitlistView.title
                    , body = [ viewWithNav model (Html.map WaitlistMsg (div [] waitlistView.body)) ]
                    }

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

                ContactUsPage contactUsModel ->
                    let
                        contactUsView =
                            ContactUs.view contactUsModel
                    in
                    { title = contactUsView.title
                    , body = [ viewWithNav model (Html.map ContactUsMsg (div [] contactUsView.body)) ]
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

                ChangePlanPage changePlanModel ->
                    let
                        changePlanView =
                            ChangePlan.view changePlanModel
                    in
                    { title = changePlanView.title
                    , body = [ viewWithNav model (Html.map ChangePlanMsg (div [] changePlanView.body)) ]
                    }

                AddAgentsPage addAgentModel ->
                    let
                        addAgentView =
                            AddAgent.view addAgentModel
                    in
                    { title = addAgentView.title
                    , body =
                        if addAgentModel.isSetup then
                            -- In setup flow, don't show the header
                            [ Html.map AddAgentsMsg (div [] addAgentView.body) ]

                        else
                            -- Not in setup flow, show the header
                            [ viewWithNav model (Html.map AddAgentsMsg (div [] addAgentView.body)) ]
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
                    , body = [ viewWithNav model (Html.map HomeMsg (div [] homeView.body)) ]
                    }

                ContactPage contactModel ->
                    let
                        contactView =
                            Contact.view contactModel
                    in
                    { title = contactView.title
                    , body = [ viewWithNav model (Html.map ContactMsg (div [] contactView.body)) ]
                    }

                ComparePage compareModel ->
                    let
                        compareView =
                            Compare.view compareModel
                    in
                    { title = compareView.title
                    , body = [ viewWithNav model (Html.map CompareMsg (div [] compareView.body)) ]
                    }

                QuotePage quoteModel ->
                    let
                        quoteView =
                            Quote.view quoteModel
                    in
                    { title = quoteView.title
                    , body = [ viewWithNav model (Html.map QuoteMsg (div [] quoteView.body)) ]
                    }

                EligibilityPage eligibilityModel ->
                    let
                        eligibilityView =
                            Eligibility.view eligibilityModel
                    in
                    { title = eligibilityView.title
                    , body = [ viewWithNav model (Html.map EligibilityMsg (div [] eligibilityView.body)) ]
                    }

                SchedulePage scheduleModel ->
                    let
                        scheduleView =
                            Schedule.view scheduleModel
                    in
                    { title = scheduleView.title
                    , body = [ viewWithNav model (Html.map ScheduleMsg (div [] scheduleView.body)) ]
                    }

                ScheduleMainPage scheduleMainModel ->
                    let
                        scheduleMainView =
                            ScheduleMain.view scheduleMainModel
                    in
                    { title = scheduleMainView.title
                    , body = [ viewWithNav model (Html.map ScheduleMainMsg (div [] scheduleMainView.body)) ]
                    }

                DashboardPage dashboardModel ->
                    let
                        dashboardView =
                            Dashboard.view dashboardModel
                    in
                    { title = dashboardView.title
                    , body = [ viewWithNav model (Html.map DashboardMsg (div [] dashboardView.body)) ]
                    }

                LogoutPage logoutModel ->
                    let
                        logoutView =
                            Logout.view logoutModel
                    in
                    { title = logoutView.title
                    , body = List.map (Html.map LogoutMsg) logoutView.body
                    }

                OnboardingPage pageModel ->
                    let
                        onboardingView =
                            Onboarding.view pageModel
                    in
                    { title = onboardingView.title

                    -- Use the body from the Onboarding.view Document directly
                    , body = List.map (Html.map OnboardingMsg) onboardingView.body
                    }

                WalkthroughPage pageModel ->
                    { title = "Walkthrough"
                    , body = [ viewWithNav model (Html.map WalkthroughMsg (Walkthrough.view pageModel)) ]
                    }

                SelfOnboardingPage pageModel ->
                    let
                        selfOnboardingView =
                            SelfServiceOnboarding.view pageModel
                    in
                    { title = selfOnboardingView.title
                    , body = [ viewWithNav model (Html.map SelfOnboardingMsg (div [] selfOnboardingView.body)) ]
                    }

                LandingPage landingModel ->
                    let
                        landingView =
                            Landing.view landingModel
                    in
                    { title = landingView.title
                    , body = [ viewWithNav model (Html.map LandingMsg (div [] landingView.body)) ]
                    }

                PricingPage pricingModel ->
                    let
                        pricingView =
                            Pricing.view pricingModel
                    in
                    { title = "Pricing"
                    , body = [ viewWithNav model (Html.map PricingMsg pricingView) ]
                    }

                Pricing2Page pricing2Model ->
                    let
                        pricing2View =
                            Pricing2.view pricing2Model
                    in
                    { title = "Pricing"
                    , body = [ viewWithNav model (Html.map Pricing2Msg pricing2View) ]
                    }

                StripePage stripeModel ->
                    let
                        stripeView =
                            Stripe.view stripeModel
                    in
                    { title = stripeView.title
                    , body = [ viewWithNav model (Html.map StripeMsg (div [] stripeView.body)) ]
                    }
    in
    viewPage


viewWithNav : Model -> Html Msg -> Html Msg
viewWithNav model content =
    div []
        [ if model.isSetup then
            -- Don't show header during setup flow
            content

          else
            -- Show header for regular pages
            div []
                [ viewNavHeader model
                , Html.map DemoModeBannerMsg (DemoModeBanner.view model.demoModeBanner)
                , content
                ]
        ]


viewPublicNav : Model -> Html Msg
viewPublicNav model =
    div []
        [ -- Desktop navigation
          nav [ class "max-w-7xl mx-auto px-6 sm:px-6 lg:px-8 py-4 sm:py-6 sticky top-0 z-50 bg-white hidden lg:block" ]
            [ div
                [ class "flex justify-between items-center" ]
                [ div [ class "flex items-center" ]
                    [ a [ href "/" ]
                        [ img
                            [ src "/images/medicare-max-logo.png"
                            , class "h-6 sm:h-8 w-auto"
                            , alt "Medicare Max logo"
                            ]
                            []
                        ]
                    ]
                , div [ class "flex items-center justify-end gap-8" ]
                    [ div [ class "flex items-center gap-2" ]
                        [ button
                            [ onClick (InternalLinkClicked "/schedule-main")
                            , class "px-4 text-gray-600 hover:text-gray-900 text-base font-medium cursor-pointer transition-all duration-200"
                            ]
                            [ text "Book a Demo" ]
                        , button
                            [ onClick (InternalLinkClicked "/pricing")
                            , class "px-4 text-gray-600 hover:text-gray-900 text-base font-medium cursor-pointer transition-all duration-200"
                            ]
                            [ text "Pricing" ]
                        ]
                    , div [ class "flex items-center gap-x-3" ]
                        [ button
                            [ onClick (InternalLinkClicked "/signup")
                            , class "bg-[#03045E] text-white border-2 border-[#03045E] px-4 py-2 rounded-lg text-sm font-medium hover:bg-[#1a1f5f] transition-colors duration-200 w-24 text-center"
                            ]
                            [ text "Sign up" ]
                        , button
                            [ onClick (InternalLinkClicked "/login")
                            , class "bg-white text-[#03045E] border-2 border-[#03045E] px-4 py-2 rounded-lg text-sm font-medium hover:bg-gray-50 transition-colors duration-200 w-24 text-center"
                            ]
                            [ text "Log in" ]
                        ]
                    ]
                ]
            ]

        -- Mobile navigation
        , nav [ class "lg:hidden sticky top-0 z-50 bg-white" ]
            [ div [ class "max-w-7xl mx-auto px-4 py-4" ]
                [ div [ class "flex justify-between items-center" ]
                    [ a [ href "/" ]
                        [ img
                            [ src "/images/medicare-max-logo.png"
                            , class "h-6 w-auto"
                            , alt "Medicare Max logo"
                            ]
                            []
                        ]
                    , button
                        [ onClick ToggleDropdown
                        , class "p-2 rounded-md text-gray-600 hover:text-gray-900 hover:bg-gray-100 focus:outline-none"
                        ]
                        [ -- Hamburger icon
                          svg
                            [ Svg.Attributes.class "h-6 w-6"
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.viewBox "0 0 24 24"
                            , Svg.Attributes.stroke "currentColor"
                            ]
                            [ path
                                [ Svg.Attributes.strokeLinecap "round"
                                , Svg.Attributes.strokeLinejoin "round"
                                , Svg.Attributes.strokeWidth "2"
                                , Svg.Attributes.d "M4 6h16M4 12h16M4 18h16"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , if model.showDropdown then
                div
                    [ class "absolute top-full left-0 w-full bg-white shadow-lg border-t border-gray-200"
                    , stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
                    ]
                    [ div [ class "px-4 py-2 space-y-2" ]
                        [ button
                            [ onClick (InternalLinkClicked "/schedule-main")
                            , class "block w-full text-left px-4 py-2 text-gray-700 hover:bg-gray-100 rounded-md"
                            ]
                            [ text "Book a Demo" ]
                        , button
                            [ onClick (InternalLinkClicked "/pricing")
                            , class "block w-full text-left px-4 py-2 text-gray-700 hover:bg-gray-100 rounded-md"
                            ]
                            [ text "Pricing" ]
                        , button
                            [ onClick (InternalLinkClicked "/contact-us")
                            , class "block w-full text-left px-4 py-2 text-gray-700 hover:bg-gray-100 rounded-md"
                            ]
                            [ text "Contact Us" ]
                        , button
                            [ onClick (InternalLinkClicked "/signup")
                            , class "block w-full text-left px-4 py-2 text-[#03045E] font-medium hover:bg-gray-100 rounded-md"
                            ]
                            [ text "Signup" ]
                        , button
                            [ onClick (InternalLinkClicked "/login")
                            , class "block w-full text-left px-4 py-2 text-[#03045E] font-medium hover:bg-gray-100 rounded-md"
                            ]
                            [ text "Login" ]
                        ]
                    ]

              else
                text ""
            ]
        ]


viewNavHeader : Model -> Html Msg
viewNavHeader model =
    let
        -- Check if current page is one of the quote flow pages that should have simplified header
        isQuoteFlowPage =
            case model.page of
                QuotePage _ ->
                    True

                ComparePage _ ->
                    True

                EligibilityPage _ ->
                    True

                SchedulePage _ ->
                    True

                SelfOnboardingPage _ ->
                    True

                LandingPage _ ->
                    True

                _ ->
                    False

        isPublicPage =
            case model.page of
                HomePage _ ->
                    True

                PricingPage _ ->
                    True

                Pricing2Page _ ->
                    True

                WaitlistPage _ ->
                    True

                ScheduleMainPage _ ->
                    True

                _ ->
                    False

        isPaymentActive =
            model.currentUser
                |> Maybe.andThen .accountStatus
                |> Maybe.map (\status -> status.paymentCompleted)
                |> Maybe.withDefault False

        paymentStatusMessage =
            model.currentUser
                |> Maybe.andThen .accountStatus
                |> Maybe.map
                    (\status ->
                        if status.subscriptionStatus == "active" then
                            "Payments active"

                        else
                            "Payment needs to be updated. Click to update payment information."
                    )
                |> Maybe.withDefault "Payment status unknown"

        paymentStatusIcon =
            if isPaymentActive then
                MyIcon.zap 20 "#10B981"
                -- Green color for active

            else
                MyIcon.zapOff 20 "#EF4444"

        -- Red color for inactive
        paymentStatusIndicator =
            div [ class "relative" ]
                [ button
                    [ class "flex items-center space-x-2 px-3 py-1.5 text-gray-700 text-sm font-medium hover:bg-[#DCE2E5] rounded-md transition-colors duration-200"
                    , onClick TogglePaymentStatus
                    ]
                    [ div [ class "flex items-center space-x-2" ]
                        [ paymentStatusIcon
                        ]
                    ]
                , if model.showPaymentStatus then
                    div
                        [ class "absolute right-0 mt-2 w-64 rounded-md shadow-lg py-1 bg-white ring-1 ring-black ring-opacity-5 z-50"
                        , stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
                        ]
                        [ div [ class "px-4 py-2 text-sm text-gray-700" ]
                            [ text paymentStatusMessage
                            , if not isPaymentActive then
                                button
                                    [ class "mt-2 w-full px-4 py-2 bg-red-100 text-red-700 rounded-md hover:bg-red-200 transition-colors duration-200"
                                    , onClick (InternalLinkClicked "/stripe")
                                    ]
                                    [ text "Update Payment" ]

                              else
                                text ""
                            ]
                        ]

                  else
                    text ""
                ]
    in
    if isPublicPage then
        viewPublicNav model

    else if isQuoteFlowPage then
        text ""

    else
        -- Full header with navigation for other pages
        nav [ class "bg-white" ]
            [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                [ div [ class "flex justify-between h-16" ]
                    [ div [ class "flex items-center" ]
                        [ div [ class "shrink-0 flex items-center" ]
                            [ a
                                [ href "#"
                                , onClick (InternalLinkClicked "/dashboard")
                                , class "cursor-pointer"
                                ]
                                [ img
                                    [ src "/images/medicare-max-logo.png"
                                    , class "h-6 sm:h-6 w-auto mr-2 sm:mr-8"
                                    , alt "Medicare Max logo"
                                    ]
                                    []
                                ]
                            ]
                        , div [ class "hidden sm:flex items-center space-x-4" ]
                            [ button
                                [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:bg-[#DCE2E5] rounded-md transition-colors duration-200"
                                , onClick (InternalLinkClicked "/dashboard")
                                ]
                                [ text "Dashboard" ]
                            , button
                                [ class "px-3 py-1.5 text-gray-700 text-sm font-medium hover:bg-[#DCE2E5] rounded-md transition-colors duration-200"
                                , onClick (InternalLinkClicked "/contacts")
                                ]
                                [ text "Contacts" ]
                            ]
                        ]
                    , div [ class "flex items-center space-x-2" ]
                        [ div [ class "relative" ]
                            [ button
                                [ class "flex items-center space-x-1 sm:space-x-2 px-2 sm:px-3 py-1.5 text-gray-700 text-xs sm:text-sm font-medium hover:bg-[#DCE2E5] rounded-md transition-colors duration-200"
                                , onClick ToggleDropdown
                                , stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
                                ]
                                [ case model.currentUser of
                                    Just user ->
                                        text (user.firstName ++ " " ++ user.lastName)

                                    Nothing ->
                                        text "Menu"
                                , div [ class "w-4 h-4 flex-shrink-0" ]
                                    [ svg
                                        [ Svg.Attributes.viewBox "0 0 20 20"
                                        , Svg.Attributes.fill "currentColor"
                                        ]
                                        [ path
                                            [ Svg.Attributes.d "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z" ]
                                            []
                                        ]
                                    ]
                                ]
                            , if model.showDropdown then
                                div
                                    [ class "absolute right-0 mt-2 w-48 rounded-md shadow-lg py-1 bg-white ring-1 ring-black ring-opacity-5 z-50"
                                    , stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
                                    ]
                                    [ -- Always show on mobile
                                      div [ class "block sm:hidden" ]
                                        [ button
                                            [ class "block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-[#DCE2E5]"
                                            , onClick (InternalLinkClicked "/dashboard")
                                            ]
                                            [ text "Dashboard" ]
                                        , button
                                            [ class "block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-[#DCE2E5]"
                                            , onClick (InternalLinkClicked "/contacts")
                                            ]
                                            [ text "Contacts" ]
                                        ]
                                    , if isAdmin model.currentUser then
                                        button
                                            [ class "block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-[#DCE2E5]"
                                            , onClick (InternalLinkClicked "/profile")
                                            ]
                                            [ text "Profile" ]

                                      else
                                        text ""
                                    , if isAdmin model.currentUser then
                                        button
                                            [ class "block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-[#DCE2E5]"
                                            , onClick (InternalLinkClicked "/settings")
                                            ]
                                            [ text "Organization Settings" ]

                                      else
                                        text ""
                                    , if isAdmin model.currentUser then
                                        button
                                            [ class "block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-[#DCE2E5]"
                                            , onClick (InternalLinkClicked "/add-agents")
                                            ]
                                            [ text "Agents" ]

                                      else
                                        text ""
                                    , button
                                        [ class "block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-[#DCE2E5]"
                                        , onClick (InternalLinkClicked "/stripe")
                                        ]
                                        [ text "Payment Settings" ]
                                    , button
                                        [ class "block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-[#DCE2E5]"
                                        , onClick (InternalLinkClicked "/contact-us")
                                        ]
                                        [ text "Contact Us" ]
                                    , button
                                        [ class "block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-[#DCE2E5]"
                                        , onClick InitiateLogout
                                        ]
                                        [ text "Log out" ]
                                    ]

                              else
                                text ""
                            ]
                        ]
                    ]
                ]
            ]


isAdminOrAdminAgent : Maybe User -> Bool
isAdminOrAdminAgent maybeUser =
    case maybeUser of
        Just user ->
            user.isAdmin && user.isAgent

        Nothing ->
            False


isAdmin : Maybe User -> Bool
isAdmin maybeUser =
    case maybeUser of
        Just user ->
            user.isAdmin

        Nothing ->
            False


viewNotFound : Browser.Document msg
viewNotFound =
    { title = "404 - Page Not Found"
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
    let
        dropdownSub =
            if model.showDropdown || model.showPaymentStatus then
                Browser.Events.onMouseDown (Decode.succeed CloseDropdown)

            else
                Sub.none

        escDropdownSub =
            if model.showDropdown then
                Browser.Events.onKeyDown (Decode.succeed CloseDropdown)

            else
                Sub.none

        pageSubs =
            case model.page of
                LoadingPage ->
                    Sub.none

                WaitlistPage pageModel ->
                    Sub.map WaitlistMsg (Waitlist.subscriptions pageModel)

                LoginPage pageModel ->
                    Sub.map LoginMsg (Login.subscriptions pageModel)

                ContactsPage pageModel ->
                    Sub.map ContactsMsg (Contacts.subscriptions pageModel)

                ContactUsPage pageModel ->
                    Sub.none

                TempLandingPage pageModel ->
                    Sub.map TempLandingMsg (TempLanding.subscriptions pageModel)

                SettingsPage pageModel ->
                    Sub.map SettingsMsg (Settings.subscriptions pageModel)

                Signup signupModel ->
                    Sub.map SignupMsg (Signup.subscriptions signupModel)

                ChoosePlanPage pageModel ->
                    Sub.map ChoosePlanMsg (ChoosePlan.subscriptions pageModel)

                ChangePlanPage pageModel ->
                    Sub.map ChangePlanMsg (ChangePlan.subscriptions pageModel)

                AddAgentsPage pageModel ->
                    Sub.map AddAgentsMsg (AddAgent.subscriptions pageModel)

                ProfilePage pageModel ->
                    Sub.map ProfileMsg (Profile.subscriptions pageModel)

                HomePage pageModel ->
                    Sub.map HomeMsg (Home.subscriptions pageModel)

                ContactPage pageModel ->
                    Sub.map ContactMsg (Contact.subscriptions pageModel)

                ComparePage pageModel ->
                    Sub.map CompareMsg (Compare.subscriptions pageModel)

                QuotePage pageModel ->
                    Sub.map QuoteMsg (Quote.subscriptions pageModel)

                EligibilityPage pageModel ->
                    Sub.map EligibilityMsg (Eligibility.subscriptions pageModel)

                SchedulePage pageModel ->
                    Sub.map ScheduleMsg (Schedule.subscriptions pageModel)

                ScheduleMainPage pageModel ->
                    Sub.map ScheduleMainMsg (ScheduleMain.subscriptions pageModel)

                DashboardPage pageModel ->
                    Sub.map DashboardMsg (Dashboard.subscriptions pageModel)

                NotFoundPage ->
                    Sub.none

                LogoutPage pageModel ->
                    Sub.map LogoutMsg (Logout.subscriptions pageModel)

                OnboardingPage pageModel ->
                    Sub.map OnboardingMsg (Onboarding.subscriptions pageModel)

                WalkthroughPage pageModel ->
                    Sub.map WalkthroughMsg (Walkthrough.subscriptions pageModel)

                SelfOnboardingPage _ ->
                    Sub.none

                LandingPage landingModel ->
                    Sub.map LandingMsg (Landing.subscriptions landingModel)

                PricingPage pricingModel ->
                    Sub.none

                Pricing2Page pricing2Model ->
                    Sub.map Pricing2Msg (Pricing2.subscriptions pricing2Model)

                StripePage stripeModel ->
                    Sub.none
    in
    Sub.batch [ dropdownSub, escDropdownSub, pageSubs ]


routeAccessType : Route -> RouteAccess
routeAccessType route =
    case route of
        PublicRoute _ ->
            Public

        ProtectedRoute _ ->
            Protected

        AdminRoute _ ->
            Protected

        -- Still Protected, but we'll check admin status separately
        SetupRoute _ ->
            Setup

        NotFound ->
            Public


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Pipeline.required "id" (Decode.map String.fromInt Decode.int)
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "is_admin"
            (Decode.oneOf
                [ Decode.bool
                , Decode.int |> Decode.map (\n -> n == 1)
                ]
            )
        |> Pipeline.required "is_agent"
            (Decode.oneOf
                [ Decode.bool
                , Decode.int |> Decode.map (\n -> n == 1)
                ]
            )
        |> Pipeline.required "organization_slug" Decode.string
        |> Pipeline.required "organization_id" (Decode.map String.fromInt Decode.int)
        |> Pipeline.required "firstName" Decode.string
        |> Pipeline.required "lastName" Decode.string
        |> Pipeline.required "subscription_tier" Decode.string
        |> Pipeline.optional "accountStatus" (Decode.nullable accountStatusDecoder) Nothing
        |> Pipeline.optional "demo_mode" Decode.bool False
        |> Pipeline.required
            "orgCreateDate"
            (Decode.string
                |> Decode.andThen
                    (\s ->
                        case Date.fromIsoString s of
                            Ok date ->
                                Decode.succeed (Just date)

                            Err _ ->
                                Decode.fail "Invalid date format"
                    )
            )
        |> Pipeline.optional "hasCompletedWalkthrough" Decode.bool False


type SetupStep
    = NotStarted
    | PlanSelection
    | OrganizationSetup
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

            else if not (hasAgents user) then
                AgentSetup

            else
                Complete


hasOrganizationSettings : User -> Bool
hasOrganizationSettings user =
    -- Check if both organization and brand settings are configured
    case user.organizationSlug of
        "" ->
            False

        _ ->
            -- For now return True since we've merged brand settings into org settings
            -- TODO: Add actual check for required settings once API is updated
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

        AgentSetup ->
            case model.currentUser of
                Just user ->
                    ( model
                    , Nav.pushUrl model.key
                        ("/setup/add-agents?plan=" ++ user.organizationSlug ++ "&org=complete")
                    )

                Nothing ->
                    ( model, Nav.pushUrl model.key "/setup/add-agents" )

        Complete ->
            ( model, Nav.pushUrl model.key "/contacts" )


shouldRedirectAdminRoute : Route -> Model -> Maybe String
shouldRedirectAdminRoute route model =
    case route of
        AdminRoute _ ->
            case model.currentUser of
                Just user ->
                    if user.isAdmin then
                        Nothing
                        -- Admin user, no redirect needed

                    else
                        Just "/contacts"

                -- Non-admin user, redirect to contacts
                Nothing ->
                    Just "/login"

        -- Not logged in, redirect to login
        _ ->
            Nothing


shouldRedirectToLogin : Route -> Model -> Bool
shouldRedirectToLogin route model =
    let
        result =
            case route of
                PublicRoute _ ->
                    False

                NotFound ->
                    False

                AdminRoute _ ->
                    case model.session of
                        Verified _ ->
                            False

                        _ ->
                            True

                _ ->
                    case model.session of
                        Verified _ ->
                            False

                        _ ->
                            True
    in
    -- Add a debug log for the redirect check
    result


shouldRedirectToSetup : Route -> Model -> Bool
shouldRedirectToSetup route model =
    -- Only check setup state if we're in setup mode
    if model.isSetup then
        case route of
            SetupRoute _ ->
                -- Already in a setup route, no redirect needed
                False

            PublicRoute _ ->
                -- Public routes are always accessible
                False

            NotFound ->
                -- Not found routes don't redirect
                False

            AdminRoute _ ->
                -- Admin routes redirect if setup is not complete
                getSetupStep model /= Complete

            ProtectedRoute _ ->
                -- Protected routes redirect if setup is not complete
                getSetupStep model /= Complete

    else
        False


updatePage : Url -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePage url ( model, cmd ) =
    case model.session of
        Unknown ->
            -- When session state is Unknown, still allow public routes to render
            case Parser.parse routeParser url of
                Just route ->
                    case routeAccessType route of
                        Public ->
                            -- For public routes, redirect to the force update function
                            updatePageForcePublic url ( model, cmd )

                        _ ->
                            -- For non-public routes, keep showing loading while we wait
                            ( { model | page = LoadingPage }
                            , cmd
                            )

                Nothing ->
                    ( { model | page = NotFoundPage }
                    , cmd
                    )

        -- Rest of the function remains the same for Verified and NoSession states
        _ ->
            case Parser.parse routeParser url of
                Just route ->
                    let
                        -- Update isSetup based on the route
                        modelWithUpdatedSetup =
                            updateIsSetup model route

                        adminRedirect =
                            shouldRedirectAdminRoute route modelWithUpdatedSetup

                        -- Determine if we should make authenticated requests based on session state
                        -- Only fetch user data if we have a verified session AND don't already have user info
                        authCmd =
                            case ( model.session, model.currentUser ) of
                                ( Verified _, Nothing ) ->
                                    -- Only fetch user data if we have a verified session but no user data
                                    fetchCurrentUser

                                _ ->
                                    -- Don't make authenticated requests if no session or already have user data
                                    Cmd.none
                    in
                    case adminRedirect of
                        Just redirectUrl ->
                            ( modelWithUpdatedSetup, Nav.pushUrl modelWithUpdatedSetup.key redirectUrl )

                        Nothing ->
                            let
                                needsLogin =
                                    shouldRedirectToLogin route modelWithUpdatedSetup

                                needsSetup =
                                    shouldRedirectToSetup route modelWithUpdatedSetup
                            in
                            if needsLogin then
                                ( { modelWithUpdatedSetup
                                    | intendedDestination = Just (Url.toString url)
                                    , page = LoginPage (Login.init modelWithUpdatedSetup.key False url |> Tuple.first)
                                  }
                                , if String.contains "/login" (Url.toString url) then
                                    -- Already on login page, don't redirect
                                    Cmd.none

                                  else
                                    Nav.pushUrl modelWithUpdatedSetup.key "/login"
                                )

                            else if needsSetup then
                                -- Check if we're already on a setup route to prevent redirect loops
                                case route of
                                    SetupRoute _ ->
                                        -- Already on a setup route, just update the page
                                        ( modelWithUpdatedSetup, authCmd )

                                    _ ->
                                        redirectToSetupStep modelWithUpdatedSetup

                            else
                                -- Continue with the original logic for handling different routes
                                -- Rest of the function remains the same
                                case route of
                                    PublicRoute HomeRoute ->
                                        -- Home page handles its own session checking
                                        let
                                            ( homeModel, homeCmd ) =
                                                Home.init modelWithUpdatedSetup.key
                                        in
                                        ( { modelWithUpdatedSetup | page = HomePage homeModel }
                                        , Cmd.map HomeMsg homeCmd
                                        )

                                    PublicRoute WaitlistRoute ->
                                        let
                                            ( waitlistModel, waitlistCmd ) =
                                                Waitlist.init
                                        in
                                        ( { modelWithUpdatedSetup | page = WaitlistPage waitlistModel }
                                        , Cmd.map WaitlistMsg waitlistCmd
                                        )

                                    PublicRoute LoginRoute ->
                                        let
                                            ( loginModel, loginCmd ) =
                                                Login.init modelWithUpdatedSetup.key False url
                                        in
                                        ( { modelWithUpdatedSetup | page = LoginPage loginModel }
                                        , Cmd.map LoginMsg loginCmd
                                        )

                                    PublicRoute SignupRoute ->
                                        -- Initialize signup page without making authenticated API calls
                                        let
                                            ( signupModel, signupCmd ) =
                                                Signup.init modelWithUpdatedSetup.key
                                        in
                                        ( { modelWithUpdatedSetup | page = Signup signupModel }
                                        , Cmd.map SignupMsg signupCmd
                                        )

                                    PublicRoute OnboardingRoute ->
                                        let
                                            -- Extract the query parameters from the URL
                                            queryParams =
                                                url.query
                                                    |> Maybe.map (\q -> String.split "&" q)
                                                    |> Maybe.withDefault []
                                                    |> List.filterMap
                                                        (\param ->
                                                            case String.split "=" param of
                                                                key :: value :: [] ->
                                                                    Just ( key, value )

                                                                _ ->
                                                                    Nothing
                                                        )

                                            -- Initialize the new onboarding module with query params
                                            ( onboardingModel, onboardingCmd ) =
                                                Onboarding.init modelWithUpdatedSetup.key url
                                        in
                                        ( { modelWithUpdatedSetup | page = OnboardingPage onboardingModel }
                                        , Cmd.map OnboardingMsg onboardingCmd
                                        )

                                    PublicRoute (VerifyRoute params) ->
                                        -- For verification, we need to make an API call
                                        let
                                            verifyUrl =
                                                case params of
                                                    VerifyParams orgSlug token ->
                                                        "/api/auth/verify/" ++ orgSlug ++ "/" ++ token

                                            verifyCmd =
                                                Http.get
                                                    { url = verifyUrl
                                                    , expect = Http.expectJson GotVerification verificationDecoder
                                                    }
                                        in
                                        ( model, verifyCmd )

                                    PublicRoute (CompareRoute params) ->
                                        case params.quoteId of
                                            Just quoteId ->
                                                -- We have a quote ID, which is what we prefer
                                                let
                                                    ( compareModel, compareCmd ) =
                                                        Compare.init model.key (Just params)
                                                in
                                                ( { model | page = ComparePage compareModel }
                                                , Cmd.map CompareMsg compareCmd
                                                )

                                            Nothing ->
                                                -- No quote ID, check if we have a valid orgId
                                                if isValidOrgId params.orgId then
                                                    -- We have a valid orgId but no quoteId, so use the params
                                                    let
                                                        ( compareModel, compareCmd ) =
                                                            Compare.init model.key (Just params)
                                                    in
                                                    ( { model | page = ComparePage compareModel }
                                                    , Cmd.map CompareMsg compareCmd
                                                    )

                                                else
                                                    -- No valid orgId either, redirect to 404
                                                    ( { model | page = NotFoundPage }
                                                    , Nav.pushUrl model.key "/404"
                                                    )

                                    PublicRoute (QuoteRoute params) ->
                                        -- First check if there's a valid quoteId
                                        if isValidQuoteId params.quoteId then
                                            let
                                                initialValues =
                                                    { zipCode = Nothing
                                                    , dateOfBirth = Nothing
                                                    , tobacco = Nothing
                                                    , gender = Nothing
                                                    , quoteId = params.quoteId
                                                    , planType = params.planType
                                                    , orgId = params.orgId -- Pass orgId even if it's Nothing
                                                    }

                                                ( quoteModel, quoteCmd ) =
                                                    Quote.init model.key initialValues
                                            in
                                            ( { model | page = QuotePage quoteModel }
                                            , Cmd.map QuoteMsg quoteCmd
                                            )
                                            -- If there's no valid quoteId, show error

                                        else
                                            -- Redirect to an error page or show an error
                                            ( { model | page = NotFoundPage }
                                            , Nav.pushUrl model.key "/error?message=Missing%20or%20invalid%20quote%20ID"
                                            )

                                    PublicRoute (EligibilityRoute params) ->
                                        let
                                            ( quoteId, _, orgIdStr ) =
                                                params
                                        in
                                        if isValidOrgId orgIdStr then
                                            let
                                                ( eligibilityModel, eligibilityCmd ) =
                                                    Eligibility.init model.key { quoteId = quoteId, orgId = orgIdStr }
                                            in
                                            ( { model | page = EligibilityPage eligibilityModel }
                                            , Cmd.map EligibilityMsg eligibilityCmd
                                            )

                                        else
                                            -- Redirect to an error page or show an error
                                            ( { model | page = NotFoundPage }
                                            , Nav.pushUrl model.key "/error?message=Missing%20or%20invalid%20organization%20ID"
                                            )

                                    PublicRoute (ScheduleRoute params) ->
                                        let
                                            ( scheduleModel, scheduleCmd ) =
                                                Schedule.init model.key
                                                    ((\( id, _, _ ) -> id) params)
                                                    ((\( _, status, _ ) -> status) params)
                                        in
                                        ( { model | page = SchedulePage scheduleModel }
                                        , Cmd.map ScheduleMsg scheduleCmd
                                        )

                                    PublicRoute ScheduleMainRoute ->
                                        let
                                            ( scheduleMainModel, scheduleMainCmd ) =
                                                ScheduleMain.init model.key
                                        in
                                        ( { model | page = ScheduleMainPage scheduleMainModel }
                                        , Cmd.map ScheduleMainMsg scheduleMainCmd
                                        )

                                    ProtectedRoute ContactsRoute ->
                                        let
                                            -- Convert Main.elm User to Contacts.elm User format
                                            contactsUser =
                                                modelWithUpdatedSetup.currentUser
                                                    |> Maybe.map
                                                        (\user ->
                                                            { id = String.toInt user.id |> Maybe.withDefault 0
                                                            , email = user.email
                                                            , firstName = user.firstName
                                                            , lastName = user.lastName
                                                            , isAdmin = user.isAdmin
                                                            , isAgent = user.isAgent
                                                            , isDefault = False -- Add isDefault field
                                                            , organizationId = String.toInt user.organizationId |> Maybe.withDefault 0
                                                            , isActive = True -- Assume active
                                                            , phone = "" -- Default empty
                                                            , carriers = [] -- Default empty
                                                            , stateLicenses = [] -- Default empty
                                                            }
                                                        )

                                            ( contactsModel, contactsCmd ) =
                                                Contacts.init modelWithUpdatedSetup.key contactsUser
                                        in
                                        ( { modelWithUpdatedSetup | page = ContactsPage contactsModel }
                                        , Cmd.map ContactsMsg contactsCmd
                                        )

                                    ProtectedRoute ProfileRoute ->
                                        let
                                            ( profileModel, profileCmd ) =
                                                Profile.init ()
                                        in
                                        ( { modelWithUpdatedSetup | page = ProfilePage profileModel }
                                        , Cmd.batch
                                            [ Cmd.map ProfileMsg profileCmd
                                            , authCmd
                                            ]
                                        )

                                    ProtectedRoute TempLandingRoute ->
                                        let
                                            ( tempLandingModel, tempLandingCmd ) =
                                                TempLanding.init ()
                                        in
                                        ( { modelWithUpdatedSetup | page = TempLandingPage tempLandingModel }
                                        , Cmd.batch
                                            [ Cmd.map TempLandingMsg tempLandingCmd
                                            , authCmd
                                            ]
                                        )

                                    ProtectedRoute (ContactRoute id) ->
                                        let
                                            demoMode =
                                                case modelWithUpdatedSetup.currentUser of
                                                    Just user ->
                                                        user.demoMode

                                                    Nothing ->
                                                        False

                                            ( contactModel, contactCmd ) =
                                                Contact.init modelWithUpdatedSetup.key id demoMode
                                        in
                                        ( { modelWithUpdatedSetup | page = ContactPage contactModel }
                                        , Cmd.batch
                                            [ Cmd.map ContactMsg contactCmd
                                            , authCmd
                                            ]
                                        )

                                    ProtectedRoute ChangePlanRoute ->
                                        let
                                            ( changePlanModel, changePlanCmd ) =
                                                ChangePlan.init
                                                    { key = modelWithUpdatedSetup.key
                                                    , session = extractSession modelWithUpdatedSetup.session
                                                    , orgSlug = modelWithUpdatedSetup.currentUser |> Maybe.map .organizationSlug |> Maybe.withDefault ""
                                                    }
                                        in
                                        ( { modelWithUpdatedSetup | page = ChangePlanPage changePlanModel }
                                        , Cmd.batch
                                            [ Cmd.map ChangePlanMsg changePlanCmd
                                            , authCmd
                                            ]
                                        )

                                    ProtectedRoute DashboardRoute ->
                                        let
                                            dashboardFlags =
                                                { isPostPayment =
                                                    case
                                                        Parser.parse
                                                            (Parser.s "dashboard" <?> Query.string "payment_success")
                                                            url
                                                    of
                                                        Just (Just "true") ->
                                                            Just True

                                                        _ ->
                                                            Nothing
                                                }

                                            ( dashboardModel, dashboardCmd ) =
                                                Dashboard.init dashboardFlags
                                        in
                                        ( { modelWithUpdatedSetup | page = DashboardPage dashboardModel }
                                        , Cmd.batch
                                            [ Cmd.map DashboardMsg dashboardCmd
                                            , authCmd
                                            , case modelWithUpdatedSetup.currentUser of
                                                Just user ->
                                                    Cmd.map DashboardMsg (Task.perform (\hasCompleted -> Dashboard.UserDataReceived hasCompleted) (Task.succeed user.hasCompletedWalkthrough))

                                                Nothing ->
                                                    Cmd.none
                                            ]
                                        )

                                    ProtectedRoute WalkthroughRoute ->
                                        let
                                            ( walkthroughModel, walkthroughCmd ) =
                                                Walkthrough.init False
                                        in
                                        ( { modelWithUpdatedSetup | page = WalkthroughPage walkthroughModel }
                                        , Cmd.batch
                                            [ Cmd.map WalkthroughMsg walkthroughCmd
                                            , authCmd
                                            ]
                                        )

                                    AdminRoute SettingsRoute ->
                                        let
                                            -- Convert Main.elm User to Settings.elm CurrentUser format
                                            settingsUser =
                                                modelWithUpdatedSetup.currentUser
                                                    |> Maybe.map
                                                        (\user ->
                                                            { id = user.id
                                                            , email = user.email
                                                            , isAdmin = user.isAdmin
                                                            , isAgent = user.isAgent
                                                            , organizationSlug = user.organizationSlug
                                                            , organizationId = user.organizationId
                                                            }
                                                        )

                                            ( settingsModel, settingsCmd ) =
                                                Settings.init
                                                    { isSetup = False
                                                    , key = modelWithUpdatedSetup.key
                                                    , currentUser = settingsUser
                                                    , planType =
                                                        modelWithUpdatedSetup.currentUser
                                                            |> Maybe.map .subscriptionTier
                                                            |> Maybe.withDefault ""
                                                    }
                                        in
                                        ( { modelWithUpdatedSetup | page = SettingsPage settingsModel }
                                        , Cmd.batch
                                            [ Cmd.map SettingsMsg settingsCmd
                                            , authCmd
                                            ]
                                        )

                                    AdminRoute AgentsRoute ->
                                        let
                                            -- Convert Main.elm User to AddAgent.elm CurrentUser format
                                            addAgentUser =
                                                modelWithUpdatedSetup.currentUser
                                                    |> Maybe.map
                                                        (\user ->
                                                            { id = user.id
                                                            , email = user.email
                                                            , firstName = user.firstName
                                                            , lastName = user.lastName
                                                            , isAdmin = user.isAdmin
                                                            , isAgent = user.isAgent
                                                            , phone = ""
                                                            , orgSlug = user.organizationSlug -- ADDED
                                                            }
                                                        )

                                            ( addAgentModel, addAgentCmd ) =
                                                AddAgent.init
                                                    False
                                                    modelWithUpdatedSetup.key
                                                    addAgentUser
                                                    (modelWithUpdatedSetup.currentUser
                                                        |> Maybe.map .subscriptionTier
                                                        |> Maybe.withDefault ""
                                                    )
                                        in
                                        ( { modelWithUpdatedSetup | page = AddAgentsPage addAgentModel }
                                        , Cmd.batch
                                            [ Cmd.map AddAgentsMsg addAgentCmd
                                            , authCmd
                                            ]
                                        )

                                    SetupRoute (ChoosePlanRoute progress) ->
                                        let
                                            orgSlug =
                                                modelWithUpdatedSetup.currentUser
                                                    |> Maybe.map .organizationSlug
                                                    |> Maybe.withDefault ""

                                            session =
                                                extractSession modelWithUpdatedSetup.session

                                            ( choosePlanModel, choosePlanCmd ) =
                                                ChoosePlan.init orgSlug session modelWithUpdatedSetup.key True
                                        in
                                        ( { modelWithUpdatedSetup | page = ChoosePlanPage choosePlanModel }
                                        , Cmd.batch
                                            [ Cmd.map ChoosePlanMsg choosePlanCmd
                                            , authCmd
                                            ]
                                        )

                                    SetupRoute (SetupSettingsRoute progress) ->
                                        let
                                            -- Get plan type from progress if available, otherwise use subscription tier
                                            planType =
                                                progress
                                                    |> Maybe.andThen .plan
                                                    |> Maybe.withDefault
                                                        (modelWithUpdatedSetup.currentUser
                                                            |> Maybe.map .subscriptionTier
                                                            |> Maybe.withDefault ""
                                                        )

                                            -- Convert Main.elm User to Settings.elm CurrentUser format
                                            settingsUser =
                                                modelWithUpdatedSetup.currentUser
                                                    |> Maybe.map
                                                        (\user ->
                                                            { id = user.id
                                                            , email = user.email
                                                            , isAdmin = user.isAdmin
                                                            , isAgent = user.isAgent
                                                            , organizationSlug = user.organizationSlug
                                                            , organizationId = user.organizationId
                                                            }
                                                        )

                                            ( settingsModel, settingsCmd ) =
                                                Settings.init
                                                    { isSetup = True
                                                    , key = modelWithUpdatedSetup.key
                                                    , currentUser = settingsUser
                                                    , planType = planType
                                                    }
                                        in
                                        ( { modelWithUpdatedSetup | page = SettingsPage settingsModel }
                                        , Cmd.batch
                                            [ Cmd.map SettingsMsg settingsCmd
                                            , authCmd
                                            ]
                                        )

                                    SetupRoute (AddAgentsRoute progress) ->
                                        let
                                            -- Get plan type from progress if available, otherwise use subscription tier
                                            planType =
                                                progress
                                                    |> Maybe.andThen .plan
                                                    |> Maybe.withDefault
                                                        (modelWithUpdatedSetup.currentUser
                                                            |> Maybe.map .subscriptionTier
                                                            |> Maybe.withDefault ""
                                                        )

                                            -- Convert Main.elm User to AddAgent.elm CurrentUser format
                                            addAgentUser =
                                                modelWithUpdatedSetup.currentUser
                                                    |> Maybe.map
                                                        (\user ->
                                                            { id = user.id
                                                            , email = user.email
                                                            , firstName = user.firstName
                                                            , lastName = user.lastName
                                                            , isAdmin = user.isAdmin
                                                            , isAgent = user.isAgent
                                                            , phone = ""
                                                            , orgSlug = user.organizationSlug -- ADDED
                                                            }
                                                        )

                                            ( addAgentModel, addAgentCmd ) =
                                                AddAgent.init True modelWithUpdatedSetup.key addAgentUser planType
                                        in
                                        ( { modelWithUpdatedSetup | page = AddAgentsPage addAgentModel }
                                        , Cmd.batch
                                            [ Cmd.map AddAgentsMsg addAgentCmd
                                            , authCmd
                                            ]
                                        )

                                    NotFound ->
                                        ( modelWithUpdatedSetup, Cmd.none )

                                    PublicRoute (SelfOnboardingRoute orgSlug) ->
                                        let
                                            ( selfOnboardingModel, selfOnboardingCmd ) =
                                                SelfServiceOnboarding.init model.key url
                                        in
                                        ( { model | page = SelfOnboardingPage selfOnboardingModel }
                                        , Cmd.map SelfOnboardingMsg selfOnboardingCmd
                                        )

                                    PublicRoute (LandingRoute params) ->
                                        let
                                            ( landingModel, landingCmd ) =
                                                Landing.init params
                                        in
                                        ( { model | page = LandingPage landingModel }
                                        , Cmd.map LandingMsg landingCmd
                                        )

                                    PublicRoute PricingRoute ->
                                        let
                                            ( pricingModel, pricingCmd ) =
                                                Pricing.init
                                        in
                                        ( { modelWithUpdatedSetup | page = PricingPage pricingModel }
                                        , Cmd.batch
                                            [ Cmd.map PricingMsg pricingCmd
                                            , authCmd
                                            ]
                                        )

                                    PublicRoute Pricing2Route ->
                                        let
                                            ( pricing2Model, pricing2Cmd ) =
                                                Pricing2.init
                                        in
                                        ( { modelWithUpdatedSetup | page = Pricing2Page pricing2Model }
                                        , Cmd.batch
                                            [ Cmd.map Pricing2Msg pricing2Cmd
                                            , authCmd
                                            ]
                                        )

                                    ProtectedRoute ContactUsRoute ->
                                        let
                                            ( contactUsModel, contactUsCmd ) =
                                                ContactUs.init ()
                                        in
                                        ( { modelWithUpdatedSetup | page = ContactUsPage contactUsModel }
                                        , Cmd.batch
                                            [ Cmd.map ContactUsMsg contactUsCmd
                                            , authCmd
                                            ]
                                        )

                                    ProtectedRoute StripeRoute ->
                                        let
                                            ( stripeModel, stripeCmd ) =
                                                Stripe.init modelWithUpdatedSetup.key url
                                        in
                                        ( { modelWithUpdatedSetup | page = StripePage stripeModel }
                                        , Cmd.batch
                                            [ Cmd.map StripeMsg stripeCmd
                                            , authCmd
                                            ]
                                        )

                Nothing ->
                    ( { model | page = NotFoundPage }
                    , cmd
                    )


type alias CurrentUserResponse =
    { success : Bool
    , user : Maybe User
    }


fetchCurrentUser : Cmd Msg
fetchCurrentUser =
    Http.get
        { url = "/api/me"
        , expect = Http.expectJson GotCurrentUser currentUserResponseDecoder
        }


setSession : String -> Cmd Msg
setSession email =
    Http.post
        { url = "/api/auth/set-session"
        , expect = Http.expectJson SetSessionResponse setSessionResponseDecoder
        , body = Http.jsonBody (E.object [ ( "email", E.string email ) ])
        }


type alias SetSessionResponseAlias =
    { success : Bool
    }


setSessionResponseDecoder : Decoder SetSessionResponseAlias
setSessionResponseDecoder =
    Decode.map SetSessionResponseAlias
        (Decode.field "success" Decode.bool)


fetchAccountStatus : String -> Cmd Msg
fetchAccountStatus orgSlug =
    Http.get
        { url = "/api/organizations/" ++ orgSlug ++ "/account-status"
        , expect = Http.expectJson GotAccountStatus accountStatusResponseDecoder
        }


currentUserResponseDecoder : Decoder CurrentUserResponse
currentUserResponseDecoder =
    Decode.map2 CurrentUserResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "user"
            (Decode.nullable
                (Decode.value
                    |> Decode.andThen
                        (\_ ->
                            userDecoder
                        )
                )
            )
        )



-- Wrap the user in Just since our type expects Maybe User


isBasicPlan : Model -> Bool
isBasicPlan model =
    case model.currentUser of
        Just user ->
            user.organizationSlug == "basic"

        Nothing ->
            True


finalizeOrganization : String -> Cmd Msg
finalizeOrganization orgId =
    Http.post
        { url = "/api/organizations/" ++ orgId ++ "/finalize"
        , body = Http.emptyBody
        , expect = Http.expectWhatever OrgFinalized
        }



-- Helper function to extract session string from SessionState


extractSession : SessionState -> String
extractSession sessionState =
    case sessionState of
        Verified session ->
            session

        _ ->
            ""



-- Helper function to map Document msg to Document Msg


mapDocument : (msg -> Msg) -> Browser.Document msg -> Browser.Document Msg
mapDocument toMsg document =
    { title = document.title
    , body = List.map (Html.map toMsg) document.body
    }



-- Update the isSetup flag based on the current URL and route


updateIsSetup : Model -> Route -> Model
updateIsSetup model route =
    let
        -- Determine if we're in setup mode based on the route
        newIsSetup =
            case route of
                SetupRoute _ ->
                    True

                _ ->
                    -- For non-setup routes, only keep isSetup = True if we're in the middle
                    -- of setup flow (haven't completed it yet)
                    model.isSetup && (getSetupStep model /= Complete)
    in
    { model | isSetup = newIsSetup }



-- Add a new function to force update public routes


updatePageForcePublic : Url -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePageForcePublic url ( model, cmd ) =
    case Parser.parse routeParser url of
        Just route ->
            case route of
                PublicRoute HomeRoute ->
                    let
                        ( homeModel, homeCmd ) =
                            Home.init model.key
                    in
                    ( { model | page = HomePage homeModel }
                    , Cmd.map HomeMsg homeCmd
                    )

                PublicRoute WaitlistRoute ->
                    let
                        ( waitlistModel, waitlistCmd ) =
                            Waitlist.init
                    in
                    ( { model | page = WaitlistPage waitlistModel }
                    , Cmd.map WaitlistMsg waitlistCmd
                    )

                PublicRoute LoginRoute ->
                    let
                        ( loginModel, loginCmd ) =
                            Login.init model.key False url
                    in
                    ( { model | page = LoginPage loginModel }
                    , Cmd.map LoginMsg loginCmd
                    )

                PublicRoute SignupRoute ->
                    let
                        ( signupModel, signupCmd ) =
                            Signup.init model.key
                    in
                    ( { model | page = Signup signupModel }
                    , Cmd.map SignupMsg signupCmd
                    )

                PublicRoute OnboardingRoute ->
                    let
                        -- Extract the query parameters from the URL
                        queryParams =
                            url.query
                                |> Maybe.map (\q -> String.split "&" q)
                                |> Maybe.withDefault []
                                |> List.filterMap
                                    (\param ->
                                        case String.split "=" param of
                                            key :: value :: [] ->
                                                Just ( key, value )

                                            _ ->
                                                Nothing
                                    )

                        -- Initialize the new onboarding module with query params
                        ( onboardingModel, onboardingCmd ) =
                            Onboarding.init model.key url
                    in
                    ( { model | page = OnboardingPage onboardingModel }
                    , Cmd.map OnboardingMsg onboardingCmd
                    )

                PublicRoute (VerifyRoute params) ->
                    let
                        verifyUrl =
                            case params of
                                VerifyParams orgSlug token ->
                                    "/api/auth/verify/" ++ orgSlug ++ "/" ++ token
                    in
                    ( model
                    , Http.get
                        { url = verifyUrl
                        , expect = Http.expectJson GotVerification verificationDecoder
                        }
                    )

                PublicRoute (CompareRoute params) ->
                    case params.quoteId of
                        Just quoteId ->
                            let
                                ( compareModel, compareCmd ) =
                                    Compare.init model.key (Just params)
                            in
                            ( { model | page = ComparePage compareModel }
                            , Cmd.map CompareMsg compareCmd
                            )

                        Nothing ->
                            if isValidOrgId params.orgId then
                                let
                                    ( compareModel, compareCmd ) =
                                        Compare.init model.key (Just params)
                                in
                                ( { model | page = ComparePage compareModel }
                                , Cmd.map CompareMsg compareCmd
                                )

                            else
                                ( { model | page = NotFoundPage }
                                , Nav.pushUrl model.key "/404"
                                )

                PublicRoute (QuoteRoute params) ->
                    if isValidQuoteId params.quoteId then
                        let
                            initialValues =
                                { zipCode = Nothing
                                , dateOfBirth = Nothing
                                , tobacco = Nothing
                                , gender = Nothing
                                , quoteId = params.quoteId
                                , planType = params.planType
                                , orgId = params.orgId -- Pass orgId even if it's Nothing
                                }

                            ( quoteModel, quoteCmd ) =
                                Quote.init model.key initialValues
                        in
                        ( { model | page = QuotePage quoteModel }
                        , Cmd.map QuoteMsg quoteCmd
                        )

                    else
                        ( { model | page = NotFoundPage }
                        , Nav.pushUrl model.key "/error?message=Missing%20or%20invalid%20quote%20ID"
                        )

                PublicRoute (EligibilityRoute params) ->
                    let
                        ( quoteId, _, orgIdStr ) =
                            params
                    in
                    if isValidOrgId orgIdStr then
                        let
                            ( eligibilityModel, eligibilityCmd ) =
                                Eligibility.init model.key { quoteId = quoteId, orgId = orgIdStr }
                        in
                        ( { model | page = EligibilityPage eligibilityModel }
                        , Cmd.map EligibilityMsg eligibilityCmd
                        )

                    else
                        ( { model | page = NotFoundPage }
                        , Nav.pushUrl model.key "/error?message=Missing%20or%20invalid%20organization%20ID"
                        )

                PublicRoute (ScheduleRoute params) ->
                    let
                        ( scheduleModel, scheduleCmd ) =
                            Schedule.init model.key
                                ((\( id, _, _ ) -> id) params)
                                ((\( _, status, _ ) -> status) params)
                    in
                    ( { model | page = SchedulePage scheduleModel }
                    , Cmd.map ScheduleMsg scheduleCmd
                    )

                PublicRoute ScheduleMainRoute ->
                    let
                        ( scheduleMainModel, scheduleMainCmd ) =
                            ScheduleMain.init model.key
                    in
                    ( { model | page = ScheduleMainPage scheduleMainModel }
                    , Cmd.map ScheduleMainMsg scheduleMainCmd
                    )

                PublicRoute (SelfOnboardingRoute orgSlug) ->
                    let
                        ( selfOnboardingModel, selfOnboardingCmd ) =
                            SelfServiceOnboarding.init model.key url
                    in
                    ( { model | page = SelfOnboardingPage selfOnboardingModel }
                    , Cmd.map SelfOnboardingMsg selfOnboardingCmd
                    )

                ProtectedRoute _ ->
                    updatePage url ( model, cmd )

                AdminRoute _ ->
                    updatePage url ( model, cmd )

                SetupRoute _ ->
                    updatePage url ( model, cmd )

                NotFound ->
                    ( { model | page = NotFoundPage }, cmd )

                PublicRoute (LandingRoute params) ->
                    let
                        ( landingModel, landingCmd ) =
                            Landing.init params
                    in
                    ( { model | page = LandingPage landingModel }
                    , Cmd.map LandingMsg landingCmd
                    )

                PublicRoute PricingRoute ->
                    let
                        ( pricingModel, pricingCmd ) =
                            Pricing.init
                    in
                    ( { model | page = PricingPage pricingModel }
                    , Cmd.map PricingMsg pricingCmd
                    )

                PublicRoute Pricing2Route ->
                    let
                        ( pricing2Model, pricing2Cmd ) =
                            Pricing2.init
                    in
                    ( { model | page = Pricing2Page pricing2Model }
                    , Cmd.map Pricing2Msg pricing2Cmd
                    )


        Nothing ->
            ( { model | page = NotFoundPage }
            , cmd
            )



-- Add a helper function to check if an organization ID is valid
-- It should return True only if the orgId is not empty, not "default", and is a proper string


isValidOrgId : Maybe String -> Bool
isValidOrgId maybeOrgId =
    case maybeOrgId of
        Just orgId ->
            not (String.isEmpty orgId) && orgId /= "default"

        Nothing ->
            False



-- Add a helper function to check if a quote ID is valid


isValidQuoteId : Maybe String -> Bool
isValidQuoteId maybeQuoteId =
    case maybeQuoteId of
        Just quoteId ->
            not (String.isEmpty quoteId)

        Nothing ->
            False
