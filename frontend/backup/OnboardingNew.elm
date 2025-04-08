port module OnboardingNew exposing
    ( Model
    , Msg(..)
    , Step(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Components.SetupLayout as SetupLayout
import Html exposing (..)
import Html.Attributes exposing (alt, checked, class, for, href, id, placeholder, src, type_, value)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random
import Task
import Url
import Utils.RandomOrgName exposing (generateOrgName)



-- Subscription Tier Type


type alias SubscriptionTier =
    { id : String
    , name : String
    , description : String
    , price : Float
    , features : List String
    , maxAgents : Int
    , maxContacts : Int
    }



-- Ports for onboarding cookie management


port setOnboardingCookie : { sessionId : String, planType : String, step : Int } -> Cmd msg


port getOnboardingCookie : () -> Cmd msg


port onboardingCookieReceived : (Maybe { sessionId : String, planType : String, step : Int } -> msg) -> Sub msg



-- Ports for storing section data


port storeOnboardingData : { section : String, data : Encode.Value } -> Cmd msg


port getOnboardingData : String -> Cmd msg


port onboardingDataReceived : (Maybe Encode.Value -> msg) -> Sub msg



-- TYPES


type Step
    = PlanSelectionStep
    | UserDetailsStep
    | CompanyDetailsStep
    | LicensingSettingsStep
    | AddAgentsStep
    | PaymentStep
    | EnterpriseFormStep


type alias UserDetails =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    }


userDetailsInit : UserDetails
userDetailsInit =
    { firstName = ""
    , lastName = ""
    , email = ""
    , phone = ""
    }


userDetailsDecoder : Decoder UserDetails
userDetailsDecoder =
    Decode.succeed UserDetails
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "email" Decode.string
        |> required "phone" Decode.string


userDetailsEncoder : UserDetails -> Encode.Value
userDetailsEncoder details =
    Encode.object
        [ ( "firstName", Encode.string details.firstName )
        , ( "lastName", Encode.string details.lastName )
        , ( "email", Encode.string details.email )
        , ( "phone", Encode.string details.phone )
        ]


type alias CompanyDetails =
    { agencyName : String
    , website : String
    , phone : String
    , primaryColor : String
    , secondaryColor : String
    , logo : Maybe String
    }


companyDetailsInit : CompanyDetails
companyDetailsInit =
    { agencyName = ""
    , website = ""
    , phone = ""
    , primaryColor = "#6B46C1"
    , secondaryColor = "#9F7AEA"
    , logo = Nothing
    }


companyDetailsDecoder : Decoder CompanyDetails
companyDetailsDecoder =
    Decode.succeed CompanyDetails
        |> required "agencyName" Decode.string
        |> required "website" Decode.string
        |> optional "phone" Decode.string ""
        |> optional "primaryColor" Decode.string ""
        |> optional "secondaryColor" Decode.string ""
        |> optional "logo" (Decode.nullable Decode.string) Nothing


companyDetailsEncoder : CompanyDetails -> Encode.Value
companyDetailsEncoder details =
    Encode.object
        [ ( "agencyName", Encode.string details.agencyName )
        , ( "website", Encode.string details.website )
        , ( "phone", Encode.string details.phone )
        , ( "primaryColor", Encode.string details.primaryColor )
        , ( "secondaryColor", Encode.string details.secondaryColor )
        , ( "logo"
          , case details.logo of
                Just logoPath ->
                    Encode.string logoPath

                Nothing ->
                    Encode.null
          )
        ]


type alias LicensingSettings =
    { carrierContracts : List String
    , useSmartSendForGI : Bool
    }


licensingSettingsInit : LicensingSettings
licensingSettingsInit =
    { carrierContracts = []
    , useSmartSendForGI = True
    }


licensingSettingsDecoder : Decoder LicensingSettings
licensingSettingsDecoder =
    Decode.succeed LicensingSettings
        |> required "carrierContracts" (Decode.list Decode.string)
        |> required "useSmartSendForGI" Decode.bool


licensingSettingsEncoder : LicensingSettings -> Encode.Value
licensingSettingsEncoder settings =
    Encode.object
        [ ( "carrierContracts", Encode.list Encode.string settings.carrierContracts )
        , ( "useSmartSendForGI", Encode.bool settings.useSmartSendForGI )
        ]


type alias Agent =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    , isAdmin : Bool
    , isAgent : Bool
    }


agentInit : Agent
agentInit =
    { id = ""
    , firstName = ""
    , lastName = ""
    , email = ""
    , phone = ""
    , isAdmin = False
    , isAgent = True
    }


agentDecoder : Decoder Agent
agentDecoder =
    Decode.succeed Agent
        |> required "id" Decode.string
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "email" Decode.string
        |> required "phone" Decode.string
        |> required "isAdmin" Decode.bool
        |> optional "isAgent" Decode.bool True


agentEncoder : Agent -> Encode.Value
agentEncoder agent =
    Encode.object
        [ ( "id", Encode.string agent.id )
        , ( "firstName", Encode.string agent.firstName )
        , ( "lastName", Encode.string agent.lastName )
        , ( "email", Encode.string agent.email )
        , ( "phone", Encode.string agent.phone )
        , ( "isAdmin", Encode.bool agent.isAdmin )
        , ( "isAgent", Encode.bool agent.isAgent )
        ]


type alias NewAgentForm =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , isAdmin : Bool
    , isAgent : Bool
    }


newAgentInit : NewAgentForm
newAgentInit =
    { firstName = ""
    , lastName = ""
    , email = ""
    , phone = ""
    , isAdmin = False
    , isAgent = True
    }


type alias AddAgents =
    { agents : List Agent
    , newAgent : NewAgentForm
    , agentEmailStatus : EmailStatus
    }


addAgentsInit : AddAgents
addAgentsInit =
    { agents = []
    , newAgent = newAgentInit
    , agentEmailStatus = NotChecked
    }


addAgentsEncoder : AddAgents -> Encode.Value
addAgentsEncoder addAgents =
    Encode.object
        [ ( "agents", Encode.list agentEncoder addAgents.agents ) ]


type EmailStatus
    = NotChecked
    | Checking
    | Valid
    | Invalid String


type alias Payment =
    { paymentSucceeded : Bool
    , paymentError : Maybe String
    , extraAgents : Int
    , extraContacts : Int
    }


paymentInit : Payment
paymentInit =
    { paymentSucceeded = False
    , paymentError = Nothing
    , extraAgents = 0
    , extraContacts = 0
    }


paymentDecoder : Decoder Payment
paymentDecoder =
    Decode.succeed Payment
        |> required "paymentSucceeded" Decode.bool
        |> optional "paymentError" (Decode.nullable Decode.string) Nothing
        |> optional "extraAgents" Decode.int 0
        |> optional "extraContacts" Decode.int 0


paymentEncoder : Payment -> Encode.Value
paymentEncoder payment =
    Encode.object
        [ ( "paymentSucceeded", Encode.bool payment.paymentSucceeded )
        , ( "paymentError"
          , case payment.paymentError of
                Just err ->
                    Encode.string err

                Nothing ->
                    Encode.null
          )
        , ( "extraAgents", Encode.int payment.extraAgents )
        , ( "extraContacts", Encode.int payment.extraContacts )
        ]


type alias EnterpriseForm =
    { enterpriseName : String
    , enterpriseEmail : String
    , enterprisePhone : String
    , message : String
    }


enterpriseFormInit : EnterpriseForm
enterpriseFormInit =
    { enterpriseName = ""
    , enterpriseEmail = ""
    , enterprisePhone = ""
    , message = ""
    }


enterpriseFormDecoder : Decoder EnterpriseForm
enterpriseFormDecoder =
    Decode.succeed EnterpriseForm
        |> required "enterpriseName" Decode.string
        |> required "enterpriseEmail" Decode.string
        |> required "enterprisePhone" Decode.string
        |> required "message" Decode.string


enterpriseFormEncoder : EnterpriseForm -> Encode.Value
enterpriseFormEncoder form =
    Encode.object
        [ ( "enterpriseName", Encode.string form.enterpriseName )
        , ( "enterpriseEmail", Encode.string form.enterpriseEmail )
        , ( "enterprisePhone", Encode.string form.enterprisePhone )
        , ( "message", Encode.string form.message )
        ]


type FormStatus
    = NotSubmitted
    | Submitted
    | SubmittedSuccessfully
    | SubmittedWithError String


type alias OnboardingSession =
    { sessionId : String
    , planType : String
    , currentStep : Int
    }


type alias Model =
    { step : Step
    , userDetails : UserDetails
    , userEmailStatus : EmailStatus
    , companyDetails : CompanyDetails
    , licensingSettings : LicensingSettings
    , addAgents : AddAgents
    , payment : Payment
    , enterpriseForm : EnterpriseForm
    , enterpriseEmailStatus : EmailStatus
    , enterpriseFormStatus : FormStatus
    , session : Maybe OnboardingSession
    , orgSlug : String
    , isBasicPlan : Bool
    , error : Maybe String
    , isLoading : Bool
    , key : Nav.Key
    , stepHistory : List Step
    , subscriptionTiers : List SubscriptionTier
    }


init : Nav.Key -> String -> String -> Step -> ( Model, Cmd Msg )
init key initialOrgSlug sessionToken initialStep =
    let
        model =
            { step = initialStep
            , userDetails = userDetailsInit
            , userEmailStatus = NotChecked
            , companyDetails = companyDetailsInit
            , licensingSettings = licensingSettingsInit
            , addAgents = addAgentsInit
            , payment = paymentInit
            , enterpriseForm = enterpriseFormInit
            , enterpriseEmailStatus = NotChecked
            , enterpriseFormStatus = NotSubmitted
            , session =
                if String.isEmpty sessionToken then
                    Nothing

                else
                    Just { sessionId = sessionToken, planType = "basic", currentStep = 1 }
            , orgSlug = initialOrgSlug
            , isBasicPlan = True
            , error = Nothing
            , isLoading = False
            , key = key
            , stepHistory = []
            , subscriptionTiers = []
            }
    in
    ( model, fetchSubscriptionTiers )


type alias EmailCheckResponse =
    { available : Bool
    , message : String
    }


type alias SaveResponse =
    { success : Bool
    , message : String
    , slug : String
    }


type alias OnboardingSettingsResponse =
    { userDetails : Maybe UserDetails
    , companyDetails : Maybe CompanyDetails
    , licensingSettings : Maybe LicensingSettings
    , agents : Maybe (List Agent)
    , paymentInfo : Maybe Payment
    , enterpriseForm : Maybe EnterpriseForm
    , currentStep : Int
    , planType : String
    }


onboardingSettingsDecoder : Decoder OnboardingSettingsResponse
onboardingSettingsDecoder =
    Decode.succeed OnboardingSettingsResponse
        |> optional "userDetails" (Decode.nullable userDetailsDecoder) Nothing
        |> optional "companyDetails" (Decode.nullable companyDetailsDecoder) Nothing
        |> optional "licensingSettings" (Decode.nullable licensingSettingsDecoder) Nothing
        |> optional "agents" (Decode.nullable (Decode.list agentDecoder)) Nothing
        |> optional "paymentInfo" (Decode.nullable paymentDecoder) Nothing
        |> optional "enterpriseForm" (Decode.nullable enterpriseFormDecoder) Nothing
        |> required "currentStep" Decode.int
        |> required "planType" Decode.string


type
    Msg
    -- Session Management
    = OnboardingCookieReceived (Maybe { sessionId : String, planType : String, step : Int })
    | OnboardingDataReceived (Maybe Encode.Value)
    | FetchOnboardingData String
    | GotOnboardingSettings (Result Http.Error OnboardingSettingsResponse)
    | StartOnboarding String -- planType
    | OnboardingStarted (Result Http.Error { sessionId : String, planType : String, step : Int })
      -- User Details
    | UpdateUserFirstName String
    | UpdateUserLastName String
    | UpdateUserEmail String
    | UserEmailBlurred
    | UserEmailFocused
    | GotUserEmailCheckResponse (Result Http.Error EmailCheckResponse)
    | UpdateUserPhone String
    | SaveUserDetails
    | UserDetailsSaved (Result Http.Error SaveResponse)
      -- Company Details
    | UpdateAgencyName String
    | UpdateWebsite String
    | UpdatePhone String
    | UpdatePrimaryColor String
    | UpdateSecondaryColor String
    | SaveCompanyDetails
    | CompanyDetailsSaved (Result Http.Error SaveResponse)
      -- Licensing Settings
    | AddCarrierContract String
    | RemoveCarrierContract String
    | ToggleSection String
    | ToggleAllCarriers Bool
    | ToggleSmartSendForGI Bool
    | SaveLicensingSettings
    | LicensingSettingsSaved (Result Http.Error SaveResponse)
      -- Add Agents
    | UpdateAgentFirstName String
    | UpdateAgentLastName String
    | UpdateAgentEmail String
    | AgentEmailBlurred
    | AgentEmailFocused
    | UpdateAgentPhone String
    | UpdateAgentCheckbox Bool
    | UpdateAgentRole Bool
    | AddAgent
    | SaveAgent
    | CancelAddAgent
    | GotAgentEmailCheckResponse (Result Http.Error EmailCheckResponse)
    | AgentSaved (Result Http.Error SaveResponse)
      -- Payment
    | UpdateExtraAgents Int
    | UpdateExtraContacts Int
    | CompletePayment
    | PaymentCompleted (Result Http.Error SaveResponse)
      -- Enterprise Form
    | UpdateEnterpriseName String
    | UpdateEnterpriseEmail String
    | UpdateEnterprisePhone String
    | UpdateEnterpriseMessage String
    | SaveEnterpriseForm
    | EnterpriseFormSaved (Result Http.Error SaveResponse)
      -- Navigation
    | NavigateToStep Step
    | NavigateBack
    | SkipStep
    | CompleteOnboarding
    | OnboardingCompleted (Result Http.Error SaveResponse)
    | RandomOrgNameGenerated String
    | ScrollToTop
    | NoOp
      -- Subscription Tiers
    | GotSubscriptionTiers (Result Http.Error (List SubscriptionTier))
    | SelectPlanType String



-- HTTP Fetches


fetchSubscriptionTiers : Cmd Msg
fetchSubscriptionTiers =
    Http.get
        { url = "/api/organizations/subscription-tiers"
        , expect = Http.expectJson GotSubscriptionTiers subscriptionTiersDecoder
        }


subscriptionTiersDecoder : Decoder (List SubscriptionTier)
subscriptionTiersDecoder =
    Decode.field "tiers" <|
        Decode.list
            (Decode.map7 SubscriptionTier
                (Decode.field "id" Decode.string)
                (Decode.field "name" Decode.string)
                (Decode.oneOf
                    [ Decode.field "description" Decode.string
                    , Decode.succeed ""
                    ]
                )
                (Decode.map
                    (\priceStr ->
                        String.replace "$" "" priceStr
                            |> String.replace "/mo" ""
                            |> String.toFloat
                            |> Maybe.withDefault 0
                    )
                    (Decode.field "price" Decode.string)
                )
                (Decode.field "features" (Decode.list Decode.string))
                (Decode.field "agentLimit" Decode.int)
                (Decode.field "contactLimit" Decode.int)
            )


fetchOnboardingSettings : String -> Cmd Msg
fetchOnboardingSettings sessionId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Onboarding-Session" sessionId ]
        , url = "/api/onboarding/settings"
        , body = Http.emptyBody
        , expect = Http.expectJson GotOnboardingSettings onboardingSettingsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


startOnboarding : String -> Cmd Msg
startOnboarding planType =
    Http.post
        { url = "/api/onboarding/start"
        , body = Http.jsonBody (Encode.object [ ( "planType", Encode.string planType ) ])
        , expect = Http.expectJson OnboardingStarted onboardingSessionDecoder
        }


onboardingSessionDecoder : Decoder { sessionId : String, planType : String, step : Int }
onboardingSessionDecoder =
    Decode.map3 (\id plan step -> { sessionId = id, planType = plan, step = step })
        (Decode.field "sessionId" Decode.string)
        (Decode.field "planType" Decode.string)
        (Decode.field "step" Decode.int)


saveUserDetails : String -> UserDetails -> Cmd Msg
saveUserDetails sessionId userDetails =
    let
        -- Only add the session header if a non-empty session ID is provided
        headers =
            if String.isEmpty sessionId then
                []

            else
                [ Http.header "X-Onboarding-Session" sessionId ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "/api/onboarding/user-details"
        , body = Http.jsonBody (userDetailsEncoder userDetails)
        , expect = Http.expectJson UserDetailsSaved saveResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveResponseDecoder : Decoder SaveResponse
saveResponseDecoder =
    Decode.map3 SaveResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "message" Decode.string)
        (Decode.oneOf
            [ Decode.field "slug" Decode.string
            , Decode.succeed "" -- Default to empty string if no slug is provided
            ]
        )


saveCompanyDetails : String -> CompanyDetails -> Cmd Msg
saveCompanyDetails sessionId companyDetails =
    let
        -- Only add the session header if a non-empty session ID is provided
        headers =
            if String.isEmpty sessionId then
                []

            else
                [ Http.header "X-Onboarding-Session" sessionId ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "/api/onboarding/company-details"
        , body = Http.jsonBody (companyDetailsEncoder companyDetails)
        , expect = Http.expectJson CompanyDetailsSaved saveResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveLicensingSettings : String -> LicensingSettings -> Cmd Msg
saveLicensingSettings sessionId licensingSettings =
    let
        -- Only add the session header if a non-empty session ID is provided
        headers =
            if String.isEmpty sessionId then
                []

            else
                [ Http.header "X-Onboarding-Session" sessionId ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "/api/onboarding/licensing-settings"
        , body = Http.jsonBody (licensingSettingsEncoder licensingSettings)
        , expect = Http.expectJson LicensingSettingsSaved saveResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveAgent : String -> Agent -> Cmd Msg
saveAgent sessionId agent =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Onboarding-Session" sessionId ]
        , url = "/api/onboarding/agents"
        , body = Http.jsonBody (agentEncoder agent)
        , expect = Http.expectJson AgentSaved saveResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


completePayment : String -> Payment -> Cmd Msg
completePayment sessionId payment =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Onboarding-Session" sessionId ]
        , url = "/api/onboarding/payment"
        , body = Http.jsonBody (paymentEncoder payment)
        , expect = Http.expectJson PaymentCompleted saveResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveEnterpriseForm : String -> EnterpriseForm -> Cmd Msg
saveEnterpriseForm sessionId form =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Onboarding-Session" sessionId ]
        , url = "/api/onboarding/enterprise"
        , body = Http.jsonBody (enterpriseFormEncoder form)
        , expect = Http.expectJson EnterpriseFormSaved saveResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


completeOnboardingRequest : String -> Cmd Msg
completeOnboardingRequest sessionId =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Onboarding-Session" sessionId ]
        , url = "/api/onboarding/complete"
        , body = Http.emptyBody
        , expect = Http.expectJson OnboardingCompleted saveResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


checkEmail : String -> String -> Cmd Msg
checkEmail email endpoint =
    Http.post
        { url = "/api/onboarding/check-email"
        , body = Http.jsonBody (Encode.object [ ( "email", Encode.string email ), ( "endpoint", Encode.string endpoint ) ])
        , expect = Http.expectJson GotUserEmailCheckResponse emailCheckResponseDecoder
        }


emailCheckResponseDecoder : Decoder EmailCheckResponse
emailCheckResponseDecoder =
    Decode.map2 EmailCheckResponse
        (Decode.field "available" Decode.bool)
        (Decode.field "message" Decode.string)


checkAgentEmail : String -> String -> Cmd Msg
checkAgentEmail email sessionId =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Onboarding-Session" sessionId ]
        , url = "/api/onboarding/check-agent-email"
        , body = Http.jsonBody (Encode.object [ ( "email", Encode.string email ) ])
        , expect = Http.expectJson GotAgentEmailCheckResponse emailCheckResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Session Management
        OnboardingCookieReceived maybeCookie ->
            case maybeCookie of
                Just cookie ->
                    -- If we have a cookie, fetch all onboarding data from the backend
                    let
                        session =
                            { sessionId = cookie.sessionId
                            , planType = cookie.planType
                            , currentStep = cookie.step
                            }

                        isBasicPlan =
                            cookie.planType == "basic"

                        newStep =
                            case cookie.step of
                                1 ->
                                    UserDetailsStep

                                2 ->
                                    CompanyDetailsStep

                                3 ->
                                    LicensingSettingsStep

                                4 ->
                                    AddAgentsStep

                                5 ->
                                    PaymentStep

                                6 ->
                                    EnterpriseFormStep

                                _ ->
                                    PlanSelectionStep
                    in
                    ( { model
                        | session = Just session
                        , isBasicPlan = isBasicPlan
                        , step = newStep
                      }
                    , fetchOnboardingSettings cookie.sessionId
                    )

                Nothing ->
                    -- No cookie, stay on initial step
                    ( model, Cmd.none )

        OnboardingDataReceived maybeData ->
            -- Not used directly, as we use specific endpoints instead of generic data
            ( model, Cmd.none )

        FetchOnboardingData sectionName ->
            -- For now, we'll fetch all settings at once
            case model.session of
                Just session ->
                    ( model, fetchOnboardingSettings session.sessionId )

                Nothing ->
                    ( model, Cmd.none )

        GotOnboardingSettings result ->
            case result of
                Ok settings ->
                    -- Update all models with the retrieved settings
                    let
                        updatedUserDetails =
                            Maybe.withDefault model.userDetails settings.userDetails

                        updatedCompanyDetails =
                            Maybe.withDefault model.companyDetails settings.companyDetails

                        updatedLicensingSettings =
                            Maybe.withDefault model.licensingSettings settings.licensingSettings

                        updatedAddAgents =
                            case settings.agents of
                                Just agents ->
                                    { newAgent = model.addAgents.newAgent
                                    , agents = agents
                                    , agentEmailStatus = NotChecked
                                    }

                                Nothing ->
                                    model.addAgents

                        updatedPayment =
                            Maybe.withDefault model.payment settings.paymentInfo

                        updatedEnterpriseForm =
                            Maybe.withDefault model.enterpriseForm settings.enterpriseForm

                        isBasicPlan =
                            settings.planType == "basic"

                        newStep =
                            case settings.currentStep of
                                1 ->
                                    UserDetailsStep

                                2 ->
                                    CompanyDetailsStep

                                3 ->
                                    LicensingSettingsStep

                                4 ->
                                    AddAgentsStep

                                5 ->
                                    PaymentStep

                                6 ->
                                    EnterpriseFormStep

                                _ ->
                                    PlanSelectionStep

                        -- Update the session if needed
                        updatedSession =
                            case model.session of
                                Just session ->
                                    Just { session | currentStep = settings.currentStep, planType = settings.planType }

                                Nothing ->
                                    model.session
                    in
                    ( { model
                        | userDetails = updatedUserDetails
                        , companyDetails = updatedCompanyDetails
                        , licensingSettings = updatedLicensingSettings
                        , addAgents = updatedAddAgents
                        , payment = updatedPayment
                        , enterpriseForm = updatedEnterpriseForm
                        , isBasicPlan = isBasicPlan
                        , step = newStep
                        , session = updatedSession
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to load onboarding settings.", isLoading = False }, Cmd.none )

        StartOnboarding planType ->
            ( { model | isLoading = True }, startOnboarding planType )

        OnboardingStarted result ->
            case result of
                Ok response ->
                    -- Store the session in a cookie and update the model
                    let
                        session =
                            { sessionId = response.sessionId
                            , planType = response.planType
                            , currentStep = response.step
                            }

                        isBasicPlan =
                            response.planType == "basic"

                        -- Update the cookie
                        cookieCmd =
                            setOnboardingCookie
                                { sessionId = response.sessionId
                                , planType = response.planType
                                , step = response.step
                                }
                    in
                    ( { model
                        | session = Just session
                        , isBasicPlan = isBasicPlan
                        , step = UserDetailsStep
                        , isLoading = False
                      }
                    , Cmd.batch [ cookieCmd, Nav.pushUrl model.key "/onboarding/personal" ]
                    )

                Err _ ->
                    ( { model | error = Just "Failed to start onboarding process.", isLoading = False }, Cmd.none )

        -- User Details
        UpdateUserFirstName firstName ->
            let
                userDetails =
                    model.userDetails
            in
            ( { model | userDetails = { userDetails | firstName = firstName } }, Cmd.none )

        UpdateUserLastName lastName ->
            let
                userDetails =
                    model.userDetails
            in
            ( { model | userDetails = { userDetails | lastName = lastName } }, Cmd.none )

        UpdateUserEmail email ->
            let
                userDetails =
                    model.userDetails
            in
            ( { model | userDetails = { userDetails | email = email }, userEmailStatus = NotChecked }, Cmd.none )

        UserEmailBlurred ->
            -- Check email availability when field loses focus
            if String.length model.userDetails.email > 0 then
                ( { model | userEmailStatus = Checking }, checkEmail model.userDetails.email "user" )

            else
                ( model, Cmd.none )

        UserEmailFocused ->
            -- Reset validation when field gets focus
            ( { model | userEmailStatus = NotChecked }, Cmd.none )

        GotUserEmailCheckResponse result ->
            case result of
                Ok response ->
                    if response.available then
                        ( { model | userEmailStatus = Valid }, Cmd.none )

                    else
                        ( { model | userEmailStatus = Invalid response.message }, Cmd.none )

                Err _ ->
                    ( { model | userEmailStatus = Invalid "Error checking email." }, Cmd.none )

        UpdateUserPhone phone ->
            let
                userDetails =
                    model.userDetails
            in
            ( { model | userDetails = { userDetails | phone = phone } }, Cmd.none )

        SaveUserDetails ->
            -- Always proceed with submitting the form, even without a session
            ( { model | isLoading = True, error = Nothing }
            , case model.session of
                Just session ->
                    saveUserDetails session.sessionId model.userDetails

                Nothing ->
                    -- If no session ID, just call the function without a session ID
                    saveUserDetails "" model.userDetails
            )

        UserDetailsSaved result ->
            case result of
                Ok response ->
                    -- Update the org slug with the one returned from the server
                    let
                        -- Move to the next step
                        nextStep =
                            getNextStep UserDetailsStep model.isBasicPlan

                        -- Store the updated onboarding state in a cookie
                        cookieCmd =
                            case model.session of
                                Just session ->
                                    setOnboardingCookie
                                        { sessionId = session.sessionId
                                        , planType = session.planType
                                        , step = 2 -- UserDetails is step 1, next is 2
                                        }

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model
                        | orgSlug = response.slug
                        , step = nextStep
                        , isLoading = False
                        , stepHistory = model.step :: model.stepHistory
                      }
                    , Cmd.batch [ cookieCmd, Nav.pushUrl model.key (getStepUrl nextStep) ]
                    )

                Err _ ->
                    ( { model | error = Just "Failed to save user details.", isLoading = False }, Cmd.none )

        -- Company Details
        UpdateAgencyName agencyName ->
            let
                companyDetails =
                    model.companyDetails
            in
            ( { model | companyDetails = { companyDetails | agencyName = agencyName } }, Cmd.none )

        UpdateWebsite website ->
            let
                companyDetails =
                    model.companyDetails
            in
            ( { model | companyDetails = { companyDetails | website = website } }, Cmd.none )

        UpdatePhone phone ->
            let
                companyDetails =
                    model.companyDetails
            in
            ( { model | companyDetails = { companyDetails | phone = phone } }, Cmd.none )

        UpdatePrimaryColor color ->
            let
                companyDetails =
                    model.companyDetails
            in
            ( { model | companyDetails = { companyDetails | primaryColor = color } }, Cmd.none )

        UpdateSecondaryColor color ->
            let
                companyDetails =
                    model.companyDetails
            in
            ( { model | companyDetails = { companyDetails | secondaryColor = color } }, Cmd.none )

        SaveCompanyDetails ->
            -- Always proceed with submitting the form, even without a session
            ( { model | isLoading = True, error = Nothing }
            , case model.session of
                Just session ->
                    saveCompanyDetails session.sessionId model.companyDetails

                Nothing ->
                    -- If no session ID, just call the function without a session ID
                    saveCompanyDetails "" model.companyDetails
            )

        CompanyDetailsSaved result ->
            case result of
                Ok _ ->
                    -- Move to the next step
                    let
                        nextStep =
                            getNextStep CompanyDetailsStep model.isBasicPlan

                        -- Store the updated onboarding state in a cookie
                        cookieCmd =
                            case model.session of
                                Just session ->
                                    setOnboardingCookie
                                        { sessionId = session.sessionId
                                        , planType = session.planType
                                        , step = 3 -- Company details is step 2, next is 3
                                        }

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model
                        | step = nextStep
                        , isLoading = False
                        , stepHistory = model.step :: model.stepHistory
                      }
                    , Cmd.batch [ cookieCmd, Nav.pushUrl model.key (getStepUrl nextStep) ]
                    )

                Err error ->
                    let
                        errorMsg =
                            case error of
                                Http.BadUrl url ->
                                    "Invalid URL: " ++ url

                                Http.Timeout ->
                                    "Request timed out"

                                Http.NetworkError ->
                                    "Network error"

                                Http.BadStatus code ->
                                    "Server error: " ++ String.fromInt code

                                Http.BadBody message ->
                                    "Data error: " ++ message
                    in
                    ( { model | error = Just ("Failed to save company details: " ++ errorMsg), isLoading = False }, Cmd.none )

        -- Licensing Settings
        AddCarrierContract carrier ->
            let
                settings =
                    model.licensingSettings

                updatedCarriers =
                    if List.member carrier settings.carrierContracts then
                        settings.carrierContracts

                    else
                        carrier :: settings.carrierContracts
            in
            ( { model | licensingSettings = { settings | carrierContracts = updatedCarriers } }, Cmd.none )

        RemoveCarrierContract carrier ->
            let
                settings =
                    model.licensingSettings

                updatedCarriers =
                    List.filter (\c -> c /= carrier) settings.carrierContracts
            in
            ( { model | licensingSettings = { settings | carrierContracts = updatedCarriers } }, Cmd.none )

        ToggleSection _ ->
            -- This is a UI-only change, doesn't affect the model
            ( model, Cmd.none )

        ToggleAllCarriers selected ->
            let
                settings =
                    model.licensingSettings

                newSettings =
                    if selected then
                        { settings | carrierContracts = allCarriers }

                    else
                        { settings | carrierContracts = [] }
            in
            ( { model | licensingSettings = newSettings }, Cmd.none )

        ToggleSmartSendForGI value ->
            let
                settings =
                    model.licensingSettings
            in
            ( { model | licensingSettings = { settings | useSmartSendForGI = value } }, Cmd.none )

        SaveLicensingSettings ->
            -- Always proceed with submitting the form, even without a session
            ( { model | isLoading = True, error = Nothing }
            , case model.session of
                Just session ->
                    saveLicensingSettings session.sessionId model.licensingSettings

                Nothing ->
                    -- If no session ID, just call the function without a session ID
                    saveLicensingSettings "" model.licensingSettings
            )

        LicensingSettingsSaved result ->
            case result of
                Ok _ ->
                    -- Move to the next step, depends on plan type
                    let
                        nextStep =
                            getNextStep LicensingSettingsStep model.isBasicPlan

                        -- Store the updated onboarding state in a cookie
                        cookieCmd =
                            case model.session of
                                Just session ->
                                    setOnboardingCookie
                                        { sessionId = session.sessionId
                                        , planType = session.planType
                                        , step =
                                            if model.isBasicPlan then
                                                5

                                            else
                                                4

                                        -- Skip AgentsStep if basic plan
                                        }

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model
                        | step = nextStep
                        , isLoading = False
                        , stepHistory = model.step :: model.stepHistory
                      }
                    , Cmd.batch [ cookieCmd, Nav.pushUrl model.key (getStepUrl nextStep) ]
                    )

                Err _ ->
                    ( { model | error = Just "Failed to save licensing settings.", isLoading = False }, Cmd.none )

        -- Add Agents
        UpdateAgentFirstName name ->
            let
                addAgents =
                    model.addAgents

                newAgent =
                    addAgents.newAgent

                updatedNewAgent =
                    { newAgent | firstName = name }
            in
            ( { model | addAgents = { addAgents | newAgent = updatedNewAgent } }, Cmd.none )

        UpdateAgentLastName name ->
            let
                addAgents =
                    model.addAgents

                newAgent =
                    addAgents.newAgent

                updatedNewAgent =
                    { newAgent | lastName = name }
            in
            ( { model | addAgents = { addAgents | newAgent = updatedNewAgent } }, Cmd.none )

        UpdateAgentEmail email ->
            let
                addAgents =
                    model.addAgents

                newAgent =
                    addAgents.newAgent

                updatedNewAgent =
                    { newAgent | email = email }
            in
            ( { model | addAgents = { addAgents | newAgent = updatedNewAgent, agentEmailStatus = NotChecked } }, Cmd.none )

        AgentEmailBlurred ->
            -- Check email availability when field loses focus
            let
                agentEmail =
                    model.addAgents.newAgent.email
            in
            if String.length agentEmail > 0 then
                case model.session of
                    Just session ->
                        let
                            addAgents =
                                model.addAgents
                        in
                        ( { model | addAgents = { addAgents | agentEmailStatus = Checking } }
                        , checkAgentEmail agentEmail session.sessionId
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        AgentEmailFocused ->
            -- Reset validation when field gets focus
            let
                addAgents =
                    model.addAgents
            in
            ( { model | addAgents = { addAgents | agentEmailStatus = NotChecked } }, Cmd.none )

        GotAgentEmailCheckResponse result ->
            case result of
                Ok response ->
                    let
                        addAgents =
                            model.addAgents

                        newStatus =
                            if response.available then
                                Valid

                            else
                                Invalid response.message
                    in
                    ( { model | addAgents = { addAgents | agentEmailStatus = newStatus } }, Cmd.none )

                Err _ ->
                    let
                        addAgents =
                            model.addAgents
                    in
                    ( { model | addAgents = { addAgents | agentEmailStatus = Invalid "Error checking email." } }, Cmd.none )

        UpdateAgentPhone phone ->
            let
                addAgents =
                    model.addAgents

                newAgent =
                    addAgents.newAgent

                updatedNewAgent =
                    { newAgent | phone = phone }
            in
            ( { model | addAgents = { addAgents | newAgent = updatedNewAgent } }, Cmd.none )

        UpdateAgentCheckbox isAdmin ->
            let
                addAgents =
                    model.addAgents

                newAgent =
                    addAgents.newAgent

                updatedNewAgent =
                    { newAgent | isAdmin = isAdmin }
            in
            ( { model | addAgents = { addAgents | newAgent = updatedNewAgent } }, Cmd.none )

        UpdateAgentRole isAgent ->
            let
                addAgents =
                    model.addAgents

                newAgent =
                    addAgents.newAgent

                updatedNewAgent =
                    { newAgent | isAgent = isAgent }
            in
            ( { model | addAgents = { addAgents | newAgent = updatedNewAgent } }, Cmd.none )

        AddAgent ->
            -- Create a new agent from the form data
            let
                addAgents =
                    model.addAgents

                newAgentForm =
                    addAgents.newAgent

                -- Create a new agent with a temporary ID
                newAgent =
                    { id = String.fromInt (List.length addAgents.agents + 1)
                    , firstName = newAgentForm.firstName
                    , lastName = newAgentForm.lastName
                    , email = newAgentForm.email
                    , phone = newAgentForm.phone
                    , isAdmin = newAgentForm.isAdmin
                    , isAgent = newAgentForm.isAgent
                    }

                updatedAgents =
                    newAgent :: addAgents.agents
            in
            case model.session of
                Just session ->
                    -- Save the agent to the server and then clear the form
                    ( { model | isLoading = True }
                    , saveAgent session.sessionId newAgent
                    )

                Nothing ->
                    -- Just add to the local list and clear the form
                    ( { model | addAgents = { addAgents | agents = updatedAgents, newAgent = newAgentInit } }, Cmd.none )

        SaveAgent ->
            -- Add the current agent being edited
            -- For simplicity, this is the same as AddAgent in this implementation
            update AddAgent model

        CancelAddAgent ->
            -- Clear the form without adding the agent
            let
                addAgents =
                    model.addAgents
            in
            ( { model | addAgents = { addAgents | newAgent = newAgentInit, agentEmailStatus = NotChecked } }, Cmd.none )

        AgentSaved result ->
            case result of
                Ok _ ->
                    -- Refresh the agents list from the server
                    case model.session of
                        Just session ->
                            ( { model | isLoading = False }
                            , fetchOnboardingSettings session.sessionId
                            )

                        Nothing ->
                            -- Just clear the form
                            let
                                addAgents =
                                    model.addAgents
                            in
                            ( { model | isLoading = False, addAgents = { addAgents | newAgent = newAgentInit, agentEmailStatus = NotChecked } }, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Failed to save agent.", isLoading = False }, Cmd.none )

        -- Payment
        UpdateExtraAgents count ->
            let
                payment =
                    model.payment
            in
            ( { model | payment = { payment | extraAgents = count } }, Cmd.none )

        UpdateExtraContacts count ->
            let
                payment =
                    model.payment
            in
            ( { model | payment = { payment | extraContacts = count } }, Cmd.none )

        CompletePayment ->
            case model.session of
                Just session ->
                    ( { model | isLoading = True }
                    , completePayment session.sessionId model.payment
                    )

                Nothing ->
                    ( { model | error = Just "No active session." }, Cmd.none )

        PaymentCompleted result ->
            case result of
                Ok _ ->
                    -- Complete the onboarding process
                    ( { model | isLoading = False }, update CompleteOnboarding model |> Tuple.second )

                Err _ ->
                    ( { model | error = Just "Failed to process payment.", isLoading = False }, Cmd.none )

        -- Enterprise Form
        UpdateEnterpriseName name ->
            let
                form =
                    model.enterpriseForm
            in
            ( { model | enterpriseForm = { form | enterpriseName = name } }, Cmd.none )

        UpdateEnterpriseEmail email ->
            let
                form =
                    model.enterpriseForm
            in
            ( { model | enterpriseForm = { form | enterpriseEmail = email } }, Cmd.none )

        UpdateEnterprisePhone phone ->
            let
                form =
                    model.enterpriseForm
            in
            ( { model | enterpriseForm = { form | enterprisePhone = phone } }, Cmd.none )

        UpdateEnterpriseMessage message ->
            let
                form =
                    model.enterpriseForm
            in
            ( { model | enterpriseForm = { form | message = message } }, Cmd.none )

        SaveEnterpriseForm ->
            case model.session of
                Just session ->
                    ( { model | enterpriseFormStatus = Submitted, isLoading = True }
                    , saveEnterpriseForm session.sessionId model.enterpriseForm
                    )

                Nothing ->
                    ( { model | error = Just "No active session." }, Cmd.none )

        EnterpriseFormSaved result ->
            case result of
                Ok _ ->
                    -- Return to plan selection
                    ( { model
                        | enterpriseFormStatus = SubmittedSuccessfully
                        , isLoading = False
                      }
                    , Nav.pushUrl model.key "/onboarding/plan"
                    )

                Err _ ->
                    ( { model
                        | enterpriseFormStatus = SubmittedWithError "Failed to submit enterprise form."
                        , isLoading = False
                      }
                    , Cmd.none
                    )

        -- Navigation
        NavigateToStep step ->
            -- Handle navigation between steps
            let
                newHistory =
                    model.step :: model.stepHistory
            in
            ( { model | step = step, stepHistory = newHistory }
            , Nav.pushUrl model.key (getStepUrl step)
            )

        NavigateBack ->
            -- Go back to previous step
            case model.stepHistory of
                prevStep :: restHistory ->
                    ( { model | step = prevStep, stepHistory = restHistory }
                    , Nav.pushUrl model.key (getStepUrl prevStep)
                    )

                [] ->
                    -- No history, stay on current step
                    ( model, Cmd.none )

        SkipStep ->
            -- Skip to the next step
            let
                nextStep =
                    getNextStep model.step model.isBasicPlan
            in
            ( { model | step = nextStep, stepHistory = model.step :: model.stepHistory }
            , Nav.pushUrl model.key (getStepUrl nextStep)
            )

        CompleteOnboarding ->
            -- Complete the onboarding process
            case model.session of
                Just session ->
                    ( { model | isLoading = True }
                    , completeOnboardingRequest session.sessionId
                    )

                Nothing ->
                    ( { model | error = Just "No active session." }, Cmd.none )

        OnboardingCompleted result ->
            case result of
                Ok _ ->
                    -- Redirect to login with special parameters
                    ( { model | isLoading = False }
                    , Nav.pushUrl model.key ("/login?onboarding=completed&email=" ++ Url.percentEncode model.userDetails.email)
                    )

                Err _ ->
                    ( { model | error = Just "Failed to complete onboarding.", isLoading = False }, Cmd.none )

        RandomOrgNameGenerated orgName ->
            -- After generating a random organization name, start onboarding with this name
            ( { model | orgSlug = orgName }
            , startOnboarding
                (if model.isBasicPlan then
                    "basic"

                 else
                    "pro"
                )
            )

        ScrollToTop ->
            ( model, Task.perform (\_ -> NoOp) (Dom.setViewport 0 0) )

        NoOp ->
            ( model, Cmd.none )

        -- Subscription Tiers
        GotSubscriptionTiers result ->
            case result of
                Ok tiers ->
                    -- Store the subscription tiers and use them in the UI
                    ( { model | subscriptionTiers = tiers, isLoading = False }, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Failed to load subscription plans.", isLoading = False }, Cmd.none )

        SelectPlanType planType ->
            -- When a plan is selected, set isBasicPlan flag
            ( { model | isBasicPlan = planType == "basic" }, Cmd.none )



-- Helper functions for navigation


getStepUrl : Step -> String
getStepUrl step =
    "/onboarding/"
        ++ (case step of
                PlanSelectionStep ->
                    "plan"

                UserDetailsStep ->
                    "personal"

                CompanyDetailsStep ->
                    "company"

                LicensingSettingsStep ->
                    "licensing"

                AddAgentsStep ->
                    "agents"

                PaymentStep ->
                    "payment"

                EnterpriseFormStep ->
                    "enterprise"
           )


getNextStep : Step -> Bool -> Step
getNextStep currentStep isBasicPlan =
    case currentStep of
        PlanSelectionStep ->
            UserDetailsStep

        UserDetailsStep ->
            CompanyDetailsStep

        CompanyDetailsStep ->
            LicensingSettingsStep

        LicensingSettingsStep ->
            if isBasicPlan then
                -- Skip adding agents for basic plan
                PaymentStep

            else
                AddAgentsStep

        AddAgentsStep ->
            PaymentStep

        PaymentStep ->
            -- Last step, don't advance
            PaymentStep

        EnterpriseFormStep ->
            -- Special case, goes back to plan selection
            PlanSelectionStep



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onboardingCookieReceived OnboardingCookieReceived
        , onboardingDataReceived OnboardingDataReceived
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = getStepTitle model.step
    , body =
        [ div []
            [ if model.isLoading then
                -- Show loading spinner when loading
                div [ class "flex justify-center items-center h-screen" ]
                    [ div [ class "animate-spin rounded-full h-32 w-32 border-t-2 border-b-2 border-blue-500" ] [] ]

              else
                div []
                    [ viewHeader model
                    , SetupLayout.view
                        (mapStepToSetupStep model.step)
                        model.isBasicPlan
                        (getStepNumber model.step)
                        [ div [ class "max-w-3xl mx-auto" ]
                            [ case model.error of
                                Just errorMsg ->
                                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded mb-4" ]
                                        [ text errorMsg ]

                                Nothing ->
                                    text ""
                            , viewCurrentStep model
                            , viewNavigationControls model
                            ]
                        ]
                    ]
            ]
        ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "bg-white shadow" ]
        [ div [ class "max-w-7xl mx-auto py-4 px-4 sm:px-6 lg:px-8 flex items-center justify-between" ]
            [ div [ class "flex items-center" ]
                [ div [ class "flex-shrink-0 mr-4" ]
                    [ if not (String.isEmpty model.companyDetails.agencyName) && String.isEmpty (Maybe.withDefault "" model.companyDetails.logo) then
                        -- If no logo but has company name, show initials
                        div [ class "w-10 h-10 rounded-full bg-blue-600 flex items-center justify-center text-white font-bold text-xl" ]
                            [ text (String.left 1 model.companyDetails.agencyName) ]

                      else if not (String.isEmpty (Maybe.withDefault "" model.companyDetails.logo)) then
                        -- If logo exists, show it
                        img [ class "h-10 w-auto", src (Maybe.withDefault "" model.companyDetails.logo), alt "Company Logo" ] []

                      else
                        -- Default Medicare Portal logo
                        div [ class "w-10 h-10 rounded-full bg-blue-600 flex items-center justify-center text-white font-bold text-xl" ]
                            [ text "M" ]
                    ]
                ]
            , div [ class "hidden md:block" ]
                [ div [ class "text-sm text-gray-600" ]
                    [ text "Need help? "
                    , a [ href "mailto:support@medicareportal.org", class "text-blue-600 hover:text-blue-800" ]
                        [ text "Contact Support" ]
                    ]
                ]
            ]
        ]


viewCurrentStep : Model -> Html Msg
viewCurrentStep model =
    case model.step of
        PlanSelectionStep ->
            viewPlanSelection model

        UserDetailsStep ->
            viewUserDetails model

        CompanyDetailsStep ->
            viewCompanyDetails model

        LicensingSettingsStep ->
            viewLicensingSettings model

        AddAgentsStep ->
            if model.isBasicPlan then
                div [ class "text-center p-8" ]
                    [ text "This step is not available for the basic plan. Please continue to payment."
                    , div [ class "mt-4" ]
                        [ button
                            [ class "px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
                            , onClick (NavigateToStep PaymentStep)
                            ]
                            [ text "Continue to Payment" ]
                        ]
                    ]

            else
                viewAddAgents model

        PaymentStep ->
            viewPayment model

        EnterpriseFormStep ->
            viewEnterpriseForm model


viewPlanSelection : Model -> Html Msg
viewPlanSelection model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Choose your plan" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Select a plan that best fits your organization's needs" ]
            ]
        , if model.isLoading then
            viewLoading

          else
            div []
                [ div [ class "grid grid-cols-1 md:grid-cols-3 gap-4" ]
                    (if List.isEmpty model.subscriptionTiers then
                        -- Fallback to hardcoded plans if no tiers available
                        [ viewPlanOption
                            "basic"
                            "Basic"
                            "$29/month"
                            [ "1 agent seat"
                            , "Up to 100 clients"
                            , "Medicare Advantage quotes"
                            , "Medicare Supplement quotes"
                            , "PDP quotes"
                            , "Client management"
                            ]
                            1
                            100
                            model.isBasicPlan
                        , viewPlanOption
                            "pro"
                            "Pro"
                            "$99/month"
                            [ "Up to 3 agent seats"
                            , "Up to 1,000 clients"
                            , "Medicare Advantage quotes"
                            , "Medicare Supplement quotes"
                            , "PDP quotes"
                            , "Client management"
                            , "Team collaboration"
                            , "Advanced reporting"
                            , "Email campaigns"
                            ]
                            3
                            1000
                            (not model.isBasicPlan && not (model.step == EnterpriseFormStep))
                        , viewPlanOption
                            "enterprise"
                            "Enterprise"
                            "Contact Us"
                            [ "Custom agent seats"
                            , "Unlimited clients"
                            , "Medicare Advantage quotes"
                            , "Medicare Supplement quotes"
                            , "PDP quotes"
                            , "Client management"
                            , "Team collaboration"
                            , "Advanced reporting"
                            , "Email campaigns"
                            , "Custom integrations"
                            , "Dedicated account manager"
                            , "Priority support"
                            ]
                            -1
                            -1
                            (model.step == EnterpriseFormStep)
                        ]

                     else
                        -- Use tiers from the database
                        List.map
                            (\tier ->
                                viewPlanOption
                                    tier.id
                                    tier.name
                                    (if tier.id == "enterprise" then
                                        "Contact Us"

                                     else
                                        "$" ++ String.fromFloat tier.price ++ "/month"
                                    )
                                    tier.features
                                    tier.maxAgents
                                    tier.maxContacts
                                    (if tier.id == "basic" then
                                        model.isBasicPlan

                                     else if tier.id == "enterprise" then
                                        model.step == EnterpriseFormStep

                                     else
                                        not model.isBasicPlan && not (model.step == EnterpriseFormStep)
                                    )
                            )
                            model.subscriptionTiers
                    )
                , if canAddMoreResources model then
                    viewExtraResources model

                  else
                    text ""
                , if model.error /= Nothing then
                    div [ class "mt-4 text-red-500" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , div [ class "mt-8 flex justify-center" ]
                    [ button
                        [ class
                            ("px-6 py-3 rounded-lg transition-colors duration-200 "
                                ++ (if not (isPlanBasic model) && not (isPlanPro model) && not (isPlanEnterprise model) then
                                        "bg-[#2563EB]/50 cursor-not-allowed text-white"

                                    else
                                        "bg-[#2563EB] hover:bg-[#1D4ED8] text-white"
                                   )
                            )
                        , onClick
                            (if isPlanEnterprise model then
                                NavigateToStep EnterpriseFormStep

                             else
                                StartOnboarding
                                    (if model.isBasicPlan then
                                        "basic"

                                     else
                                        "pro"
                                    )
                            )
                        , Html.Attributes.disabled (not (isPlanBasic model) && not (isPlanPro model) && not (isPlanEnterprise model))
                        ]
                        [ text "Continue" ]
                    ]
                ]
        ]


viewLoading : Html msg
viewLoading =
    div [ class "text-center" ]
        [ div [ class "animate-spin rounded-full h-16 w-16 border-t-4 border-b-4 border-blue-500 mx-auto" ] []
        , p [ class "mt-4 text-gray-500" ]
            [ text "Loading subscription tiers..." ]
        ]


viewPlanOption : String -> String -> String -> List String -> Int -> Int -> Bool -> Html Msg
viewPlanOption id name price features agentLimit contactLimit isSelected =
    div
        [ class
            ("p-6 rounded-lg cursor-pointer transition-all "
                ++ (if isSelected then
                        "bg-[#2563EB]/10 ring-2 ring-[#2563EB]"

                    else
                        "bg-gray-50 hover:bg-gray-100"
                   )
            )
        , onClick
            (if id == "enterprise" then
                NavigateToStep EnterpriseFormStep

             else
                SelectPlanType id
            )
        ]
        [ div [ class "space-y-4" ]
            [ div []
                [ h3 [ class "text-xl font-semibold text-gray-900" ] [ text name ]
                , p [ class "text-3xl font-bold text-gray-900 mt-2" ]
                    [ text price ]
                ]
            , div [ class "space-y-2 py-4 border-t border-b border-gray-200" ]
                [ if id /= "enterprise" then
                    div [ class "text-gray-600" ]
                        [ text
                            (if id == "pro" then
                                "5+ agent seats"

                             else if agentLimit == -1 then
                                "Unlimited agent seats"

                             else if agentLimit == 1 then
                                "1 agent seat"

                             else
                                "Up to " ++ String.fromInt agentLimit ++ " agent seats"
                            )
                        ]

                  else
                    div [ class "text-gray-600" ]
                        [ text "Custom agent seats" ]
                , if id /= "enterprise" then
                    div [ class "text-gray-600" ]
                        [ text
                            (if id == "pro" then
                                "5,000+ clients"

                             else if contactLimit == -1 then
                                "Unlimited clients"

                             else if contactLimit == 1000 then
                                "1,000 clients"

                             else
                                "Up to " ++ String.fromInt contactLimit ++ " clients"
                            )
                        ]

                  else
                    div [ class "text-gray-600" ]
                        [ text "Unlimited clients" ]
                ]
            , div [ class "mt-4" ]
                [ p [ class "text-sm font-medium text-gray-900 mb-2" ] [ text "Features:" ]
                , ul [ class "space-y-2" ]
                    (List.map
                        (\feature ->
                            li [ class "flex items-center text-sm text-gray-600" ]
                                [ span [ class "text-[#059669] mr-2" ] [ text "✓" ]
                                , text feature
                                ]
                        )
                        features
                    )
                ]
            ]
        ]


viewExtraResources : Model -> Html Msg
viewExtraResources model =
    div [ class "mt-8 p-4 bg-gray-50 rounded-lg border border-gray-200" ]
        [ h3 [ class "text-lg font-semibold text-gray-900 mb-4" ]
            [ text "Additional Resources" ]
        , div [ class "grid grid-cols-1 md:grid-cols-2 gap-6" ]
            [ div [ class "space-y-2" ]
                [ label [ class "block text-sm font-medium text-gray-700" ]
                    [ text "Extra Agents" ]
                , p [ class "text-xs text-gray-500" ]
                    [ text "Add more agent seats beyond your plan's included limit ($20/agent seat/month)" ]
                , div [ class "flex items-center" ]
                    [ button
                        [ class "bg-gray-200 px-3 py-1 rounded-l-md hover:bg-gray-300"
                        , onClick (UpdateExtraAgents (max 0 (model.payment.extraAgents - 1)))
                        ]
                        [ text "-" ]
                    , input
                        [ type_ "number"
                        , class "w-16 text-center border-y border-gray-200 py-1"
                        , value (String.fromInt model.payment.extraAgents)
                        , onInput (\val -> UpdateExtraAgents (String.toInt val |> Maybe.withDefault 0))
                        ]
                        []
                    , button
                        [ class "bg-gray-200 px-3 py-1 rounded-r-md hover:bg-gray-300"
                        , onClick (UpdateExtraAgents (model.payment.extraAgents + 1))
                        ]
                        [ text "+" ]
                    , span [ class "ml-2 text-sm font-medium" ]
                        [ text ("$" ++ String.fromInt (model.payment.extraAgents * 20) ++ "/mo") ]
                    ]
                ]
            , div [ class "space-y-2" ]
                [ label [ class "block text-sm font-medium text-gray-700" ]
                    [ text "Extra Clients" ]
                , p [ class "text-xs text-gray-500" ]
                    [ text "Add more clients beyond your plan's included limit ($50/5,000 clients/month)" ]
                , div [ class "flex items-center" ]
                    [ button
                        [ class "bg-gray-200 px-3 py-1 rounded-l-md hover:bg-gray-300"
                        , onClick (UpdateExtraContacts (max 0 (model.payment.extraContacts - 5000)))
                        ]
                        [ text "-" ]
                    , input
                        [ type_ "number"
                        , class "w-20 text-center border-y border-gray-200 py-1"
                        , value (String.fromInt model.payment.extraContacts)
                        , onInput (\val -> UpdateExtraContacts (String.toInt val |> Maybe.withDefault 0))
                        , Html.Attributes.step "5000"
                        ]
                        []
                    , button
                        [ class "bg-gray-200 px-3 py-1 rounded-r-md hover:bg-gray-300"
                        , onClick (UpdateExtraContacts (model.payment.extraContacts + 5000))
                        ]
                        [ text "+" ]
                    , span [ class "ml-2 text-sm font-medium" ]
                        [ text ("$" ++ String.fromInt (model.payment.extraContacts // 5000 * 50) ++ "/mo") ]
                    ]
                ]
            ]
        ]


viewEnterpriseForm : Model -> Html Msg
viewEnterpriseForm model =
    div [ class "py-8" ]
        [ h2 [ class "text-2xl font-semibold mb-6" ] [ text "Enterprise Inquiry" ]
        , div [ class "bg-white shadow rounded-lg p-6" ]
            [ p [ class "mb-6 text-gray-600" ]
                [ text "Please fill in the details below, and our team will contact you with a customized enterprise quote." ]
            , div [ class "space-y-6" ]
                [ div []
                    [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "enterpriseName" ] [ text "Company Name" ]
                    , input
                        [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                        , id "enterpriseName"
                        , type_ "text"
                        , placeholder "Enter your company name"
                        , value model.enterpriseForm.enterpriseName
                        , onInput UpdateEnterpriseName
                        ]
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "enterpriseEmail" ] [ text "Contact Email" ]
                    , input
                        [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                        , id "enterpriseEmail"
                        , type_ "email"
                        , placeholder "Enter your contact email"
                        , value model.enterpriseForm.enterpriseEmail
                        , onInput UpdateEnterpriseEmail
                        ]
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "enterprisePhone" ] [ text "Contact Phone" ]
                    , input
                        [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                        , id "enterprisePhone"
                        , type_ "tel"
                        , placeholder "Enter your contact phone"
                        , value model.enterpriseForm.enterprisePhone
                        , onInput UpdateEnterprisePhone
                        ]
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "enterpriseMessage" ] [ text "Additional Information" ]
                    , textarea
                        [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                        , id "enterpriseMessage"
                        , placeholder "Tell us about your needs, team size, and any specific requirements"
                        , value model.enterpriseForm.message
                        , onInput UpdateEnterpriseMessage
                        , Html.Attributes.rows 4
                        ]
                        []
                    ]
                , div [ class "flex space-x-4 mt-6" ]
                    [ button
                        [ class "flex-1 px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2"
                        , onClick SaveEnterpriseForm
                        ]
                        [ text "Submit Inquiry" ]
                    , button
                        [ class "flex-1 px-4 py-2 border border-gray-300 text-gray-700 rounded hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-gray-500 focus:ring-offset-2"
                        , onClick (NavigateToStep PlanSelectionStep)
                        ]
                        [ text "Back to Plans" ]
                    ]
                ]
            , if model.enterpriseFormStatus == SubmittedSuccessfully then
                div [ class "mt-6 p-4 bg-green-100 text-green-700 rounded" ]
                    [ text "Thank you for your inquiry! Our team will contact you shortly." ]

              else if model.enterpriseFormStatus == Submitted then
                div [ class "mt-6 p-4 bg-blue-100 text-blue-700 rounded" ]
                    [ text "Submitting your inquiry..." ]

              else
                text ""
            ]
        ]


viewNavigationControls : Model -> Html Msg
viewNavigationControls model =
    div [ class "flex justify-between mt-8 border-t pt-4" ]
        [ if not (List.isEmpty model.stepHistory) then
            button
                [ class "px-4 py-2 text-blue-600 border border-blue-600 rounded hover:bg-blue-50"
                , onClick NavigateBack
                ]
                [ text "Back" ]

          else
            div [] []

        -- Empty spacer
        , let
            canSkip =
                case model.step of
                    PlanSelectionStep ->
                        False

                    -- Can't skip plan selection
                    PaymentStep ->
                        False

                    -- Can't skip payment
                    EnterpriseFormStep ->
                        False

                    -- Can't skip enterprise form
                    _ ->
                        True

            -- Can skip other steps
            nextStep =
                getNextStep model.step model.isBasicPlan

            isLastStep =
                model.step == PaymentStep
          in
          if canSkip && not isLastStep then
            button
                [ class "px-4 py-2 text-gray-600 hover:text-gray-800"
                , onClick SkipStep
                ]
                [ text ("Skip to " ++ getStepTitle nextStep) ]

          else
            div [] []

        -- Empty spacer
        ]


mapStepToSetupStep : Step -> SetupLayout.SetupStep
mapStepToSetupStep step =
    case step of
        PlanSelectionStep ->
            SetupLayout.PlanSelection

        UserDetailsStep ->
            SetupLayout.OrganizationSetup

        CompanyDetailsStep ->
            SetupLayout.OrganizationSetup

        LicensingSettingsStep ->
            SetupLayout.OrganizationSetup

        AddAgentsStep ->
            SetupLayout.AgentSetup

        PaymentStep ->
            SetupLayout.OrganizationSetup

        EnterpriseFormStep ->
            SetupLayout.OrganizationSetup


getStepTitle : Step -> String
getStepTitle step =
    case step of
        PlanSelectionStep ->
            "Choose Your Plan"

        UserDetailsStep ->
            "Personal Details"

        CompanyDetailsStep ->
            "Company Details"

        LicensingSettingsStep ->
            "Licensing & Carriers"

        AddAgentsStep ->
            "Add Team Members"

        PaymentStep ->
            "Payment"

        EnterpriseFormStep ->
            "Enterprise Form"


getStepNumber : Step -> Int
getStepNumber step =
    case step of
        PlanSelectionStep ->
            1

        UserDetailsStep ->
            2

        CompanyDetailsStep ->
            3

        LicensingSettingsStep ->
            4

        AddAgentsStep ->
            5

        PaymentStep ->
            6

        EnterpriseFormStep ->
            7


viewUserDetails : Model -> Html Msg
viewUserDetails model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Personal Details" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Tell us about yourself" ]
            ]
        , if model.isLoading then
            viewLoading

          else
            div [ class "space-y-6" ]
                [ div [ class "bg-white shadow rounded-lg p-6" ]
                    [ div [ class "space-y-6" ]
                        [ div [ class "grid grid-cols-1 sm:grid-cols-2 gap-6" ]
                            [ div []
                                [ label [ class "block text-sm font-medium text-gray-700" ]
                                    [ text "First Name" ]
                                , input
                                    [ type_ "text"
                                    , class "mt-1 block w-full px-3 py-2 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                    , value model.userDetails.firstName
                                    , onInput UpdateUserFirstName
                                    , placeholder "Enter your first name"
                                    ]
                                    []
                                ]
                            , div []
                                [ label [ class "block text-sm font-medium text-gray-700" ]
                                    [ text "Last Name" ]
                                , input
                                    [ type_ "text"
                                    , class "mt-1 block w-full px-3 py-2 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                    , value model.userDetails.lastName
                                    , onInput UpdateUserLastName
                                    , placeholder "Enter your last name"
                                    ]
                                    []
                                ]
                            ]
                        , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-6 pt-2" ]
                            [ div [ class "relative pb-6" ]
                                [ label [ class "block text-sm font-medium text-gray-700" ]
                                    [ text "Email" ]
                                , div [ class "relative" ]
                                    [ input
                                        [ type_ "email"
                                        , class "mt-1 block w-full px-3 py-2 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                        , value model.userDetails.email
                                        , onInput UpdateUserEmail
                                        , onFocus UserEmailFocused
                                        , onBlur UserEmailBlurred
                                        , placeholder "you@example.com"
                                        ]
                                        []
                                    , viewEmailStatus model.userEmailStatus
                                    ]
                                ]
                            , div []
                                [ label [ class "block text-sm font-medium text-gray-700" ]
                                    [ text "Phone" ]
                                , input
                                    [ type_ "tel"
                                    , class "mt-1 block w-full px-3 py-2 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                    , value (formatPhoneNumber model.userDetails.phone)
                                    , onInput UpdateUserPhone
                                    , placeholder "(555) 555-5555"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , if model.error /= Nothing then
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , div [ class "flex justify-center" ]
                    [ button
                        [ class
                            (if isUserDetailsFormValid model then
                                "px-4 py-2 sm:px-6 sm:py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"

                             else
                                "px-4 py-2 sm:px-6 sm:py-3 bg-gray-300 text-gray-500 rounded-md cursor-not-allowed"
                            )
                        , onClick SaveUserDetails
                        , Html.Attributes.disabled (not (isUserDetailsFormValid model))
                        ]
                        [ text "Continue" ]
                    ]
                ]
        ]


viewEmailStatus : EmailStatus -> Html msg
viewEmailStatus status =
    case status of
        NotChecked ->
            -- When not checked, explicitly render an empty div structure to properly replace any previous status
            div []
                [ div [ class "absolute right-0 inset-y-0" ] [ text "" ]
                , text "" -- Empty text element to replace any error message
                ]

        Checking ->
            -- Show loading spinner while checking
            div []
                [ div
                    [ class "absolute right-0 inset-y-0" ]
                    [ div
                        [ class "absolute right-0 inset-y-0 flex items-center pr-3" ]
                        [ div
                            [ class "animate-spin h-5 w-5 text-blue-500" ]
                            [ text "•" ]

                        -- Simple spinner replacement
                        ]
                    ]
                , text "" -- Empty text element to replace any error message
                ]

        Valid ->
            -- Show only the success icon for available emails
            div []
                [ div
                    [ class "absolute right-0 inset-y-0" ]
                    [ div
                        [ class "absolute right-0 inset-y-0 flex items-center pr-3" ]
                        [ div
                            [ class "text-green-500" ]
                            [ text "✓" ]

                        -- Simple checkmark
                        ]
                    ]
                , text "" -- Empty text element to replace the error message
                ]

        Invalid message ->
            -- For unavailable emails, show icon and error message
            div []
                [ div
                    [ class "absolute right-0 inset-y-0" ]
                    [ div
                        [ class "absolute right-0 inset-y-0 flex items-center pr-3" ]
                        [ div
                            [ class "text-red-500" ]
                            [ text "✗" ]

                        -- Simple X mark
                        ]
                    ]
                , p
                    [ class "text-xs text-red-600 mt-1 absolute left-0 top-full w-full" ]
                    [ text message ]
                ]


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    let
        digits =
            String.filter Char.isDigit phone
                |> String.left 10

        len =
            String.length digits
    in
    if len == 0 then
        ""

    else if len <= 3 then
        "(" ++ digits

    else if len <= 6 then
        "(" ++ String.left 3 digits ++ ") " ++ String.dropLeft 3 digits

    else
        "("
            ++ String.left 3 digits
            ++ ") "
            ++ String.slice 3 6 digits
            ++ "-"
            ++ String.dropLeft 6 digits


isUserDetailsFormValid : Model -> Bool
isUserDetailsFormValid model =
    let
        emailValid =
            case model.userEmailStatus of
                Valid ->
                    True

                -- If the email hasn't been checked yet, consider it invalid
                NotChecked ->
                    False

                -- Email is being checked, consider it invalid until check completes
                Checking ->
                    False

                -- Email is unavailable (error state)
                Invalid _ ->
                    False
    in
    not (String.isEmpty (String.trim model.userDetails.firstName))
        && not (String.isEmpty (String.trim model.userDetails.lastName))
        && not (String.isEmpty (String.trim model.userDetails.email))
        && not (String.isEmpty (String.trim model.userDetails.phone))
        && emailValid


viewLicensingSettings : Model -> Html Msg
viewLicensingSettings model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Licensing & Carriers" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Select carrier contracts you currently have" ]
            ]
        , if model.isLoading then
            viewLoading

          else
            div [ class "space-y-6" ]
                [ div [ class "bg-white shadow rounded-lg p-6" ]
                    [ div [ class "space-y-6" ]
                        [ div []
                            [ h3 [ class "text-lg font-medium mb-3" ] [ text "Carrier Contracts" ]
                            , div [ class "mb-4 flex items-center" ]
                                [ viewSelectAllCarriers model.licensingSettings ]
                            , div [ class "grid grid-cols-1 md:grid-cols-2 gap-3" ]
                                (List.map
                                    (\carrier ->
                                        viewCarrierCheckbox carrier model.licensingSettings
                                    )
                                    allCarriers
                                )
                            ]
                        , div [ class "mt-4" ]
                            [ label [ class "flex items-center space-x-2 cursor-pointer" ]
                                [ input
                                    [ class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
                                    , type_ "checkbox"
                                    , checked model.licensingSettings.useSmartSendForGI
                                    , onCheck ToggleSmartSendForGI
                                    ]
                                    []
                                , span [ class "text-sm font-medium text-gray-700" ] [ text "Use SmartSend for Group Insurance" ]
                                ]
                            ]
                        ]
                    ]
                , if model.error /= Nothing then
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , div [ class "flex justify-center" ]
                    [ button
                        [ class "px-4 py-2 sm:px-6 sm:py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                        , onClick SaveLicensingSettings
                        ]
                        [ text "Continue" ]
                    ]
                ]
        ]


viewSelectAllCarriers : LicensingSettings -> Html Msg
viewSelectAllCarriers settings =
    label [ class "flex items-center space-x-3 mb-4" ]
        [ input
            [ type_ "checkbox"
            , checked (List.length settings.carrierContracts == List.length allCarriers)
            , onCheck ToggleAllCarriers
            , class "h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
            ]
            []
        , span [ class "text-gray-700" ] [ text "Select All Carriers" ]
        ]


viewCarrierCheckbox : String -> LicensingSettings -> Html Msg
viewCarrierCheckbox carrier settings =
    let
        isChecked =
            List.member carrier settings.carrierContracts
    in
    label [ class "flex items-center space-x-2 cursor-pointer p-2 border rounded hover:bg-gray-50" ]
        [ input
            [ class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
            , type_ "checkbox"
            , checked isChecked
            , onClick
                (if isChecked then
                    RemoveCarrierContract carrier

                 else
                    AddCarrierContract carrier
                )
            ]
            []
        , span [ class "text-sm font-medium text-gray-700" ] [ text carrier ]
        ]


viewAddAgents : Model -> Html Msg
viewAddAgents model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Add Team Members" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Add your team members to get started" ]
            ]
        , if model.isLoading then
            viewLoading

          else
            div [ class "space-y-6" ]
                [ div [ class "mb-8 bg-white shadow rounded-lg overflow-hidden" ]
                    [ if List.isEmpty model.addAgents.agents then
                        div [ class "p-6 text-center text-gray-500" ]
                            [ text "No team members added yet. Use the form below to add your first team member." ]

                      else
                        div []
                            [ table [ class "min-w-full divide-y divide-gray-200" ]
                                [ thead [ class "bg-gray-50" ]
                                    [ tr []
                                        [ th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Name" ]
                                        , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Email" ]
                                        , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Phone" ]
                                        , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Role" ]
                                        ]
                                    ]
                                , tbody [ class "bg-white divide-y divide-gray-200" ]
                                    (List.map viewAgentRow model.addAgents.agents)
                                ]
                            ]
                    ]
                , div [ class "bg-white shadow rounded-lg p-6" ]
                    [ h3 [ class "text-lg font-medium mb-4" ] [ text "Add New Team Member" ]
                    , div [ class "space-y-4" ]
                        [ div [ class "grid grid-cols-1 md:grid-cols-2 gap-4" ]
                            [ div []
                                [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "agentFirstName" ] [ text "First Name" ]
                                , input
                                    [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                                    , id "agentFirstName"
                                    , type_ "text"
                                    , placeholder "First Name"
                                    , value model.addAgents.newAgent.firstName
                                    , onInput UpdateAgentFirstName
                                    ]
                                    []
                                ]
                            , div []
                                [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "agentLastName" ] [ text "Last Name" ]
                                , input
                                    [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                                    , id "agentLastName"
                                    , type_ "text"
                                    , placeholder "Last Name"
                                    , value model.addAgents.newAgent.lastName
                                    , onInput UpdateAgentLastName
                                    ]
                                    []
                                ]
                            ]
                        , div []
                            [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "agentEmail" ] [ text "Email" ]
                            , input
                                [ class
                                    ("w-full px-3 py-2 border rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500 "
                                        ++ (case model.addAgents.agentEmailStatus of
                                                Valid ->
                                                    "border-green-500"

                                                Invalid _ ->
                                                    "border-red-500"

                                                _ ->
                                                    "border-gray-300"
                                           )
                                    )
                                , id "agentEmail"
                                , type_ "email"
                                , placeholder "Email"
                                , value model.addAgents.newAgent.email
                                , onInput UpdateAgentEmail
                                , onBlur AgentEmailBlurred
                                , onFocus AgentEmailFocused
                                ]
                                []
                            , viewEmailStatus model.addAgents.agentEmailStatus
                            ]
                        , div []
                            [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "agentPhone" ] [ text "Phone" ]
                            , input
                                [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                                , id "agentPhone"
                                , type_ "tel"
                                , placeholder "Phone Number"
                                , value (formatPhoneNumber model.addAgents.newAgent.phone)
                                , onInput UpdateAgentPhone
                                ]
                                []
                            ]
                        , div [ class "flex items-center mt-4" ]
                            [ input
                                [ class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
                                , id "adminRole"
                                , type_ "checkbox"
                                , checked model.addAgents.newAgent.isAdmin
                                , onCheck UpdateAgentCheckbox
                                ]
                                []
                            , label [ class "ml-2 block text-sm text-gray-900", for "adminRole" ] [ text "Admin access" ]
                            ]
                        , div [ class "flex space-x-4 mt-6" ]
                            [ button
                                [ class "flex-1 px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2"
                                , onClick AddAgent
                                ]
                                [ text "Add Team Member" ]
                            , button
                                [ class "flex-1 px-4 py-2 border border-gray-300 text-gray-700 rounded hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-gray-500 focus:ring-offset-2"
                                , onClick CancelAddAgent
                                ]
                                [ text "Clear Form" ]
                            ]
                        ]
                    ]
                , if model.error /= Nothing then
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , div [ class "flex justify-center" ]
                    [ button
                        [ class "px-4 py-2 sm:px-6 sm:py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                        , onClick SkipStep
                        ]
                        [ text "Continue" ]
                    ]
                ]
        ]


viewAgentRow : Agent -> Html Msg
viewAgentRow agent =
    tr []
        [ td [ class "px-6 py-4 whitespace-nowrap" ]
            [ div [ class "text-sm font-medium text-gray-900" ] [ text (agent.firstName ++ " " ++ agent.lastName) ] ]
        , td [ class "px-6 py-4 whitespace-nowrap" ]
            [ div [ class "text-sm text-gray-900" ] [ text agent.email ] ]
        , td [ class "px-6 py-4 whitespace-nowrap" ]
            [ div [ class "text-sm text-gray-900" ] [ text (formatPhoneNumber agent.phone) ] ]
        , td [ class "px-6 py-4 whitespace-nowrap" ]
            [ if agent.isAdmin then
                span [ class "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-blue-100 text-blue-800" ] [ text "Admin" ]

              else
                span [ class "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-gray-100 text-gray-800" ] [ text "Agent" ]
            ]
        ]


viewCompanyDetails : Model -> Html Msg
viewCompanyDetails model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Company Details" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Tell us about your company" ]
            ]
        , if model.isLoading then
            viewLoading

          else
            div [ class "space-y-6" ]
                [ div [ class "bg-white shadow rounded-lg p-6" ]
                    [ div [ class "space-y-6" ]
                        [ div []
                            [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "agencyName" ]
                                [ text "Agency Name" ]
                            , input
                                [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                                , id "agencyName"
                                , type_ "text"
                                , placeholder "Enter your agency name"
                                , value model.companyDetails.agencyName
                                , onInput UpdateAgencyName
                                ]
                                []
                            ]
                        , div []
                            [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "website" ]
                                [ text "Website" ]
                            , input
                                [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                                , id "website"
                                , type_ "text"
                                , placeholder "https://example.com"
                                , value model.companyDetails.website
                                , onInput UpdateWebsite
                                ]
                                []
                            ]
                        , div []
                            [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "companyPhone" ]
                                [ text "Company Phone" ]
                            , input
                                [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                                , id "companyPhone"
                                , type_ "tel"
                                , placeholder "(555) 555-5555"
                                , value (formatPhoneNumber model.companyDetails.phone)
                                , onInput UpdatePhone
                                ]
                                []
                            ]
                        , div [ class "mt-6" ]
                            [ h3 [ class "text-lg font-medium mb-3" ] [ text "Brand Colors" ]
                            , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-4" ]
                                [ div []
                                    [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "primaryColor" ]
                                        [ text "Primary Color" ]
                                    , input
                                        [ class "h-10 w-full cursor-pointer"
                                        , id "primaryColor"
                                        , type_ "color"
                                        , value
                                            (if String.isEmpty model.companyDetails.primaryColor then
                                                "#2563EB"

                                             else
                                                model.companyDetails.primaryColor
                                            )
                                        , onInput UpdatePrimaryColor
                                        ]
                                        []
                                    ]
                                , div []
                                    [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "secondaryColor" ]
                                        [ text "Secondary Color" ]
                                    , input
                                        [ class "h-10 w-full cursor-pointer"
                                        , id "secondaryColor"
                                        , type_ "color"
                                        , value
                                            (if String.isEmpty model.companyDetails.secondaryColor then
                                                "#10B981"

                                             else
                                                model.companyDetails.secondaryColor
                                            )
                                        , onInput UpdateSecondaryColor
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    ]
                , if model.error /= Nothing then
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , div [ class "flex justify-center" ]
                    [ button
                        [ class "px-4 py-2 sm:px-6 sm:py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                        , onClick SaveCompanyDetails
                        ]
                        [ text "Continue" ]
                    ]
                ]
        ]


viewPayment : Model -> Html Msg
viewPayment model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Complete Your Subscription" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Enter your payment information to complete registration" ]
            ]
        , if model.isLoading then
            viewLoading

          else
            div [ class "space-y-6" ]
                [ div [ class "bg-white shadow rounded-lg overflow-hidden" ]
                    [ div [ class "md:flex" ]
                        [ div [ class "p-6 bg-gray-50 md:w-1/3" ]
                            [ h3 [ class "text-lg font-semibold mb-4" ] [ text "Order Summary" ]
                            , div [ class "space-y-3" ]
                                [ div [ class "flex justify-between" ]
                                    [ span [ class "text-gray-600" ] [ text "Plan Type" ]
                                    , span [ class "font-medium" ]
                                        [ text
                                            (if model.isBasicPlan then
                                                "Basic"

                                             else
                                                "Pro"
                                            )
                                        ]
                                    ]
                                , div [ class "flex justify-between" ]
                                    [ span [ class "text-gray-600" ] [ text "Base Price" ]
                                    , span [ class "font-medium" ]
                                        [ text
                                            (if model.isBasicPlan then
                                                "$29/month"

                                             else
                                                "$99/month"
                                            )
                                        ]
                                    ]
                                , if model.payment.extraAgents > 0 then
                                    div [ class "flex justify-between" ]
                                        [ span [ class "text-gray-600" ]
                                            [ text ("Extra Agents (" ++ String.fromInt model.payment.extraAgents ++ ")") ]
                                        , span [ class "font-medium" ]
                                            [ text ("$" ++ String.fromInt (model.payment.extraAgents * 20) ++ "/month") ]
                                        ]

                                  else
                                    text ""
                                , if model.payment.extraContacts > 0 then
                                    div [ class "flex justify-between" ]
                                        [ span [ class "text-gray-600" ]
                                            [ text ("Extra Clients (" ++ String.fromInt model.payment.extraContacts ++ ")") ]
                                        , span [ class "font-medium" ]
                                            [ text ("$" ++ String.fromInt (model.payment.extraContacts // 5000 * 50) ++ "/month") ]
                                        ]

                                  else
                                    text ""
                                , div [ class "pt-3 mt-3 border-t border-gray-200" ]
                                    [ div [ class "flex justify-between" ]
                                        [ span [ class "font-medium text-gray-900" ] [ text "Total" ]
                                        , span [ class "font-bold text-blue-600" ]
                                            [ text
                                                ("$"
                                                    ++ String.fromInt
                                                        ((if model.isBasicPlan then
                                                            29

                                                          else
                                                            99
                                                         )
                                                            + (model.payment.extraAgents * 20)
                                                            + (model.payment.extraContacts // 5000 * 50)
                                                        )
                                                    ++ "/month"
                                                )
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "p-6 md:w-2/3" ]
                            [ h3 [ class "text-lg font-semibold mb-4" ] [ text "Payment Information" ]
                            , div [ class "space-y-4" ]
                                [ div [ class "mb-4" ]
                                    [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                                        [ text "Credit Card" ]
                                    , div [ class "p-4 bg-gray-100 text-center rounded-md" ]
                                        [ text "Credit card form will be displayed here" ]
                                    ]
                                , div [ class "flex items-center mt-6" ]
                                    [ input
                                        [ class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
                                        , id "agreeTerms"
                                        , type_ "checkbox"
                                        , checked False -- Simplified for now, without using the missing field
                                        ]
                                        []
                                    , label [ class "ml-2 block text-sm text-gray-900", for "agreeTerms" ]
                                        [ text "I agree to the "
                                        , a [ class "text-blue-600 hover:underline" ] [ text "Terms of Service" ]
                                        , text " and "
                                        , a [ class "text-blue-600 hover:underline" ] [ text "Privacy Policy" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , if model.error /= Nothing then
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , div [ class "flex justify-center" ]
                    [ button
                        [ class "px-6 py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                        , onClick CompletePayment
                        , Html.Attributes.disabled False
                        ]
                        [ text "Complete Registration" ]
                    ]
                ]
        ]



-- Helper functions for checking plan types


isPlanBasic : Model -> Bool
isPlanBasic model =
    model.isBasicPlan


isPlanPro : Model -> Bool
isPlanPro model =
    not model.isBasicPlan && not (model.step == EnterpriseFormStep)


isPlanEnterprise : Model -> Bool
isPlanEnterprise model =
    model.step == EnterpriseFormStep


canAddMoreResources : Model -> Bool
canAddMoreResources model =
    not model.isBasicPlan && model.step /= EnterpriseFormStep



-- CONSTANTS


allCarriers : List String
allCarriers =
    [ "Aetna"
    , "Humana"
    , "UnitedHealthcare"
    , "Cigna"
    , "BlueCross BlueShield"
    , "Kaiser Permanente"
    , "Anthem"
    , "Molina Healthcare"
    ]
