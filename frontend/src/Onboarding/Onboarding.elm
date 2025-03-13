port module Onboarding.Onboarding exposing
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
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Onboarding.Steps.AddAgents as AddAgents
import Onboarding.Steps.CompanyDetails as CompanyDetails
import Onboarding.Steps.EnterpriseForm as EnterpriseForm
import Onboarding.Steps.LicensingSettings as LicensingSettings
import Onboarding.Steps.Payment as Payment
import Onboarding.Steps.PlanSelection as PlanSelection exposing (fetchSubscriptionTiers)
import Onboarding.Steps.UserDetails as UserDetails
import Random
import Task
import Url
import Utils.RandomOrgName exposing (generateOrgName)



-- Ports for local storage


port storeOnboardingState : { organizationId : String, slug : String, sessionToken : String, step : Int, planType : String } -> Cmd msg


port retrieveOnboardingState : () -> Cmd msg


port onboardingStateReceived : (Maybe { organizationId : String, slug : String, sessionToken : String, step : Int, planType : String } -> msg) -> Sub msg



-- New ports for plan type


port retrievePlanType :
    String
    -> Cmd msg -- Send organization slug to get plan


port planTypeReceived :
    (Maybe String -> msg)
    -> Sub msg -- Receive plan type


port storePlanType :
    { orgSlug : String, planType : String }
    -> Cmd msg -- Store plan type in localStorage



-- New ports for user details


port storeUserDetails : { organizationId : String, firstName : String, lastName : String, email : String, phone : String } -> Cmd msg


port retrieveUserDetails :
    String
    -> Cmd msg -- Pass organization ID to get user details


port userDetailsReceived : (Maybe { firstName : String, lastName : String, email : String, phone : String } -> msg) -> Sub msg



-- MODEL


type Step
    = PlanSelectionStep
    | UserDetailsStep
    | CompanyDetailsStep
    | LicensingSettingsStep
    | AddAgentsStep
    | PaymentStep
    | EnterpriseFormStep


type alias Model =
    { step : Step
    , planSelectionModel : PlanSelection.Model
    , userDetailsModel : UserDetails.Model
    , companyDetailsModel : CompanyDetails.Model
    , licensingSettingsModel : LicensingSettings.Model
    , addAgentsModel : Maybe AddAgents.Model -- Optional, only for Pro plans
    , paymentModel : Payment.Model
    , enterpriseFormModel : Maybe EnterpriseForm.Model -- Optional, only for Enterprise plan
    , key : Nav.Key
    , orgSlug : String
    , session : String
    , isBasicPlan : Bool
    , error : Maybe String
    , isLoading : Bool

    -- New fields for progressive onboarding
    , organizationId : Maybe Int
    , sessionToken : Maybe String
    , onboardingInProgress : Bool

    -- Track if each step's model has been initialized
    , userDetailsInitialized : Bool
    , companyDetailsInitialized : Bool
    , licensingSettingsInitialized : Bool
    , addAgentsInitialized : Bool
    , paymentInitialized : Bool
    , enterpriseFormInitialized : Bool

    -- Track step history for back navigation
    , stepHistory : List Step
    }


init : Nav.Key -> String -> String -> Step -> ( Model, Cmd Msg )
init key orgSlug session initialStep =
    let
        -- Only fetch tiers when we're on the plan selection step
        isOnPlanSelectionStep =
            initialStep == PlanSelectionStep

        -- Check localStorage for any stored onboarding state
        ( planSelectionModel, planSelectionCmd ) =
            -- If we're not on the plan selection step but have a stored state with plan selection,
            -- we should initialize with the saved plan to ensure consistency
            if isOnPlanSelectionStep then
                PlanSelection.initWithFetch key orgSlug session True

            else
                PlanSelection.initWithFetch key orgSlug session False

        -- Determine if this is a new signup
        isNewSignup =
            String.isEmpty (String.trim orgSlug) || initialStep == PlanSelectionStep

        -- Initialize UserDetails model
        ( userDetailsModel, userDetailsCmd ) =
            UserDetails.init key orgSlug isNewSignup

        -- Initialize CompanyDetails model only if needed
        ( companyDetailsModel, companyDetailsCmd ) =
            if initialStep == CompanyDetailsStep then
                CompanyDetails.init key orgSlug session

            else
                -- Create an empty model without making API calls
                ( { agencyName = ""
                  , website = ""
                  , phone = ""
                  , primaryColor = "#0047AB" -- Default blue
                  , secondaryColor = "#FFFFFF" -- Default white
                  , logo = Nothing
                  , isLoading = False
                  , error = Nothing
                  , key = key
                  , orgSlug = orgSlug
                  , uploadingLogo = False
                  , sessionToken = session
                  , loadedFromSession = False
                  }
                , Cmd.none
                )

        -- Initialize LicensingSettings model only if needed
        ( licensingSettingsModel, licensingSettingsCmd ) =
            if initialStep == LicensingSettingsStep then
                LicensingSettings.init key orgSlug

            else
                -- Create an empty model without making API calls
                ( { carrierContracts = []
                  , useSmartSendForGI = True
                  , isLoading = False
                  , error = Nothing
                  , key = key
                  , orgSlug = orgSlug
                  , expandedSections = [ "Carrier Contracts", "Guaranteed Issue Settings" ]
                  }
                , Cmd.none
                )

        -- Initialize Payment model only if needed
        ( paymentModel, paymentCmd ) =
            if initialStep == PaymentStep then
                Payment.init key orgSlug

            else
                -- Create an empty model without making API calls
                ( { isLoading = False
                  , error = Nothing
                  , key = key
                  , orgSlug = orgSlug
                  , subscriptionDetails = Nothing
                  , processingPayment = False
                  , email = ""
                  }
                , Cmd.none
                )

        -- Check if we need to initialize the AddAgents model based on the initial step
        -- or if we're starting with a non-basic plan
        shouldInitAddAgents =
            initialStep
                == AddAgentsStep
                && (planSelectionModel.selectedPlan |> Maybe.map (\p -> p /= "basic") |> Maybe.withDefault False)

        ( addAgentsModel, addAgentsCmd ) =
            if shouldInitAddAgents then
                let
                    ( model, cmd ) =
                        AddAgents.init key orgSlug True
                in
                ( Just model, Cmd.map AddAgentsMsg cmd )

            else
                ( Nothing, Cmd.none )

        -- Initialize enterprise form model if needed
        ( enterpriseFormModel, enterpriseFormCmd ) =
            if initialStep == EnterpriseFormStep then
                let
                    ( model, cmd ) =
                        EnterpriseForm.init key
                in
                ( Just model, Cmd.map EnterpriseFormMsg cmd )

            else
                ( Nothing, Cmd.none )

        -- Determine if we're on a basic plan
        isBasicPlan =
            planSelectionModel.selectedPlan
                |> Maybe.map (\p -> p == "basic")
                |> Maybe.withDefault True

        -- Determine which commands to run based on the current step
        initCmds =
            case initialStep of
                PlanSelectionStep ->
                    [ Cmd.map PlanSelectionMsg planSelectionCmd
                    , retrieveOnboardingState () -- Check for any in-progress onboarding
                    , if not (String.isEmpty orgSlug) then
                        -- When on plan selection, also try to get the plan type directly
                        retrievePlanType orgSlug

                      else
                        Cmd.none
                    ]

                UserDetailsStep ->
                    [ Cmd.map UserDetailsMsg userDetailsCmd
                    , retrieveOnboardingState () -- Check for any in-progress onboarding
                    , if not isNewSignup && not (String.isEmpty orgSlug) then
                        -- When on user details step and not a new signup, fetch user details
                        fetchUserDetails orgSlug ""

                      else
                        Cmd.none
                    ]

                CompanyDetailsStep ->
                    [ Cmd.map CompanyDetailsMsg companyDetailsCmd ]

                LicensingSettingsStep ->
                    [ Cmd.map LicensingSettingsMsg licensingSettingsCmd ]

                AddAgentsStep ->
                    [ addAgentsCmd ]

                PaymentStep ->
                    [ Cmd.map PaymentMsg paymentCmd ]

                EnterpriseFormStep ->
                    [ enterpriseFormCmd ]
    in
    ( { step = initialStep
      , planSelectionModel = planSelectionModel
      , userDetailsModel = userDetailsModel
      , companyDetailsModel = companyDetailsModel
      , licensingSettingsModel = licensingSettingsModel
      , addAgentsModel = addAgentsModel
      , paymentModel = paymentModel
      , enterpriseFormModel = enterpriseFormModel
      , key = key
      , orgSlug = orgSlug
      , session = session
      , isBasicPlan = isBasicPlan
      , error = Nothing
      , isLoading = False

      -- Initialize new fields
      , organizationId = Nothing
      , sessionToken = Nothing
      , onboardingInProgress = False

      -- Initialize step tracking to match current models
      , userDetailsInitialized = initialStep == UserDetailsStep
      , companyDetailsInitialized = initialStep == CompanyDetailsStep
      , licensingSettingsInitialized = initialStep == LicensingSettingsStep
      , addAgentsInitialized = shouldInitAddAgents
      , paymentInitialized = initialStep == PaymentStep
      , enterpriseFormInitialized = initialStep == EnterpriseFormStep

      -- Initialize step history
      , stepHistory = []
      }
    , Cmd.batch initCmds
    )



-- Helper function to update step and maintain history


changeStep : Step -> Model -> Model
changeStep newStep model =
    -- Only add to history if actually changing step
    if newStep /= model.step then
        { model
            | step = newStep
            , stepHistory = model.step :: model.stepHistory

            -- Reset initialization flags based on the new step
            , userDetailsInitialized =
                if newStep == UserDetailsStep then
                    False

                else
                    model.userDetailsInitialized
            , companyDetailsInitialized =
                if newStep == CompanyDetailsStep then
                    False

                else
                    model.companyDetailsInitialized
            , licensingSettingsInitialized =
                if newStep == LicensingSettingsStep then
                    False

                else
                    model.licensingSettingsInitialized
            , addAgentsInitialized =
                if newStep == AddAgentsStep then
                    False

                else
                    model.addAgentsInitialized
            , paymentInitialized =
                if newStep == PaymentStep then
                    False

                else
                    model.paymentInitialized
            , enterpriseFormInitialized =
                if newStep == EnterpriseFormStep then
                    False

                else
                    model.enterpriseFormInitialized
        }

    else
        model



-- UPDATE


type Msg
    = PlanSelectionMsg PlanSelection.Msg
    | UserDetailsMsg UserDetails.Msg
    | CompanyDetailsMsg CompanyDetails.Msg
    | LicensingSettingsMsg LicensingSettings.Msg
    | AddAgentsMsg AddAgents.Msg
    | PaymentMsg Payment.Msg
    | EnterpriseFormMsg EnterpriseForm.Msg
    | NavigateToStep Step
    | NavigateBack
    | SkipStep
    | CompleteOnboarding (Result Http.Error ())
    | GotError String
    | NoOp
      -- New messages for progressive onboarding
    | InitOnboarding String String -- planType, email
    | OnboardingInitialized (Result Http.Error { organizationId : Int, slug : String, sessionToken : String, onboardingStep : Int, planType : String })
    | OnboardingStateReceived (Maybe { organizationId : String, slug : String, sessionToken : String, step : Int, planType : String })
    | UpdateUser
    | UserUpdated (Result Http.Error { onboardingStep : Int })
    | UpdateCompany
    | CompanyUpdated (Result Http.Error { onboardingStep : Int })
    | UpdateLicensing
    | LicensingUpdated (Result Http.Error { onboardingStep : Int, nextStep : Int, isBasicPlan : Bool })
    | AddTeamMembers
    | TeamMembersAdded (Result Http.Error { onboardingStep : Int })
    | CompleteSubscription
    | SubscriptionCompleted (Result Http.Error { clientSecret : String, publishableKey : String })
    | ResumeOnboarding String -- email
    | PlanTypeReceived (Maybe String) -- New message for plan type
    | UserDetailsReceived (Maybe { firstName : String, lastName : String, email : String, phone : String })
    | ScrollToTop
    | RandomOrgNameGenerated String
    | GotUserDetails (Result Http.Error { firstName : String, lastName : String, email : String, phone : String })
    | InitializeCurrentStep
    | CompanyDetailsSaved (Result Http.Error { onboardingStep : Int })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlanSelectionMsg subMsg ->
            let
                ( updatedPlanModel, planCmd, outMsg ) =
                    PlanSelection.update subMsg model.planSelectionModel

                -- Handle the selected plan type to determine if we need to initialize the AddAgents model
                ( isBasicPlan, storePlanCmd ) =
                    case outMsg of
                        PlanSelection.SelectedPlan planType ->
                            -- Only store plan type if we haven't already
                            if model.sessionToken == Nothing then
                                ( planType == "basic"
                                , storePlanType { orgSlug = model.orgSlug, planType = planType }
                                )

                            else
                                ( planType == "basic", Cmd.none )

                        _ ->
                            ( model.isBasicPlan, Cmd.none )

                -- Initialize AddAgents model if needed and not already initialized
                ( addAgentsModel, addAgentsInitialized, addAgentsCmd ) =
                    if not isBasicPlan && model.addAgentsModel == Nothing then
                        let
                            ( newModel, cmd ) =
                                AddAgents.init model.key model.orgSlug True
                        in
                        ( Just newModel, True, Cmd.map AddAgentsMsg cmd )

                    else
                        ( model.addAgentsModel, model.addAgentsInitialized, Cmd.none )

                -- Create navigation command (now just updates the model)
                updatedModelWithStep =
                    case outMsg of
                        PlanSelection.NextStep ->
                            changeStep UserDetailsStep model

                        _ ->
                            model

                -- Process onboarding initialization if needed
                ( updatedModel, onboardingCmd ) =
                    case outMsg of
                        PlanSelection.OnboardingInitializedSuccess response ->
                            -- Only store onboarding state if we haven't already
                            if model.sessionToken == Nothing then
                                let
                                    state =
                                        { organizationId = String.fromInt response.organizationId
                                        , slug = response.slug
                                        , sessionToken = response.sessionToken
                                        , step = response.onboardingStep
                                        , planType = response.planType
                                        }

                                    oldUserDetailsModel =
                                        model.userDetailsModel

                                    updatedUserDetailsModel =
                                        { oldUserDetailsModel | orgSlug = response.slug }

                                    navigationCmd =
                                        Nav.pushUrl model.key "/onboarding/personal"
                                in
                                ( { updatedModelWithStep
                                    | organizationId = Just response.organizationId
                                    , orgSlug = response.slug
                                    , sessionToken = Just response.sessionToken
                                    , onboardingInProgress = True
                                    , step = UserDetailsStep
                                    , userDetailsModel = updatedUserDetailsModel
                                  }
                                , Cmd.batch
                                    [ storeOnboardingState state
                                    , navigationCmd
                                    ]
                                )

                            else
                                ( updatedModelWithStep, Cmd.none )

                        PlanSelection.NextStep ->
                            let
                                navigationCmd =
                                    Nav.pushUrl model.key "/onboarding/personal"
                            in
                            ( updatedModelWithStep
                            , navigationCmd
                            )

                        _ ->
                            ( updatedModelWithStep, Cmd.none )

                -- Handle error messages
                errorCmd =
                    case outMsg of
                        PlanSelection.ShowError err ->
                            Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { updatedModel
                | planSelectionModel = updatedPlanModel
                , isBasicPlan = isBasicPlan
                , addAgentsModel = addAgentsModel
                , addAgentsInitialized = addAgentsInitialized
              }
            , Cmd.batch
                [ Cmd.map PlanSelectionMsg planCmd
                , errorCmd
                , addAgentsCmd
                , onboardingCmd
                , storePlanCmd
                , Task.perform (\_ -> ScrollToTop) (Task.succeed ())
                ]
            )

        UserDetailsMsg subMsg ->
            let
                ( updatedUserDetailsModel, userDetailsCmd, outMsg ) =
                    UserDetails.update subMsg model.userDetailsModel

                -- Update navigation without URL changes
                updatedModelWithStep =
                    case outMsg of
                        UserDetails.NextStep ->
                            let
                                nextStep =
                                    getNextStep UserDetailsStep model.isBasicPlan

                                stepChangedModel =
                                    changeStep nextStep model

                                -- Initialize the next step
                                ( _, initializedModel ) =
                                    initializeStep nextStep stepChangedModel
                            in
                            initializedModel

                        _ ->
                            model

                -- Handle the new slug update
                updatedModelWithSlug =
                    case outMsg of
                        UserDetails.UpdateOrgSlug newSlug ->
                            let
                                -- Update all sub-models that need the slug
                                oldCompanyDetails =
                                    model.companyDetailsModel

                                updatedCompanyDetailsModel =
                                    { oldCompanyDetails | orgSlug = newSlug }

                                oldLicensingSettings =
                                    model.licensingSettingsModel

                                updatedLicensingSettingsModel =
                                    { oldLicensingSettings | orgSlug = newSlug }

                                updatedAddAgentsModel =
                                    model.addAgentsModel
                                        |> Maybe.map (\m -> { m | orgSlug = newSlug })
                            in
                            { updatedModelWithStep
                                | orgSlug = newSlug
                                , companyDetailsModel = updatedCompanyDetailsModel
                                , licensingSettingsModel = updatedLicensingSettingsModel
                                , addAgentsModel = updatedAddAgentsModel
                            }

                        UserDetails.NextStepAndUpdateSlug newSlug ->
                            let
                                -- Update all sub-models that need the slug
                                oldCompanyDetails =
                                    model.companyDetailsModel

                                updatedCompanyDetailsModel =
                                    { oldCompanyDetails | orgSlug = newSlug }

                                oldLicensingSettings =
                                    model.licensingSettingsModel

                                updatedLicensingSettingsModel =
                                    { oldLicensingSettings | orgSlug = newSlug }

                                updatedAddAgentsModel =
                                    model.addAgentsModel
                                        |> Maybe.map (\m -> { m | orgSlug = newSlug })

                                nextStep =
                                    getNextStep UserDetailsStep model.isBasicPlan
                            in
                            { model
                                | orgSlug = newSlug
                                , companyDetailsModel = updatedCompanyDetailsModel
                                , licensingSettingsModel = updatedLicensingSettingsModel
                                , addAgentsModel = updatedAddAgentsModel
                                , step = nextStep
                                , sessionToken = Just updatedUserDetailsModel.sessionToken
                            }

                        _ ->
                            updatedModelWithStep

                outCmd =
                    case outMsg of
                        UserDetails.NoOutMsg ->
                            Cmd.none

                        UserDetails.NextStep ->
                            case model.sessionToken of
                                Just token ->
                                    updateUserDetails updatedUserDetailsModel.orgSlug token updatedUserDetailsModel

                                Nothing ->
                                    -- Generate a random org name when creating a new organization
                                    Random.generate RandomOrgNameGenerated generateOrgName

                        UserDetails.ShowError err ->
                            Cmd.none

                        UserDetails.SaveUserToCookie userData ->
                            -- Store user details in localStorage
                            case model.organizationId of
                                Just orgId ->
                                    storeUserDetails
                                        { organizationId = String.fromInt orgId
                                        , firstName = userData.firstName
                                        , lastName = userData.lastName
                                        , email = userData.email
                                        , phone = userData.phone
                                        }

                                Nothing ->
                                    -- If no organization ID, try to use slug
                                    if not (String.isEmpty model.orgSlug) then
                                        storeUserDetails
                                            { organizationId = model.orgSlug
                                            , firstName = userData.firstName
                                            , lastName = userData.lastName
                                            , email = userData.email
                                            , phone = userData.phone
                                            }

                                    else
                                        Cmd.none

                        UserDetails.UpdateOrgSlug _ ->
                            Cmd.none

                        UserDetails.NextStepAndUpdateSlug _ ->
                            case model.sessionToken of
                                Just token ->
                                    updateUserDetails updatedUserDetailsModel.orgSlug token updatedUserDetailsModel

                                Nothing ->
                                    -- Generate a random org name when creating a new organization
                                    Random.generate RandomOrgNameGenerated generateOrgName
            in
            ( { updatedModelWithSlug
                | userDetailsModel = updatedUserDetailsModel
                , userDetailsInitialized = True
                , sessionToken = Just updatedUserDetailsModel.sessionToken
              }
            , Cmd.batch
                [ Cmd.map UserDetailsMsg userDetailsCmd
                , outCmd
                ]
            )

        CompanyDetailsMsg subMsg ->
            let
                ( updatedCompanyDetailsModel, companyDetailsCmd, outMsg ) =
                    CompanyDetails.update subMsg model.companyDetailsModel

                -- Update navigation without URL changes
                updatedModelWithStep =
                    case outMsg of
                        CompanyDetails.NextStep ->
                            let
                                nextStep =
                                    getNextStep CompanyDetailsStep model.isBasicPlan

                                -- Add navigation command
                                navigationCmd =
                                    Nav.pushUrl model.key (getStepFragment nextStep)

                                stepChangedModel =
                                    changeStep nextStep model
                            in
                            stepChangedModel

                        _ ->
                            model

                -- When NextStep is triggered, also update company details and navigate
                outCmd =
                    case outMsg of
                        CompanyDetails.NoOutMsg ->
                            Cmd.none

                        CompanyDetails.NextStep ->
                            let
                                nextStep =
                                    getNextStep CompanyDetailsStep model.isBasicPlan

                                -- Add URL navigation command
                                navCmd =
                                    Nav.pushUrl model.key (getStepFragment nextStep)

                                updateCmd =
                                    case model.sessionToken of
                                        Just token ->
                                            updateCompanyDetails model.orgSlug token updatedCompanyDetailsModel

                                        Nothing ->
                                            Cmd.none
                            in
                            Cmd.batch [ updateCmd, navCmd ]

                        CompanyDetails.ShowError err ->
                            Cmd.none

                -- Always ensure the company details model has the latest orgSlug
                updatedCompanyDetailsModelWithSlug =
                    { updatedCompanyDetailsModel | orgSlug = model.orgSlug }
            in
            ( { updatedModelWithStep
                | companyDetailsModel = updatedCompanyDetailsModelWithSlug
              }
            , Cmd.batch
                [ Cmd.map CompanyDetailsMsg companyDetailsCmd
                , outCmd
                ]
            )

        CompanyDetailsSaved result ->
            case result of
                Ok response ->
                    let
                        nextStep =
                            getNextStep CompanyDetailsStep model.isBasicPlan

                        -- Add URL navigation command
                        navCmd =
                            Nav.pushUrl model.key (getStepFragment nextStep)
                    in
                    ( { model | isLoading = False, companyDetailsInitialized = True }
                    , navCmd
                    )

                Err error ->
                    ( { model | error = Just "Failed to save company details. Please try again." }
                    , Cmd.none
                    )

        LicensingSettingsMsg subMsg ->
            let
                ( updatedLicensingSettingsModel, licensingSettingsCmd, outMsg ) =
                    LicensingSettings.update subMsg model.licensingSettingsModel

                -- Ensure addAgentsModel is initialized if we're on a non-basic plan
                ( addAgentsModel, addAgentsInitialized, addAgentsCmd ) =
                    if not model.isBasicPlan && model.addAgentsModel == Nothing then
                        let
                            ( newModel, cmd ) =
                                AddAgents.init model.key model.orgSlug True
                        in
                        ( Just newModel, True, Cmd.map AddAgentsMsg cmd )

                    else
                        ( model.addAgentsModel, model.addAgentsInitialized, Cmd.none )

                -- Update navigation without URL changes
                updatedModelWithStep =
                    case outMsg of
                        LicensingSettings.NextStep ->
                            let
                                nextStep =
                                    getNextStep LicensingSettingsStep model.isBasicPlan

                                stepChangedModel =
                                    changeStep nextStep model

                                -- Initialize the next step
                                ( _, initializedModel ) =
                                    initializeStep nextStep stepChangedModel
                            in
                            initializedModel

                        _ ->
                            model

                -- When NextStep is triggered, also update licensing settings
                outCmd =
                    case outMsg of
                        LicensingSettings.NoOutMsg ->
                            Cmd.none

                        LicensingSettings.NextStep ->
                            case model.sessionToken of
                                Just token ->
                                    updateLicensingDetails model.orgSlug token updatedLicensingSettingsModel

                                Nothing ->
                                    Cmd.none

                        LicensingSettings.ShowError err ->
                            Cmd.none
            in
            ( { updatedModelWithStep
                | licensingSettingsModel = updatedLicensingSettingsModel
                , addAgentsModel = addAgentsModel
                , addAgentsInitialized = addAgentsInitialized
              }
            , Cmd.batch
                [ Cmd.map LicensingSettingsMsg licensingSettingsCmd
                , outCmd
                , addAgentsCmd
                ]
            )

        AddAgentsMsg subMsg ->
            case model.addAgentsModel of
                Just addAgentsModel ->
                    let
                        ( updatedAddAgentsModel, addAgentsCmd, outMsg ) =
                            AddAgents.update subMsg addAgentsModel

                        -- Update navigation without URL changes
                        updatedModelWithStep =
                            case outMsg of
                                AddAgents.NextStep ->
                                    let
                                        nextStep =
                                            getNextStep AddAgentsStep model.isBasicPlan
                                    in
                                    changeStep nextStep model

                                _ ->
                                    model

                        outCmd =
                            case outMsg of
                                AddAgents.NoOutMsg ->
                                    Cmd.none

                                AddAgents.NextStep ->
                                    Task.perform (\_ -> ScrollToTop) (Task.succeed ())

                                AddAgents.ShowError err ->
                                    Cmd.none
                    in
                    ( { updatedModelWithStep
                        | addAgentsModel = Just updatedAddAgentsModel
                        , addAgentsInitialized = True
                      }
                    , Cmd.batch
                        [ Cmd.map AddAgentsMsg addAgentsCmd
                        , outCmd
                        ]
                    )

                Nothing ->
                    -- Initialize the model if it doesn't exist
                    let
                        ( newAddAgentsModel, addAgentsCmd ) =
                            AddAgents.init model.key model.orgSlug True
                    in
                    ( { model
                        | addAgentsModel = Just newAddAgentsModel
                        , addAgentsInitialized = True
                      }
                    , Cmd.map AddAgentsMsg addAgentsCmd
                    )

        PaymentMsg subMsg ->
            let
                ( updatedPaymentModel, paymentCmd, outMsg ) =
                    Payment.update subMsg model.paymentModel

                outCmd =
                    case outMsg of
                        Payment.NoOutMsg ->
                            Cmd.none

                        Payment.Completed ->
                            -- Only complete onboarding if we're at the final step
                            if model.step == PaymentStep then
                                -- First update company details if needed
                                if not model.companyDetailsInitialized then
                                    case model.sessionToken of
                                        Just token ->
                                            updateCompanyDetails model.orgSlug token model.companyDetailsModel

                                        Nothing ->
                                            Cmd.none
                                    -- Then update licensing settings if needed

                                else if not model.licensingSettingsInitialized then
                                    case model.sessionToken of
                                        Just token ->
                                            updateLicensingDetails model.orgSlug token model.licensingSettingsModel

                                        Nothing ->
                                            Cmd.none
                                    -- Then add team members if needed (for non-basic plans)

                                else if not model.isBasicPlan && not model.addAgentsInitialized then
                                    case ( model.sessionToken, model.addAgentsModel ) of
                                        ( Just token, Just agentsModel ) ->
                                            addTeamMembers model.orgSlug token agentsModel

                                        _ ->
                                            Cmd.none
                                    -- Finally complete the onboarding

                                else
                                    completeOnboarding model

                            else
                                Cmd.none

                        Payment.NavigateToWalkthrough ->
                            -- Only complete onboarding if we're at the final step
                            if model.step == PaymentStep then
                                -- Same flow as Completed
                                if not model.companyDetailsInitialized then
                                    case model.sessionToken of
                                        Just token ->
                                            updateCompanyDetails model.orgSlug token model.companyDetailsModel

                                        Nothing ->
                                            Cmd.none

                                else if not model.licensingSettingsInitialized then
                                    case model.sessionToken of
                                        Just token ->
                                            updateLicensingDetails model.orgSlug token model.licensingSettingsModel

                                        Nothing ->
                                            Cmd.none

                                else if not model.isBasicPlan && not model.addAgentsInitialized then
                                    case ( model.sessionToken, model.addAgentsModel ) of
                                        ( Just token, Just agentsModel ) ->
                                            addTeamMembers model.orgSlug token agentsModel

                                        _ ->
                                            Cmd.none

                                else
                                    completeOnboarding model

                            else
                                Cmd.none

                        Payment.ShowError err ->
                            Cmd.none
            in
            ( { model
                | paymentModel = updatedPaymentModel
                , paymentInitialized = True
              }
            , Cmd.batch
                [ Cmd.map PaymentMsg paymentCmd
                , outCmd
                ]
            )

        EnterpriseFormMsg subMsg ->
            case model.enterpriseFormModel of
                Just enterpriseFormModel ->
                    let
                        ( updatedEnterpriseFormModel, enterpriseFormCmd, outMsg ) =
                            EnterpriseForm.update subMsg enterpriseFormModel

                        -- Check if we're navigating back to plan selection
                        updatedModelWithStep =
                            if outMsg == EnterpriseForm.BackToPlanSelection then
                                changeStep PlanSelectionStep model

                            else
                                model

                        outCmd =
                            case outMsg of
                                EnterpriseForm.NoOutMsg ->
                                    Cmd.none

                                EnterpriseForm.BackToPlanSelection ->
                                    Task.perform (\_ -> ScrollToTop) (Task.succeed ())

                                EnterpriseForm.ShowError err ->
                                    Cmd.none
                    in
                    ( { updatedModelWithStep
                        | enterpriseFormModel = Just updatedEnterpriseFormModel
                        , enterpriseFormInitialized = True
                      }
                    , Cmd.batch
                        [ Cmd.map EnterpriseFormMsg enterpriseFormCmd
                        , outCmd
                        ]
                    )

                Nothing ->
                    -- If the model doesn't exist, create it
                    let
                        ( enterpriseFormModel, enterpriseFormCmd ) =
                            EnterpriseForm.init model.key
                    in
                    ( { model
                        | enterpriseFormModel = Just enterpriseFormModel
                        , enterpriseFormInitialized = True
                      }
                    , Cmd.map EnterpriseFormMsg enterpriseFormCmd
                    )

        NavigateToStep step ->
            let
                updatedModel =
                    changeStep step model

                -- Update URL fragment without page reload to preserve state in browser history
                urlUpdateCmd =
                    Nav.pushUrl model.key (getStepFragment step)

                -- Initialize the step directly
                ( initCmd, finalModel ) =
                    initializeStep step updatedModel
            in
            ( finalModel
            , Cmd.batch
                [ urlUpdateCmd
                , initCmd
                , Task.perform (\_ -> ScrollToTop) (Task.succeed ())
                ]
            )

        NavigateBack ->
            case model.stepHistory of
                previousStep :: restHistory ->
                    let
                        updatedModel =
                            { model
                                | step = previousStep
                                , stepHistory = restHistory
                            }

                        -- Initialize the step directly
                        ( initCmd, finalModel ) =
                            initializeStep previousStep updatedModel
                    in
                    ( finalModel
                    , Cmd.batch
                        [ Nav.pushUrl model.key (getStepFragment previousStep)
                        , initCmd
                        , Task.perform (\_ -> ScrollToTop) (Task.succeed ())
                        ]
                    )

                [] ->
                    -- If no history, stay on current step
                    ( model, Cmd.none )

        SkipStep ->
            let
                nextStep =
                    getNextStep model.step model.isBasicPlan

                updatedModel =
                    changeStep nextStep model

                -- Initialize the step directly
                ( initCmd, finalModel ) =
                    initializeStep nextStep updatedModel
            in
            ( finalModel
            , Cmd.batch
                [ Nav.pushUrl model.key (getStepFragment nextStep)
                , initCmd
                , Task.perform (\_ -> ScrollToTop) (Task.succeed ())
                ]
            )

        ScrollToTop ->
            ( model, Dom.setViewport 0 0 |> Task.attempt (\_ -> NoOp) )

        CompleteOnboarding result ->
            case result of
                Ok _ ->
                    -- Redirect directly to walkthrough, bypassing login
                    ( { model | isLoading = False }
                    , Nav.load "/walkthrough"
                    )

                Err _ ->
                    ( { model | isLoading = False, error = Just "Failed to complete onboarding. Please try again." }
                    , Cmd.none
                    )

        GotError errorMsg ->
            ( { model | error = Just errorMsg, isLoading = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        -- New messages for progressive onboarding
        InitOnboarding planType email ->
            ( { model | isLoading = True }
            , initializeOnboarding planType email model.orgSlug
            )

        OnboardingInitialized result ->
            case result of
                Ok response ->
                    -- Store state in localStorage
                    let
                        state =
                            { organizationId = String.fromInt response.organizationId
                            , slug = response.slug
                            , sessionToken = response.sessionToken
                            , step = response.onboardingStep
                            , planType = response.planType
                            }

                        -- Reinitialize the user details model with the proper org slug
                        ( newUserDetailsModel, userDetailsCmd ) =
                            UserDetails.init model.key response.slug True

                        -- Update plan selection model with the plan type
                        oldPlanSelectionModel =
                            model.planSelectionModel

                        newPlanSelectionModel =
                            { oldPlanSelectionModel | selectedPlan = Just response.planType }
                    in
                    ( { model
                        | isLoading = False
                        , organizationId = Just response.organizationId
                        , orgSlug = response.slug
                        , sessionToken = Just response.sessionToken
                        , onboardingInProgress = True
                        , step = UserDetailsStep
                        , userDetailsModel = newUserDetailsModel
                        , planSelectionModel = newPlanSelectionModel
                      }
                    , Cmd.batch
                        [ storeOnboardingState state
                        , Nav.pushUrl model.key "/onboarding/personal"
                        , Cmd.map UserDetailsMsg userDetailsCmd
                        ]
                    )

                Err error ->
                    ( { model
                        | isLoading = False
                        , error = Just "Failed to initialize onboarding. Please try again."
                      }
                    , Cmd.none
                    )

        OnboardingStateReceived maybeState ->
            case maybeState of
                Just state ->
                    -- If we have a stored state, resume onboarding
                    let
                        step =
                            case state.step of
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

                                _ ->
                                    PlanSelectionStep

                        organizationId =
                            String.toInt state.organizationId

                        -- Use the planType from the state
                        planType =
                            state.planType

                        -- Extract plan type from session - in a real app this should be retrieved from API
                        -- For now we'll load it from cookie/session storage via a port
                        fetchPlanTypeCmd =
                            if not (String.isEmpty state.slug) && not (String.isEmpty state.sessionToken) then
                                -- In a real implementation, issue an API request to get the plan
                                -- For now, trigger the port to get the plan type from localStorage
                                retrievePlanType state.slug

                            else
                                Cmd.none

                        -- Update the plan selection model with the saved plan
                        updatedPlanSelectionModel =
                            let
                                currentModel =
                                    model.planSelectionModel

                                -- Only update if we don't already have a selection
                                planSelectionWithSession =
                                    if currentModel.selectedPlan == Nothing then
                                        -- Set the selected plan and mark as loaded from session
                                        { currentModel
                                            | selectedPlan = Just planType
                                            , loadedFromSession = True
                                            , orgSlug = state.slug
                                        }

                                    else
                                        currentModel
                            in
                            planSelectionWithSession

                        oldUserDetailsModel =
                            model.userDetailsModel

                        updatedUserDetailsModel =
                            { oldUserDetailsModel | orgSlug = state.slug }

                        updatedModel =
                            { model
                                | orgSlug = state.slug
                                , sessionToken = Just state.sessionToken
                                , organizationId = organizationId
                                , onboardingInProgress = True
                                , step = step
                                , planSelectionModel = updatedPlanSelectionModel
                                , isBasicPlan = planType == "basic"
                                , userDetailsModel = updatedUserDetailsModel
                            }

                        -- Command to navigate to the appropriate step
                        navigateCmd =
                            Nav.pushUrl model.key (getStepFragment step)

                        -- If we're on the plan selection step, send the plan type to PlanSelection
                        planSelectionCmd =
                            if step == PlanSelectionStep then
                                -- Send command to load the plan from session if we have one
                                case updatedPlanSelectionModel.selectedPlan of
                                    Just plan ->
                                        Cmd.map PlanSelectionMsg (Task.perform (\_ -> PlanSelection.LoadPlanFromSession plan) (Task.succeed ()))

                                    Nothing ->
                                        Cmd.none

                            else
                                Cmd.none
                    in
                    ( updatedModel
                    , Cmd.batch
                        [ navigateCmd
                        , fetchPlanTypeCmd
                        , planSelectionCmd
                        ]
                    )

                Nothing ->
                    -- No stored state, continue as normal
                    ( model, Cmd.none )

        UpdateUser ->
            case model.sessionToken of
                Just token ->
                    ( { model | isLoading = True }
                    , updateUserDetails model.orgSlug token model.userDetailsModel
                    )

                Nothing ->
                    ( { model | error = Just "Session information is missing. Please try again." }
                    , Cmd.none
                    )

        UserUpdated result ->
            case result of
                Ok response ->
                    ( { model
                        | isLoading = False
                        , step = CompanyDetailsStep
                      }
                    , Nav.pushUrl model.key "/onboarding/company"
                    )

                Err error ->
                    ( { model
                        | isLoading = False
                        , error = Just "Failed to update user details. Please try again."
                      }
                    , Cmd.none
                    )

        UpdateCompany ->
            case model.sessionToken of
                Just token ->
                    ( { model | isLoading = True }
                    , updateCompanyDetails model.orgSlug token model.companyDetailsModel
                    )

                Nothing ->
                    ( { model | error = Just "Session information is missing. Please try again." }
                    , Cmd.none
                    )

        CompanyUpdated result ->
            case result of
                Ok response ->
                    -- After company details are updated, update licensing settings
                    let
                        nextCmd =
                            case model.sessionToken of
                                Just token ->
                                    updateLicensingDetails model.orgSlug token model.licensingSettingsModel

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model
                        | isLoading = False
                        , companyDetailsInitialized = True
                      }
                    , nextCmd
                    )

                Err error ->
                    ( { model
                        | isLoading = False
                        , error = Just "Failed to update company details. Please try again."
                      }
                    , Cmd.none
                    )

        UpdateLicensing ->
            case model.sessionToken of
                Just token ->
                    ( { model | isLoading = True }
                    , updateLicensingDetails model.orgSlug token model.licensingSettingsModel
                    )

                Nothing ->
                    ( { model | error = Just "Session information is missing. Please try again." }
                    , Cmd.none
                    )

        LicensingUpdated result ->
            case result of
                Ok response ->
                    -- After licensing settings are updated, navigate to the next step
                    -- Only navigate if we have actual data (carrier contracts or useSmartSendForGI is true)
                    let
                        hasLicensingData =
                            not (List.isEmpty model.licensingSettingsModel.carrierContracts) || model.licensingSettingsModel.useSmartSendForGI

                        nextStep =
                            if not response.isBasicPlan then
                                AddAgentsStep

                            else
                                PaymentStep

                        -- Only navigate if we have actual licensing data
                        navCmd =
                            if hasLicensingData then
                                Nav.pushUrl model.key (getStepFragment nextStep)

                            else
                                Cmd.none
                    in
                    ( { model
                        | isLoading = False
                        , licensingSettingsInitialized = True
                        , isBasicPlan = response.isBasicPlan
                        , step =
                            if hasLicensingData then
                                nextStep

                            else
                                model.step
                      }
                    , navCmd
                    )

                Err error ->
                    ( { model
                        | isLoading = False
                        , error = Just "Failed to update licensing details. Please try again."
                      }
                    , Cmd.none
                    )

        AddTeamMembers ->
            case model.sessionToken of
                Just token ->
                    case model.addAgentsModel of
                        Just agentsModel ->
                            ( { model | isLoading = True }
                            , addTeamMembers model.orgSlug token agentsModel
                            )

                        Nothing ->
                            ( { model | error = Just "Team members information is missing. Please try again." }
                            , Cmd.none
                            )

                Nothing ->
                    ( { model | error = Just "Session information is missing. Please try again." }
                    , Cmd.none
                    )

        TeamMembersAdded result ->
            case result of
                Ok response ->
                    -- After team members are added, navigate to payment step
                    let
                        nextStep =
                            PaymentStep

                        navCmd =
                            Nav.pushUrl model.key (getStepFragment nextStep)
                    in
                    ( { model
                        | isLoading = False
                        , addAgentsInitialized = True
                        , step = nextStep
                      }
                    , navCmd
                    )

                Err error ->
                    ( { model
                        | isLoading = False
                        , error = Just "Failed to add team members. Please try again."
                      }
                    , Cmd.none
                    )

        CompleteSubscription ->
            case model.sessionToken of
                Just token ->
                    let
                        selectedPlan =
                            model.planSelectionModel.selectedPlan |> Maybe.withDefault "basic"

                        extraAgents =
                            model.planSelectionModel.extraAgents

                        extraContacts =
                            model.planSelectionModel.extraContacts
                    in
                    ( { model | isLoading = True }
                    , completeSubscription model.orgSlug token selectedPlan extraAgents extraContacts
                    )

                Nothing ->
                    ( { model | error = Just "Session information is missing. Please try again." }
                    , Cmd.none
                    )

        SubscriptionCompleted result ->
            case result of
                Ok response ->
                    -- Clear onboarding state from localStorage and redirect to success page
                    ( { model | isLoading = False }
                    , Cmd.batch
                        [ Nav.pushUrl model.key ("/login?onboarding=completed&email=" ++ Url.percentEncode model.userDetailsModel.email)
                        ]
                    )

                -- don't remove the parens here
                Err error ->
                    ( { model
                        | isLoading = False
                        , error = Just "Failed to complete subscription. Please try again."
                      }
                    , Cmd.none
                    )

        ResumeOnboarding email ->
            ( { model | isLoading = True }
            , resumeOnboarding email
            )

        PlanTypeReceived maybePlanType ->
            let
                planType =
                    Maybe.withDefault "basic" maybePlanType

                updatedPlanSelectionModel =
                    let
                        currentModel =
                            model.planSelectionModel
                    in
                    { currentModel
                        | selectedPlan = Just planType
                        , loadedFromSession = True
                    }

                -- Update the model with the plan type
                updatedModel =
                    { model
                        | planSelectionModel = updatedPlanSelectionModel
                        , isBasicPlan = planType == "basic"
                    }

                -- Command to update the PlanSelection model with the received plan
                planSelectionCmd =
                    if model.step == PlanSelectionStep then
                        -- Pass the plan type to PlanSelection module
                        Cmd.map PlanSelectionMsg (Task.perform (\_ -> PlanSelection.LoadPlanFromSession planType) (Task.succeed ()))

                    else
                        Cmd.none
            in
            ( updatedModel, planSelectionCmd )

        UserDetailsReceived maybeUserData ->
            case maybeUserData of
                Just userData ->
                    ( model
                    , Cmd.map UserDetailsMsg (Task.perform (\_ -> UserDetails.loadUserFromSession userData) (Task.succeed ()))
                    )

                -- don't remove this paren
                -- don't remove the parens here
                Nothing ->
                    ( model, Cmd.none )

        RandomOrgNameGenerated orgName ->
            -- When we get a random org name, update both the org slug and the CompanyDetails model
            let
                oldCompanyDetailsModel =
                    model.companyDetailsModel

                updatedCompanyDetailsModel =
                    { oldCompanyDetailsModel | orgSlug = orgName }

                oldUserDetailsModel =
                    model.userDetailsModel

                updatedUserDetailsModel =
                    { oldUserDetailsModel | orgSlug = orgName }
            in
            ( { model
                | companyDetailsModel = updatedCompanyDetailsModel
                , userDetailsModel = updatedUserDetailsModel
                , orgSlug = orgName -- Update the orgSlug in the main model
              }
            , Cmd.none
            )

        GotUserDetails result ->
            case result of
                Ok userDetails ->
                    let
                        oldUserDetailsModel =
                            model.userDetailsModel

                        updatedUserDetailsModel =
                            { oldUserDetailsModel
                                | firstName = userDetails.firstName
                                , lastName = userDetails.lastName
                                , email = userDetails.email
                                , phone = userDetails.phone
                                , emailStatus = UserDetails.Available -- Consider email as valid since it's already registered
                            }
                    in
                    ( { model | userDetailsModel = updatedUserDetailsModel }, Cmd.none )

                Err error ->
                    ( { model | error = Just ("Failed to load user details: " ++ httpErrorToString error) }, Cmd.none )

        InitializeCurrentStep ->
            -- Initialize the current step based on the model's step field
            let
                ( cmd, updatedModel ) =
                    initializeStep model.step model
            in
            ( updatedModel, cmd )



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
                    [ SetupLayout.view
                        (mapStepToSetupStep model.step)
                        model.isBasicPlan
                        (getStepNumber model.step)
                        [ div [ class "max-w-3xl mx-auto" ]
                            [ viewCurrentStep model
                            , viewNavigationControls model
                            ]
                        ]
                    ]
            ]
        ]
    }


viewCurrentStep : Model -> Html Msg
viewCurrentStep model =
    if model.step == PlanSelectionStep && not model.onboardingInProgress then
        -- On the first step, show the plan selection
        Html.map PlanSelectionMsg (PlanSelection.view model.planSelectionModel)

    else if model.step == PlanSelectionStep && model.onboardingInProgress then
        -- If we have an in-progress onboarding, show the resume view
        viewResumeOnboarding model

    else
        case model.step of
            PlanSelectionStep ->
                -- This case is handled above
                text ""

            UserDetailsStep ->
                Html.map UserDetailsMsg (UserDetails.view model.userDetailsModel)

            CompanyDetailsStep ->
                Html.map CompanyDetailsMsg (CompanyDetails.view model.companyDetailsModel)

            LicensingSettingsStep ->
                Html.map LicensingSettingsMsg (LicensingSettings.view model.licensingSettingsModel)

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
                    -- don't add another bracket here

                else
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

            -- don't add another bracket here
            PaymentStep ->
                Html.map PaymentMsg (Payment.view model.paymentModel)

            EnterpriseFormStep ->
                case model.enterpriseFormModel of
                    Just enterpriseFormModel ->
                        Html.map EnterpriseFormMsg (EnterpriseForm.view enterpriseFormModel)

                    Nothing ->
                        div [ class "text-center p-8" ]
                            [ text "Error: Enterprise Form model not initialized." ]



-- Add navigation controls to every step


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
        , div [] [] -- Removed "Skip to X" link
        ]



-- UTILS


getStepTitle : Step -> String
getStepTitle step =
    case step of
        PlanSelectionStep ->
            "Choose Your Plan - Onboarding"

        UserDetailsStep ->
            "Personal Details - Onboarding"

        CompanyDetailsStep ->
            "Company Details - Onboarding"

        LicensingSettingsStep ->
            "Licensing & Carriers - Onboarding"

        AddAgentsStep ->
            "Add Team Members - Onboarding"

        PaymentStep ->
            "Payment - Onboarding"

        EnterpriseFormStep ->
            "Enterprise Form - Onboarding"


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



-- Define the Agent type locally


type alias Agent =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    , isAdmin : Bool
    , isAgent : Bool
    }



-- Helper function to encode agent data


encodeAgent : Agent -> Encode.Value
encodeAgent agent =
    Encode.object
        [ ( "id", Encode.string agent.id )
        , ( "firstName", Encode.string agent.firstName )
        , ( "lastName", Encode.string agent.lastName )
        , ( "email", Encode.string agent.email )
        , ( "phone", Encode.string agent.phone )
        , ( "isAdmin", Encode.bool agent.isAdmin )
        , ( "isAgent", Encode.bool agent.isAgent )
        ]



-- Update agents list handling to convert AddAgents.Model.agents to our local Agent type


encodeOnboardingData : Model -> Encode.Value
encodeOnboardingData model =
    let
        selectedPlan =
            model.planSelectionModel.selectedPlan |> Maybe.withDefault "basic"

        extraAgents =
            model.planSelectionModel.extraAgents

        extraContacts =
            model.planSelectionModel.extraContacts

        -- User details
        firstName =
            model.userDetailsModel.firstName

        lastName =
            model.userDetailsModel.lastName

        email =
            model.userDetailsModel.email

        phone =
            model.userDetailsModel.phone

        -- Company details
        agencyName =
            model.companyDetailsModel.agencyName

        website =
            model.companyDetailsModel.website

        companyPhone =
            model.companyDetailsModel.phone

        primaryColor =
            model.companyDetailsModel.primaryColor

        secondaryColor =
            model.companyDetailsModel.secondaryColor

        logo =
            model.companyDetailsModel.logo

        -- Licensing settings
        carrierContracts =
            model.licensingSettingsModel.carrierContracts

        stateCarrierSettings =
            -- Create state carrier settings based on whether SmartSend for GI is enabled
            List.map
                (\carrier ->
                    { carrier = carrier
                    , active = True
                    , targetGI = model.licensingSettingsModel.useSmartSendForGI
                    }
                )
                carrierContracts

        -- Agents (if applicable)
        agents =
            case model.addAgentsModel of
                Just addAgentsModel ->
                    -- Use the agents from the model (already correct type)
                    Encode.list encodeAgent addAgentsModel.agents

                Nothing ->
                    Encode.list identity []
    in
    Encode.object
        [ ( "plan"
          , Encode.object
                [ ( "type", Encode.string selectedPlan )
                , ( "extraAgents", Encode.int extraAgents )
                , ( "extraContacts", Encode.int extraContacts )
                , ( "price", Encode.int (getPlanPrice selectedPlan) )
                , ( "billingCycle", Encode.string "monthly" )
                ]
          )
        , ( "user"
          , Encode.object
                [ ( "firstName", Encode.string firstName )
                , ( "lastName", Encode.string lastName )
                , ( "email", Encode.string email )
                , ( "phone", Encode.string phone )
                ]
          )
        , ( "company"
          , Encode.object
                [ ( "agencyName", Encode.string agencyName )
                , ( "website", Encode.string website )
                , ( "phone", Encode.string companyPhone )
                , ( "primaryColor", Encode.string primaryColor )
                , ( "secondaryColor", Encode.string secondaryColor )
                , ( "logo", Maybe.withDefault Encode.null (Maybe.map Encode.string logo) )
                ]
          )
        , ( "licensing"
          , Encode.object
                [ ( "carrierContracts", Encode.list Encode.string carrierContracts )
                , ( "useSmartSendForGI", Encode.bool model.licensingSettingsModel.useSmartSendForGI )
                ]
          )
        , ( "agents", agents )
        ]



-- Update the completeOnboarding function to submit all data at once


completeOnboarding : Model -> Cmd Msg
completeOnboarding model =
    case model.sessionToken of
        Just token ->
            Http.post
                { url = "/api/onboarding/complete"
                , body = Http.emptyBody
                , expect = Http.expectWhatever CompleteOnboarding
                }

        Nothing ->
            -- If we don't have a session token, something went wrong
            Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map PlanSelectionMsg (PlanSelection.subscriptions model.planSelectionModel)
        , Sub.map UserDetailsMsg (UserDetails.subscriptions model.userDetailsModel)
        , Sub.map CompanyDetailsMsg (CompanyDetails.subscriptions model.companyDetailsModel)
        , Sub.map LicensingSettingsMsg (LicensingSettings.subscriptions model.licensingSettingsModel)
        , case model.addAgentsModel of
            Just addAgentsModel ->
                Sub.map AddAgentsMsg (AddAgents.subscriptions addAgentsModel)

            Nothing ->
                Sub.none
        , Sub.map PaymentMsg (Payment.subscriptions model.paymentModel)
        , case model.enterpriseFormModel of
            Just enterpriseFormModel ->
                Sub.map EnterpriseFormMsg (EnterpriseForm.subscriptions enterpriseFormModel)

            Nothing ->
                Sub.none
        , onboardingStateReceived OnboardingStateReceived
        , planTypeReceived PlanTypeReceived
        , userDetailsReceived UserDetailsReceived
        ]



-- Define StateCarrierSetting type locally


type alias StateCarrierSetting =
    { state : String
    , carrier : String
    , active : Bool
    , targetGI : Bool
    }



-- Encode StateCarrierSetting


encodeStateCarrierSetting : StateCarrierSetting -> Encode.Value
encodeStateCarrierSetting setting =
    Encode.object
        [ ( "state", Encode.string setting.state )
        , ( "carrier", Encode.string setting.carrier )
        , ( "active", Encode.bool setting.active )
        , ( "targetGI", Encode.bool setting.targetGI )
        ]



-- Helper function to get URL for a step (full URL instead of just fragment)


getStepFragment : Step -> String
getStepFragment step =
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



-- Helper function to get readable label for a step


getStepLabel : Step -> String
getStepLabel step =
    case step of
        PlanSelectionStep ->
            "Plan Selection"

        UserDetailsStep ->
            "Personal Details"

        CompanyDetailsStep ->
            "Company Details"

        LicensingSettingsStep ->
            "Licensing Settings"

        AddAgentsStep ->
            "Add Agents"

        PaymentStep ->
            "Payment"

        EnterpriseFormStep ->
            "Enterprise Form"



-- Helper function to convert the step to a number for progress tracking


getStepNumber : Step -> Int
getStepNumber step =
    case step of
        PlanSelectionStep ->
            0

        UserDetailsStep ->
            1

        CompanyDetailsStep ->
            2

        LicensingSettingsStep ->
            3

        AddAgentsStep ->
            4

        PaymentStep ->
            5

        EnterpriseFormStep ->
            6



-- Helper function to get the next step in sequence


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
                PaymentStep

            else
                AddAgentsStep

        AddAgentsStep ->
            PaymentStep

        PaymentStep ->
            PaymentStep

        -- No next step after payment
        EnterpriseFormStep ->
            EnterpriseFormStep



-- Can't proceed from enterprise form
-- Helper function to get plan price based on type


getPlanPrice : String -> Int
getPlanPrice planType =
    case planType of
        "basic" ->
            29

        "pro" ->
            99

        "enterprise" ->
            499

        _ ->
            29



-- Default to basic price
-- HTTP functions for progressive onboarding


initializeOnboarding : String -> String -> String -> Cmd Msg
initializeOnboarding planType email orgSlug =
    let
        url =
            "/api/onboarding/initialize"

        body =
            Encode.object
                [ ( "planType", Encode.string planType )
                , ( "email", Encode.string email )
                , ( "organizationName", Encode.string orgSlug )
                ]
                |> Http.jsonBody

        decoder =
            Decode.map5
                (\organizationId slug sessionToken onboardingStep planTypeValue ->
                    { organizationId = organizationId
                    , slug = slug
                    , sessionToken = sessionToken
                    , onboardingStep = onboardingStep
                    , planType = planTypeValue
                    }
                )
                (Decode.field "organizationId" Decode.int)
                (Decode.field "slug" Decode.string)
                (Decode.field "sessionToken" Decode.string)
                (Decode.field "onboardingStep" Decode.int)
                (Decode.field "planType" (Decode.oneOf [ Decode.string, Decode.null "basic" ]))
    in
    Http.post
        { url = url
        , body = body
        , expect = Http.expectJson OnboardingInitialized decoder
        }


resumeOnboarding : String -> Cmd Msg
resumeOnboarding email =
    let
        url =
            "/api/onboarding/resume-onboarding"

        body =
            Encode.object
                [ ( "email", Encode.string email )
                ]
                |> Http.jsonBody

        decoder =
            Decode.map5
                (\organizationId slug sessionToken onboardingStep planTypeValue ->
                    { organizationId = organizationId
                    , slug = slug
                    , sessionToken = sessionToken
                    , onboardingStep = onboardingStep
                    , planType = planTypeValue
                    }
                )
                (Decode.field "organizationId" Decode.int)
                (Decode.field "slug" Decode.string)
                (Decode.field "sessionToken" Decode.string)
                (Decode.field "onboardingStep" Decode.int)
                (Decode.field "planType" (Decode.oneOf [ Decode.string, Decode.null "basic" ]))
    in
    Http.post
        { url = url
        , body = body
        , expect = Http.expectJson OnboardingInitialized decoder
        }


updateUserDetails : String -> String -> UserDetails.Model -> Cmd Msg
updateUserDetails orgSlug sessionToken model =
    let
        url =
            "/api/onboarding/user-details"

        -- Ensure phone number only contains digits
        phoneDigits =
            String.filter Char.isDigit model.phone

        body =
            Encode.object
                [ ( "firstName", Encode.string model.firstName )
                , ( "lastName", Encode.string model.lastName )
                , ( "email", Encode.string model.email )
                , ( "phone", Encode.string phoneDigits )
                ]
                |> Http.jsonBody

        decoder =
            Decode.map
                (\onboardingStep ->
                    { onboardingStep = onboardingStep }
                )
                (Decode.field "onboardingStep" Decode.int)
    in
    Http.request
        { method = "POST"
        , url = url
        , body = body
        , expect = Http.expectJson UserUpdated decoder
        , headers = [] -- The session token is in the cookies, no need to pass it
        , timeout = Nothing
        , tracker = Nothing
        }


updateCompanyDetails : String -> String -> CompanyDetails.Model -> Cmd Msg
updateCompanyDetails orgSlug sessionToken model =
    let
        url =
            "/api/onboarding/company-details"

        body =
            Encode.object
                [ ( "agencyName", Encode.string model.agencyName )
                , ( "website", Encode.string model.website )
                , ( "phone", Encode.string model.phone )
                , ( "primaryColor", Encode.string model.primaryColor )
                , ( "secondaryColor", Encode.string model.secondaryColor )
                , ( "logo", Maybe.map Encode.string model.logo |> Maybe.withDefault Encode.null )
                ]
                |> Http.jsonBody

        decoder =
            Decode.map
                (\onboardingStep ->
                    { onboardingStep = onboardingStep }
                )
                (Decode.field "onboardingStep" Decode.int)
    in
    Http.request
        { method = "POST"
        , url = url
        , body = body
        , expect = Http.expectJson CompanyUpdated decoder
        , headers = [] -- The session token is in the cookies, no need to pass it
        , timeout = Nothing
        , tracker = Nothing
        }


updateLicensingDetails : String -> String -> LicensingSettings.Model -> Cmd Msg
updateLicensingDetails orgSlug sessionToken model =
    let
        url =
            "/api/onboarding/licensing-settings"

        body =
            Encode.object
                [ ( "carrierContracts", Encode.list Encode.string model.carrierContracts )
                , ( "useSmartSendForGI", Encode.bool model.useSmartSendForGI )
                ]
                |> Http.jsonBody

        decoder =
            Decode.map3
                (\onboardingStep nextStep isBasicPlan ->
                    { onboardingStep = onboardingStep
                    , nextStep = nextStep
                    , isBasicPlan = isBasicPlan
                    }
                )
                (Decode.field "onboardingStep" Decode.int)
                (Decode.field "nextStep" Decode.int)
                (Decode.field "isBasicPlan" Decode.bool)
    in
    Http.request
        { method = "POST"
        , url = url
        , body = body
        , expect = Http.expectJson LicensingUpdated decoder
        , headers =
            if String.isEmpty sessionToken then
                []

            else
                [ Http.header "x-onboarding-session" sessionToken ]
        , timeout = Nothing
        , tracker = Nothing
        }


addTeamMembers : String -> String -> AddAgents.Model -> Cmd Msg
addTeamMembers orgSlug sessionToken model =
    let
        url =
            "/api/onboarding/agents"

        encodeTeamMember agent =
            Encode.object
                [ ( "firstName", Encode.string agent.firstName )
                , ( "lastName", Encode.string agent.lastName )
                , ( "email", Encode.string agent.email )
                , ( "phone", Encode.string agent.phone )
                , ( "isAdmin", Encode.bool agent.isAdmin )
                , ( "isAgent", Encode.bool agent.isAgent )
                ]

        body =
            Encode.object
                [ ( "agents", Encode.list encodeTeamMember model.agents )
                ]
                |> Http.jsonBody

        headers =
            [ Http.header "x-onboarding-session" sessionToken ]

        decoder =
            Decode.map
                (\onboardingStep ->
                    { onboardingStep = onboardingStep }
                )
                (Decode.field "onboardingStep" Decode.int)
    in
    Http.request
        { method = "POST"
        , url = url
        , body = body
        , expect = Http.expectJson TeamMembersAdded decoder
        , headers = headers
        , timeout = Nothing
        , tracker = Nothing
        }


completeSubscription : String -> String -> String -> Int -> Int -> Cmd Msg
completeSubscription orgSlug sessionToken selectedPlan extraAgents extraContacts =
    let
        url =
            "/api/onboarding/" ++ orgSlug ++ "/complete"

        body =
            Encode.object
                [ ( "planType", Encode.string selectedPlan )
                , ( "extraAgents", Encode.int extraAgents )
                , ( "extraContacts", Encode.int extraContacts )
                ]
                |> Http.jsonBody

        headers =
            [ Http.header "Authorization" ("Bearer " ++ sessionToken) ]

        decoder =
            Decode.map2
                (\clientSecret publishableKey ->
                    { clientSecret = clientSecret
                    , publishableKey = publishableKey
                    }
                )
                (Decode.field "clientSecret" Decode.string)
                (Decode.field "publishableKey" Decode.string)
    in
    Http.request
        { method = "POST"
        , url = url
        , body = body
        , expect = Http.expectJson SubscriptionCompleted decoder
        , headers = headers
        , timeout = Nothing
        , tracker = Nothing
        }



-- Add a function to handle the email input for resuming onboarding


viewResumeOnboarding : Model -> Html Msg
viewResumeOnboarding model =
    let
        emailValue =
            model.userDetailsModel.email
    in
    div [ class "max-w-md mx-auto mt-10 p-6 bg-white rounded-lg shadow-md" ]
        [ h2 [ class "text-2xl font-bold mb-6 text-center" ] [ text "Resume Your Onboarding" ]
        , p [ class "mb-6 text-gray-600" ]
            [ text "It looks like you've already started the onboarding process. Enter your email to continue where you left off." ]
        , div [ class "mb-4" ]
            [ label [ class "block text-gray-700 text-sm font-bold mb-2", for "email" ] [ text "Email" ]
            , input
                [ class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                , id "email"
                , type_ "email"
                , placeholder "your.email@example.com"
                , value emailValue
                , onInput (\email -> ResumeOnboarding email)
                ]
                []
            ]
        , div [ class "flex items-center justify-between" ]
            [ button
                [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline"
                , onClick (ResumeOnboarding emailValue)
                ]
                [ text "Resume Onboarding" ]
            , button
                [ class "bg-gray-300 hover:bg-gray-400 text-gray-800 font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline"
                , onClick (NavigateToStep PlanSelectionStep)
                ]
                [ text "Start New" ]
            ]
        ]



-- Add this function near the other HTTP functions


fetchUserDetails : String -> String -> Cmd Msg
fetchUserDetails _ _ =
    let
        url =
            "/api/onboarding/user-details"

        decoder =
            Decode.map4
                (\firstName lastName email phone ->
                    { firstName = firstName
                    , lastName = lastName
                    , email = email
                    , phone = phone
                    }
                )
                (Decode.field "firstName" Decode.string)
                (Decode.field "lastName" Decode.string)
                (Decode.field "email" Decode.string)
                (Decode.field "phone" Decode.string)
    in
    Http.request
        { method = "GET"
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson GotUserDetails decoder
        , headers = [] -- The session token is in the cookies, no need to pass it
        , timeout = Nothing
        , tracker = Nothing
        }



-- Helper function to convert Http.Error to a readable string


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus statusCode ->
            "Bad status: " ++ String.fromInt statusCode

        Http.BadBody message ->
            "Bad body: " ++ message



-- Helper function to initialize a specific step


initializeStep : Step -> Model -> ( Cmd Msg, Model )
initializeStep step model =
    case step of
        PlanSelectionStep ->
            -- Plan selection is always initialized in init, so just return the model
            ( Cmd.none, model )

        UserDetailsStep ->
            -- Always fetch user details from backend when navigating to this step
            let
                fetchCmd =
                    case model.sessionToken of
                        Just token ->
                            -- If we have a session token, fetch user details from backend
                            fetchUserDetails model.orgSlug token

                        Nothing ->
                            -- Otherwise, just initialize the model
                            Cmd.map UserDetailsMsg (UserDetails.fetchUserDetails model.orgSlug)
            in
            ( fetchCmd, { model | userDetailsInitialized = True } )

        CompanyDetailsStep ->
            -- Always initialize company details from backend when navigating to this step
            let
                ( newModel, subCmd ) =
                    case model.sessionToken of
                        Just token ->
                            CompanyDetails.init model.key model.orgSlug token

                        Nothing ->
                            CompanyDetails.init model.key model.orgSlug model.session

                -- Preserve existing company details if they exist
                modelWithDetails =
                    { newModel
                        | agencyName = model.companyDetailsModel.agencyName
                        , website = model.companyDetailsModel.website
                        , phone = model.companyDetailsModel.phone
                        , primaryColor = model.companyDetailsModel.primaryColor
                        , secondaryColor = model.companyDetailsModel.secondaryColor
                        , logo = model.companyDetailsModel.logo
                    }
            in
            ( Cmd.map CompanyDetailsMsg subCmd
            , { model | companyDetailsModel = modelWithDetails, companyDetailsInitialized = True }
            )

        LicensingSettingsStep ->
            -- Always initialize licensing settings from backend when navigating to this step
            let
                ( newModel, subCmd ) =
                    LicensingSettings.init model.key model.orgSlug
            in
            ( Cmd.map LicensingSettingsMsg subCmd
            , { model | licensingSettingsModel = newModel, licensingSettingsInitialized = True }
            )

        AddAgentsStep ->
            -- Always initialize add agents from backend when navigating to this step
            let
                ( newModel, subCmd ) =
                    AddAgents.init model.key model.orgSlug True
            in
            ( Cmd.map AddAgentsMsg subCmd
            , { model | addAgentsModel = Just newModel, addAgentsInitialized = True }
            )

        PaymentStep ->
            -- Always initialize payment from backend when navigating to this step
            let
                ( newModel, subCmd ) =
                    Payment.init model.key model.orgSlug
            in
            ( Cmd.map PaymentMsg subCmd
            , { model | paymentModel = newModel, paymentInitialized = True }
            )

        EnterpriseFormStep ->
            -- Always initialize enterprise form from backend when navigating to this step
            let
                ( newModel, subCmd ) =
                    EnterpriseForm.init model.key
            in
            ( Cmd.map EnterpriseFormMsg subCmd
            , { model | enterpriseFormModel = Just newModel, enterpriseFormInitialized = True }
            )
