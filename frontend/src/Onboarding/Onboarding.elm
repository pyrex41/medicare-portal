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


port storeOnboardingState : { organizationId : String, slug : String, sessionToken : String, step : Int } -> Cmd msg


port retrieveOnboardingState : () -> Cmd msg


port onboardingStateReceived : (Maybe { organizationId : String, slug : String, sessionToken : String, step : Int } -> msg) -> Sub msg



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

        ( userDetailsModel, userDetailsCmd ) =
            UserDetails.init key orgSlug isNewSignup

        ( companyDetailsModel, companyDetailsCmd ) =
            CompanyDetails.init key orgSlug

        ( licensingSettingsModel, licensingSettingsCmd ) =
            LicensingSettings.init key orgSlug

        ( paymentModel, paymentCmd ) =
            Payment.init key orgSlug

        -- Check if we need to initialize the AddAgents model based on the initial step
        -- or if we're starting with a non-basic plan
        shouldInitAddAgents =
            initialStep
                == AddAgentsStep
                || (planSelectionModel.selectedPlan |> Maybe.map (\p -> p /= "basic") |> Maybe.withDefault False)

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
      , userDetailsInitialized = True
      , companyDetailsInitialized = True
      , licensingSettingsInitialized = True
      , addAgentsInitialized = shouldInitAddAgents
      , paymentInitialized = True
      , enterpriseFormInitialized = initialStep == EnterpriseFormStep

      -- Initialize step history
      , stepHistory = []
      }
    , Cmd.batch
        [ Cmd.map PlanSelectionMsg planSelectionCmd
        , Cmd.map UserDetailsMsg userDetailsCmd
        , Cmd.map CompanyDetailsMsg companyDetailsCmd
        , Cmd.map LicensingSettingsMsg licensingSettingsCmd
        , Cmd.map PaymentMsg paymentCmd
        , addAgentsCmd
        , enterpriseFormCmd
        , retrieveOnboardingState () -- Check for any in-progress onboarding
        , if initialStep == PlanSelectionStep && not (String.isEmpty orgSlug) then
            -- When on plan selection, also try to get the plan type directly
            retrievePlanType orgSlug

          else
            Cmd.none
        ]
    )



-- Helper function to update step and maintain history


changeStep : Step -> Model -> Model
changeStep newStep model =
    -- Only add to history if actually changing step
    if newStep /= model.step then
        { model
            | step = newStep
            , stepHistory = model.step :: model.stepHistory
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
    | OnboardingInitialized (Result Http.Error { organizationId : Int, slug : String, sessionToken : String, onboardingStep : Int })
    | OnboardingStateReceived (Maybe { organizationId : String, slug : String, sessionToken : String, step : Int })
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
                                        }

                                    oldUserDetailsModel =
                                        model.userDetailsModel

                                    updatedUserDetailsModel =
                                        { oldUserDetailsModel | orgSlug = response.slug }
                                in
                                ( { updatedModelWithStep
                                    | organizationId = Just response.organizationId
                                    , orgSlug = response.slug
                                    , sessionToken = Just response.sessionToken
                                    , onboardingInProgress = True
                                    , step = UserDetailsStep
                                    , userDetailsModel = updatedUserDetailsModel
                                  }
                                , storeOnboardingState state
                                )

                            else
                                ( updatedModelWithStep, Cmd.none )

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
                            in
                            changeStep nextStep model

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
                            in
                            changeStep nextStep model

                        _ ->
                            model

                -- When NextStep is triggered, also update company details
                outCmd =
                    case outMsg of
                        CompanyDetails.NoOutMsg ->
                            Cmd.none

                        CompanyDetails.NextStep ->
                            case model.sessionToken of
                                Just token ->
                                    updateCompanyDetails model.orgSlug token updatedCompanyDetailsModel

                                Nothing ->
                                    Cmd.none

                        CompanyDetails.ShowError err ->
                            Cmd.none
            in
            ( { updatedModelWithStep
                | companyDetailsModel = updatedCompanyDetailsModel
              }
            , Cmd.batch
                [ Cmd.map CompanyDetailsMsg companyDetailsCmd
                , outCmd
                ]
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
                            in
                            changeStep nextStep model

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

                        Payment.NavigateToWalkthrough ->
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
                -- Only initialize models if they haven't been initialized yet
                ( mainCmd, updatedModel ) =
                    case step of
                        PlanSelectionStep ->
                            -- When navigating to plan selection, keep existing state
                            ( Cmd.none, model )

                        UserDetailsStep ->
                            -- Keep existing user details state
                            ( Cmd.none, model )

                        CompanyDetailsStep ->
                            if not model.companyDetailsInitialized then
                                let
                                    ( newModel, subCmd ) =
                                        CompanyDetails.init model.key model.orgSlug
                                in
                                ( Cmd.map CompanyDetailsMsg subCmd
                                , { model | companyDetailsModel = newModel, companyDetailsInitialized = True }
                                )

                            else
                                ( Cmd.none, model )

                        LicensingSettingsStep ->
                            if not model.licensingSettingsInitialized then
                                let
                                    ( newModel, subCmd ) =
                                        LicensingSettings.init model.key model.orgSlug
                                in
                                ( Cmd.map LicensingSettingsMsg subCmd
                                , { model | licensingSettingsModel = newModel, licensingSettingsInitialized = True }
                                )

                            else
                                ( Cmd.none, model )

                        AddAgentsStep ->
                            if not model.addAgentsInitialized then
                                let
                                    ( newModel, subCmd ) =
                                        AddAgents.init model.key model.orgSlug True
                                in
                                ( Cmd.map AddAgentsMsg subCmd
                                , { model | addAgentsModel = Just newModel, addAgentsInitialized = True }
                                )

                            else
                                ( Cmd.none, model )

                        PaymentStep ->
                            if not model.paymentInitialized then
                                let
                                    ( newModel, subCmd ) =
                                        Payment.init model.key model.orgSlug
                                in
                                ( Cmd.map PaymentMsg subCmd
                                , { model | paymentModel = newModel, paymentInitialized = True }
                                )

                            else
                                ( Cmd.none, model )

                        EnterpriseFormStep ->
                            if not model.enterpriseFormInitialized then
                                let
                                    ( newModel, subCmd ) =
                                        EnterpriseForm.init model.key
                                in
                                ( Cmd.map EnterpriseFormMsg subCmd
                                , { model | enterpriseFormModel = Just newModel, enterpriseFormInitialized = True }
                                )

                            else
                                ( Cmd.none, model )

                -- Update URL fragment without page reload to preserve state in browser history
                urlUpdateCmd =
                    Nav.pushUrl model.key (getStepFragment step)
            in
            ( changeStep step updatedModel
            , Cmd.batch
                [ mainCmd
                , urlUpdateCmd
                , Task.perform (\_ -> ScrollToTop) (Task.succeed ())
                ]
            )

        NavigateBack ->
            case model.stepHistory of
                previousStep :: restHistory ->
                    ( { model
                        | step = previousStep
                        , stepHistory = restHistory
                      }
                    , Cmd.batch
                        [ Nav.pushUrl model.key (getStepFragment previousStep)
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
            in
            ( changeStep nextStep model
            , Cmd.batch
                [ Nav.pushUrl model.key (getStepFragment nextStep)
                , Task.perform (\_ -> ScrollToTop) (Task.succeed ())
                ]
            )

        ScrollToTop ->
            ( model, Dom.setViewport 0 0 |> Task.attempt (\_ -> NoOp) )

        CompleteOnboarding result ->
            case result of
                Ok _ ->
                    -- Redirect to login with special parameters instead of walkthrough
                    ( { model | isLoading = False }
                    , Nav.pushUrl model.key ("/login?onboarding=completed&email=" ++ Url.percentEncode model.userDetailsModel.email)
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
                            }

                        -- Reinitialize the user details model with the proper org slug
                        ( newUserDetailsModel, userDetailsCmd ) =
                            UserDetails.init model.key response.slug True
                    in
                    ( { model
                        | isLoading = False
                        , organizationId = Just response.organizationId
                        , orgSlug = response.slug
                        , sessionToken = Just response.sessionToken
                        , onboardingInProgress = True
                        , step = UserDetailsStep
                        , userDetailsModel = newUserDetailsModel
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

                        -- Extract plan type from session - in a real app this should be retrieved from API
                        -- For now we'll load it from cookie/session storage via a port
                        planType =
                            "basic"

                        -- This should actually be retrieved from API using state.slug and state.sessionToken
                        -- Create command to fetch the plan type from the API
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
                    -- After licensing settings are updated, add team members if needed
                    let
                        nextCmd =
                            if not model.isBasicPlan then
                                case ( model.sessionToken, model.addAgentsModel ) of
                                    ( Just token, Just agentsModel ) ->
                                        addTeamMembers model.orgSlug token agentsModel

                                    _ ->
                                        completeOnboarding model

                            else
                                completeOnboarding model
                    in
                    ( { model
                        | isLoading = False
                        , licensingSettingsInitialized = True
                        , isBasicPlan = response.isBasicPlan
                      }
                    , nextCmd
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
                    -- After team members are added, complete the onboarding
                    ( { model
                        | isLoading = False
                        , addAgentsInitialized = True
                      }
                    , completeOnboarding model
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
                    -- Pass the data to the UserDetails module
                    ( model
                    , Cmd.map UserDetailsMsg (Task.perform (\_ -> UserDetails.loadUserFromSession userData) (Task.succeed ()))
                    )

                Nothing ->
                    ( model, Cmd.none )

        RandomOrgNameGenerated orgName ->
            -- When we get a random org name, update both the org slug and the CompanyDetails model
            let
                _ =
                    Debug.log "orgName" orgName

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

                else
                    case model.addAgentsModel of
                        Just addAgentsModel ->
                            Html.map AddAgentsMsg (AddAgents.view addAgentsModel)

                        Nothing ->
                            div [ class "text-center p-8" ]
                                [ text "There was an issue loading the team members form. Please try refreshing the page."
                                , div [ class "mt-4" ]
                                    [ button
                                        [ class "px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
                                        , onClick (NavigateToStep PaymentStep)
                                        ]
                                        [ text "Continue to Payment" ]
                                    ]
                                ]

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
                [ text ("Skip to " ++ getStepLabel nextStep) ]

          else
            div [] []

        -- Empty spacer
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
                Just agentsModel ->
                    -- Use the agents from the model (already correct type)
                    Encode.list encodeAgent agentsModel.agents

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
                { url = "/api/organizations/finalize-onboarding"
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "orgSlug", Encode.string model.orgSlug )
                            , ( "sessionToken", Encode.string token )
                            ]
                        )
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



-- Helper function to get URL fragment for a step (instead of full URL)


getStepFragment : Step -> String
getStepFragment step =
    "#"
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
            Decode.map4
                (\organizationId slug sessionToken onboardingStep ->
                    { organizationId = organizationId
                    , slug = slug
                    , sessionToken = sessionToken
                    , onboardingStep = onboardingStep
                    }
                )
                (Decode.field "organizationId" Decode.int)
                (Decode.field "slug" Decode.string)
                (Decode.field "sessionToken" Decode.string)
                (Decode.field "onboardingStep" Decode.int)
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
            Decode.map4
                (\organizationId slug sessionToken onboardingStep ->
                    { organizationId = organizationId
                    , slug = slug
                    , sessionToken = sessionToken
                    , onboardingStep = onboardingStep
                    }
                )
                (Decode.field "organizationId" Decode.int)
                (Decode.field "slug" Decode.string)
                (Decode.field "sessionToken" Decode.string)
                (Decode.field "onboardingStep" Decode.int)
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
            "/api/organizations/" ++ orgSlug ++ "/update-user"

        body =
            Encode.object
                [ ( "firstName", Encode.string model.firstName )
                , ( "lastName", Encode.string model.lastName )
                , ( "email", Encode.string model.email )
                , ( "phone", Encode.string model.phone )
                , ( "tempSessionId", Encode.string sessionToken )
                ]
                |> Http.jsonBody

        headers =
            [ Http.header "Authorization" ("Bearer " ++ sessionToken) ]

        decoder =
            Decode.map
                (\onboardingStep ->
                    { onboardingStep = onboardingStep }
                )
                (Decode.field "onboardingStep" Decode.int)
    in
    Http.request
        { method = "PUT"
        , url = url
        , body = body
        , expect = Http.expectJson UserUpdated decoder
        , headers = headers
        , timeout = Nothing
        , tracker = Nothing
        }


updateCompanyDetails : String -> String -> CompanyDetails.Model -> Cmd Msg
updateCompanyDetails orgSlug sessionToken model =
    let
        url =
            "/api/onboarding/" ++ orgSlug ++ "/company"

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

        headers =
            [ Http.header "Authorization" ("Bearer " ++ sessionToken) ]

        decoder =
            Decode.map
                (\onboardingStep ->
                    { onboardingStep = onboardingStep }
                )
                (Decode.field "onboardingStep" Decode.int)
    in
    Http.request
        { method = "PUT"
        , url = url
        , body = body
        , expect = Http.expectJson CompanyUpdated decoder
        , headers = headers
        , timeout = Nothing
        , tracker = Nothing
        }


updateLicensingDetails : String -> String -> LicensingSettings.Model -> Cmd Msg
updateLicensingDetails orgSlug sessionToken model =
    let
        url =
            "/api/onboarding/" ++ orgSlug ++ "/licensing"

        body =
            Encode.object
                [ ( "carrierContracts", Encode.list Encode.string model.carrierContracts )
                , ( "useSmartSendForGI", Encode.bool model.useSmartSendForGI )
                ]
                |> Http.jsonBody

        headers =
            [ Http.header "Authorization" ("Bearer " ++ sessionToken) ]

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
        { method = "PUT"
        , url = url
        , body = body
        , expect = Http.expectJson LicensingUpdated decoder
        , headers = headers
        , timeout = Nothing
        , tracker = Nothing
        }


addTeamMembers : String -> String -> AddAgents.Model -> Cmd Msg
addTeamMembers orgSlug sessionToken model =
    let
        url =
            "/api/onboarding/" ++ orgSlug ++ "/team-members"

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
            [ Http.header "Authorization" ("Bearer " ++ sessionToken) ]

        decoder =
            Decode.map
                (\onboardingStep ->
                    { onboardingStep = onboardingStep }
                )
                (Decode.field "onboardingStep" Decode.int)
    in
    Http.request
        { method = "PUT"
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
