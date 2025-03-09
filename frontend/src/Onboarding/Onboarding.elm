module Onboarding.Onboarding exposing
    ( Model
    , Msg(..)
    , Step(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser
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
import Url



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
    }


init : Nav.Key -> String -> String -> Step -> ( Model, Cmd Msg )
init key orgSlug session initialStep =
    let
        -- Only fetch tiers when we're on the plan selection step
        ( planSelectionModel, planSelectionCmd ) =
            PlanSelection.initWithFetch key orgSlug session (initialStep == PlanSelectionStep)

        ( userDetailsModel, userDetailsCmd ) =
            UserDetails.init key orgSlug

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
      }
    , Cmd.batch
        [ Cmd.map PlanSelectionMsg planSelectionCmd
        , Cmd.map UserDetailsMsg userDetailsCmd
        , Cmd.map CompanyDetailsMsg companyDetailsCmd
        , Cmd.map LicensingSettingsMsg licensingSettingsCmd
        , Cmd.map PaymentMsg paymentCmd
        , addAgentsCmd
        , enterpriseFormCmd
        ]
    )



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
    | SkipStep
    | CompleteOnboarding
    | OnboardingCompleted (Result Http.Error ())
    | GotError String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlanSelectionMsg subMsg ->
            let
                ( updatedPlanModel, planCmd, outMsg ) =
                    PlanSelection.update subMsg model.planSelectionModel

                -- Handle the selected plan type to determine if we need to initialize the AddAgents model
                newIsBasicPlan =
                    case outMsg of
                        PlanSelection.SelectedPlan planType ->
                            planType == "basic"

                        _ ->
                            model.isBasicPlan

                addAgentsModel =
                    if not newIsBasicPlan && model.addAgentsModel == Nothing then
                        -- Initialize AddAgents model for non-basic plans
                        Just (AddAgents.init model.key model.orgSlug True |> Tuple.first)

                    else
                        model.addAgentsModel

                -- Handle enterprise plan selection
                ( enterpriseFormModel, enterpriseFormCmd, navigationCmd ) =
                    case outMsg of
                        PlanSelection.SelectedPlan "enterprise" ->
                            let
                                ( newModel, cmd ) =
                                    EnterpriseForm.init model.key
                            in
                            ( Just newModel, Cmd.map EnterpriseFormMsg cmd, Nav.pushUrl model.key (getStepUrl EnterpriseFormStep) )

                        _ ->
                            ( model.enterpriseFormModel
                            , Cmd.none
                            , case outMsg of
                                PlanSelection.NextStep ->
                                    Nav.pushUrl model.key (getStepUrl UserDetailsStep)

                                _ ->
                                    Cmd.none
                            )

                outCmd =
                    case outMsg of
                        PlanSelection.NoOutMsg ->
                            Cmd.none

                        PlanSelection.SelectedPlan _ ->
                            Cmd.none

                        PlanSelection.NextStep ->
                            Cmd.none

                        PlanSelection.ShowError err ->
                            Cmd.none
            in
            ( { model
                | planSelectionModel = updatedPlanModel
                , isBasicPlan = newIsBasicPlan
                , addAgentsModel = addAgentsModel
                , enterpriseFormModel = enterpriseFormModel
              }
            , Cmd.batch
                [ Cmd.map PlanSelectionMsg planCmd
                , outCmd
                , enterpriseFormCmd
                , navigationCmd
                ]
            )

        UserDetailsMsg subMsg ->
            let
                ( updatedUserDetailsModel, userDetailsCmd, outMsg ) =
                    UserDetails.update subMsg model.userDetailsModel

                navigationCmd =
                    case outMsg of
                        UserDetails.NextStep ->
                            let
                                nextStep =
                                    getNextStep UserDetailsStep model.isBasicPlan
                            in
                            Nav.pushUrl model.key (getStepUrl nextStep)

                        _ ->
                            Cmd.none

                outCmd =
                    case outMsg of
                        UserDetails.NoOutMsg ->
                            Cmd.none

                        UserDetails.NextStep ->
                            Cmd.none

                        UserDetails.ShowError err ->
                            Cmd.none
            in
            ( { model | userDetailsModel = updatedUserDetailsModel }
            , Cmd.batch
                [ Cmd.map UserDetailsMsg userDetailsCmd
                , outCmd
                , navigationCmd
                ]
            )

        CompanyDetailsMsg subMsg ->
            let
                ( updatedCompanyDetailsModel, companyDetailsCmd, outMsg ) =
                    CompanyDetails.update subMsg model.companyDetailsModel

                navigationCmd =
                    case outMsg of
                        CompanyDetails.NextStep ->
                            let
                                nextStep =
                                    getNextStep CompanyDetailsStep model.isBasicPlan
                            in
                            Nav.pushUrl model.key (getStepUrl nextStep)

                        _ ->
                            Cmd.none

                outCmd =
                    case outMsg of
                        CompanyDetails.NoOutMsg ->
                            Cmd.none

                        CompanyDetails.NextStep ->
                            Cmd.none

                        CompanyDetails.ShowError err ->
                            Cmd.none
            in
            ( { model | companyDetailsModel = updatedCompanyDetailsModel }
            , Cmd.batch
                [ Cmd.map CompanyDetailsMsg companyDetailsCmd
                , outCmd
                , navigationCmd
                ]
            )

        LicensingSettingsMsg subMsg ->
            let
                ( updatedLicensingSettingsModel, licensingSettingsCmd, outMsg ) =
                    LicensingSettings.update subMsg model.licensingSettingsModel

                -- Ensure addAgentsModel is initialized if we're on a non-basic plan
                updatedModel =
                    if not model.isBasicPlan && model.addAgentsModel == Nothing then
                        { model
                            | licensingSettingsModel = updatedLicensingSettingsModel
                            , addAgentsModel = Just (AddAgents.init model.key model.orgSlug True |> Tuple.first)
                        }

                    else
                        { model | licensingSettingsModel = updatedLicensingSettingsModel }

                navigationCmd =
                    case outMsg of
                        LicensingSettings.NextStep ->
                            let
                                nextStep =
                                    getNextStep LicensingSettingsStep model.isBasicPlan
                            in
                            Nav.pushUrl model.key (getStepUrl nextStep)

                        _ ->
                            Cmd.none

                outCmd =
                    case outMsg of
                        LicensingSettings.NoOutMsg ->
                            Cmd.none

                        LicensingSettings.NextStep ->
                            Cmd.none

                        LicensingSettings.ShowError err ->
                            Cmd.none
            in
            ( updatedModel
            , Cmd.batch
                [ Cmd.map LicensingSettingsMsg licensingSettingsCmd
                , outCmd
                , navigationCmd
                ]
            )

        AddAgentsMsg subMsg ->
            case model.addAgentsModel of
                Just addAgentsModel ->
                    let
                        ( updatedAddAgentsModel, addAgentsCmd, outMsg ) =
                            AddAgents.update subMsg addAgentsModel

                        navigationCmd =
                            case outMsg of
                                AddAgents.NextStep ->
                                    let
                                        nextStep =
                                            getNextStep AddAgentsStep model.isBasicPlan
                                    in
                                    Nav.pushUrl model.key (getStepUrl nextStep)

                                _ ->
                                    Cmd.none

                        outCmd =
                            case outMsg of
                                AddAgents.NoOutMsg ->
                                    Cmd.none

                                AddAgents.NextStep ->
                                    Cmd.none

                                AddAgents.ShowError err ->
                                    Cmd.none
                    in
                    ( { model | addAgentsModel = Just updatedAddAgentsModel }
                    , Cmd.batch
                        [ Cmd.map AddAgentsMsg addAgentsCmd
                        , outCmd
                        , navigationCmd
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        PaymentMsg subMsg ->
            let
                ( updatedPaymentModel, paymentCmd, outMsg ) =
                    Payment.update subMsg model.paymentModel

                outCmd =
                    case outMsg of
                        Payment.NoOutMsg ->
                            Cmd.none

                        Payment.Completed ->
                            completeOnboarding model

                        Payment.NavigateToWalkthrough ->
                            completeOnboarding model

                        Payment.ShowError err ->
                            Cmd.none
            in
            ( { model | paymentModel = updatedPaymentModel }
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
                        isNavigatingBackToPlans =
                            outMsg == EnterpriseForm.BackToPlanSelection

                        -- Navigate back to plan selection if requested
                        navigationCmd =
                            if isNavigatingBackToPlans then
                                Nav.pushUrl model.key (getStepUrl PlanSelectionStep)

                            else
                                Cmd.none

                        -- Don't reload plan data - avoid unnecessary API call
                        outCmd =
                            case outMsg of
                                EnterpriseForm.NoOutMsg ->
                                    Cmd.none

                                EnterpriseForm.BackToPlanSelection ->
                                    Cmd.none

                                EnterpriseForm.ShowError err ->
                                    Cmd.none
                    in
                    ( { model | enterpriseFormModel = Just updatedEnterpriseFormModel }
                    , Cmd.batch
                        [ Cmd.map EnterpriseFormMsg enterpriseFormCmd
                        , outCmd
                        , navigationCmd
                        ]
                    )

                Nothing ->
                    -- If the model doesn't exist, create it
                    let
                        ( enterpriseFormModel, enterpriseFormCmd ) =
                            EnterpriseForm.init model.key
                    in
                    ( { model | enterpriseFormModel = Just enterpriseFormModel }
                    , Cmd.map EnterpriseFormMsg enterpriseFormCmd
                    )

        NavigateToStep step ->
            let
                cmd =
                    if step == PlanSelectionStep then
                        -- When navigating directly to plan selection, fetch the tiers
                        Cmd.batch
                            [ Nav.pushUrl model.key (getStepUrl step)
                            , Cmd.map PlanSelectionMsg fetchSubscriptionTiers
                            ]

                    else
                        Nav.pushUrl model.key (getStepUrl step)
            in
            ( model, cmd )

        SkipStep ->
            let
                nextStep =
                    getNextStep model.step model.isBasicPlan
            in
            ( model, Nav.pushUrl model.key (getStepUrl nextStep) )

        CompleteOnboarding ->
            ( { model | isLoading = True }
            , completeOnboarding model
            )

        OnboardingCompleted result ->
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



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = getStepTitle model.step
    , body =
        [ div []
            [ SetupLayout.view (mapStepToSetupStep model.step)
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
                    ]
                ]
            ]
        ]
    }


viewCurrentStep : Model -> Html Msg
viewCurrentStep model =
    case model.step of
        PlanSelectionStep ->
            Html.map PlanSelectionMsg (PlanSelection.view model.planSelectionModel)

        UserDetailsStep ->
            Html.map UserDetailsMsg (UserDetails.view model.userDetailsModel)

        CompanyDetailsStep ->
            Html.map CompanyDetailsMsg (CompanyDetails.view model.companyDetailsModel)

        LicensingSettingsStep ->
            Html.map LicensingSettingsMsg (LicensingSettings.view model.licensingSettingsModel)

        AddAgentsStep ->
            if model.isBasicPlan then
                -- If on basic plan but somehow on this step, redirect to payment
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
                        -- If model not initialized, show error with option to continue
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
        stateLicenses =
            model.licensingSettingsModel.stateLicenses

        carrierContracts =
            model.licensingSettingsModel.carrierContracts

        stateCarrierSettings =
            -- Create state carrier settings based on whether SmartSend for GI is enabled
            List.concatMap
                (\state ->
                    List.map
                        (\carrier ->
                            { state = state
                            , carrier = carrier
                            , active = True
                            , targetGI = model.licensingSettingsModel.useSmartSendForGI
                            }
                        )
                        carrierContracts
                )
                stateLicenses

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
                [ ( "stateLicenses", Encode.list Encode.string stateLicenses )
                , ( "carrierContracts", Encode.list Encode.string carrierContracts )
                , ( "stateCarrierSettings", Encode.list encodeStateCarrierSetting stateCarrierSettings )
                ]
          )
        , ( "agents", agents )
        ]



-- Update the completeOnboarding function to submit all data at once


completeOnboarding : Model -> Cmd Msg
completeOnboarding model =
    Http.post
        { url = "/api/organizations/complete-onboarding"
        , body = Http.jsonBody (encodeOnboardingData model)
        , expect = Http.expectWhatever OnboardingCompleted
        }



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



-- Helper function to get URL for a step


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
