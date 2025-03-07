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
import Onboarding.Steps.LicensingSettings as LicensingSettings
import Onboarding.Steps.Payment as Payment
import Onboarding.Steps.PlanSelection as PlanSelection
import Onboarding.Steps.UserDetails as UserDetails



-- MODEL


type Step
    = PlanSelectionStep
    | UserDetailsStep
    | CompanyDetailsStep
    | LicensingSettingsStep
    | AddAgentsStep
    | PaymentStep


type alias Model =
    { step : Step
    , planSelectionModel : PlanSelection.Model
    , userDetailsModel : UserDetails.Model
    , companyDetailsModel : CompanyDetails.Model
    , licensingSettingsModel : LicensingSettings.Model
    , addAgentsModel : Maybe AddAgents.Model -- Optional, only for Pro plans
    , paymentModel : Payment.Model
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
        ( planSelectionModel, planSelectionCmd ) =
            PlanSelection.init key orgSlug session

        ( userDetailsModel, userDetailsCmd ) =
            UserDetails.init key orgSlug

        ( companyDetailsModel, companyDetailsCmd ) =
            CompanyDetails.init key orgSlug

        ( licensingSettingsModel, licensingSettingsCmd ) =
            LicensingSettings.init key orgSlug

        ( paymentModel, paymentCmd ) =
            Payment.init key orgSlug
    in
    ( { step = initialStep
      , planSelectionModel = planSelectionModel
      , userDetailsModel = userDetailsModel
      , companyDetailsModel = companyDetailsModel
      , licensingSettingsModel = licensingSettingsModel
      , addAgentsModel = Nothing -- Will be initialized if needed
      , paymentModel = paymentModel
      , key = key
      , orgSlug = orgSlug
      , session = session
      , isBasicPlan = True -- Default to basic, will be updated after plan selection
      , error = Nothing
      , isLoading = False
      }
    , Cmd.batch
        [ Cmd.map PlanSelectionMsg planSelectionCmd
        , Cmd.map UserDetailsMsg userDetailsCmd
        , Cmd.map CompanyDetailsMsg companyDetailsCmd
        , Cmd.map LicensingSettingsMsg licensingSettingsCmd
        , Cmd.map PaymentMsg paymentCmd
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
    | NavigateToStep Step
    | SkipStep
    | CompleteOnboarding
    | OnboardingCompleted (Result Http.Error ())
    | GotError String


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
                        Just (AddAgents.init model.key model.orgSlug |> Tuple.first)

                    else
                        model.addAgentsModel

                newStep =
                    case outMsg of
                        PlanSelection.NextStep ->
                            UserDetailsStep

                        _ ->
                            model.step

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
                , step = newStep
              }
            , Cmd.batch
                [ Cmd.map PlanSelectionMsg planCmd
                , outCmd
                ]
            )

        UserDetailsMsg subMsg ->
            let
                ( updatedUserDetailsModel, userDetailsCmd, outMsg ) =
                    UserDetails.update subMsg model.userDetailsModel

                newStep =
                    case outMsg of
                        UserDetails.NextStep ->
                            CompanyDetailsStep

                        _ ->
                            model.step

                outCmd =
                    case outMsg of
                        UserDetails.NoOutMsg ->
                            Cmd.none

                        UserDetails.NextStep ->
                            Cmd.none

                        UserDetails.ShowError err ->
                            Cmd.none
            in
            ( { model | userDetailsModel = updatedUserDetailsModel, step = newStep }
            , Cmd.batch
                [ Cmd.map UserDetailsMsg userDetailsCmd
                , outCmd
                ]
            )

        CompanyDetailsMsg subMsg ->
            let
                ( updatedCompanyDetailsModel, companyDetailsCmd, outMsg ) =
                    CompanyDetails.update subMsg model.companyDetailsModel

                newStep =
                    case outMsg of
                        CompanyDetails.NextStep ->
                            LicensingSettingsStep

                        _ ->
                            model.step

                outCmd =
                    case outMsg of
                        CompanyDetails.NoOutMsg ->
                            Cmd.none

                        CompanyDetails.NextStep ->
                            Cmd.none

                        CompanyDetails.ShowError err ->
                            Cmd.none
            in
            ( { model | companyDetailsModel = updatedCompanyDetailsModel, step = newStep }
            , Cmd.batch
                [ Cmd.map CompanyDetailsMsg companyDetailsCmd
                , outCmd
                ]
            )

        LicensingSettingsMsg subMsg ->
            let
                ( updatedLicensingSettingsModel, licensingSettingsCmd, outMsg ) =
                    LicensingSettings.update subMsg model.licensingSettingsModel

                newStep =
                    case outMsg of
                        LicensingSettings.NextStep ->
                            if model.isBasicPlan then
                                -- Skip AddAgentsStep for basic plan
                                PaymentStep

                            else
                                AddAgentsStep

                        _ ->
                            model.step

                outCmd =
                    case outMsg of
                        LicensingSettings.NoOutMsg ->
                            Cmd.none

                        LicensingSettings.NextStep ->
                            Cmd.none

                        LicensingSettings.ShowError err ->
                            Cmd.none
            in
            ( { model | licensingSettingsModel = updatedLicensingSettingsModel, step = newStep }
            , Cmd.batch
                [ Cmd.map LicensingSettingsMsg licensingSettingsCmd
                , outCmd
                ]
            )

        AddAgentsMsg subMsg ->
            case model.addAgentsModel of
                Just addAgentsModel ->
                    let
                        ( updatedAddAgentsModel, addAgentsCmd, outMsg ) =
                            AddAgents.update subMsg addAgentsModel

                        newStep =
                            case outMsg of
                                AddAgents.NextStep ->
                                    PaymentStep

                                _ ->
                                    model.step

                        outCmd =
                            case outMsg of
                                AddAgents.NoOutMsg ->
                                    Cmd.none

                                AddAgents.NextStep ->
                                    Cmd.none

                                AddAgents.ShowError err ->
                                    Cmd.none
                    in
                    ( { model | addAgentsModel = Just updatedAddAgentsModel, step = newStep }
                    , Cmd.batch
                        [ Cmd.map AddAgentsMsg addAgentsCmd
                        , outCmd
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

                        Payment.ShowError err ->
                            Cmd.none
            in
            ( { model | paymentModel = updatedPaymentModel }
            , Cmd.batch
                [ Cmd.map PaymentMsg paymentCmd
                , outCmd
                ]
            )

        NavigateToStep step ->
            ( { model | step = step }, Cmd.none )

        SkipStep ->
            let
                nextStep =
                    case model.step of
                        PlanSelectionStep ->
                            UserDetailsStep

                        UserDetailsStep ->
                            CompanyDetailsStep

                        CompanyDetailsStep ->
                            LicensingSettingsStep

                        LicensingSettingsStep ->
                            if model.isBasicPlan then
                                PaymentStep

                            else
                                AddAgentsStep

                        AddAgentsStep ->
                            PaymentStep

                        PaymentStep ->
                            PaymentStep

                -- Can't skip payment
            in
            ( { model | step = nextStep }, Cmd.none )

        CompleteOnboarding ->
            ( { model | isLoading = True }
            , completeOnboarding model
            )

        OnboardingCompleted result ->
            case result of
                Ok _ ->
                    -- Redirect to dashboard on successful completion
                    ( { model | isLoading = False }
                    , Nav.pushUrl model.key "/dashboard"
                    )

                Err _ ->
                    ( { model | isLoading = False, error = Just "Failed to complete onboarding. Please try again." }
                    , Cmd.none
                    )

        GotError errorMsg ->
            ( { model | error = Just errorMsg, isLoading = False }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = getStepTitle model.step
    , body =
        [ div []
            [ SetupLayout.view (mapStepToSetupStep model.step)
                model.isBasicPlan
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
            case model.addAgentsModel of
                Just addAgentsModel ->
                    Html.map AddAgentsMsg (AddAgents.view addAgentsModel)

                Nothing ->
                    div [ class "text-center p-8" ]
                        [ text "Error: Add Agents model not initialized." ]

        PaymentStep ->
            Html.map PaymentMsg (Payment.view model.paymentModel)



-- UTILS


getStepTitle : Step -> String
getStepTitle step =
    case step of
        PlanSelectionStep ->
            "Choose Your Plan - Onboarding"

        UserDetailsStep ->
            "Your Details - Onboarding"

        CompanyDetailsStep ->
            "Company Details - Onboarding"

        LicensingSettingsStep ->
            "Licensing & Carriers - Onboarding"

        AddAgentsStep ->
            "Add Team Members - Onboarding"

        PaymentStep ->
            "Payment - Onboarding"


mapStepToSetupStep : Step -> SetupLayout.SetupStep
mapStepToSetupStep step =
    case step of
        PlanSelectionStep ->
            SetupLayout.PlanSelection

        UserDetailsStep ->
            SetupLayout.UserDetailsStep

        CompanyDetailsStep ->
            SetupLayout.CompanyDetailsStep

        LicensingSettingsStep ->
            SetupLayout.OrganizationSetup

        AddAgentsStep ->
            SetupLayout.AgentSetup

        PaymentStep ->
            SetupLayout.PaymentStep



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

        bookingLink =
            model.userDetailsModel.bookingLink

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
            model.licensingSettingsModel.stateCarrierSettings

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
        [ ( "subscription"
          , Encode.object
                [ ( "tierId", Encode.string selectedPlan )
                , ( "extraAgents", Encode.int extraAgents )
                , ( "extraContacts", Encode.int extraContacts )
                ]
          )
        , ( "user"
          , Encode.object
                [ ( "firstName", Encode.string firstName )
                , ( "lastName", Encode.string lastName )
                , ( "email", Encode.string email )
                , ( "phone", Encode.string phone )
                , ( "bookingLink", Encode.string bookingLink )
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
