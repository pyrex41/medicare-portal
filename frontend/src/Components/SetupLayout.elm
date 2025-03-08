module Components.SetupLayout exposing (SetupStep(..), view)

import Components.ProgressIndicator as ProgressIndicator
import Html exposing (..)
import Html.Attributes exposing (class, style)


type SetupStep
    = PlanSelection
    | OrganizationSetup
    | AgentSetup


type alias StepInfo =
    { step : SetupStep
    , icon : String
    , title : String
    , description : String
    }


view : SetupStep -> Bool -> Int -> List (Html msg) -> Html msg
view currentStep isBasicPlan stepNumber content =
    div [ class "min-h-screen bg-gray-50 flex" ]
        [ viewProgressIndicator currentStep isBasicPlan stepNumber
        , div [ class "flex-1 ml-[280px] pb-24" ]
            [ div [ class "max-w-3xl mx-auto py-6 px-4 sm:px-6 lg:px-8" ]
                content
            ]
        ]


viewProgressIndicator : SetupStep -> Bool -> Int -> Html msg
viewProgressIndicator currentStep isBasicPlan stepNumber =
    let
        basicSteps =
            [ { step = PlanSelection
              , icon = "1"
              , title = "Choose Plan"
              , description = "Select your subscription"
              }
            , { step = OrganizationSetup
              , icon = "2"
              , title = "Personal Details"
              , description = "Enter your information"
              }
            , { step = OrganizationSetup
              , icon = "3"
              , title = "Company Details"
              , description = "Agency information"
              }
            , { step = OrganizationSetup
              , icon = "4"
              , title = "Licensing Settings"
              , description = "States and carriers"
              }
            , { step = OrganizationSetup
              , icon = "5"
              , title = "Payment"
              , description = "Complete setup"
              }
            ]

        multiAgentSteps =
            [ { step = PlanSelection
              , icon = "1"
              , title = "Choose Plan"
              , description = "Select your subscription"
              }
            , { step = OrganizationSetup
              , icon = "2"
              , title = "Personal Details"
              , description = "Enter your information"
              }
            , { step = OrganizationSetup
              , icon = "3"
              , title = "Company Details"
              , description = "Agency information"
              }
            , { step = OrganizationSetup
              , icon = "4"
              , title = "Licensing Settings"
              , description = "States and carriers"
              }
            , { step = AgentSetup
              , icon = "5"
              , title = "Add Team Members"
              , description = "Invite your team"
              }
            , { step = OrganizationSetup
              , icon = "6"
              , title = "Payment"
              , description = "Complete setup"
              }
            ]

        steps =
            if isBasicPlan then
                basicSteps

            else
                multiAgentSteps

        makeStep index info =
            { icon = info.icon
            , title = info.title
            , description = info.description
            , isCompleted = isStepComplete currentStep info.step index stepNumber
            , isActive = info.step == currentStep && index == stepNumber
            }

        -- Calculate progress percentage for the progress bar
        totalSteps =
            List.length steps

        currentStepIndex =
            case currentStep of
                PlanSelection ->
                    0

                OrganizationSetup ->
                    stepNumber

                AgentSetup ->
                    stepNumber

        progressPercentage =
            String.fromInt (min 100 (ceiling (toFloat (currentStepIndex * 100) / toFloat (totalSteps - 1))))

        progressBar =
            div [ class "px-8 mt-4" ]
                [ div [ class "h-1 w-full bg-gray-200 rounded overflow-hidden" ]
                    [ div
                        [ class "h-full bg-[#03045e] transition-all duration-300"
                        , style "width" (progressPercentage ++ "%")
                        ]
                        []
                    ]
                , div [ class "mt-2 text-xs text-gray-500 flex justify-between" ]
                    [ span [] [ text "Setup Progress" ]
                    , span [] [ text (progressPercentage ++ "%") ]
                    ]
                ]
    in
    div []
        [ ProgressIndicator.view (List.indexedMap makeStep steps)
        , progressBar
        ]


isStepComplete : SetupStep -> SetupStep -> Int -> Int -> Bool
isStepComplete currentStep step stepIndex currentStepNumber =
    case ( currentStep, step ) of
        ( PlanSelection, _ ) ->
            -- When on plan selection, no steps are completed
            False

        ( OrganizationSetup, PlanSelection ) ->
            -- When on org settings, plan selection is completed
            True

        ( OrganizationSetup, OrganizationSetup ) ->
            -- For org setup steps, complete those before the current one
            stepIndex < currentStepNumber

        ( OrganizationSetup, _ ) ->
            -- Other steps aren't completed yet
            False

        ( AgentSetup, AgentSetup ) ->
            -- The current step isn't completed
            False

        ( AgentSetup, _ ) ->
            -- When on agent setup, all previous steps are completed
            True
