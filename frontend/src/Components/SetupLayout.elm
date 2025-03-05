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


view : SetupStep -> Bool -> List (Html msg) -> Html msg
view currentStep isBasicPlan content =
    div [ class "min-h-screen bg-gray-50 flex" ]
        [ viewProgressIndicator currentStep isBasicPlan
        , div [ class "flex-1 ml-[280px] pb-24" ]
            [ div [ class "max-w-3xl mx-auto py-6 px-4 sm:px-6 lg:px-8" ]
                content
            ]
        ]


viewProgressIndicator : SetupStep -> Bool -> Html msg
viewProgressIndicator currentStep isBasicPlan =
    let
        basicSteps =
            [ { step = PlanSelection
              , icon = "1"
              , title = "Choose Plan"
              , description = "Select your subscription"
              }
            , { step = OrganizationSetup
              , icon = "2"
              , title = "Agency Settings"
              , description = "Configure your agency"
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
              , title = "Organization Settings"
              , description = "Configure your organization"
              }
            , { step = AgentSetup
              , icon = "3"
              , title = "Add Team Members"
              , description = "Invite your team"
              }
            ]

        steps =
            if isBasicPlan then
                basicSteps

            else
                multiAgentSteps

        makeStep info =
            { icon = info.icon
            , title = info.title
            , description = info.description
            , isCompleted = isStepComplete currentStep info.step
            , isActive = info.step == currentStep
            }

        -- Calculate progress percentage for the progress bar
        totalSteps =
            List.length steps

        currentStepIndex =
            case currentStep of
                PlanSelection ->
                    0

                OrganizationSetup ->
                    1

                AgentSetup ->
                    2

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
        [ ProgressIndicator.view (List.map makeStep steps)
        , progressBar
        ]


isStepComplete : SetupStep -> SetupStep -> Bool
isStepComplete currentStep step =
    case ( currentStep, step ) of
        ( PlanSelection, _ ) ->
            -- When on plan selection, no steps are completed
            False

        ( OrganizationSetup, PlanSelection ) ->
            -- When on org settings, plan selection is completed
            True

        ( OrganizationSetup, _ ) ->
            -- Other steps aren't completed yet
            False

        ( AgentSetup, AgentSetup ) ->
            -- The current step isn't completed
            False

        ( AgentSetup, _ ) ->
            -- When on agent setup, all previous steps are completed
            True
