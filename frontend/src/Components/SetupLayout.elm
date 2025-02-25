module Components.SetupLayout exposing (SetupStep(..), view)

import Components.ProgressIndicator as ProgressIndicator
import Html exposing (..)
import Html.Attributes exposing (..)


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
    in
    ProgressIndicator.view (List.map makeStep steps)


isStepComplete : SetupStep -> SetupStep -> Bool
isStepComplete currentStep step =
    case ( currentStep, step ) of
        ( PlanSelection, _ ) ->
            False

        ( OrganizationSetup, PlanSelection ) ->
            True

        ( OrganizationSetup, _ ) ->
            False

        ( AgentSetup, _ ) ->
            True
