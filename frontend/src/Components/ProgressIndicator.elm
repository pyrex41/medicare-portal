module Components.ProgressIndicator exposing (Step, view)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Step =
    { icon : String
    , title : String
    , description : String
    , isCompleted : Bool
    , isActive : Bool
    }


view : List Step -> Html msg
view steps =
    div [ class "fixed left-0 top-0 bottom-0 w-[280px] bg-[#0D1117] border-r border-[#1C1F26]" ]
        [ div [ class "flex flex-col h-full px-8 py-8" ]
            [ -- Logo
              div [ class "mb-14" ]
                [ div [ class "flex items-center" ]
                    [ span [ class "text-lg font-medium text-white" ]
                        [ text "Medicare" ]
                    , span [ class "text-lg font-medium text-white ml-1.5 opacity-90" ]
                        [ text "Max" ]
                    ]
                ]

            -- Steps
            , div [ class "flex-1" ]
                [ div [ class "space-y-7" ] (List.map viewStep steps)
                ]

            -- Help email
            , div [ class "text-sm text-[#8B8B8B] flex items-center mt-8" ]
                [ span [ class "mr-2.5" ] [ text "📧" ]
                , text "information@medicaremax.ai"
                ]
            ]
        ]


viewStep : Step -> Html msg
viewStep step =
    div
        [ class "flex items-start"
        , classList [ ( "opacity-50", not step.isActive && not step.isCompleted ) ]
        ]
        [ div
            [ class "shrink-0 w-6 h-6 rounded-full flex items-center justify-center mr-3 transition-all duration-300"
            , classList
                [ ( "bg-[#2563EB] text-white", step.isActive )
                , ( "bg-[#059669] text-white", step.isCompleted )
                , ( "bg-[#1F2937] text-[#9CA3AF]", not step.isActive && not step.isCompleted )
                ]
            ]
            [ span [ class "text-sm" ] [ text step.icon ]
            ]
        , div [ class "flex-1" ]
            [ h3
                [ class "text-[13px] font-medium transition-colors duration-300"
                , classList
                    [ ( "text-white", step.isActive )
                    , ( "text-white opacity-90", step.isCompleted )
                    , ( "text-[#9CA3AF]", not step.isActive && not step.isCompleted )
                    ]
                ]
                [ text step.title ]
            , p [ class "text-xs text-[#6B7280] mt-1 leading-relaxed" ]
                [ text step.description ]
            ]
        ]
