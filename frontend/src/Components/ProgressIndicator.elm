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
    div [ class "fixed left-0 top-0 bottom-0 w-[280px] bg-white border-r border-[#eaecf0] overflow-y-auto" ]
        [ div [ class "flex flex-col h-full px-8 py-8" ]
            [ -- Logo
              div [ class "mb-14" ]
                [ div [ class "flex items-center" ]
                    [ a
                        [ href "/"
                        , class "cursor-pointer"
                        ]
                        [ img
                            [ src "/images/medicare-max-logo.png"
                            , class "h-8 w-auto"
                            , alt "Medicare Max logo"
                            ]
                            []
                        ]
                    ]
                ]

            -- Steps
            , div [ class "flex-1" ]
                [ div [ class "space-y-7" ] (List.map viewStep steps)
                ]

            -- Help email
            , div [ class "text-sm text-[#667085] flex items-center mt-8" ]
                [ span [ class "mr-2" ] [ text "✉️" ]
                , text "help@medicaremax.com"
                ]
            ]
        ]


viewStep : Step -> Html msg
viewStep step =
    div
        [ class "flex items-start"
        , classList [ ( "opacity-60", not step.isActive && not step.isCompleted ) ]
        ]
        [ div
            [ class "shrink-0 w-8 h-8 rounded-full flex items-center justify-center mr-3 transition-all duration-300"
            , classList
                [ ( "bg-[#03045e] text-white", step.isActive )
                , ( "bg-[#03045e]/90 text-white", step.isCompleted )
                , ( "bg-[#f9fafb] text-[#667085] border border-[#eaecf0]", not step.isActive && not step.isCompleted )
                ]
            ]
            [ span [ class "text-base" ] [ text step.icon ]
            ]
        , div [ class "flex-1" ]
            [ h3
                [ class "text-sm font-medium transition-colors duration-300"
                , classList
                    [ ( "text-[#101828]", step.isActive )
                    , ( "text-[#101828]/90", step.isCompleted )
                    , ( "text-[#667085]", not step.isActive && not step.isCompleted )
                    ]
                ]
                [ text step.title ]
            , p [ class "text-sm text-[#667085] mt-1 leading-relaxed" ]
                [ text step.description ]
            ]
        ]
