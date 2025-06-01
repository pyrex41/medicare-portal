module StageDemoCTA exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MyIcon
import Svg
import Svg.Attributes


type alias Model =
    { tempId : String }


type Msg
    = ScheduleDemo


init : String -> Model
init tempId =
    { tempId = tempId }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScheduleDemo ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-[#F9FAFB]" ]
        [ -- Main Content
          div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-6 sm:py-12" ]
            [ -- Success Icon
              div [ class "flex justify-center mb-8" ]
                [ div [ class "w-16 h-16 bg-green-100 rounded-full flex items-center justify-center" ]
                    [ Svg.svg
                        [ Svg.Attributes.class "w-8 h-8 text-green-600"
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.stroke "currentColor"
                        , Svg.Attributes.viewBox "0 0 24 24"
                        ]
                        [ Svg.path
                            [ Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.d "M5 13l4 4L19 7"
                            ]
                            []
                        ]
                    ]
                ]

            -- Logo
            , div [ class "flex justify-center mb-8" ]
                [ img
                    [ src "/images/medicare-max-logo.png"
                    , alt "Medicare Max"
                    , class "h-16 max-w-[240px] md:max-w-[300px] object-contain px-4"
                    ]
                    []
                ]

            -- Main Card
            , div [ class "bg-white border border-[#DCE2E5] shadow-sm overflow-hidden rounded-lg" ]
                [ -- Purple Header Section
                  div [ class "bg-[#F9F5FF] p-6" ]
                    [ h1 [ class "text-2xl sm:text-3xl font-extrabold text-black mb-4" ]
                        [ text "Congratulations! You May Qualify!" ]
                    , p [ class "text-black text-base leading-relaxed" ]
                        [ text "Based on your answers, you appear to be eligible for the Medicare plans shown." ]
                    ]

                -- White Body Section
                , div [ class "bg-white p-6 sm:p-8" ]
                    [ p [ class "text-[#667085] text-sm mb-6" ]
                        [ text "Select an Option Below" ]

                    -- What's Next Section with Checklist
                    , div [ class "mb-8" ]
                        [ h2 [ class "text-xl font-bold text-[#101828] mb-4" ]
                            [ text "What's Next?" ]

                        -- Checklist Items
                        , div [ class "space-y-3" ]
                            [ checklistItem "Create your Medicare Max account"
                            , checklistItem "Schedule a personalized onboarding call"
                            ]
                        ]

                    -- CTA Buttons
                    , div [ class "space-y-4" ]
                        [ -- Sign Up Button (primary)
                          a
                            [ href "https://medicaremax.ai/signup"
                            , target "_blank"
                            , class "flex items-center justify-between w-full px-4 py-4 bg-[#03045E] rounded-md text-white hover:bg-[#020338] transition"
                            ]
                            [ div [ class "flex items-center space-x-3" ]
                                [ span [ class "w-6 h-6 flex items-center justify-center" ]
                                    [ Svg.svg
                                        [ Svg.Attributes.class "w-6 h-6"
                                        , Svg.Attributes.fill "none"
                                        , Svg.Attributes.stroke "currentColor"
                                        , Svg.Attributes.viewBox "0 0 24 24"
                                        ]
                                        [ Svg.path
                                            [ Svg.Attributes.strokeLinecap "round"
                                            , Svg.Attributes.strokeLinejoin "round"
                                            , Svg.Attributes.strokeWidth "2"
                                            , Svg.Attributes.d "M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"
                                            ]
                                            []
                                        ]
                                    ]
                                , span [ class "font-semibold text-base" ]
                                    [ text "Sign Up for Medicare Max" ]
                                ]
                            , Svg.svg
                                [ Svg.Attributes.class "w-5 h-5"
                                , Svg.Attributes.fill "none"
                                , Svg.Attributes.stroke "currentColor"
                                , Svg.Attributes.viewBox "0 0 24 24"
                                ]
                                [ Svg.path
                                    [ Svg.Attributes.strokeLinecap "round"
                                    , Svg.Attributes.strokeLinejoin "round"
                                    , Svg.Attributes.strokeWidth "2"
                                    , Svg.Attributes.d "M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14"
                                    ]
                                    []
                                ]
                            ]
                        
                        -- Schedule Onboarding Button (secondary)
                        , a
                            [ href "https://calendly.com/medicare-max-team/medicare-max-demo"
                            , target "_blank"
                            , class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                            ]
                            [ div [ class "flex items-center space-x-3" ]
                                [ span [ class "w-6 h-6 flex items-center justify-center" ]
                                    [ MyIcon.calendarDays 24 "#03045E" ]
                                , span [ class "font-semibold text-base" ]
                                    [ text "Schedule Your Medicare Max Onboarding" ]
                                ]
                            , Svg.svg
                                [ Svg.Attributes.class "w-5 h-5"
                                , Svg.Attributes.fill "none"
                                , Svg.Attributes.stroke "currentColor"
                                , Svg.Attributes.viewBox "0 0 24 24"
                                ]
                                [ Svg.path
                                    [ Svg.Attributes.strokeLinecap "round"
                                    , Svg.Attributes.strokeLinejoin "round"
                                    , Svg.Attributes.strokeWidth "2"
                                    , Svg.Attributes.d "M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14"
                                    ]
                                    []
                                ]
                            ]
                        ]

                    -- Limited Time Offer
                    , p [ class "text-center mt-8 text-sm text-[#667085]" ]
                        [ span [ class "font-semibold" ] [ text "Limited time offer: " ]
                        , text "Schedule now to lock in your discounted rates!"
                        ]
                    ]
                ]
            ]
        ]


checklistItem : String -> Html msg
checklistItem text_ =
    div [ class "flex items-start gap-3" ]
        [ div [ class "flex-shrink-0 mt-0.5" ]
            [ Svg.svg
                [ Svg.Attributes.class "w-5 h-5 text-green-500"
                , Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "currentColor"
                , Svg.Attributes.viewBox "0 0 24 24"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.d "M5 13l4 4L19 7"
                    ]
                    []
                ]
            ]
        , p [ class "text-[#475467] text-base" ] [ text text_ ]
        ]
