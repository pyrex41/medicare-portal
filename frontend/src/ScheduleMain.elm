module ScheduleMain exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MyIcon


type alias Model =
    { success : Bool
    , redirectUrl : String
    }


type Msg
    = CalendlyOpened


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { success = False
      , redirectUrl = "https://calendly.com/josh-musick-medicaremax/medicare-max-demo?month=2025-04"
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalendlyOpened ->
            ( { model | success = True }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Schedule a Demo - Medicare Max"
    , body =
        [ div [ class "min-h-screen bg-[#F9FAFB]" ]
            [ div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-6 sm:py-12" ]
                [ if model.success then
                    div [ class "text-center" ]
                        [ h1 [ class "text-2xl sm:text-3xl font-bold text-gray-900 mb-3 sm:mb-4" ]
                            [ text "Thank You" ]
                        , p [ class "text-gray-600 text-base sm:text-lg" ]
                            [ text "We'll be in touch soon to discuss your options." ]
                        ]

                  else
                    viewDemoCTA model
                ]
            ]
        ]
    }


viewDemoCTA : Model -> Html Msg
viewDemoCTA model =
    div [ class "flex flex-col max-w-xl mx-auto" ]
        [ div [ class "border border-[#DCE2E5] shadow-sm overflow-hidden rounded-lg" ]
            [ div [ class "bg-[#F9F5FF] p-6" ]
                [ h1 [ class "text-2xl sm:text-3xl font-extrabold text-black mb-4" ]
                    [ text "Let's Connect" ]
                , p [ class "text-black text-base leading-relaxed" ]
                    [ text "Interested in learning how you can use Medicare Max for your clients? Discover how our platform can help your agency maximize client retention, freeing up time to focus on what matters most." ]
                ]
            , div [ class "bg-white p-6 sm:p-8" ]
                [ p [ class "text-[#475467] text-sm mb-6" ]
                    [ text "Book a call with us to learn more" ]
                , div [ class "space-y-4" ]
                    [ a
                        [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                        , href model.redirectUrl
                        , target "_blank"
                        , onClick CalendlyOpened
                        ]
                        [ div [ class "flex items-center space-x-3" ]
                            [ span [ class "w-6 h-6 flex items-center justify-center" ]
                                [ MyIcon.calendarDays 24 "#03045E" ]
                            , span [ class "font-semibold text-base" ]
                                [ text "Schedule a Demo Call" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
