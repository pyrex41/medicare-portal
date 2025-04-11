module Pricing exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, h2, h3, h4, img, input, label, p, span, text)
import Html.Attributes exposing (class, for, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import MyIcon
import PriceModel


type alias Model =
    { calculationInputs : PriceModel.CalculationInputs
    , calculatorExpanded : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { calculationInputs =
            { contacts = 1000
            , averageAge = 3.0
            , rolloverPercent = 7
            , commissionRate = 300
            }
      , calculatorExpanded = False
      }
    , Cmd.none
    )


type Msg
    = ContactCountChanged Int
    | RolloverPercentChanged Float
    | CommissionRateChanged Float
    | ToggleCalculator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ContactCountChanged count ->
            let
                oldCalculationInputs =
                    model.calculationInputs

                newCalculationInputs =
                    { oldCalculationInputs | contacts = count }
            in
            ( { model | calculationInputs = newCalculationInputs }
            , Cmd.none
            )

        RolloverPercentChanged percent ->
            let
                oldCalculationInputs =
                    model.calculationInputs

                newCalculationInputs =
                    { oldCalculationInputs | rolloverPercent = percent }
            in
            ( { model | calculationInputs = newCalculationInputs }
            , Cmd.none
            )

        CommissionRateChanged rate ->
            let
                oldCalculationInputs =
                    model.calculationInputs

                newCalculationInputs =
                    { oldCalculationInputs | commissionRate = rate }
            in
            ( { model | calculationInputs = newCalculationInputs }
            , Cmd.none
            )

        ToggleCalculator ->
            ( { model | calculatorExpanded = not model.calculatorExpanded }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-50 flex flex-col items-center py-12 px-4 sm:px-6 lg:px-8" ]
        [ div [ class "flex flex-col items-center w-full mb-8" ]
            [ img [ Html.Attributes.src "/images/medicare-max-logo.png", class "h-10 w-auto" ] [] ]
        , div [ class "max-w-6xl w-full space-y-8 bg-white p-8 rounded-lg shadow-md" ]
            [ div [ class "flex flex-col items-center" ]
                [ MyIcon.banknote 32 "#0F172A"
                , h2 [ class "text-2xl font-semibold text-gray-900 mt-6" ] [ text "Subscription Pricing" ]
                , p [ class "text-gray-500 mt-2 mb-6" ] [ text "Simple and transparent." ]
                , div [ class "w-full max-w-3xl mt-6" ]
                    [ div [ class "flex flex-col md:flex-row gap-6" ]
                        [ div [ class "bg-white overflow-hidden shadow rounded-lg divide-y divide-gray-200 md:w-1/2" ]
                            [ div [ class "px-4 py-5 sm:px-6 bg-indigo-50" ]
                                [ h3 [ class "text-lg leading-6 font-medium text-gray-900" ]
                                    [ text "Base Subscription" ]
                                ]
                            , div [ class "px-4 py-5 sm:p-6" ]
                                [ div [ class "flex items-center justify-between" ]
                                    [ div [ class "flex items-center" ]
                                        [ span [ class "text-3xl font-bold text-gray-900" ] [ text "$60" ]
                                        , span [ class "ml-2 text-gray-500" ] [ text "/month" ]
                                        ]
                                    , span [ class "bg-green-100 text-green-800 px-2 py-1 rounded-full text-sm font-medium" ]
                                        [ text "First 500 contacts" ]
                                    ]
                                , div [ class "mt-4" ]
                                    [ p [ class "text-sm text-gray-500" ]
                                        [ text "Our base subscription includes all features of the Medicare Max portal platform and allows you to automate retention of up to 500 contacts." ]
                                    ]
                                ]
                            ]
                        , div [ class "bg-white overflow-hidden shadow rounded-lg divide-y divide-gray-200 md:w-1/2" ]
                            [ div [ class "px-4 py-5 sm:px-6 bg-indigo-50" ]
                                [ h3 [ class "text-lg leading-6 font-medium text-gray-900" ]
                                    [ text "Additional Contacts" ]
                                ]
                            , div [ class "px-4 py-5 sm:p-6" ]
                                [ div [ class "flex items-center justify-between" ]
                                    [ div [ class "flex items-center" ]
                                        [ span [ class "text-3xl font-bold text-gray-900" ] [ text "$40" ]
                                        , span [ class "ml-2 text-gray-500" ] [ text "/month" ]
                                        ]
                                    , span [ class "bg-blue-100 text-blue-800 px-2 py-1 rounded-full text-sm font-medium" ]
                                        [ text "per 500 contacts" ]
                                    ]
                                , div [ class "mt-4" ]
                                    [ p [ class "text-sm text-gray-500" ]
                                        [ text "For every additional 500 contacts (or portion thereof), we charge $40 per month." ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "bg-white overflow-hidden shadow rounded-lg divide-y divide-gray-200 mt-6" ]
                        [ div
                            [ class "px-4 py-5 sm:px-6 bg-indigo-50 cursor-pointer hover:bg-indigo-100 transition-colors"
                            , onClick ToggleCalculator
                            ]
                            [ div [ class "flex justify-between items-center" ]
                                [ h3 [ class "text-lg leading-6 font-medium text-gray-900" ]
                                    [ text "Price & Revenue Calculator" ]
                                , div
                                    [ class "transform transition-transform duration-200"
                                    , class
                                        (if model.calculatorExpanded then
                                            "-rotate-90"

                                         else
                                            "rotate-90"
                                        )
                                    ]
                                    [ MyIcon.chevronRight 24 "#4A5568" ]
                                ]
                            ]
                        , if model.calculatorExpanded then
                            div [ class "px-4 py-5 sm:p-6" ]
                                [ div [ class "space-y-6" ]
                                    [ div [ class "bg-white p-4 rounded-md shadow-sm border border-gray-200" ]
                                        [ h4 [ class "text-md font-medium text-gray-800 mb-3" ] [ text "Inputs" ]
                                        , div [ class "flex flex-wrap gap-4" ]
                                            [ div [ class "flex flex-col" ]
                                                [ label
                                                    [ class "block text-sm font-medium text-gray-700 mb-1 cursor-pointer"
                                                    , for "contact-count"
                                                    ]
                                                    [ text "Number of Contacts" ]
                                                , div [ class "w-[140px]" ]
                                                    [ input
                                                        [ class "w-full border border-gray-300 rounded-md shadow-sm py-1.5 px-2 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm text-center"
                                                        , id "contact-count"
                                                        , type_ "number"
                                                        , placeholder "Enter number"
                                                        , value (String.fromInt model.calculationInputs.contacts)
                                                        , onInput (\str -> ContactCountChanged (String.toInt str |> Maybe.withDefault 0))
                                                        , Html.Attributes.min "0"
                                                        , Html.Attributes.step "1"
                                                        ]
                                                        []
                                                    ]
                                                ]
                                            , div [ class "flex flex-col" ]
                                                [ label
                                                    [ class "block text-sm font-medium text-gray-700 mb-1 cursor-pointer"
                                                    , for "commission-rate"
                                                    ]
                                                    [ text "Commission Per Contact" ]
                                                , div [ class "flex rounded-md shadow-sm w-[140px]" ]
                                                    [ div [ class "flex-shrink-0 inline-flex items-center px-2 rounded-l-md border border-r-0 border-gray-300 bg-indigo-100 text-indigo-800 text-sm font-medium" ]
                                                        [ text "$" ]
                                                    , input
                                                        [ class "w-full border border-gray-300 rounded-none rounded-r-md shadow-sm py-1.5 px-2 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm text-center"
                                                        , id "commission-rate"
                                                        , type_ "number"
                                                        , placeholder "Commission"
                                                        , value (String.fromFloat model.calculationInputs.commissionRate)
                                                        , onInput (\str -> CommissionRateChanged (String.toFloat str |> Maybe.withDefault 0))
                                                        , Html.Attributes.min "0"
                                                        , Html.Attributes.step "5"
                                                        ]
                                                        []
                                                    ]
                                                ]
                                            , div [ class "flex flex-col" ]
                                                [ label
                                                    [ class "block text-sm font-medium text-gray-700 mb-1 cursor-pointer"
                                                    , for "rollover-percent"
                                                    ]
                                                    [ text "Annual Rollover" ]
                                                , div [ class "flex rounded-md shadow-sm w-[140px]" ]
                                                    [ div [ class "flex-shrink-0 inline-flex items-center px-2 rounded-l-md border border-r-0 border-gray-300 bg-indigo-100 text-indigo-800 text-sm font-medium" ]
                                                        [ text "%" ]
                                                    , input
                                                        [ class "w-full border border-gray-300 rounded-none rounded-r-md shadow-sm py-1.5 px-2 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm text-center"
                                                        , id "rollover-percent"
                                                        , type_ "number"
                                                        , placeholder "Rollover"
                                                        , value (String.fromFloat model.calculationInputs.rolloverPercent)
                                                        , onInput (\str -> RolloverPercentChanged (String.toFloat str |> Maybe.withDefault 0))
                                                        , Html.Attributes.min "0"
                                                        , Html.Attributes.max "100"
                                                        , Html.Attributes.step "0.1"
                                                        ]
                                                        []
                                                    ]
                                                ]
                                            ]
                                        ]
                                    , PriceModel.view model.calculationInputs
                                    ]
                                ]

                          else
                            text ""
                        ]
                    ]
                ]
            ]
        ]
