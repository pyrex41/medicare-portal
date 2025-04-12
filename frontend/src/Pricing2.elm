module Pricing2 exposing (Model, Msg, init, subscriptions, update, view)

import Basics
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MyIcon
import PriceModel


type alias Model =
    { calculationInputs : PriceModel.CalculationInputs
    , calculatorExpanded : Bool
    , activePreset : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { calculationInputs =
            { contacts = 1000
            , averageAge = 3.0
            , rolloverPercent = 7
            , commissionRate = 300
            }
      , calculatorExpanded = True
      , activePreset = Nothing
      }
    , Cmd.none
    )


type Msg
    = ContactCountChanged Int
    | RolloverPercentChanged Float
    | CommissionRateChanged Float
    | ToggleCalculator
    | SelectPreset Int


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
            ( { model
                | calculationInputs = newCalculationInputs
                , activePreset = Nothing
              }
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

        SelectPreset value ->
            let
                oldCalculationInputs =
                    model.calculationInputs

                newCalculationInputs =
                    { oldCalculationInputs | contacts = value }
            in
            ( { model
                | calculationInputs = newCalculationInputs
                , activePreset = Just value
              }
            , Cmd.none
            )



-- Helper functions


formatNumber : Float -> String
formatNumber value =
    if value == toFloat (round value) then
        -- It's a whole number - show no decimals
        addCommas (String.fromInt (round value))

    else
        -- Show with appropriate precision
        String.fromFloat (round10 1 value)


formatCurrency : Float -> String
formatCurrency value =
    "$" ++ formatNumber value


addCommas : String -> String
addCommas str =
    if String.length str <= 3 then
        str

    else
        let
            -- Recursively add commas
            addCommasHelper : String -> String -> String
            addCommasHelper acc remaining =
                if String.length remaining <= 3 then
                    remaining ++ acc

                else
                    let
                        len =
                            String.length remaining

                        front =
                            String.dropRight 3 remaining

                        back =
                            String.right 3 remaining
                    in
                    addCommasHelper ("," ++ back ++ acc) front
        in
        addCommasHelper "" str


round10 : Int -> Float -> Float
round10 n value =
    let
        factor =
            10 ^ n |> toFloat
    in
    (value * factor) |> round |> toFloat |> (\x -> x / factor)



-- New pricing calculation function based on MedicareMax pricing tiers


calculatePricing : Int -> { bundles : Int, pricePerBundle : Float, totalPrice : Float }
calculatePricing contacts =
    let
        -- Calculate bundles (250 contacts per bundle)
        bundles =
            Basics.max 1 (ceiling (toFloat contacts / 250))

        -- Determine the price per bundle based on contact volume
        pricePerBundle =
            if contacts >= 5000 then
                25
                -- $25 per bundle for 5000+ contacts

            else if contacts >= 1000 then
                30
                -- $30 per bundle for 1000-4999 contacts

            else
                35

        -- $35 per bundle for under 1000 contacts
        totalPrice =
            toFloat (bundles * round pricePerBundle)
    in
    { bundles = bundles
    , pricePerBundle = pricePerBundle
    , totalPrice = totalPrice
    }



-- Calculate enhanced revenue metrics based on MedicareMax model


calculateEnhancedRevenue : PriceModel.CalculationInputs -> { price : Float, annualPrice : Float, monthlyConverted : Float, annualConverted : Float, monthlyLtv : Float, annualLtv : Float, roi : Float, netBenefit : Float }
calculateEnhancedRevenue inputs =
    let
        -- Constants from MedicareMax model
        annualConversionRate =
            inputs.rolloverPercent / 100

        -- Using rollover percent as conversion rate
        monthlyRate =
            monthlyConversionRate inputs

        contactLtv =
            inputs.commissionRate

        -- Pricing calculation
        pricing =
            calculatePricing inputs.contacts

        -- Converted contacts
        monthlyConverted =
            toFloat inputs.contacts * monthlyRate

        annualConverted =
            toFloat inputs.contacts * annualConversionRate

        -- LTV calculations
        monthlyLtv =
            monthlyConverted * contactLtv

        annualLtv =
            annualConverted * contactLtv

        -- ROI and benefit
        annualPrice =
            pricing.totalPrice * 12

        roi =
            annualLtv / annualPrice

        netBenefit =
            annualLtv - annualPrice
    in
    { price = pricing.totalPrice
    , annualPrice = annualPrice
    , monthlyConverted = monthlyConverted
    , annualConverted = annualConverted
    , monthlyLtv = monthlyLtv
    , annualLtv = annualLtv
    , roi = roi
    , netBenefit = netBenefit
    }



-- Create chart data


createChartData : Float -> Float -> List { x : Float, cost : Float, benefit : Float }
createChartData annualPrice annualLtv =
    [ { x = 1, cost = annualPrice, benefit = annualLtv } ]



-- Main view


view : Model -> Html Msg
view model =
    let
        pricing =
            calculatePricing model.calculationInputs.contacts

        revenue =
            calculateEnhancedRevenue model.calculationInputs

        pricePerContact =
            if model.calculationInputs.contacts > 0 then
                pricing.totalPrice / toFloat model.calculationInputs.contacts

            else
                0

        chartData =
            createChartData revenue.annualPrice revenue.annualLtv
    in
    div [ class "min-h-screen bg-gray-50 flex flex-col items-center py-12 px-4 sm:px-6 lg:px-8" ]
        [ div [ class "max-w-6xl w-full space-y-8 bg-white p-8 rounded-lg shadow-md" ]
            [ div [ class "flex flex-col items-center" ]
                [ MyIcon.banknote 32 "#0F172A"
                , h2 [ class "text-2xl font-semibold text-gray-900 mt-6" ] [ text "MedicareMax Pricing Calculator" ]
                , p [ class "text-gray-500 mt-2 mb-6" ] [ text "Understand your costs and potential value from our service" ]

                -- Contact Input Section
                , div [ class "w-full max-w-5xl mt-6" ]
                    [ div [ class "mb-4" ]
                        [ label [ class "block text-gray-700 text-sm font-bold mb-2", for "contacts" ]
                            [ text "Number of Contacts:" ]
                        , div [ class "flex items-center" ]
                            [ input
                                [ id "contacts"
                                , type_ "number"
                                , value (String.fromInt model.calculationInputs.contacts)
                                , onInput (\str -> ContactCountChanged (String.toInt str |> Maybe.withDefault 0))
                                , class "shadow appearance-none border rounded py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline w-full"
                                , Html.Attributes.min "0"
                                ]
                                []
                            ]

                        -- Preset buttons
                        , div [ class "mt-3 flex flex-wrap gap-2" ]
                            ([ 250, 1000, 5000, 10000, 20000, 40000 ]
                                |> List.map
                                    (\presetValue ->
                                        let
                                            isActive =
                                                model.activePreset == Just presetValue

                                            baseClass =
                                                "hover:bg-blue-200 text-blue-800 font-semibold py-1 px-3 rounded-full text-sm"

                                            activeClass =
                                                "bg-blue-500 text-white"

                                            inactiveClass =
                                                "bg-blue-100"
                                        in
                                        button
                                            [ onClick (SelectPreset presetValue)
                                            , class
                                                (baseClass
                                                    ++ " "
                                                    ++ (if isActive then
                                                            activeClass

                                                        else
                                                            inactiveClass
                                                       )
                                                )
                                            ]
                                            [ text (formatNumber (toFloat presetValue)) ]
                                    )
                            )
                        ]

                    -- Pricing Tiers - Horizontal Layout
                    , div [ class "flex flex-row mb-4 gap-4" ]
                        [ div
                            [ class
                                ("flex-1 p-3 border rounded-lg bg-white "
                                    ++ (if model.calculationInputs.contacts < 1000 then
                                            "border-blue-300 shadow-md"

                                        else
                                            "border-gray-200"
                                       )
                                )
                            ]
                            [ div [ class "flex flex-col" ]
                                [ div [ class "flex items-center justify-between mb-1" ]
                                    [ h3 [ class "font-bold text-sm" ] [ text "Under 1,000 contacts" ]
                                    , if model.calculationInputs.contacts < 1000 then
                                        span [ class "bg-blue-100 text-blue-800 text-xs px-2 py-1 rounded-full" ]
                                            [ text "Your tier" ]

                                      else
                                        text ""
                                    ]
                                , p [ class "text-xl font-bold text-gray-800" ]
                                    [ text "$35"
                                    , span [ class "text-sm font-normal text-gray-500" ] [ text "/bundle" ]
                                    ]
                                ]
                            ]
                        , div
                            [ class
                                ("flex-1 p-3 border rounded-lg bg-white "
                                    ++ (if model.calculationInputs.contacts >= 1000 && model.calculationInputs.contacts < 5000 then
                                            "border-blue-300 shadow-md"

                                        else
                                            "border-gray-200"
                                       )
                                )
                            ]
                            [ div [ class "flex flex-col" ]
                                [ div [ class "flex items-center justify-between mb-1" ]
                                    [ h3 [ class "font-bold text-sm" ] [ text "1,000 - 4,999 contacts" ]
                                    , if model.calculationInputs.contacts >= 1000 && model.calculationInputs.contacts < 5000 then
                                        span [ class "bg-blue-100 text-blue-800 text-xs px-2 py-1 rounded-full" ]
                                            [ text "Your tier" ]

                                      else
                                        text ""
                                    ]
                                , p [ class "text-xl font-bold text-gray-800" ]
                                    [ text "$30"
                                    , span [ class "text-sm font-normal text-gray-500" ] [ text "/bundle" ]
                                    ]
                                ]
                            ]
                        , div
                            [ class
                                ("flex-1 p-3 border rounded-lg bg-white "
                                    ++ (if model.calculationInputs.contacts >= 5000 then
                                            "border-blue-300 shadow-md"

                                        else
                                            "border-gray-200"
                                       )
                                )
                            ]
                            [ div [ class "flex flex-col" ]
                                [ div [ class "flex items-center justify-between mb-1" ]
                                    [ h3 [ class "font-bold text-sm" ] [ text "5,000+ contacts" ]
                                    , if model.calculationInputs.contacts >= 5000 then
                                        span [ class "bg-blue-100 text-blue-800 text-xs px-2 py-1 rounded-full" ]
                                            [ text "Your tier" ]

                                      else
                                        text ""
                                    ]
                                , p [ class "text-xl font-bold text-gray-800" ]
                                    [ text "$25"
                                    , span [ class "text-sm font-normal text-gray-500" ] [ text "/bundle" ]
                                    ]
                                ]
                            ]
                        ]

                    -- Value Analysis Section
                    , div [ class "mb-6 bg-gradient-to-r from-purple-50 to-blue-50 rounded-lg p-4 border border-purple-100" ]
                        [ h2 [ class "text-xl font-bold text-gray-800 mb-3" ] [ text "Value Analysis" ]
                        , p [ class "text-gray-600 mb-3 text-sm" ]
                            [ text
                                ("Based on "
                                    ++ String.fromFloat model.calculationInputs.rolloverPercent
                                    ++ "% of contacts changing their policy over the course of a year, with each change adding $"
                                    ++ formatNumber model.calculationInputs.commissionRate
                                    ++ " in LTV"
                                )
                            ]
                        , div [ class "grid grid-cols-1 md:grid-cols-2 gap-4" ]
                            [ div [ class "bg-white rounded-lg p-3 shadow-sm border border-gray-200" ]
                                [ h3 [ class "font-bold text-gray-700 mb-2 text-sm" ] [ text "Monthly Breakdown" ]
                                , div [ class "space-y-2 text-sm" ]
                                    [ div [ class "flex justify-between items-center" ]
                                        [ span [ class "text-gray-600" ] [ text "Monthly investment:" ]
                                        , span [ class "font-bold" ] [ text (formatCurrency revenue.price) ]
                                        ]
                                    , div [ class "flex justify-between items-center" ]
                                        [ span [ class "text-gray-600" ] [ text "Monthly converted contacts:" ]
                                        , span [ class "font-bold" ]
                                            [ text (formatNumber revenue.monthlyConverted ++ " ")
                                            , span [ class "text-xs font-normal text-gray-500" ]
                                                [ text
                                                    ("("
                                                        ++ formatNumber (monthlyConversionRate model.calculationInputs * 100)
                                                        ++ "% of "
                                                        ++ formatNumber (toFloat model.calculationInputs.contacts)
                                                        ++ ")"
                                                    )
                                                ]
                                            ]
                                        ]
                                    , div [ class "flex justify-between items-center text-purple-700" ]
                                        [ span [] [ text "New LTV Added Monthly:" ]
                                        , span [ class "font-bold" ] [ text (formatCurrency revenue.monthlyLtv) ]
                                        ]
                                    ]
                                ]
                            , div [ class "bg-white rounded-lg p-3 shadow-sm border border-gray-200" ]
                                [ h3 [ class "font-bold text-gray-700 mb-2 text-sm" ] [ text "Annual Impact" ]
                                , div [ class "space-y-2 text-sm" ]
                                    [ div [ class "flex justify-between items-center" ]
                                        [ span [ class "text-gray-600" ] [ text "Annual investment:" ]
                                        , span [ class "font-bold" ] [ text (formatCurrency revenue.annualPrice) ]
                                        ]
                                    , div [ class "flex justify-between items-center" ]
                                        [ span [ class "text-gray-600" ] [ text "Annual converted contacts:" ]
                                        , span [ class "font-bold" ]
                                            [ text (formatNumber revenue.annualConverted ++ " ")
                                            , span [ class "text-xs font-normal text-gray-500" ]
                                                [ text
                                                    ("("
                                                        ++ formatNumber model.calculationInputs.rolloverPercent
                                                        ++ "% of "
                                                        ++ formatNumber (toFloat model.calculationInputs.contacts)
                                                        ++ ")"
                                                    )
                                                ]
                                            ]
                                        ]
                                    , div [ class "flex justify-between items-center text-green-700" ]
                                        [ span [] [ text "New LTV Added Annually:" ]
                                        , span [ class "font-bold" ] [ text (formatCurrency revenue.annualLtv) ]
                                        ]
                                    , div [ class "flex justify-between items-center pt-1 mt-1 border-t border-gray-200" ]
                                        [ span [ class "text-gray-600" ] [ text "Return on investment:" ]
                                        , span [ class "font-bold text-purple-700" ] [ text (formatNumber revenue.roi ++ "x") ]
                                        ]
                                    , div [ class "flex justify-between items-center" ]
                                        [ span [ class "text-gray-600" ] [ text "Net annual benefit:" ]
                                        , span [ class "font-bold text-green-700" ] [ text (formatCurrency revenue.netBenefit) ]
                                        ]
                                    ]
                                ]
                            ]
                        ]

                    -- Plan Summary and Chart
                    , div [ class "flex flex-col md:flex-row gap-6 mb-6" ]
                        [ div [ class "w-full md:w-1/3 bg-gray-50 rounded-lg p-4" ]
                            [ h2 [ class "text-lg font-bold text-gray-800 mb-3" ] [ text "Your Plan Summary" ]
                            , div [ class "space-y-3" ]
                                [ div [ class "flex justify-between items-center" ]
                                    [ span [ class "text-gray-600" ] [ text "Price per bundle:" ]
                                    , span [ class "font-bold" ] [ text (formatCurrency pricing.pricePerBundle) ]
                                    ]
                                , div [ class "flex justify-between items-center" ]
                                    [ span [ class "text-gray-600" ] [ text "Contact bundles:" ]
                                    , span [ class "font-bold" ] [ text (String.fromInt pricing.bundles) ]
                                    ]
                                , div [ class "flex justify-between items-center" ]
                                    [ span [ class "text-gray-600" ] [ text "Total contacts:" ]
                                    , span [ class "font-bold" ] [ text (formatNumber (toFloat model.calculationInputs.contacts)) ]
                                    ]
                                , div [ class "flex justify-between items-center" ]
                                    [ span [ class "text-gray-600" ] [ text "Price per contact:" ]
                                    , span [ class "font-bold" ] [ text (formatCurrency pricePerContact) ]
                                    ]
                                ]
                            , div [ class "mt-4 pt-4 border-t border-gray-200" ]
                                [ div [ class "bg-blue-600 rounded-lg p-4 text-white text-center" ]
                                    [ h2 [ class "font-bold mb-1 text-sm" ] [ text "Monthly Price" ]
                                    , div [ class "text-2xl font-bold" ] [ text (formatCurrency pricing.totalPrice) ]
                                    , p [ class "text-xs mt-1 text-blue-100" ]
                                        [ text ("For " ++ formatNumber (toFloat model.calculationInputs.contacts) ++ " contacts") ]
                                    ]
                                ]
                            ]
                        , div [ class "w-full md:w-2/3" ]
                            [ div [ class "h-[300px] bg-white border border-gray-200 rounded-lg p-4" ]
                                [ h2 [ class "text-lg font-bold text-gray-800 mb-3" ] [ text "Cost vs. Value Comparison" ]
                                , C.chart
                                    [ CA.height 240
                                    , CA.width 400
                                    , CA.margin { top = 10, bottom = 30, left = 60, right = 10 }
                                    , CA.padding { top = 10, bottom = 30, left = 10, right = 10 }
                                    ]
                                    [ C.yLabels
                                        [ CA.withGrid
                                        , CA.amount 5
                                        , CA.format (\n -> "$" ++ formatNumber n)
                                        ]
                                    , C.bars
                                        [ CA.x1 .x
                                        , CA.margin 0.2
                                        ]
                                        [ C.bar .cost [ CA.color "#3B82F6" ]
                                        , C.bar .benefit [ CA.color "#8B5CF6" ]
                                        ]
                                        chartData
                                    ]
                                , div [ class "flex justify-center mt-4 space-x-6" ]
                                    [ div [ class "flex items-center" ]
                                        [ div [ class "w-3 h-3 rounded-full bg-blue-500 mr-2" ] []
                                        , text "Annual Cost"
                                        ]
                                    , div [ class "flex items-center" ]
                                        [ div [ class "w-3 h-3 rounded-full bg-purple-500 mr-2" ] []
                                        , text "Annual Value"
                                        ]
                                    ]
                                ]
                            ]
                        ]

                    -- How Pricing Works
                    , div [ class "bg-gray-50 p-3 rounded-lg text-sm" ]
                        [ div [ class "flex items-center justify-between" ]
                            [ h3 [ class "text-base font-bold text-gray-800" ] [ text "How Our Pricing Works" ]
                            , p [ class "text-gray-600" ] [ text "Each bundle contains 250 contacts" ]
                            ]
                        , div [ class "mt-3 grid grid-cols-3 gap-3" ]
                            [ div [ class "bg-white p-2 rounded border border-gray-200" ]
                                [ p [ class "font-bold text-gray-700" ] [ text "Under 1,000 contacts" ]
                                , p [ class "text-gray-600" ] [ text "$35 per bundle" ]
                                ]
                            , div [ class "bg-white p-2 rounded border border-gray-200" ]
                                [ p [ class "font-bold text-gray-700" ] [ text "1,000 - 4,999 contacts" ]
                                , p [ class "text-gray-600" ] [ text "$30 per bundle" ]
                                ]
                            , div [ class "bg-white p-2 rounded border border-gray-200" ]
                                [ p [ class "font-bold text-gray-700" ] [ text "5,000+ contacts" ]
                                , p [ class "text-gray-600" ] [ text "$25 per bundle" ]
                                ]
                            ]
                        , div [ class "mt-3 p-2 bg-purple-50 rounded border border-purple-200" ]
                            [ p [ class "text-purple-700" ]
                                [ strong [] [ text "Value Analysis Assumptions: " ]
                                , text
                                    (String.fromFloat model.calculationInputs.rolloverPercent
                                        ++ "% of contacts change policy annually, each adding $"
                                        ++ formatNumber model.calculationInputs.commissionRate
                                        ++ " in LTV"
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



-- Helper function to calculate monthly conversion rate


monthlyConversionRate : PriceModel.CalculationInputs -> Float
monthlyConversionRate inputs =
    inputs.rolloverPercent / (100 * 12)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
