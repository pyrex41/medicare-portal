module Pricing2 exposing (Model, Msg, init, subscriptions, update, view)

import Basics
import Chart as C
import Chart.Attributes as CA
import Chart.Item as CI
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import MyIcon
import PriceModel
import Svg
import Svg.Attributes as SA


type alias Model =
    { calculationInputs : PriceModel.CalculationInputs
    , calculatorExpanded : Bool
    , activePreset : Maybe Int
    }


type alias Pricing =
    { contacts : Int
    , price : Float
    }


basePricing =
    { contacts = 0
    , price = 50
    }


tier1Pricing =
    { contacts = 250
    , price = 0.15
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


addCommas : String -> String
addCommas str =
    let
        parts =
            String.split "." str

        beforeDecimal =
            List.head parts |> Maybe.withDefault ""

        afterDecimal =
            List.tail parts |> Maybe.withDefault [] |> List.head |> Maybe.withDefault ""

        formatBeforeDecimal s =
            if String.length s <= 3 then
                s

            else
                let
                    reversedDigits =
                        String.reverse s

                    withCommas =
                        reversedDigits
                            |> String.toList
                            |> List.indexedMap
                                (\i c ->
                                    if i > 0 && modBy 3 i == 0 then
                                        [ ',', c ]

                                    else
                                        [ c ]
                                )
                            |> List.concat
                            |> String.fromList
                            |> String.reverse
                in
                withCommas
    in
    if String.isEmpty afterDecimal then
        formatBeforeDecimal beforeDecimal

    else
        formatBeforeDecimal beforeDecimal ++ "." ++ afterDecimal


round10 : Int -> Float -> Float
round10 n value =
    let
        factor =
            10 ^ n |> toFloat
    in
    (value * factor) |> round |> toFloat |> (\x -> x / factor)



-- New graduated pricing calculation function


calculatePricing : Int -> { basePrice : Float, tierPrices : List { contacts : Int, price : Float }, totalPrice : Float }
calculatePricing contacts =
    let
        baseSubscription =
            basePricing.price

        -- Calculate price for contacts above base threshold
        additionalContacts =
            if contacts <= tier1Pricing.contacts then
                0

            else
                contacts - tier1Pricing.contacts

        additionalPrice =
            toFloat additionalContacts * tier1Pricing.price

        totalPrice =
            baseSubscription + additionalPrice

        -- Create list of tier prices for display
        tierPrices =
            [ { contacts = additionalContacts
              , price = additionalPrice
              }
            ]
    in
    { basePrice = baseSubscription
    , tierPrices = tierPrices
    , totalPrice = totalPrice
    }



-- Calculate enhanced revenue metrics based on MedicareMax model


type alias EnhancedRevenue =
    { price : Float
    , annualPrice : Float
    , monthlyConverted : Float
    , annualConverted : Float
    , monthlyLtv : Float
    , annualLtv : Float
    , roi : Float
    , netBenefit : Float
    }


calculateEnhancedRevenue : PriceModel.CalculationInputs -> EnhancedRevenue
calculateEnhancedRevenue inputs =
    let
        -- Constants from MedicareMax model
        annualConversionRate =
            inputs.rolloverPercent / 100

        -- Using rollover percent as conversion rate
        monthlyRate =
            monthlyConversionRate inputs

        contactLtv =
            inputs.commissionRate * 3

        -- Pricing calculation
        pricing =
            calculatePricing inputs.contacts

        -- Converted contacts
        monthlyConverted =
            toFloat inputs.contacts * monthlyRate |> round |> toFloat

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
    in
    div [ class "min-h-screen bg-gray-50 flex flex-col items-center py-0 md:py-12 px-4 sm:px-6 lg:px-8" ]
        [ div [ class "max-w-6xl w-full space-y-8 bg-white p-4 sm:p-8 rounded-lg shadow-md" ]
            [ div [ class "flex flex-col items-center" ]
                [ MyIcon.banknote 32 "#0F172A"
                , h2 [ class "text-xl sm:text-2xl font-semibold text-gray-900 mt-6" ] [ text "Subscription Pricing" ]
                , p [ class "text-gray-500 mt-2 mb-6 text-center" ] [ text "Transparent pricing. Pay for what you use." ]

                -- Pricing Tiers - Responsive Layout
                , div [ class "w-full flex flex-col md:flex-row gap-4 sm:gap-6 mb-8 sm:mb-12" ]
                    [ div [ class "w-full md:w-1/2 p-4 sm:p-5 border rounded-lg bg-white shadow-sm" ]
                        [ div [ class "flex flex-col" ]
                            [ div [ class "flex justify-between items-center mb-3" ]
                                [ h3 [ class "font-bold text-lg sm:text-xl text-gray-800" ] [ text "Base Subscription" ]
                                , span [ class "px-3 py-1 bg-green-100 text-green-800 text-sm rounded-full" ]
                                    [ text "Includes 250 contacts" ]
                                ]
                            , div [ class "flex items-baseline gap-2 mb-3" ]
                                [ span [ class "text-2xl sm:text-3xl font-bold text-gray-900" ] [ text "$50" ]
                                , span [ class "text-gray-600" ] [ text "/month" ]
                                ]
                            , p [ class "text-gray-600 text-sm" ]
                                [ text "Includes all features of the Medicare Max portal platform." ]
                            ]
                        ]
                    , div [ class "w-full md:w-1/2 p-4 sm:p-5 border rounded-lg bg-white shadow-sm" ]
                        [ div [ class "flex flex-col" ]
                            [ div [ class "flex justify-between items-center mb-3" ]
                                [ h3 [ class "font-bold text-lg sm:text-xl text-gray-800" ] [ text "Additional Contacts" ]
                                , span [ class "px-3 py-1 bg-blue-100 text-blue-800 text-sm rounded-full" ]
                                    [ text "Additional Contacts" ]
                                ]
                            , div [ class "flex items-baseline gap-2 mb-3" ]
                                [ span [ class "text-2xl sm:text-3xl font-bold text-gray-900" ] [ text <| formatCurrency tier1Pricing.price ]
                                , span [ class "text-gray-600" ] [ text "/contact" ]
                                ]
                            , p [ class "text-gray-600 text-sm" ]
                                [ text "Simple per-contact pricing above base tier." ]
                            ]
                        ]
                    ]

                -- Calculator Section - Responsive Layout
                , div [ class "w-full grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8" ]
                    [ div [ class "flex flex-col space-y-6" ]
                        [ div [ class "flex flex-col sm:flex-row gap-4 sm:gap-6 items-start" ]
                            [ div [ class "w-full sm:w-48" ]
                                [ label [ class "block text-gray-700 text-sm font-bold mb-2", for "contacts" ]
                                    [ text "Number of Contacts:" ]
                                , input
                                    [ id "contacts"
                                    , type_ "number"
                                    , value (String.fromInt model.calculationInputs.contacts)
                                    , onInput (\str -> ContactCountChanged (String.toInt str |> Maybe.withDefault 0))
                                    , class "shadow appearance-none border rounded py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline w-full"
                                    , Html.Attributes.min "0"
                                    ]
                                    []
                                ]
                            , div [ class "flex-1 w-full" ]
                                [ div [ class "grid grid-cols-2 sm:grid-cols-3 gap-2" ]
                                    ([ 250, 1000, 5000, 10000, 20000, 40000 ]
                                        |> List.map
                                            (\presetValue ->
                                                let
                                                    isActive =
                                                        model.activePreset == Just presetValue

                                                    baseClass =
                                                        "hover:bg-blue-200 text-blue-800 font-semibold py-1 px-2 rounded-full text-sm text-center"

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
                            ]
                        , div [ class "bg-gray-50 rounded-lg p-4" ]
                            [ h3 [ class "text-lg font-bold text-gray-800 mb-3" ] [ text "Your Plan Summary" ]
                            , div [ class "space-y-2" ]
                                [ div [ class "flex justify-between items-center text-sm" ]
                                    [ span [ class "text-gray-600" ] [ text "Base subscription:" ]
                                    , span [ class "font-bold" ] [ text (formatCurrency pricing.basePrice) ]
                                    ]
                                , List.filterMap
                                    (\tier ->
                                        if tier.contacts > 0 && tier.price > 0 then
                                            Just
                                                (div [ class "flex justify-between items-center text-sm" ]
                                                    [ span [ class "text-gray-600" ]
                                                        [ text
                                                            (if tier.contacts <= 250 then
                                                                ""

                                                             else if tier.contacts <= 750 then
                                                                formatNumber (toFloat tier.contacts) ++ " @ " ++ formatCurrency tier1Pricing.price

                                                             else if tier.contacts <= 4000 then
                                                                formatNumber (toFloat tier.contacts) ++ " @ " ++ formatCurrency tier1Pricing.price

                                                             else
                                                                formatNumber (toFloat tier.contacts) ++ " @ " ++ formatCurrency tier1Pricing.price
                                                            )
                                                        ]
                                                    , span [ class "font-bold" ] [ text (formatCurrency tier.price) ]
                                                    ]
                                                )

                                        else
                                            Nothing
                                    )
                                    pricing.tierPrices
                                    |> div [ class "space-y-2" ]
                                ]
                            ]
                        ]
                    , div [ class "flex items-center justify-center" ]
                        [ div [ class "bg-blue-600 rounded-lg p-6 text-white text-center w-full lg:w-96" ]
                            [ h2 [ class "font-bold mb-2 text-lg" ] [ text "Monthly Price" ]
                            , div [ class "text-4xl sm:text-5xl font-bold mb-2" ] [ text (formatCurrency pricing.totalPrice) ]
                            , p [ class "text-sm text-blue-100" ]
                                [ text ("For " ++ formatNumber (toFloat model.calculationInputs.contacts) ++ " contacts") ]
                            ]
                        ]
                    ]

                -- Value Analysis Section - Responsive Layout
                , div [ class "w-full mt-8 mb-8" ]
                    [ div [ class "bg-gradient-to-r from-purple-50 to-blue-50 rounded-lg p-4 border border-purple-100" ]
                        [ div [ class "flex flex-col gap-6" ]
                            [ div [ class "grid grid-cols-1 lg:grid-cols-3 gap-6" ]
                                [ div [ class "flex flex-col gap-4" ]
                                    [ h2 [ class "text-lg font-bold text-gray-800" ] [ text "Value Analysis" ]
                                    , div [ class "grid grid-cols-2 gap-4" ]
                                        [ div [ class "flex flex-col gap-5" ]
                                            -- Left column for inputs
                                            [ div [ class "flex flex-col" ]
                                                [ label
                                                    [ class "block text-sm font-medium text-gray-700 mb-1 cursor-pointer h-5"
                                                    , for "commission-rate"
                                                    ]
                                                    [ text "Annual Commission" ]
                                                , div [ class "flex rounded-md shadow-sm w-[100px]" ]
                                                    [ div [ class "flex-shrink-0 inline-flex items-center px-2 rounded-l-md border border-r-0 border-gray-300 bg-indigo-100 text-indigo-800 text-sm font-medium" ]
                                                        [ text "$" ]
                                                    , input
                                                        [ class "w-full border border-gray-300 rounded-none rounded-r-md shadow-sm py-1 px-2 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm text-center"
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
                                                    [ class "block text-sm font-medium text-gray-700 mb-1 cursor-pointer h-5"
                                                    , for "rollover-percent"
                                                    ]
                                                    [ text "Annual Rollover" ]
                                                , div [ class "flex rounded-md shadow-sm w-[100px]" ]
                                                    [ div [ class "flex-shrink-0 inline-flex items-center px-2 rounded-l-md border border-r-0 border-gray-300 bg-indigo-100 text-indigo-800 text-sm font-medium" ]
                                                        [ text "%" ]
                                                    , input
                                                        [ class "w-full border border-gray-300 rounded-none rounded-r-md shadow-sm py-1 px-2 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm text-center"
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
                                        , div [ class "flex flex-col gap-6" ]
                                            -- Right column for outputs
                                            [ div [ class "flex flex-col" ]
                                                [ div [ class "text-sm font-medium text-gray-700 h-5" ] [ text "Baseline LTV" ]
                                                , div [ class "h-[30px] flex flex-col justify-start" ]
                                                    [ div [ class "text-lg font-semibold text-indigo-600 -mb-1" ]
                                                        [ text ("$" ++ formatNumber (model.calculationInputs.commissionRate * 6)) ]
                                                    , div [ class "text-sm font-normal text-gray-500" ] [ text "6 years" ]
                                                    ]
                                                ]
                                            , div [ class "flex flex-col" ]
                                                [ div [ class "text-sm font-medium text-gray-700 h-5" ] [ text "Added LTV (Avg.)" ]
                                                , div [ class "h-[30px] flex flex-col justify-start" ]
                                                    [ div [ class "text-lg font-semibold text-indigo-600 -mb-1" ]
                                                        [ text ("$" ++ formatNumber (model.calculationInputs.commissionRate * 3)) ]
                                                    , div [ class "text-sm font-normal text-gray-500" ] [ text "3 extra years" ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                , div [ class "bg-white rounded-lg p-3 shadow-sm border border-gray-200" ]
                                    [ h3 [ class "font-bold text-gray-700 mb-2 text-md" ] [ text "Monthly Impact" ]
                                    , div [ class "space-y-2 text-sm" ]
                                        [ div [ class "flex justify-between items-start" ]
                                            [ div [ class "flex flex-col" ]
                                                [ span [ class "text-gray-600" ] [ text "Investment:" ]
                                                ]
                                            , span [ class "font-bold" ] [ text (formatCurrencyRounded revenue.price) ]
                                            ]
                                        , div [ class "flex justify-between items-start" ]
                                            [ div [ class "flex flex-col" ]
                                                [ span [ class "text-gray-600" ] [ text "Converted contacts:" ]
                                                , span [ class "text-xs text-gray-500" ]
                                                    [ text
                                                        ("("
                                                            ++ formatNumber (monthlyConversionRate model.calculationInputs * 100)
                                                            ++ "% of "
                                                            ++ formatNumber (toFloat model.calculationInputs.contacts)
                                                            ++ ")"
                                                        )
                                                    ]
                                                ]
                                            , span [ class "font-bold" ] [ text (formatNumber revenue.monthlyConverted) ]
                                            ]
                                        , div [ class "flex justify-between items-start" ]
                                            [ div [ class "flex flex-col" ]
                                                [ span [ class "text-gray-600" ] [ text "New LTV Added:" ]
                                                , span [ class "text-xs text-gray-500" ]
                                                    [ text ("(" ++ formatNumber revenue.monthlyConverted ++ " × $" ++ formatNumber (model.calculationInputs.commissionRate * 3) ++ ")") ]
                                                ]
                                            , span [ class "font-bold text-purple-700" ] [ text (formatCurrencyRounded revenue.monthlyLtv) ]
                                            ]
                                        ]
                                    ]
                                , div [ class "bg-white rounded-lg p-3 shadow-sm border border-gray-200" ]
                                    [ h3 [ class "font-bold text-gray-700 mb-2 text-md" ] [ text "Annual Impact" ]
                                    , div [ class "space-y-2 text-sm" ]
                                        [ div [ class "flex justify-between items-start" ]
                                            [ div [ class "flex flex-col" ]
                                                [ span [ class "text-gray-600" ] [ text "Investment:" ]
                                                ]
                                            , span [ class "font-bold" ] [ text (formatCurrencyRounded revenue.annualPrice) ]
                                            ]
                                        , div [ class "flex justify-between items-start" ]
                                            [ div [ class "flex flex-col" ]
                                                [ span [ class "text-gray-600" ] [ text "Converted contacts:" ]
                                                , span [ class "text-xs text-gray-500" ]
                                                    [ text
                                                        ("("
                                                            ++ formatNumber model.calculationInputs.rolloverPercent
                                                            ++ "% of "
                                                            ++ formatNumber (toFloat model.calculationInputs.contacts)
                                                            ++ ")"
                                                        )
                                                    ]
                                                ]
                                            , span [ class "font-bold" ] [ text (formatNumber revenue.annualConverted) ]
                                            ]
                                        , div [ class "flex justify-between items-start" ]
                                            [ div [ class "flex flex-col" ]
                                                [ span [ class "text-gray-600" ] [ text "New LTV Added:" ]
                                                , span [ class "text-xs text-gray-500" ]
                                                    [ text ("(" ++ formatNumber revenue.annualConverted ++ " × $" ++ formatNumber (model.calculationInputs.commissionRate * 3) ++ ")") ]
                                                ]
                                            , span [ class "font-bold text-green-700" ] [ text (formatCurrencyRounded revenue.annualLtv) ]
                                            ]
                                        ]
                                    ]
                                ]
                            , div [ class "mt-6" ]
                                [ div [ class "bg-emerald-600 rounded-lg p-4 sm:p-6 text-white text-center flex flex-col sm:flex-row justify-center gap-8 sm:gap-16" ]
                                    [ div [ class "text-center" ]
                                        [ h3 [ class "text-lg mb-2 font-medium text-emerald-100" ] [ text "Return on Investment" ]
                                        , div [ class "text-3xl sm:text-4xl font-bold mb-1" ] [ text (formatNumber revenue.roi ++ "x") ]
                                        ]
                                    , div [ class "text-center" ]
                                        [ h3 [ class "text-lg mb-2 font-medium text-emerald-100" ] [ text "Net Annual Benefit" ]
                                        , div [ class "text-3xl sm:text-4xl font-bold mb-1" ] [ text (formatCurrencyRounded revenue.netBenefit) ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "w-full overflow-x-auto" ]
                    [ renderRevenueChart model.calculationInputs ]
                , div [ class "w-full overflow-x-auto" ]
                    [ renderLtvChart model.calculationInputs ]
                ]
            ]
        ]



-- Helper function to calculate monthly conversion rate


monthlyConversionRate : PriceModel.CalculationInputs -> Float
monthlyConversionRate inputs =
    inputs.rolloverPercent / (100 * 12)


type alias CashFlowModel =
    List ( Int, Float )


baseCase : PriceModel.CalculationInputs -> CashFlowModel
baseCase inputs =
    List.range 0 6
        |> List.map
            (\i ->
                ( i, toFloat inputs.contacts * inputs.commissionRate * (6 - toFloat i) / 6 )
            )


flatCase : PriceModel.CalculationInputs -> CashFlowModel
flatCase inputs =
    List.range 0 6
        |> List.map
            (\i ->
                ( i, toFloat inputs.contacts * inputs.commissionRate )
            )


payingContactsLadder : PriceModel.CalculationInputs -> List ( Int, Float )
payingContactsLadder inputs =
    List.range 0 6
        |> List.map
            (\i ->
                let
                    nFact =
                        List.range 0 i |> List.sum
                in
                ( i
                , (1 + (inputs.rolloverPercent / toFloat 100) / 6) ^ toFloat nFact
                )
            )


rolloverCase : PriceModel.CalculationInputs -> CashFlowModel
rolloverCase inputs =
    payingContactsLadder inputs
        |> List.map (\( i, v ) -> ( i, v * inputs.commissionRate * toFloat inputs.contacts ))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


formatCurrency : Float -> String
formatCurrency value =
    let
        valueCents =
            value * 100 |> round

        valueDollars =
            toFloat valueCents / 100
    in
    "$" ++ addCommas (String.fromFloat valueDollars |> padCents)


padCents : String -> String
padCents str =
    if String.contains "." str then
        let
            parts =
                String.split "." str

            beforeDecimal =
                List.head parts |> Maybe.withDefault ""

            afterDecimal =
                List.tail parts |> Maybe.withDefault [] |> List.head |> Maybe.withDefault ""

            padded =
                if String.length afterDecimal == 1 then
                    afterDecimal ++ "0"

                else
                    afterDecimal
        in
        beforeDecimal ++ "." ++ padded

    else
        str ++ ".00"


formatCurrencyRounded : Float -> String
formatCurrencyRounded value =
    "$" ++ addCommas (String.fromInt (round value))



-- Add this new function before the view function


renderRevenueChart : PriceModel.CalculationInputs -> Html Msg
renderRevenueChart inputs =
    let
        func : ( Int, Float ) -> { x : String, y : Float }
        func ( year, value ) =
            { x = String.fromInt year
            , y = Basics.max 0 (value / inputs.commissionRate / toFloat inputs.contacts)
            }

        baseCaseData =
            baseCase inputs
                |> List.map func

        flatCaseData =
            flatCase inputs
                |> List.map func

        rolloverCaseData =
            rolloverCase inputs
                |> List.map func

        allData =
            List.map3
                (\base flat rollover ->
                    { x = base.x
                    , baseCase = Basics.max 0 base.y
                    , flatCase = Basics.max 0 flat.y
                    , rolloverCase = Basics.max 0 rollover.y
                    }
                )
                baseCaseData
                flatCaseData
                rolloverCaseData
    in
    div [ class "w-full bg-white rounded-lg p-2 sm:p-4 shadow-sm border border-gray-200" ]
        [ div [ class "flex justify-between items-center text-lg font-bold text-gray-700 mb-1 sm:mb-4" ]
            [ text "Cash Flow" ]
        , div [ class "flex flex-col" ]
            [ div [ class "w-full h-[120px] md:h-[350px] overflow-x-auto overflow-y-hidden" ]
                [ C.chart
                    [ CA.height 250
                    , CA.width 800
                    , CA.margin { top = 20, bottom = 40, left = 60, right = 20 }
                    , CA.padding { top = 10, bottom = 20, left = 10, right = 10 }
                    ]
                    [ C.grid []
                    , C.yLabels [ CA.withGrid, CA.format (\v -> String.fromFloat (round10 1 v) ++ "x"), CA.limits [ CA.lowest 0 CA.exactly ] ]
                    , C.binLabels .x [ CA.moveDown 25, CA.fontSize 12 ]
                    , C.labelAt CA.middle
                        .max
                        [ CA.moveUp 15 ]
                        [ Svg.text_ [ SA.fontSize "18", SA.fill "#1F2937" ] [ Svg.text "Cash Flow" ] ]
                    , C.bars
                        [ CA.margin 0.1
                        ]
                        [ C.bar .baseCase [ CA.color "#3B82F6", CA.opacity 0.7 ]
                            |> C.named "Base Case"
                        , C.bar .flatCase [ CA.color "#22C55E", CA.opacity 0.7 ]
                            |> C.named "Flat Case"
                        , C.bar .rolloverCase [ CA.color "#A855F7", CA.opacity 0.7 ]
                            |> C.named "Rollover Case"
                        ]
                        allData
                    ]
                ]
            , div [ class "flex flex-wrap justify-center gap-2 mt-1 sm:mt-4 text-sm" ]
                [ div [ class "flex items-center gap-2" ]
                    [ div [ class "w-3 h-3 bg-[#3B82F6] rounded-full opacity-70" ] []
                    , text "Base Case"
                    ]
                , div [ class "flex items-center gap-2" ]
                    [ div [ class "w-3 h-3 bg-[#22C55E] rounded-full opacity-70" ] []
                    , text "Flat Case"
                    ]
                , div [ class "flex items-center gap-2" ]
                    [ div [ class "w-3 h-3 bg-[#A855F7] rounded-full opacity-70" ] []
                    , text "Rollover Case"
                    ]
                ]
            ]
        ]



-- LTV Model functions


baseCaseLtv : PriceModel.CalculationInputs -> CashFlowModel
baseCaseLtv inputs =
    -- Initial book has average 3 years of LTV remaining (3x commission)
    List.range 0 6
        |> List.map
            (\i ->
                ( i
                , toFloat inputs.contacts * inputs.commissionRate * 3.0 * (6 - toFloat i) / 6
                  -- Initial 3x LTV
                )
            )


flatCaseLtv : PriceModel.CalculationInputs -> CashFlowModel
flatCaseLtv inputs =
    -- Flat case maintains the same LTV
    List.range 0 6
        |> List.map
            (\i ->
                ( i
                , toFloat inputs.contacts * inputs.commissionRate * 3.0
                  -- Constant 3x LTV
                )
            )


rolloverCaseLtv : PriceModel.CalculationInputs -> CashFlowModel
rolloverCaseLtv inputs =
    -- Each year we add (rolloverPercent)% of contacts with 6 years of LTV
    List.range 0 6
        |> List.map
            (\i ->
                let
                    baseValue =
                        toFloat inputs.contacts * inputs.commissionRate * 3.0

                    -- Initial 3x LTV
                    additionalLtv =
                        if i == 0 then
                            0

                        else
                            -- For each previous year, we've added rolloverPercent% of contacts with 6x commission
                            toFloat inputs.contacts
                                * (inputs.rolloverPercent / 100)
                                * inputs.commissionRate
                                * 3
                                --  New policies have a net added LTV on average of 3x
                                * toFloat i

                    -- Accumulate for each year
                in
                ( i, baseValue + additionalLtv )
            )


renderLtvChart : PriceModel.CalculationInputs -> Html Msg
renderLtvChart inputs =
    let
        func : ( Int, Float ) -> { x : String, y : Float }
        func ( year, value ) =
            { x = String.fromInt year
            , y = value / 1000000
            }

        baseCaseData =
            baseCaseLtv inputs
                |> List.map func

        flatCaseData =
            flatCaseLtv inputs
                |> List.map func

        rolloverCaseData =
            rolloverCaseLtv inputs
                |> List.map func

        allData =
            List.map3
                (\base flat rollover ->
                    { x = base.x
                    , baseCase = base.y
                    , flatCase = flat.y
                    , rolloverCase = rollover.y
                    }
                )
                baseCaseData
                flatCaseData
                rolloverCaseData
    in
    div [ class "w-full bg-white rounded-lg p-2 sm:p-4 shadow-sm border border-gray-200 mt-4 sm:mt-8" ]
        [ div [ class "flex justify-between items-center text-lg font-bold text-gray-700 mb-1 sm:mb-4" ]
            [ text "Book of Business -- Remaining LTV (Millions)" ]
        , div [ class "flex flex-col" ]
            [ div [ class "w-full h-[120px] md:h-[350px] overflow-x-auto overflow-y-hidden" ]
                [ C.chart
                    [ CA.height 250
                    , CA.width 800
                    , CA.margin { top = 20, bottom = 40, left = 60, right = 20 }
                    , CA.padding { top = 10, bottom = 20, left = 10, right = 10 }
                    ]
                    [ C.grid []
                    , C.yLabels [ CA.withGrid, CA.format (\v -> "$" ++ formatNumber v ++ "M"), CA.limits [ CA.lowest 0 CA.exactly ] ]
                    , C.binLabels .x [ CA.moveDown 25, CA.fontSize 12 ]
                    , C.labelAt CA.middle
                        .max
                        [ CA.moveUp 15 ]
                        [ Svg.text_ [ SA.fontSize "18", SA.fill "#1F2937" ] [ Svg.text "Book Value" ] ]
                    , C.bars
                        [ CA.margin 0.1
                        ]
                        [ C.bar .baseCase [ CA.color "#3B82F6", CA.opacity 0.7 ]
                            |> C.named "Base Case"
                        , C.bar .flatCase [ CA.color "#22C55E", CA.opacity 0.7 ]
                            |> C.named "Flat Case"
                        , C.bar .rolloverCase [ CA.color "#A855F7", CA.opacity 0.7 ]
                            |> C.named "Rollover Case"
                        ]
                        allData
                    ]
                ]
            , div [ class "flex flex-wrap justify-center gap-2 mt-1 sm:mt-4 text-sm" ]
                [ div [ class "flex items-center gap-2" ]
                    [ div [ class "w-3 h-3 bg-[#3B82F6] rounded-full opacity-70" ] []
                    , text "Base Case"
                    ]
                , div [ class "flex items-center gap-2" ]
                    [ div [ class "w-3 h-3 bg-[#22C55E] rounded-full opacity-70" ] []
                    , text "Flat Case"
                    ]
                , div [ class "flex items-center gap-2" ]
                    [ div [ class "w-3 h-3 bg-[#A855F7] rounded-full opacity-70" ] []
                    , text "Rollover Case"
                    ]
                ]
            ]
        ]
