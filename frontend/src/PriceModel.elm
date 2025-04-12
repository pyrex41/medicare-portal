module PriceModel exposing (CalculationInputs, view)

import Html exposing (Html, div, h4, p, table, tbody, td, text, tr)
import Html.Attributes exposing (class)


type alias CalculationInputs =
    { contacts : Int
    , averageAge : Float
    , rolloverPercent : Float
    , commissionRate : Float
    }



-- Calculate the price based on the number of contacts


calculatePrice : Int -> Int
calculatePrice contacts =
    let
        basePrice =
            150

        -- Base price for up to 250 contacts
        additionalTiers =
            Basics.max 0 (ceiling (toFloat (Basics.max 0 (contacts - 250)) / 250))

        additionalPrice =
            additionalTiers * 35

        -- $35 for each additional 250 contacts
    in
    basePrice + additionalPrice



-- Parse calculation inputs from the model
-- Calculate revenue based on inputs


type alias RevenueModel =
    { ltvGainPerYear : Float
    , ltvPerContact : Float
    , price : Int
    , firstYearPayout : Float
    , ltvGainPerContact : Float
    , contactsRolledOver : Float
    , roi : Float
    }


calculateRevenue : CalculationInputs -> RevenueModel
calculateRevenue inputs =
    let
        -- Constants
        price =
            calculatePrice inputs.contacts

        maxYears =
            6.0

        ltvDiscountMultiplier =
            1.0

        -- Basic calculations
        ltvPerContact =
            inputs.commissionRate * maxYears * ltvDiscountMultiplier

        rolloverFraction =
            inputs.rolloverPercent / 100.0

        contactsRolledOver =
            toFloat inputs.contacts * rolloverFraction

        -- LTV calculations
        remainingYears =
            maxYears - inputs.averageAge

        additionalYearsForRolled =
            maxYears - remainingYears

        additionalYearsDiscount =
            (1 - ltvDiscountMultiplier) * additionalYearsForRolled / maxYears

        additionalYearsDiscountMultiplier =
            1 - additionalYearsDiscount

        ltvGainPerContact =
            inputs.commissionRate * additionalYearsForRolled * additionalYearsDiscountMultiplier

        ltvGainPerYear =
            ltvGainPerContact * contactsRolledOver

        contactsAtMaxAge =
            -- assumes equal distribution across 6 years
            toFloat inputs.contacts / 6.0

        firstYearPayout =
            contactsAtMaxAge * inputs.commissionRate * rolloverFraction

        roi =
            ltvGainPerYear / toFloat (calculatePrice inputs.contacts * 12)
    in
    { ltvGainPerYear = ltvGainPerYear
    , ltvPerContact = ltvPerContact
    , price = price
    , firstYearPayout = firstYearPayout
    , ltvGainPerContact = ltvGainPerContact
    , contactsRolledOver = contactsRolledOver
    , roi = roi
    }


view : CalculationInputs -> Html msg
view inputs =
    let
        revenueModel =
            calculateRevenue inputs
    in
    div [ class "space-y-6" ]
        [ div
            [ class "bg-white p-4 rounded-md shadow-sm border border-gray-200" ]
            [ h4 [ class "text-md font-medium text-gray-800 mb-4" ] [ text "Subscription Cost" ]
            , table [ class "w-full text-sm" ]
                [ tbody []
                    [ tr [ class "border-b border-gray-200" ]
                        [ td [ class "py-2 text-gray-600" ] [ text "Base monthly subscription:" ]
                        , td [ class "py-2 text-right font-medium" ] [ text "$150/month" ]
                        ]
                    , if revenueModel.price /= 150 then
                        tr [ class "border-b border-gray-200" ]
                            [ td [ class "py-2 text-gray-600" ] [ text "Additional monthly contacts cost:" ]
                            , td [ class "py-2 text-right font-medium" ]
                                [ text <| "$" ++ String.fromInt (revenueModel.price - 150) ++ "/month" ]
                            ]

                      else
                        text ""

                    {--
                    , tr [ class "border-b border-gray-200" ]
                        [ td [ class "py-2 font-medium" ] [ text "Monthly total:" ]
                        , td [ class "py-2 text-right font-bold text-indigo-600" ]
                            [ text <| "$" ++ String.fromInt (Maybe.withDefault 0 model.calculatedPrice) ]
                        ]
                    --}
                    , tr []
                        [ td [ class "py-2 font-medium" ] [ text "Annual Cost:" ]
                        , td [ class "py-2 text-right font-bold text-indigo-600" ]
                            [ text <| "$" ++ String.fromInt (revenueModel.price * 12) ]
                        ]
                    ]
                ]
            ]
        , div [ class "bg-white p-4 rounded-md shadow-sm border border-gray-200" ]
            [ h4 [ class "text-md font-medium text-gray-800 mb-4" ] [ text "Revenue Impact" ]
            , p [ class "text-xs text-gray-600 mb-4 italic" ]
                [ text "Calculations assume an average book age of 3 years." ]
            , div [ class "space-y-4" ]
                [ div [ class "grid grid-cols-1 gap-2" ]
                    [ div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                        [ div [ class "text-gray-600 truncate pr-4" ] [ text "Lifetime Revenue Per Contact:" ]
                        , div [ class "text-right font-medium whitespace-nowrap" ]
                            [ text <| "$" ++ formatPreciseMoney revenueModel.ltvPerContact ]
                        ]
                    , div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                        [ div [ class "text-gray-600 truncate pr-4" ] [ text "Lifetime Gain Per Contact Rolled Over:" ]
                        , div [ class "text-right font-medium whitespace-nowrap" ]
                            [ text <| "$" ++ formatPreciseMoney revenueModel.ltvGainPerContact ]
                        ]
                    , div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                        [ div [ class "text-gray-600 truncate pr-4" ] [ text "Contacts Rolled Over Per Year:" ]
                        , div [ class "text-right font-medium whitespace-nowrap" ]
                            [ text <| formatNumber revenueModel.contactsRolledOver ]
                        ]

                    {--
                    , div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                        [ div [ class "text-gray-600 truncate pr-4" ] [ text "Average First Year Payout:" ]
                        , div [ class "text-right font-medium text-green-600 whitespace-nowrap" ]
                            [ text <| "$" ++ formatPreciseMoney revenueModel.firstYearPayout ]
                        ]
                    --}
                    , div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                        [ div [ class "text-gray-600 truncate pr-4" ] [ text "Lifetime Revenue Added Per Year:" ]
                        , div [ class "text-right font-bold text-indigo-600 whitespace-nowrap" ]
                            [ text <| "$" ++ formatRoughMoney revenueModel.ltvGainPerYear ]
                        ]
                    , div [ class "flex justify-between items-center py-2" ]
                        [ div [ class "font-medium text-gray-700 truncate pr-4" ] [ text "Annual Return on Investment:" ]
                        , div [ class "text-right font-bold text-green-600 whitespace-nowrap" ]
                            [ text <| formatNumber revenueModel.roi ++ "x" ]
                        ]
                    ]
                ]
            ]
        ]


formatPreciseMoney : Float -> String
formatPreciseMoney value =
    let
        -- Round to 2 decimal places
        roundedValue =
            round10 2 value

        -- Format the number with commas and 2 decimal places
        valueStr =
            String.fromFloat roundedValue
    in
    formatMoney valueStr


formatRoughMoney : Float -> String
formatRoughMoney value =
    let
        roundedValue =
            round10 0 value
    in
    formatMoney (String.fromFloat roundedValue)


formatNumber : Float -> String
formatNumber value =
    if value == toFloat (round value) then
        -- It's a whole number - show no decimals
        String.fromInt (round value)

    else
        -- Show with appropriate precision
        String.fromFloat (round10 1 value)



-- Helper function to format money values


formatMoney : String -> String
formatMoney valueStr =
    let
        parts =
            String.split "." valueStr

        intPart =
            List.head parts |> Maybe.withDefault ""

        decPart =
            List.drop 1 parts
                |> List.head
                |> Maybe.withDefault ""
                |> (\s ->
                        if String.length s == 0 then
                            "00"

                        else if String.length s == 1 then
                            s ++ "0"

                        else
                            String.left 2 s
                   )

        -- Format integer part with commas
        formattedInt =
            addCommas intPart
    in
    formattedInt ++ "." ++ decPart



-- Add commas to numbers for better readability


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



-- Helper function to round to specific decimal places


round10 : Int -> Float -> Float
round10 n value =
    let
        factor =
            10 ^ n |> toFloat
    in
    (value * factor) |> round |> toFloat |> (\x -> x / factor)
