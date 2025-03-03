module Calculator exposing (main)

import Browser
import Html exposing (Html, div, h3, input, li, p, text, ul)
import Html.Attributes exposing (max, min, step, type_, value)
import Html.Events exposing (onInput)



-- Model Definition


type alias Model =
    { totalContacts : String -- Total book of business size as string input
    , yearWeights : List Float -- Weights for distribution over 6 years
    , responseRate : String -- Response rate as percentage (e.g., "5" for 5%)
    }



-- Initial State


init : Model
init =
    { totalContacts = ""
    , yearWeights = [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0 ] -- Initial weights, heavier toward recent years
    , responseRate = "5" -- Default response rate of 5%
    }



-- Messages


type Msg
    = SetTotalContacts String -- Update total contacts
    | SetYearWeight Int String -- Update weight for a specific year (index, value)
    | SetResponseRate String -- Update response rate



-- Update Function


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTotalContacts s ->
            { model | totalContacts = s }

        SetYearWeight i s ->
            case String.toFloat s of
                Just f ->
                    let
                        yearWeights =
                            List.indexedMap
                                (\j w ->
                                    if j == i then
                                        f

                                    else
                                        w
                                )
                                model.yearWeights
                    in
                    { model | yearWeights = yearWeights }

                Nothing ->
                    model

        -- Ignore invalid slider input (shouldn’t happen with range)
        SetResponseRate s ->
            { model | responseRate = s }



-- View Function


view : Model -> Html Msg
view model =
    div []
        [ -- Input for Total Contacts
          p [] [ text "Total Book of Business Size:" ]
        , input [ type_ "text", value model.totalContacts, onInput SetTotalContacts ] []

        -- Sliders for Year Distribution
        , h3 [] [ text "Adjust Distribution Over Years:" ]
        , div []
            (List.indexedMap
                (\i weight ->
                    div []
                        [ text ("Policies started " ++ String.fromInt (5 - i) ++ " years ago: ")
                        , input
                            [ type_ "range"
                            , min "0"
                            , max "100"
                            , step "1"
                            , value (String.fromFloat weight)
                            , onInput (SetYearWeight i)
                            ]
                            []
                        ]
                )
                model.yearWeights
            )

        -- Input for Response Rate
        , p [] [ text "Response Rate (%):" ]
        , input [ type_ "text", value model.responseRate, onInput SetResponseRate ] []

        -- Calculated Outputs
        , case ( String.toFloat model.totalContacts, String.toFloat model.responseRate ) of
            ( Ok n, Ok r ) ->
                let
                    -- Distribution Calculation
                    sumWeights =
                        List.sum model.yearWeights

                    contactsPerYear =
                        if sumWeights > 0 then
                            List.map (\w -> (w / sumWeights) * n) model.yearWeights

                        else
                            List.repeat 6 0.0

                    -- Core Calculations
                    weeklyCalls =
                        (n / 52) * (r / 100)

                    -- Calls based on one event per contact per year
                    monthlyCost =
                        (n / 10000) * 100

                    -- $100 per 10,000 contacts
                    annualCost =
                        12 * monthlyCost

                    -- Yearly cost
                    totalCallsPerYear =
                        n * (r / 100)

                    -- Total calls in a year
                    numberOfSales =
                        totalCallsPerYear * 0.5

                    -- 50% close rate
                    totalRevenue =
                        numberOfSales * 500

                    -- $500 increased LTV
                    roi =
                        if annualCost > 0 then
                            ((totalRevenue - annualCost) / annualCost) * 100
                            -- ROI as percentage

                        else
                            0
                in
                div []
                    [ -- Distribution Display (Placeholder for Chart)
                      h3 [] [ text "Distribution of Contacts:" ]
                    , ul []
                        (List.indexedMap
                            (\i c ->
                                li [] [ text ("Year -" ++ String.fromInt (5 - i) ++ ": " ++ String.fromFloat c ++ " contacts") ]
                            )
                            contactsPerYear
                        )

                    -- Calculated Metrics
                    , h3 [] [ text "Results:" ]
                    , p [] [ text ("Weekly Calls: " ++ String.fromFloat weeklyCalls) ]
                    , p [] [ text ("Monthly Cost: $" ++ String.fromFloat monthlyCost) ]
                    , p [] [ text ("Annual Cost: $" ++ String.fromFloat annualCost) ]
                    , p [] [ text ("Total Calls per Year: " ++ String.fromFloat totalCallsPerYear) ]
                    , p [] [ text ("Number of Sales: " ++ String.fromFloat numberOfSales) ]
                    , p [] [ text ("Total Revenue: $" ++ String.fromFloat totalRevenue) ]
                    , p [] [ text ("ROI: " ++ String.fromFloat roi ++ "%") ]
                    ]

            _ ->
                p [] [ text "Please enter valid numbers for total contacts and response rate." ]
        ]



-- Main Program


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
