module Earnings exposing
    ( EarningsInputs
    , EarningsModel
    , baseCase
    , flatCase
    , rolloverCase
    )

import PriceModel


type alias EarningsInputs =
    { calculationInputs : PriceModel.CalculationInputs
    , overheadCost : Float -- Annual fixed overhead cost
    , customerAcquisitionCost : Float -- Cost per new customer
    , earningsMultiple : Float -- Multiple for enterprise value calculation
    }


type alias EarningsModel =
    List
        { year : Int
        , revenue : Float
        , costs : Float
        , earnings : Float
        , enterpriseValue : Float
        }



-- Helper to calculate enterprise value based on earnings and multiple


calculateEnterpriseValue : Float -> Float -> Float
calculateEnterpriseValue earnings multiple =
    earnings * multiple


baseCase : EarningsInputs -> EarningsModel
baseCase inputs =
    List.range 0 6
        |> List.map
            (\year ->
                let
                    -- Base revenue stays constant (existing book)
                    revenue =
                        toFloat inputs.calculationInputs.contacts
                            * inputs.calculationInputs.commissionRate

                    -- Only fixed costs in base case
                    costs =
                        inputs.overheadCost

                    -- Calculate earnings
                    earnings =
                        revenue - costs

                    -- Calculate enterprise value as simple multiple of earnings
                    enterpriseValue =
                        calculateEnterpriseValue earnings inputs.earningsMultiple
                in
                { year = year
                , revenue = revenue
                , costs = costs
                , earnings = earnings
                , enterpriseValue = enterpriseValue
                }
            )


flatCase : EarningsInputs -> EarningsModel
flatCase inputs =
    List.range 0 6
        |> List.map
            (\year ->
                let
                    -- Total contacts stays constant - these are existing policies
                    totalContacts =
                        toFloat inputs.calculationInputs.contacts

                    -- Revenue from existing book - already paying commissions
                    revenue =
                        totalContacts * inputs.calculationInputs.commissionRate

                    -- Need to replace 1/6 of book every year to maintain size
                    replacementCustomers =
                        totalContacts / 6

                    -- Constant replacement rate
                    -- Costs: fixed overhead + CAC for replacements
                    costs =
                        inputs.overheadCost
                            + (replacementCustomers * inputs.customerAcquisitionCost)

                    -- Calculate earnings
                    earnings =
                        revenue - costs

                    -- Calculate enterprise value as simple multiple of earnings
                    enterpriseValue =
                        calculateEnterpriseValue earnings inputs.earningsMultiple
                in
                { year = year
                , revenue = revenue
                , costs = costs
                , earnings = earnings
                , enterpriseValue = enterpriseValue
                }
            )


rolloverCase : EarningsInputs -> EarningsModel
rolloverCase inputs =
    List.range 0 6
        |> List.map
            (\year ->
                let
                    -- Base revenue from existing book
                    baseRevenue =
                        toFloat inputs.calculationInputs.contacts
                            * inputs.calculationInputs.commissionRate

                    -- Calculate retained customers (starting year 1)
                    -- These are customers we would have lost (1/6 of book) but kept (at rollover rate)
                    retainedCustomers =
                        if year == 0 then
                            0

                        else
                            -- 1/6 of book * rollover rate for each year
                            (toFloat inputs.calculationInputs.contacts / 6)
                                * (inputs.calculationInputs.rolloverPercent / 100)
                                * toFloat year

                    -- Additional revenue from retained customers
                    retainedRevenue =
                        retainedCustomers * inputs.calculationInputs.commissionRate

                    revenue =
                        baseRevenue + retainedRevenue

                    -- Need to replace 1/6 of original book minus those we retain
                    baseReplacements =
                        if year == 0 then
                            toFloat inputs.calculationInputs.contacts / 6

                        else
                            -- Only replace those we didn't retain
                            (toFloat inputs.calculationInputs.contacts / 6)
                                * (1 - inputs.calculationInputs.rolloverPercent / 100)

                    -- Costs include fixed overhead plus CAC for replacements
                    costs =
                        inputs.overheadCost
                            + (baseReplacements * inputs.customerAcquisitionCost)

                    -- Calculate earnings
                    earnings =
                        revenue - costs

                    -- Calculate enterprise value as simple multiple of earnings
                    enterpriseValue =
                        calculateEnterpriseValue earnings inputs.earningsMultiple
                in
                { year = year
                , revenue = revenue
                , costs = costs
                , earnings = earnings
                , enterpriseValue = enterpriseValue
                }
            )
