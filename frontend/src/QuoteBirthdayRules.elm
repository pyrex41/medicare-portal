module QuoteBirthdayRules exposing
    ( PlanRestriction(..)
    , canPresentPlan
    , getQuotePlanRestriction
    )

{-| This module handles birthday rule restrictions for the quote page.
It determines which plans can be presented based on the contact's state and current plan.
-}

import BirthdayRules exposing (canPresentDifferentPlanOnly, isInBirthdayRuleWindow)
import Date exposing (Date)


{-| Represents the plan restriction for a quote.
-}
type PlanRestriction
    = NoRestriction
    | DifferentPlanOnly
    | NoQuoteAllowed


{-| Determines which plans can be presented based on the contact's state, birth date, and current plan.
-}
getQuotePlanRestriction : String -> Date -> Date -> String -> PlanRestriction
getQuotePlanRestriction state birthDate currentDate currentPlan =
    if isInBirthdayRuleWindow state birthDate currentDate then
        if canPresentDifferentPlanOnly state then
            DifferentPlanOnly

        else
            NoQuoteAllowed

    else
        NoRestriction


{-| Checks if a plan can be presented based on the restriction and the contact's current plan.
-}
canPresentPlan : PlanRestriction -> String -> String -> Bool
canPresentPlan restriction currentPlan planToPresent =
    case restriction of
        NoRestriction ->
            True

        NoQuoteAllowed ->
            False

        DifferentPlanOnly ->
            -- Only allow presenting a different plan type
            -- For example, if current plan is "Plan G", only allow "Plan N" and vice versa
            case ( currentPlan, planToPresent ) of
                ( "Plan G", "Plan N" ) ->
                    True

                ( "G", "Plan N" ) ->
                    True

                ( "G", "N" ) ->
                    True

                ( "Plan G", "N" ) ->
                    True

                ( "Plan N", "Plan G" ) ->
                    True

                ( "N", "Plan G" ) ->
                    True

                ( "N", "G" ) ->
                    True

                ( "Plan N", "G" ) ->
                    True

                _ ->
                    False
