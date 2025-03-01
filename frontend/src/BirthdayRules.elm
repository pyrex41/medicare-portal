module BirthdayRules exposing
    ( BirthdayRuleType(..)
    , StateRule
    , canPresentDifferentPlanOnly
    , getDelayedEmailDate
    , getStateRule
    , isInBirthdayRuleWindow
    , isInContinuousOpenEnrollment
    )

{-| This module handles birthday rules for different states.
It provides functionality to check if a contact is in a birthday rule window,
if they are in a continuous open enrollment state, and if they can only be
presented with different plan types during their birthday window.
-}

import Date exposing (Date)
import Time exposing (Month(..))


{-| Represents the type of birthday rule for a state.
-}
type BirthdayRuleType
    = BirthdayRule
    | AnniversaryRule
    | ContinuousOpenEnrollment
    | NoSpecialRule


{-| Represents a state's birthday rule configuration.
-}
type alias StateRule =
    { state : String
    , ruleType : BirthdayRuleType
    , daysBeforeBirthday : Int
    , totalDays : Int
    , canPresentDifferentPlan : Bool
    , notes : String
    }


{-| List of states with their birthday rules.
-}
stateRules : List StateRule
stateRules =
    [ { state = "CA"
      , ruleType = BirthdayRule
      , daysBeforeBirthday = 30
      , totalDays = 60
      , canPresentDifferentPlan = False
      , notes = "60-day period starting 30 days before your birthday"
      }
    , { state = "ID"
      , ruleType = BirthdayRule
      , daysBeforeBirthday = 0
      , totalDays = 63
      , canPresentDifferentPlan = False
      , notes = "63-day period starting on your birthday"
      }
    , { state = "IL"
      , ruleType = BirthdayRule
      , daysBeforeBirthday = 0
      , totalDays = 45
      , canPresentDifferentPlan = False
      , notes = "45-day period starting on your birthday; 76+ no special GI right"
      }
    , { state = "KY"
      , ruleType = BirthdayRule
      , daysBeforeBirthday = 0
      , totalDays = 60
      , canPresentDifferentPlan = True
      , notes = "60-day period following your birthday; Can switch sideways and get GI right; Only present different plans."
      }
    , { state = "LA"
      , ruleType = BirthdayRule
      , daysBeforeBirthday = 30
      , totalDays = 93
      , canPresentDifferentPlan = False
      , notes = "93-day period starting 30 days before your birthday"
      }
    , { state = "MD"
      , ruleType = BirthdayRule
      , daysBeforeBirthday = 0
      , totalDays = 31
      , canPresentDifferentPlan = False
      , notes = "31-day period starting on your birthday"
      }
    , { state = "NV"
      , ruleType = BirthdayRule
      , daysBeforeBirthday = 0
      , totalDays = 60
      , canPresentDifferentPlan = False
      , notes = "60-day period starting on the first day of your birth month"
      }
    , { state = "OK"
      , ruleType = BirthdayRule
      , daysBeforeBirthday = 0
      , totalDays = 60
      , canPresentDifferentPlan = False
      , notes = "60-day period starting on your birthday"
      }
    , { state = "OR"
      , ruleType = BirthdayRule
      , daysBeforeBirthday = 0
      , totalDays = 31
      , canPresentDifferentPlan = False
      , notes = "31-day period starting on your birthday"
      }
    , { state = "MO"
      , ruleType = AnniversaryRule
      , daysBeforeBirthday = 30
      , totalDays = 63
      , canPresentDifferentPlan = True
      , notes = "63-day period starting 30 days before your policy anniversary date; Can switch sideways and get GI right; Only present different plans."
      }
    , { state = "CT"
      , ruleType = ContinuousOpenEnrollment
      , daysBeforeBirthday = 0
      , totalDays = 0
      , canPresentDifferentPlan = False
      , notes = "Continuous"
      }
    , { state = "MA"
      , ruleType = ContinuousOpenEnrollment
      , daysBeforeBirthday = 0
      , totalDays = 0
      , canPresentDifferentPlan = False
      , notes = "Continuous"
      }
    , { state = "NY"
      , ruleType = ContinuousOpenEnrollment
      , daysBeforeBirthday = 0
      , totalDays = 0
      , canPresentDifferentPlan = False
      , notes = "Continuous"
      }
    , { state = "WA"
      , ruleType = ContinuousOpenEnrollment
      , daysBeforeBirthday = 0
      , totalDays = 0
      , canPresentDifferentPlan = False
      , notes = "Continuous"
      }
    ]


{-| Get the birthday rule for a specific state.
-}
getStateRule : String -> Maybe StateRule
getStateRule state =
    let
        normalizedState =
            String.toUpper state
    in
    List.filter (\rule -> rule.state == normalizedState) stateRules
        |> List.head


{-| Check if a state has continuous open enrollment.
-}
isInContinuousOpenEnrollment : String -> Bool
isInContinuousOpenEnrollment state =
    case getStateRule state of
        Just rule ->
            rule.ruleType == ContinuousOpenEnrollment

        Nothing ->
            False


{-| Check if a contact can only be presented with different plan types during their birthday window.
-}
canPresentDifferentPlanOnly : String -> Bool
canPresentDifferentPlanOnly state =
    case getStateRule state of
        Just rule ->
            rule.canPresentDifferentPlan

        Nothing ->
            False


{-| Calculate the start date of a birthday rule window.
-}
getBirthdayRuleStartDate : StateRule -> Date -> Date
getBirthdayRuleStartDate rule birthDate =
    let
        currentYear =
            Date.year (Date.fromCalendarDate 2024 Jan 1)

        -- This is just a placeholder, will be replaced with actual current date
        -- For Nevada, the window starts on the first day of the birth month
        startDate =
            if rule.state == "NV" then
                Date.fromCalendarDate currentYear (Date.month birthDate) 1

            else
                -- For other states, subtract the days before birthday from the birthday
                Date.add Date.Days -rule.daysBeforeBirthday (Date.fromCalendarDate currentYear (Date.month birthDate) (Date.day birthDate))
    in
    startDate


{-| Calculate the end date of a birthday rule window.
-}
getBirthdayRuleEndDate : StateRule -> Date -> Date
getBirthdayRuleEndDate rule birthDate =
    let
        startDate =
            getBirthdayRuleStartDate rule birthDate
    in
    Date.add Date.Days rule.totalDays startDate


{-| Check if a date is within a birthday rule window.
For Anniversary rules (Missouri), pass the effectiveDate as the second parameter.
For Birthday rules, pass the birthDate as the second parameter.
-}
isInBirthdayRuleWindow : String -> Date -> Date -> Bool
isInBirthdayRuleWindow state baseDate currentDate =
    case getStateRule state of
        Just rule ->
            let
                currentYear =
                    Date.year currentDate

                -- Adjust base date to current year
                adjustedBaseDate =
                    Date.fromCalendarDate currentYear (Date.month baseDate) (Date.day baseDate)

                startDate =
                    getBirthdayRuleStartDate rule adjustedBaseDate

                endDate =
                    getBirthdayRuleEndDate rule adjustedBaseDate
            in
            -- Only check window for Birthday and Anniversary rules
            case rule.ruleType of
                BirthdayRule ->
                    Date.compare currentDate startDate /= LT && Date.compare currentDate endDate /= GT

                AnniversaryRule ->
                    Date.compare currentDate startDate /= LT && Date.compare currentDate endDate /= GT

                ContinuousOpenEnrollment ->
                    -- For continuous open enrollment states, they're always in an "open enrollment window"
                    True

                NoSpecialRule ->
                    False

        Nothing ->
            False


{-| Calculate the date when an email should be sent after a birthday rule window.
For Anniversary rules (Missouri), pass the effectiveDate as the second parameter.
For Birthday rules, pass the birthDate as the second parameter.
The third parameter should be the scheduled date that needs to be delayed.
-}
getDelayedEmailDate : String -> Date -> Date -> Date
getDelayedEmailDate state baseDate scheduledDate =
    case getStateRule state of
        Just rule ->
            let
                -- Use the year from the scheduled date
                scheduledYear =
                    Date.year scheduledDate

                -- Adjust base date to scheduled year
                adjustedBaseDate =
                    Date.fromCalendarDate scheduledYear (Date.month baseDate) (Date.day baseDate)

                -- For Nevada, the window starts on the first day of the birth month
                windowStartDate =
                    if rule.state == "NV" then
                        Date.fromCalendarDate scheduledYear (Date.month baseDate) 1

                    else
                        -- For other states, subtract the days before birthday from the birthday
                        Date.add Date.Days -rule.daysBeforeBirthday adjustedBaseDate

                -- Calculate the end date of the window
                windowEndDate =
                    Date.add Date.Days rule.totalDays windowStartDate

                -- Add one month to the end date
                delayedDate =
                    Date.add Date.Months 1 windowEndDate
            in
            delayedDate

        Nothing ->
            -- If no rule exists, just return the scheduled date
            scheduledDate
