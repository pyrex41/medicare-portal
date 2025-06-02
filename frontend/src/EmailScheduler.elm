module EmailScheduler exposing
    ( EmailSchedule
    , PlanType(..)
    , ScheduledEmail
    , ScheduledEmailStatus(..)
    , ScheduledEmailType(..)
    , getScheduledEmails
    , init
    , scheduledEmailTypeToString
    , viewFutureActivity
    , viewScheduledEmail
    )

{-| This module manages email scheduling for various events such as birthdays,
anniversaries, New Year greetings, and October blasts. It supports different plan
types and provides functionality to schedule emails, check their status, and display upcoming activities.

The module includes:

  - Type definitions for email schedules and statuses.
  - Initialization of email schedules.
  - Logic to calculate scheduled emails with status checks.
  - Views to display future scheduled emails in a user-friendly table format.

Dependencies:

  - `Date` for handling dates.
  - `Html` for rendering views.

-}

import BirthdayRules exposing (canPresentDifferentPlanOnly, getDelayedEmailDate, getStateRule, isInBirthdayRuleWindow, isInContinuousOpenEnrollment)
import Date exposing (Date)
import Html exposing (Html, div, h2, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import List.Extra exposing (uniqueBy)
import Time exposing (Month(..))



-- TYPES


{-| Represents an email schedule for a contact, including key dates and plan information.

  - `contactId`: Unique identifier for the contact.
  - `effectiveDate`: The date when the plan became effective.
  - `birthDate`: The contact's birth date.
  - `currentDate`: The current date for reference.
  - `planType`: The type of plan (PlanN, PlanG, or NoPlan).
  - `state`: The state of the contact.
  - `stateCarrierSettings`: List of state carrier settings.
  - `stateLicenses`: List of state licenses.

-}
type alias EmailSchedule =
    { contactId : Int
    , effectiveDate : Date
    , birthDate : Date
    , currentDate : Date
    , planType : PlanType
    , state : String
    , stateCarrierSettings : List StateCarrierSetting
    , stateLicenses : List String
    }


{-| Represents a scheduled email with its type, scheduled time, and status.

  - `emailType`: The type of email (e.g., Birthday, Anniversary).
  - `scheduledTime`: The date when the email is scheduled to be sent.
  - `status`: The status of the email (Scheduled or Skipped with a reason).

-}
type alias ScheduledEmail =
    { emailType : ScheduledEmailType
    , scheduledTime : Date
    , status : ScheduledEmailStatus
    }


{-| Represents the status of a scheduled email.

  - `Scheduled`: The email is scheduled to be sent.
  - `Skipped reason`: The email was skipped, with a reason provided.
  - `Delayed reason`: The email was delayed due to birthday rules, with a reason provided.

-}
type ScheduledEmailStatus
    = Scheduled
    | Skipped String
    | Delayed String


{-| Represents the type of scheduled email, associated with a plan type.

  - `Birthday`: Email for the contact's birthday.
  - `Anniversary`: Email for the plan's anniversary.
  - `NewYear`: Email for New Year greetings.
  - `OctoberBlast`: Email for an October promotional blast.
  - `NoEmails`: No scheduled emails.

-}
type ScheduledEmailType
    = Birthday
    | Anniversary
    | NewYear
    | OctoberBlast
    | NoEmails


{-| Represents the plan type for a contact.

  - `PlanN`: Plan N (specific benefits).
  - `PlanG`: Plan G (specific benefits).
  - `NoPlan`: No plan assigned.

-}
type PlanType
    = PlanN
    | PlanG
    | NoPlan


{-| Represents a state carrier setting.

  - `state`: The state.
  - `carrier`: The carrier.
  - `active`: Whether the setting is active.
  - `targetGI`: Whether the setting targets GI.

-}
type alias StateCarrierSetting =
    { state : String
    , carrier : String
    , active : Bool
    , targetGI : Bool
    }



-- INITIALIZATION


{-| Initializes an email schedule for a contact.

  - `contactId`: Unique identifier for the contact.
  - `effective`: The effective date of the plan.
  - `birth`: The contact's birth date.
  - `current`: The current date for scheduling reference.
  - `plan`: The plan type.
  - `state`: The state of the contact.
  - `settings`: List of state carrier settings.
  - `licenses`: List of state licenses.
    Returns an `EmailSchedule` record with the provided values.

-}
init : Int -> Date -> Date -> Date -> PlanType -> String -> List StateCarrierSetting -> List String -> EmailSchedule
init contactId effective birth current plan state settings licenses =
    { contactId = contactId
    , effectiveDate = effective
    , birthDate = birth
    , currentDate = current
    , planType = plan
    , state = state
    , stateCarrierSettings = settings
    , stateLicenses = licenses
    }



-- CALCULATIONS


{-| Checks if a contact's state is active.

  - `schedule`: The email schedule to check.
    Returns `True` if the contact's state is active, otherwise `False`.

-}
isStateActive : EmailSchedule -> Bool
isStateActive schedule =
    List.member schedule.state schedule.stateLicenses


{-| Calculates the list of scheduled emails for a given email schedule.

  - `schedule`: The email schedule to process.
    Returns a list of `ScheduledEmail` records, each with a type, scheduled time, and status.
    Emails are scheduled for:
  - The contact's next birthday.
  - The plan's next anniversary.
  - The next New Year (January 1st).
  - The next October blast (October 1st).
    Emails within the first year of the effective date are skipped.
    Emails for contacts in states with continuous open enrollment are skipped.
    Emails for contacts in their birthday rule window are delayed.

-}
getScheduledEmails : EmailSchedule -> List ScheduledEmail
getScheduledEmails schedule =
    if isInContinuousOpenEnrollment schedule.state then
        [ { emailType = NoEmails
          , scheduledTime = schedule.currentDate
          , status = Skipped "Contact's state has continuous open enrollment"
          }
        ]

    else
        let
            schedulingWindowStart : Date
            schedulingWindowStart =
                Date.add Date.Months -1 schedule.currentDate

            oneYearAfterEffective : Date
            oneYearAfterEffective =
                Date.add Date.Years 1 schedule.effectiveDate

            -- Check if an email should be delayed due to birthday rules
            checkBirthdayRuleDelay : ScheduledEmailType -> Date -> ( Date, ScheduledEmailStatus )
            checkBirthdayRuleDelay emailType scheduledDate =
                let
                    -- Check if the scheduled date falls within a birthday rule window
                    isScheduledDateInWindow : Date -> Date -> Bool
                    isScheduledDateInWindow referenceDate dateToCheck =
                        -- Get the rule for the state
                        case getStateRule schedule.state of
                            Just rule ->
                                let
                                    -- Calculate the start and end dates of the window for the scheduled year
                                    scheduledYear =
                                        Date.year dateToCheck

                                    adjustedReferenceDate =
                                        Date.fromCalendarDate scheduledYear (Date.month referenceDate) (Date.day referenceDate)

                                    -- For Nevada, the window starts on the first day of the birth month
                                    windowStartDate =
                                        if rule.state == "NV" then
                                            Date.fromCalendarDate scheduledYear (Date.month referenceDate) 1

                                        else
                                            -- For other states, subtract the days before birthday from the birthday
                                            Date.add Date.Days -rule.daysBeforeBirthday adjustedReferenceDate

                                    windowEndDate =
                                        Date.add Date.Days rule.totalDays windowStartDate
                                in
                                -- Check if the scheduled date falls within the window
                                Date.compare dateToCheck windowStartDate /= LT && Date.compare dateToCheck windowEndDate /= GT

                            Nothing ->
                                False
                in
                case emailType of
                    Birthday ->
                        -- For birthday emails, check if the state is Missouri (which doesn't have a birthday rule)
                        if schedule.state == "MO" then
                            -- Missouri only has anniversary rule, not birthday rule
                            ( scheduledDate, Scheduled )
                            -- For other states, check if the scheduled date falls within the birthday rule window

                        else if isScheduledDateInWindow schedule.birthDate scheduledDate then
                            -- If it does, delay until after the window
                            let
                                delayedDate =
                                    getDelayedEmailDate schedule.state schedule.birthDate scheduledDate
                            in
                            ( delayedDate, Delayed "due to birthday rule window" )

                        else
                            ( scheduledDate, Scheduled )

                    Anniversary ->
                        -- For Missouri anniversary rule
                        if schedule.state == "MO" then
                            -- Check if the scheduled date falls within the anniversary rule window
                            if isScheduledDateInWindow schedule.effectiveDate scheduledDate then
                                -- If the contact already has Plan G and we're sending a Plan G email, delay it
                                if schedule.planType == PlanG then
                                    let
                                        delayedDate =
                                            getDelayedEmailDate schedule.state schedule.effectiveDate scheduledDate
                                    in
                                    ( delayedDate, Delayed "due to anniversary rule window" )

                                else
                                    -- If it's a different plan type, we can send it during the window
                                    ( scheduledDate, Scheduled )

                            else
                                -- Outside the window, schedule normally
                                ( scheduledDate, Scheduled )

                        else
                            -- For other states, no delay for anniversary emails
                            ( scheduledDate, Scheduled )

                    _ ->
                        -- No delay for other email types
                        ( scheduledDate, Scheduled )

            -- Helper function to generate emails for a specific baseDate and emailType,
            -- considering multiple relevant years.
            generateEmailsForBaseDate : ScheduledEmailType -> Date -> List ScheduledEmail
            generateEmailsForBaseDate emailType baseDateForEvent =
                let
                    currentEventYear =
                        Date.year schedule.currentDate

                    -- Consider the year of the scheduling window start, current year, and next year
                    yearsToConsider =
                        [ Date.year schedulingWindowStart
                        , currentEventYear
                        , currentEventYear + 1
                        ]
                            |> List.sort
                            |> uniqueBy identity

                    -- uniqueBy identity works like List.Extra.unique
                    processYear : Int -> Maybe ScheduledEmail
                    processYear eventYear =
                        let
                            -- Calculate the raw date of the event (e.g., birthday, anniversary, Sept 1st) for the given eventYear
                            rawEventDateInYear =
                                case emailType of
                                    Birthday ->
                                        Date.fromCalendarDate eventYear (Date.month baseDateForEvent) (Date.day baseDateForEvent)

                                    Anniversary ->
                                        Date.fromCalendarDate eventYear (Date.month baseDateForEvent) (Date.day baseDateForEvent)

                                    OctoberBlast ->
                                        -- AEP Reminder is always September 1st
                                        Date.fromCalendarDate eventYear Sep 1

                                    NewYear ->
                                        -- This case should ideally not be hit if NewYear emails are not generated
                                        Date.fromCalendarDate eventYear Jan 1

                                    NoEmails ->
                                        -- This case should not be hit during specific email generation
                                        schedule.currentDate

                            -- Apply pre-notification offsets (e.g., -14 days for Birthday)
                            scheduledTimeWithOffset =
                                case emailType of
                                    Birthday ->
                                        Date.add Date.Days -14 rawEventDateInYear

                                    Anniversary ->
                                        Date.add Date.Days -30 rawEventDateInYear

                                    OctoberBlast ->
                                        rawEventDateInYear

                                    -- No offset defined for OctoberBlast/AEP Reminder
                                    NewYear ->
                                        rawEventDateInYear

                                    NoEmails ->
                                        schedule.currentDate

                            ( finalScheduledTime, birthdayRuleStatus ) =
                                checkBirthdayRuleDelay emailType scheduledTimeWithOffset

                            status : ScheduledEmailStatus
                            status =
                                if Date.compare scheduledTimeWithOffset schedulingWindowStart == LT then
                                    -- Skip if the calculated scheduled date is before our new observation window starts
                                    Skipped "Date is in the past (before window start)"

                                else if
                                    -- Skip if the scheduled date falls within the first year of the policy effective date
                                    Date.compare scheduledTimeWithOffset oneYearAfterEffective
                                        == LT
                                        && Date.compare scheduledTimeWithOffset schedule.effectiveDate
                                        == GT
                                then
                                    Skipped "Within first year of effective date"

                                else
                                    -- Otherwise, apply status from birthday/anniversary rule checks (Scheduled or Delayed)
                                    birthdayRuleStatus
                        in
                        -- Only create a ScheduledEmail record if it's not skipped for being too old
                        if status == Skipped "Date is in the past (before window start)" then
                            Nothing

                        else
                            Just
                                { emailType = emailType
                                , scheduledTime =
                                    if status == Delayed "due to birthday rule window" || status == Delayed "due to anniversary rule window" then
                                        finalScheduledTime

                                    else
                                        scheduledTimeWithOffset
                                , status = status
                                }
                in
                List.filterMap processYear yearsToConsider

            -- Generate emails for each relevant type
            birthdayEmails =
                generateEmailsForBaseDate Birthday schedule.birthDate

            anniversaryEmails =
                generateEmailsForBaseDate Anniversary schedule.effectiveDate

            octoberBlastEmails =
                -- For OctoberBlast (AEP Reminder), baseDateForEvent is only for year context; actual date is Sep 1st
                generateEmailsForBaseDate OctoberBlast schedule.currentDate

            allPotentialEmails =
                List.concat
                    [ birthdayEmails
                    , anniversaryEmails
                    , octoberBlastEmails
                    ]
        in
        allPotentialEmails
            |> List.filter
                (\email ->
                    -- Show all emails that made it this far: Scheduled, Delayed, or Skipped for specific reasons
                    -- (like "Within first year..."). Emails skipped for being "too old" are already filtered out.
                    case email.status of
                        Scheduled ->
                            True

                        Delayed _ ->
                            True

                        Skipped _ ->
                            True
                )
            |> List.sortWith
                (\a b ->
                    Date.compare a.scheduledTime b.scheduledTime
                )
            |> uniqueBy (\email -> ( email.emailType, email.scheduledTime ))



-- VIEW FUNCTIONS


{-| Displays a table of future scheduled emails.

  - `scheduledEmails`: The list of scheduled emails to display.
    Returns an HTML view with a table showing the email type, scheduled date, and status.

-}
viewFutureActivity : List ScheduledEmail -> Html msg
viewFutureActivity scheduledEmails =
    div []
        [ h2 [ class "text-lg font-medium text-gray-900 mb-4" ] [ text "Future Activity" ]
        , p [ class "text-sm text-gray-600 mb-4" ]
            [ text "Note: Emails with a scheduled date in the recent past (up to 30 days ago) that have not yet been sent will be sent in a catch-up batch."
            ]
        , table [ class "min-w-full divide-y divide-gray-300" ]
            [ thead [ class "bg-gray-50" ]
                [ tr []
                    [ th [ class "px-3 py-3.5 text-left text-sm font-semibold text-gray-900" ] [ text "TYPE" ]
                    , th [ class "px-3 py-3.5 text-left text-sm font-semibold text-gray-900" ] [ text "SCHEDULED DATE" ]
                    , th [ class "px-3 py-3.5 text-left text-sm font-semibold text-gray-900" ] [ text "STATUS" ]
                    ]
                ]
            , tbody [ class "divide-y divide-gray-200 bg-white" ]
                (List.map
                    (\email ->
                        tr [ class "hover:bg-gray-50" ]
                            [ td [ class "px-3 py-2 text-sm text-gray-900" ]
                                [ text (scheduledEmailTypeToString email.emailType) ]
                            , td [ class "px-3 py-2 text-sm text-gray-900" ]
                                [ text (Date.format "MMMM ddd, yyyy" email.scheduledTime) ]
                            , td [ class "px-3 py-2 text-sm" ]
                                [ case email.status of
                                    Scheduled ->
                                        span [ class "text-green-600" ] [ text "Scheduled" ]

                                    Skipped reason ->
                                        span [ class "text-orange-600" ] [ text ("Skipped: " ++ reason) ]

                                    Delayed reason ->
                                        span [ class "text-blue-600" ] [ text ("Scheduled - Delayed " ++ reason) ]
                                ]
                            ]
                    )
                    scheduledEmails
                )
            ]
        ]


{-| Displays a single scheduled email as a table row.

  - `email`: The scheduled email to display.
    Returns an HTML table row with the email type, scheduled date, and status.

-}
viewScheduledEmail : ScheduledEmail -> Html msg
viewScheduledEmail email =
    tr [ class "hover:bg-gray-50" ]
        [ td [ class "px-3 py-2 text-sm text-gray-900" ]
            [ text (scheduledEmailTypeToString email.emailType) ]
        , td [ class "px-3 py-2 text-sm text-gray-900" ]
            [ text (Date.format "MMMM ddd, y" email.scheduledTime) ]
        , td [ class "px-3 py-2 text-sm" ]
            [ case email.status of
                Scheduled ->
                    span [ class "text-green-600" ] [ text "Scheduled" ]

                Skipped reason ->
                    span [ class "text-orange-600" ] [ text ("Skipped: " ++ reason) ]

                Delayed reason ->
                    span [ class "text-blue-600" ] [ text ("Delayed " ++ reason) ]
            ]
        ]


{-| Converts a scheduled email type to a human-readable string.

  - `emailType`: The scheduled email type to convert.
    Returns a string representation, including the plan type (e.g., "Birthday (Plan N)").

-}
scheduledEmailTypeToString : ScheduledEmailType -> String
scheduledEmailTypeToString emailType =
    case emailType of
        Birthday ->
            "Birthday (14 days before)"

        Anniversary ->
            "Anniversary (30 days before)"

        NewYear ->
            "New Year"

        OctoberBlast ->
            "AEP Reminder"

        NoEmails ->
            "No Scheduled Emails"
