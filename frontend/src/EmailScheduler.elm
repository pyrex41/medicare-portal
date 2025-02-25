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

import Date exposing (Date)
import Debug
import Html exposing (Html, div, h2, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
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

-}
type ScheduledEmailStatus
    = Scheduled
    | Skipped String


{-| Represents the type of scheduled email, associated with a plan type.

  - `Birthday`: Email for the contact's birthday.
  - `Anniversary`: Email for the plan's anniversary.
  - `NewYear`: Email for New Year greetings.
  - `OctoberBlast`: Email for an October promotional blast.
  - `NoEmails`: No scheduled emails.

-}
type ScheduledEmailType
    = Birthday PlanType
    | Anniversary PlanType
    | NewYear PlanType
    | OctoberBlast PlanType
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
    let
        _ =
            Debug.log "Checking state active for"
                { contactState = schedule.state
                , availableLicenses = schedule.stateLicenses
                }

        isActive =
            List.member schedule.state schedule.stateLicenses
                |> Debug.log "State is active"
    in
    isActive


{-| Calculates the list of scheduled emails for a given email schedule.

  - `schedule`: The email schedule to process.
    Returns a list of `ScheduledEmail` records, each with a type, scheduled time, and status.
    Emails are scheduled for:
  - The contact's next birthday.
  - The plan's next anniversary.
  - The next New Year (January 1st).
  - The next October blast (October 1st).
    Emails within the first year of the effective date are skipped.

-}
getScheduledEmails : EmailSchedule -> List ScheduledEmail
getScheduledEmails schedule =
    if not (isStateActive schedule) then
        [ { emailType = NoEmails
          , scheduledTime = schedule.currentDate
          , status = Skipped "Contact's state is not active"
          }
        ]

    else
        let
            _ =
                Debug.log "Schedule"
                    { effectiveDate = Date.toIsoString schedule.effectiveDate
                    , birthDate = Date.toIsoString schedule.birthDate
                    , currentDate = Date.toIsoString schedule.currentDate
                    , planType = Debug.toString schedule.planType
                    }

            -- Calculate the date one year after the effective date for status checks.
            oneYearAfterEffective : Date
            oneYearAfterEffective =
                Date.add Date.Years 1 schedule.effectiveDate

            -- Determine the next occurrence of an event based on the email type and base date.
            nextOccurrence : ScheduledEmailType -> Date -> Date
            nextOccurrence emailType baseDate =
                let
                    currentYear : Int
                    currentYear =
                        Date.year schedule.currentDate

                    -- Calculate next year's date for birthday and anniversary
                    nextBirthdayOrAnniversaryYear : Date -> Int
                    nextBirthdayOrAnniversaryYear date =
                        let
                            thisYearDate =
                                Date.fromCalendarDate currentYear (Date.month date) (Date.day date)
                        in
                        if Date.compare thisYearDate schedule.currentDate == LT then
                            currentYear + 1

                        else
                            currentYear

                    -- Calculate next New Year's date
                    nextNewYearDate : Date
                    nextNewYearDate =
                        let
                            nextJan1 =
                                Date.fromCalendarDate (currentYear + 1) Jan 1
                        in
                        nextJan1

                    -- For October blast, use current year if October hasn't passed yet
                    octoberThisYear : Date
                    octoberThisYear =
                        Date.fromCalendarDate currentYear Oct 1

                    shouldUseNextYearForOctober : Bool
                    shouldUseNextYearForOctober =
                        Date.compare octoberThisYear schedule.currentDate == LT

                    result =
                        case emailType of
                            Birthday _ ->
                                Date.fromCalendarDate
                                    (nextBirthdayOrAnniversaryYear baseDate)
                                    (Date.month baseDate)
                                    (Date.day baseDate)

                            Anniversary _ ->
                                Date.fromCalendarDate
                                    (nextBirthdayOrAnniversaryYear baseDate)
                                    (Date.month baseDate)
                                    (Date.day baseDate)

                            NewYear _ ->
                                nextNewYearDate

                            OctoberBlast _ ->
                                Date.fromCalendarDate
                                    (if shouldUseNextYearForOctober then
                                        currentYear + 1

                                     else
                                        currentYear
                                    )
                                    Oct
                                    1

                            NoEmails ->
                                schedule.currentDate
                in
                result

            -- Create a scheduled email with the appropriate status.
            createScheduledEmail : ScheduledEmailType -> Date -> ScheduledEmail
            createScheduledEmail emailType baseDate =
                let
                    scheduledTime : Date
                    scheduledTime =
                        nextOccurrence emailType baseDate

                    status : ScheduledEmailStatus
                    status =
                        if Date.compare scheduledTime schedule.currentDate == LT then
                            -- Skip if the date is in the past
                            Skipped "Date is in the past"

                        else if
                            Date.compare scheduledTime oneYearAfterEffective
                                == LT
                                && Date.compare scheduledTime schedule.effectiveDate
                                == GT
                        then
                            Skipped "Within first year of effective date"

                        else
                            Scheduled
                in
                { emailType = emailType
                , scheduledTime = scheduledTime
                , status = status
                }

            -- Helper function to create plan-specific emails for each event type.
            planSpecificEmail : (PlanType -> ScheduledEmailType) -> ScheduledEmail
            planSpecificEmail constructor =
                let
                    emailType =
                        constructor schedule.planType

                    baseDate =
                        case emailType of
                            Birthday _ ->
                                schedule.birthDate

                            Anniversary _ ->
                                schedule.effectiveDate

                            NewYear _ ->
                                -- For New Year, we don't need a base date since we always use Jan 1
                                Date.fromCalendarDate (Date.year schedule.currentDate) Jan 1

                            OctoberBlast _ ->
                                -- For October blast, we don't need a base date since we always use Oct 1
                                Date.fromCalendarDate (Date.year schedule.currentDate) Oct 1

                            NoEmails ->
                                schedule.currentDate
                in
                createScheduledEmail emailType baseDate

            emails =
                [ planSpecificEmail Birthday
                , planSpecificEmail Anniversary
                , planSpecificEmail NewYear
                , planSpecificEmail OctoberBlast
                ]

            _ =
                Debug.log "Generated emails" (List.map (\e -> { type_ = Debug.toString e.emailType, date = Date.toIsoString e.scheduledTime }) emails)
        in
        -- Filter out past dates and sort by date in ascending order (closest dates first)
        List.filter
            (\email ->
                case email.status of
                    Scheduled ->
                        True

                    Skipped _ ->
                        False
            )
            emails
            |> List.sortWith
                (\a b ->
                    Date.compare a.scheduledTime b.scheduledTime
                )



-- VIEW FUNCTIONS


{-| Displays a table of future scheduled emails.

  - `scheduledEmails`: The list of scheduled emails to display.
    Returns an HTML view with a table showing the email type, scheduled date, and status.

-}
viewFutureActivity : List ScheduledEmail -> Html msg
viewFutureActivity scheduledEmails =
    div [ class "mt-6" ]
        [ h2 [ class "text-lg font-medium mb-4" ] [ text "Future Activity" ]
        , table [ class "min-w-full" ]
            [ thead [ class "bg-gray-50" ]
                [ tr []
                    [ th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ]
                        [ text "Type" ]
                    , th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ]
                        [ text "Scheduled Date" ]
                    , th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ]
                        [ text "Status" ]
                    ]
                ]
            , tbody [ class "divide-y divide-gray-200" ]
                (List.map viewScheduledEmail scheduledEmails)
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
            ]
        ]


{-| Converts a scheduled email type to a human-readable string.

  - `emailType`: The scheduled email type to convert.
    Returns a string representation, including the plan type (e.g., "Birthday (Plan N)").

-}
scheduledEmailTypeToString : ScheduledEmailType -> String
scheduledEmailTypeToString emailType =
    let
        planString : PlanType -> String
        planString plan =
            case plan of
                PlanN ->
                    "Plan N"

                PlanG ->
                    "Plan G"

                NoPlan ->
                    "No Plan"
    in
    case emailType of
        Birthday plan ->
            "Birthday (" ++ planString plan ++ ")"

        Anniversary plan ->
            "Anniversary (" ++ planString plan ++ ")"

        NewYear plan ->
            "New Year (" ++ planString plan ++ ")"

        OctoberBlast plan ->
            "AEP Blast (" ++ planString plan ++ ")"

        NoEmails ->
            "No Scheduled Emails"
