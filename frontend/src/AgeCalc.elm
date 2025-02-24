module AgeCalc exposing (getAgeNextMonth)

import Date exposing (Date, Interval(..), Unit(..), add, fromIsoString, toIsoString)
import Time exposing (Month(..))


getAgeNextMonth : String -> Date -> Int
getAgeNextMonth birthDateStr currentDate =
    case fromIsoString birthDateStr of
        Ok birthDate ->
            let
                -- Get first of next month
                nextMonth =
                    currentDate
                        |> add Months 1
                        |> Date.floor Month

                -- Calculate years between birth date and first of next month
                years =
                    Date.diff Years birthDate nextMonth
            in
            years

        Err _ ->
            0
