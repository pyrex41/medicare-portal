module Utils.MyDate exposing (dateFromMonthDayYear)

import Date exposing (Date)
import List.Extra


dateFromMonthDayYear : String -> Result String Date
dateFromMonthDayYear dateString =
    let
        monthDayYear =
            String.split "/" dateString

        has3 =
            List.length monthDayYear == 3

        year =
            List.Extra.last monthDayYear

        month =
            List.head monthDayYear

        day =
            List.tail monthDayYear
                |> Maybe.withDefault []
                |> List.head
    in
    if has3 then
        case ( year, month, day ) of
            ( Just y, Just m, Just d ) ->
                let
                    mm =
                        if String.length m == 1 then
                            "0" ++ m

                        else
                            m

                    dd =
                        if String.length d == 1 then
                            "0" ++ d

                        else
                            d
                in
                Date.fromIsoString (String.join "-" [ y, mm, dd ])

            _ ->
                Err "Invalid date format 1"

    else
        Err "Invalid date format 2"
