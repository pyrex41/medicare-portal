module Utils.Formatters exposing (formatPhoneNumber)


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    if String.isEmpty phone then
        ""

    else
        let
            digits =
                String.filter Char.isDigit phone

            len =
                String.length digits
        in
        if len == 0 then
            ""

        else if len == 11 && String.left 1 digits == "1" then
            let
                area =
                    String.slice 1 4 digits

                prefix =
                    String.slice 4 7 digits

                line =
                    String.slice 7 11 digits
            in
            "+1 (" ++ area ++ ") " ++ prefix ++ "-" ++ line

        else if len <= 3 then
            "(" ++ String.left len digits

        else if len <= 6 then
            "(" ++ String.left 3 digits ++ ") " ++ String.dropLeft 3 digits

        else
            "(" ++ String.left 3 digits ++ ") " ++ String.slice 3 6 digits ++ "-" ++ String.dropLeft 6 digits
