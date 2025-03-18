module CarrierNaic exposing
    ( Carrier(..)
    , allCarriers
    , carrierDecoder
    , carrierToNaics
    , carrierToString
    , naicToCarrier
    , stringToCarrier
    )

import Json.Decode as Decode exposing (Decoder)


type Carrier
    = Aetna
    | Humana
    | UnitedHealthcare
    | Cigna
    | Aflac
    | Allstate
    | MutualOfOmaha
    | AceChubb


allCarriers : List Carrier
allCarriers =
    [ Aetna
    , Humana
    , UnitedHealthcare
    , Cigna
    , Aflac
    , Allstate
    , MutualOfOmaha
    , AceChubb
    ]


carrierToString : Carrier -> String
carrierToString carrier =
    case carrier of
        Aetna ->
            "Aetna"

        Humana ->
            "Humana"

        UnitedHealthcare ->
            "UnitedHealthcare"

        Cigna ->
            "Cigna"

        Aflac ->
            "Aflac"

        Allstate ->
            "Allstate"

        MutualOfOmaha ->
            "Mutual of Omaha"

        AceChubb ->
            "Ace Chubb"


stringToCarrier : String -> Maybe Carrier
stringToCarrier str =
    case String.toLower str of
        "aetna" ->
            Just Aetna

        "humana" ->
            Just Humana

        "unitedhealthcare" ->
            Just UnitedHealthcare

        "uhc" ->
            Just UnitedHealthcare

        "united healthcare" ->
            Just UnitedHealthcare

        "cigna" ->
            Just Cigna

        "aflac" ->
            Just Aflac

        "allstate" ->
            Just Allstate

        "mutual of omaha" ->
            Just MutualOfOmaha

        "ace chubb" ->
            Just AceChubb

        "ace" ->
            Just AceChubb

        "chubb" ->
            Just AceChubb

        _ ->
            Nothing


carrierToNaics : Carrier -> List String
carrierToNaics carrier =
    case carrier of
        Aetna ->
            [ "72052" -- Aetna Hlth Ins Co
            , "78700" -- Aetna Hlth & Life Ins Co
            , "68500" -- Continental Life Ins Co Brentwood
            ]

        Humana ->
            [ "12634" -- Humana Ins Co of NY
            , "60052" -- Humana Insurance Company
            , "60219" -- Humana Insurance Company
            , "60984" -- Humana Insurance Company
            , "69671" -- Humana Insurance Company
            , "70580" -- Humana Insurance Company
            , "73288" -- Humana Ins Co
            , "88595" -- Humana Insurance Company
            , "95158" -- Humana Insurance Company
            ]

        UnitedHealthcare ->
            [ "60093" -- United Hlthcare Ins Co Of NY
            , "79413" -- UnitedHealthcare Ins Co
            ]

        Cigna ->
            [ "61727" -- Cigna National Health Ins Co
            , "65269" -- Cigna Ins Co
            , "65722" -- Loyal Amer Life Ins Co (CIGNA)
            , "67369" -- Cigna Hlth & Life Ins Co
            , "88366" -- American Retirement Life Ins Co (CIGNA)
            ]

        Aflac ->
            [ "60380" -- AFLAC
            ]

        Allstate ->
            [ "60534" -- Allstate Health Solutions (AHL)
            , "82538" -- Allstate Health Solutions
            ]

        MutualOfOmaha ->
            [ "13100" -- Omaha Ins Co
            , "71412" -- Mutual Of Omaha Ins Co
            , "72850" -- United World Life Ins Co
            ]

        AceChubb ->
            [ "20699" -- Ace Prop & Cas Ins Co
            ]


naicToCarrier : String -> Maybe Carrier
naicToCarrier naic =
    case naic of
        -- Aetna
        "72052" ->
            Just Aetna

        "78700" ->
            Just Aetna

        "68500" ->
            Just Aetna

        -- Humana
        "12634" ->
            Just Humana

        "60052" ->
            Just Humana

        "60219" ->
            Just Humana

        "60984" ->
            Just Humana

        "69671" ->
            Just Humana

        "70580" ->
            Just Humana

        "73288" ->
            Just Humana

        "88595" ->
            Just Humana

        "95158" ->
            Just Humana

        -- UnitedHealthcare
        "60093" ->
            Just UnitedHealthcare

        "79413" ->
            Just UnitedHealthcare

        -- Cigna
        "61727" ->
            Just Cigna

        "65269" ->
            Just Cigna

        "65722" ->
            Just Cigna

        "67369" ->
            Just Cigna

        "88366" ->
            Just Cigna

        -- Aflac
        "60380" ->
            Just Aflac

        -- Allstate
        "60534" ->
            Just Allstate

        "82538" ->
            Just Allstate

        -- Mutual of Omaha
        "13100" ->
            Just MutualOfOmaha

        "71412" ->
            Just MutualOfOmaha

        "72850" ->
            Just MutualOfOmaha

        -- Ace Chubb
        "20699" ->
            Just AceChubb

        _ ->
            Nothing


carrierDecoder : Decoder Carrier
carrierDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case stringToCarrier str of
                    Just carrier ->
                        Decode.succeed carrier

                    Nothing ->
                        Decode.fail "Invalid carrier"
            )
