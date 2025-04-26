module Utils.DiscountDescription exposing (..)

import CarrierNaic exposing (Carrier(..))
import Csv.Decode as Decode exposing (Decoder)


isAchieve : Carrier -> String -> String -> Bool
isAchieve carrier naic state =
    case carrier of
        Humana ->
            case naic of
                "60052" ->
                    List.member state [ "CA", "NJ", "SC" ]

                "60219" ->
                    List.member state [ "PA", "TX" ]

                "88595" ->
                    List.member state [ "DE", "MT", "NH", "SD", "WY", "WI" ]

                "73288" ->
                    List.member state [ "GA" ]

                "60984" ->
                    List.member state stateListMost

                _ ->
                    False

        _ ->
            False


stateListMost : List String
stateListMost =
    [ "AL"
    , "AZ"
    , "AR"
    , "FL"
    , "IL"
    , "IA"
    , "IN"
    , "KS"
    , "KY"
    , "LA"
    , "MI"
    , "MS"
    , "NE"
    , "NC"
    , "ND"
    , "OH"
    , "OK"
    , "PA"
    , "TN"
    , "WV"
    ]


type alias DiscountInfo =
    { state : String
    , aceChubb : Maybe String
    , aetna : Maybe String
    , aflac : Maybe String
    , allstate : Maybe String
    , anthem : Maybe String
    , cigna : Maybe String
    , mutualOfOmaha : Maybe String
    , humana : Maybe String
    , humanaAchieve : Maybe String
    , uhc : Maybe String
    }


discountInfoDecoder : Decoder DiscountInfo
discountInfoDecoder =
    Decode.into DiscountInfo
        |> Decode.pipeline (Decode.column 0 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 1 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 2 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 3 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 4 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 5 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 6 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 7 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 8 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 9 Decode.string)
        |> Decode.pipeline (Decode.optionalColumn 10 Decode.string)



{--| In a real application, this would load the CSV file using HTTP or ports.
For now, we'll use a hardcoded string since Elm doesn't directly support
file system operations. In a production app, this would be loaded from
a file during build time or fetched via HTTP.
--}
{--| Parse the CSV data and convert it to a list of DiscountInfo records.
In a real application, error handling would be more robust.
--}


getDiscountInfos : String -> Result String (List DiscountInfo)
getDiscountInfos csvData =
    Decode.decodeCsv Decode.NoFieldNames discountInfoDecoder csvData
        |> Result.mapError Decode.errorToString



{--| Get the household discount description for a specific carrier, NAIC, and state.
Returns a Maybe String wrapped in a Result to handle both the case of successful
parsing with no discount (Nothing) and the case of parsing errors (Err).
--}


discountDescription : String -> Carrier -> String -> String -> Result String (Maybe String)
discountDescription csvString carrier naic state =
    getDiscountInfos csvString
        |> Result.andThen
            (\discountInfos ->
                -- Find the discount info for the given state
                case List.filter (\info -> info.state == state) discountInfos of
                    [] ->
                        Err ("No discount information found for state: " ++ state)

                    stateInfo :: _ ->
                        -- Clean up "NA" values to be Nothing
                        let
                            cleanNA : Maybe String -> Maybe String
                            cleanNA maybeStr =
                                maybeStr
                                    |> Maybe.andThen
                                        (\str ->
                                            if str == "NA" then
                                                Nothing

                                            else
                                                Just str
                                        )
                        in
                        case carrier of
                            AceChubb ->
                                Ok (cleanNA stateInfo.aceChubb)

                            Aetna ->
                                Ok (cleanNA stateInfo.aetna)

                            Aflac ->
                                Ok (cleanNA stateInfo.aflac)

                            Allstate ->
                                Ok (cleanNA stateInfo.allstate)

                            Cigna ->
                                Ok (cleanNA stateInfo.cigna)

                            MutualOfOmaha ->
                                Ok (cleanNA stateInfo.mutualOfOmaha)

                            UnitedHealthcare ->
                                Ok (cleanNA stateInfo.uhc)

                            Humana ->
                                if isAchieve carrier naic state then
                                    Ok (cleanNA stateInfo.humanaAchieve)

                                else
                                    Ok (cleanNA stateInfo.humana)
            )
