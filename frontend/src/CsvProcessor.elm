module CsvProcessor exposing
    ( CarrierMapping
    , ColumnMapping
    , Error(..)
    , ProcessedContact
    , extractHeaders
    , extractUniqueValues
    , findBestMatch
    , processCsvToContacts
    , suggestCarrierMappings
    , suggestColumnMappings
    , transformCsvData
    )

import Csv.Parser
import Dict exposing (Dict)
import Fuzzy
import List.Extra
import Set



-- TYPES


type Error
    = ParseError String
    | EmptyFile
    | NoHeaders


type alias ColumnMapping =
    { firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , currentCarrier : String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : String
    , gender : String
    , zipCode : String
    , planType : String
    }


type alias CarrierMapping =
    { detectedCarriers : List String
    , mappings : Dict String String -- Original carrier name -> Standardized carrier name
    }


type alias FieldVariations =
    Dict String (List String)


emptyColumnMapping : ColumnMapping
emptyColumnMapping =
    { firstName = ""
    , lastName = ""
    , email = ""
    , phoneNumber = ""
    , currentCarrier = ""
    , effectiveDate = ""
    , birthDate = ""
    , tobaccoUser = ""
    , gender = ""
    , zipCode = ""
    , planType = ""
    }


emptyCarrierMapping : CarrierMapping
emptyCarrierMapping =
    { detectedCarriers = []
    , mappings = Dict.empty
    }



-- Add new type for processed contact data


type alias ProcessedContact =
    { firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , currentCarrier : String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : Bool
    , gender : String
    , zipCode : String
    , planType : String
    }



-- EXTRACT CSV DATA


extractHeaders : String -> Result Error (List String)
extractHeaders csvContent =
    if String.isEmpty (String.trim csvContent) then
        Err EmptyFile

    else
        case Csv.Parser.parse { fieldSeparator = ',' } csvContent of
            Ok rows ->
                case List.head rows of
                    Just headerRow ->
                        Ok headerRow

                    Nothing ->
                        Err NoHeaders

            Err _ ->
                Err (ParseError "Failed to parse CSV file")


extractUniqueValues : String -> String -> Result Error (List String)
extractUniqueValues csvContent columnName =
    if String.isEmpty (String.trim csvContent) then
        Err EmptyFile

    else
        case Csv.Parser.parse { fieldSeparator = ',' } csvContent of
            Ok rows ->
                case List.head rows of
                    Just headerRow ->
                        -- Find the index of the column
                        let
                            columnIndex =
                                List.indexedMap (\i h -> ( i, h )) headerRow
                                    |> List.filter (\( _, h ) -> h == columnName)
                                    |> List.head
                                    |> Maybe.map Tuple.first
                        in
                        case columnIndex of
                            Just index ->
                                -- Get values from that column (skip header)
                                let
                                    values =
                                        List.drop 1 rows
                                            |> List.filterMap
                                                (\row ->
                                                    if index < List.length row then
                                                        Just (getAt index row |> Maybe.withDefault "")

                                                    else
                                                        Nothing
                                                )
                                            |> List.filter (not << String.isEmpty)
                                            |> List.map String.trim
                                            |> uniqueValues
                                in
                                Ok values

                            Nothing ->
                                Err (ParseError ("Column not found: " ++ columnName))

                    Nothing ->
                        Err NoHeaders

            Err _ ->
                Err (ParseError "Failed to parse CSV file")



-- FUZZY MATCHING AND SUGGESTIONS


suggestColumnMappings : List String -> ColumnMapping
suggestColumnMappings headers =
    let
        -- Field name variations
        fieldVariations =
            createFieldVariations

        -- For each field, find the best matching header
        findBestHeader : String -> List String -> List String -> String
        findBestHeader field variations allHeaders =
            let
                -- Try exact matches first
                exactMatches =
                    List.filter
                        (\h ->
                            List.member (String.toLower h) (List.map String.toLower (field :: variations))
                        )
                        allHeaders
            in
            case List.head exactMatches of
                Just match ->
                    match

                Nothing ->
                    -- Try fuzzy matching if no exact matches
                    findBestMatch field variations allHeaders
    in
    { firstName = findBestHeader "firstName" (Maybe.withDefault [] (Dict.get "firstName" fieldVariations)) headers
    , lastName = findBestHeader "lastName" (Maybe.withDefault [] (Dict.get "lastName" fieldVariations)) headers
    , email = findBestHeader "email" (Maybe.withDefault [] (Dict.get "email" fieldVariations)) headers
    , phoneNumber = findBestHeader "phoneNumber" (Maybe.withDefault [] (Dict.get "phoneNumber" fieldVariations)) headers
    , currentCarrier = findBestHeader "currentCarrier" (Maybe.withDefault [] (Dict.get "currentCarrier" fieldVariations)) headers
    , effectiveDate = findBestHeader "effectiveDate" (Maybe.withDefault [] (Dict.get "effectiveDate" fieldVariations)) headers
    , birthDate = findBestHeader "birthDate" (Maybe.withDefault [] (Dict.get "birthDate" fieldVariations)) headers
    , tobaccoUser = findBestHeader "tobaccoUser" (Maybe.withDefault [] (Dict.get "tobaccoUser" fieldVariations)) headers
    , gender = findBestHeader "gender" (Maybe.withDefault [] (Dict.get "gender" fieldVariations)) headers
    , zipCode = findBestHeader "zipCode" (Maybe.withDefault [] (Dict.get "zipCode" fieldVariations)) headers
    , planType = findBestHeader "planType" (Maybe.withDefault [] (Dict.get "planType" fieldVariations)) headers
    }


suggestCarrierMappings : List String -> List { name : String, aliases : List String } -> CarrierMapping
suggestCarrierMappings detectedCarriers standardCarriers =
    let
        mappings =
            List.foldl
                (\carrier dict ->
                    let
                        matchedCarrier =
                            findBestCarrierMatch carrier standardCarriers
                    in
                    Dict.insert carrier matchedCarrier dict
                )
                Dict.empty
                detectedCarriers
    in
    { detectedCarriers = detectedCarriers
    , mappings = mappings
    }


findBestCarrierMatch : String -> List { name : String, aliases : List String } -> String
findBestCarrierMatch carrierName standardCarriers =
    let
        normalizedCarrierName =
            String.toLower (String.trim carrierName)

        -- Add special case handling for common patterns
        specialCaseMatch =
            case normalizedCarrierName of
                "aarp" ->
                    Just "United Healthcare"

                "aarp / uhc" ->
                    Just "United Healthcare"

                "aarp/uhc" ->
                    Just "United Healthcare"

                "aarp / uhica" ->
                    Just "United Healthcare"

                "aarp/uhica" ->
                    Just "United Healthcare"

                "uhc" ->
                    Just "United Healthcare"

                "uhica" ->
                    Just "United Healthcare"

                "ace / chubb" ->
                    Just "Ace Chubb"

                "ace/chubb" ->
                    Just "Ace Chubb"

                "ace chubb" ->
                    Just "Ace Chubb"

                "cigna healthspring" ->
                    Just "Cigna"

                "cigna-healthspring" ->
                    Just "Cigna"

                "bcbs" ->
                    Just "Blue Cross Blue Shield"

                "blue cross" ->
                    Just "Blue Cross Blue Shield"

                "blue shield" ->
                    Just "Blue Cross Blue Shield"

                "humana gold" ->
                    Just "Humana"

                "humana gold plus" ->
                    Just "Humana"

                "wellcare by allwell" ->
                    Just "Wellcare"

                "allwell" ->
                    Just "Wellcare"

                _ ->
                    Nothing

        -- First check for special case matches
        -- Then check for exact matches
        -- Then try fuzzy matching
        exactMatches =
            List.filter
                (\carrier ->
                    String.toLower carrier.name
                        == normalizedCarrierName
                        || List.member normalizedCarrierName (List.map String.toLower carrier.aliases)
                )
                standardCarriers

        -- Try fuzzy matching if no exact match
        fuzzyMatches =
            if List.isEmpty exactMatches then
                List.map
                    (\carrier ->
                        let
                            -- Match against carrier name
                            nameScore =
                                Fuzzy.match [] [] normalizedCarrierName (String.toLower carrier.name)

                            -- Match against aliases and take best score
                            aliasScores =
                                List.map
                                    (\alias -> Fuzzy.match [] [] normalizedCarrierName (String.toLower alias))
                                    carrier.aliases

                            bestAliasScore =
                                List.sortBy .score aliasScores
                                    |> List.head
                                    |> Maybe.withDefault { score = 100000, matches = [] }

                            -- Take the better of the name or alias match
                            bestScore =
                                if nameScore.score <= bestAliasScore.score then
                                    nameScore.score

                                else
                                    bestAliasScore.score
                        in
                        ( carrier.name, bestScore )
                    )
                    standardCarriers
                    |> List.sortBy Tuple.second
                    |> List.head

            else
                Nothing
    in
    case specialCaseMatch of
        Just name ->
            name

        Nothing ->
            case exactMatches of
                carrier :: _ ->
                    carrier.name

                [] ->
                    case fuzzyMatches of
                        Just ( name, score ) ->
                            -- Only use fuzzy match if the score is good enough
                            if score < 10 then
                                name

                            else
                                "Other"

                        Nothing ->
                            "Other"



-- Default to "Other" if no good match found


findBestMatch : String -> List String -> List String -> String
findBestMatch field variations candidates =
    let
        -- Normalize everything for comparison
        normalize s =
            String.toLower (String.trim s)

        normalizedField =
            normalize field

        normalizedVariations =
            List.map normalize variations

        normalizedCandidates =
            List.map normalize candidates

        -- Get the candidate with the best fuzzy match score
        allQueries =
            normalizedField :: normalizedVariations

        scoredCandidates =
            List.map2
                (\normalizedCandidate originalCandidate ->
                    let
                        scores =
                            List.map
                                (\query ->
                                    Fuzzy.match [] [] query normalizedCandidate
                                )
                                allQueries

                        bestScore =
                            List.sortBy .score scores
                                |> List.head
                                |> Maybe.withDefault { score = 100000, matches = [] }
                    in
                    ( originalCandidate, bestScore.score )
                )
                normalizedCandidates
                candidates

        bestMatch =
            List.sortBy Tuple.second scoredCandidates
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault ""
    in
    bestMatch



-- HELPERS


getAt : Int -> List a -> Maybe a
getAt index list =
    if index < 0 then
        Nothing

    else
        List.head (List.drop index list)


uniqueValues : List String -> List String
uniqueValues list =
    list
        |> Set.fromList
        |> Set.toList


createFieldVariations : FieldVariations
createFieldVariations =
    Dict.fromList
        [ ( "firstName", [ "first_name", "firstname", "first name", "fname", "first", "given name", "givenname" ] )
        , ( "lastName", [ "last_name", "lastname", "last name", "lname", "last", "surname", "family name", "familyname" ] )
        , ( "email", [ "email_address", "emailaddress", "email address", "e_mail", "e-mail" ] )
        , ( "phoneNumber", [ "phone_number", "phonenumber", "phone", "cell", "mobile", "telephone", "contact_number", "contact number", "phone number", "cell number", "mobile number" ] )
        , ( "currentCarrier", [ "current_carrier", "current carrier", "carrier", "insurance_provider", "insurance provider", "provider", "current_provider", "current provider", "insurance", "insurance carrier", "insurance_carrier", "insurancecarrier" ] )
        , ( "effectiveDate", [ "effective_date", "effectivedate", "effective date", "start_date", "startdate", "start date", "date_effective", "date effective", "coverage date", "plan start date" ] )
        , ( "birthDate", [ "birth_date", "birthdate", "birth date", "dob", "date_of_birth", "date of birth", "born", "birthday" ] )
        , ( "tobaccoUser", [ "tobacco_user", "tobaccouser", "tobacco user", "tobacco", "smoker", "tobacco_status", "tobacco status", "smokes", "is smoker", "smoking" ] )
        , ( "gender", [ "sex", "m/f", "male/female" ] )
        , ( "zipCode", [ "zip_code", "zipcode", "zip", "postal_code", "postalcode", "postal code", "postal", "zip code" ] )
        , ( "planType", [ "plan_type", "plantype", "plan type", "plan", "insurance_type", "insurancetype", "insurance type", "coverage type", "coverage" ] )
        ]



-- CSV TRANSFORMATION


transformCsvData : String -> ColumnMapping -> Result Error String
transformCsvData csvContent columnMapping =
    case Csv.Parser.parse { fieldSeparator = ',' } csvContent of
        Ok rows ->
            case List.head rows of
                Just headers ->
                    let
                        -- Create new standardized headers
                        standardHeaders =
                            [ "first_name"
                            , "last_name"
                            , "email"
                            , "phone_number"
                            , "current_carrier"
                            , "effective_date"
                            , "birth_date"
                            , "tobacco_user"
                            , "gender"
                            , "zip_code"
                            , "plan_type"
                            ]

                        -- Create mapping from old to new headers
                        headerMapping =
                            [ ( columnMapping.firstName, "first_name" )
                            , ( columnMapping.lastName, "last_name" )
                            , ( columnMapping.email, "email" )
                            , ( columnMapping.phoneNumber, "phone_number" )
                            , ( columnMapping.currentCarrier, "current_carrier" )
                            , ( columnMapping.effectiveDate, "effective_date" )
                            , ( columnMapping.birthDate, "birth_date" )
                            , ( columnMapping.tobaccoUser, "tobacco_user" )
                            , ( columnMapping.gender, "gender" )
                            , ( columnMapping.zipCode, "zip_code" )
                            , ( columnMapping.planType, "plan_type" )
                            ]
                                |> Dict.fromList

                        -- Get indices of columns we want to keep
                        columnIndices =
                            headers
                                |> List.indexedMap Tuple.pair
                                |> List.filter
                                    (\( _, h ) ->
                                        List.member h
                                            [ columnMapping.firstName
                                            , columnMapping.lastName
                                            , columnMapping.email
                                            , columnMapping.phoneNumber
                                            , columnMapping.currentCarrier
                                            , columnMapping.effectiveDate
                                            , columnMapping.birthDate
                                            , columnMapping.tobaccoUser
                                            , columnMapping.gender
                                            , columnMapping.zipCode
                                            , columnMapping.planType
                                            ]
                                    )
                                |> List.map Tuple.first

                        -- Transform each row
                        transformedRows =
                            List.map
                                (\row ->
                                    List.filterMap (\i -> getAt i row) columnIndices
                                )
                                (List.drop 1 rows)

                        -- Convert back to CSV string
                        toCsvRow : List String -> String
                        toCsvRow row =
                            row
                                |> List.map
                                    (\cell ->
                                        if String.contains "," cell then
                                            "\"" ++ cell ++ "\""

                                        else
                                            cell
                                    )
                                |> String.join ","
                    in
                    Ok (String.join "\n" (toCsvRow standardHeaders :: List.map toCsvRow transformedRows))

                Nothing ->
                    Err NoHeaders

        Err _ ->
            Err (ParseError "Failed to parse CSV file")



-- Add function to validate an email address


isValidEmail : String -> Bool
isValidEmail email =
    not (String.isEmpty (String.trim email))
        && String.contains "@" email
        && String.contains "." email
        && String.length email
        > 5



-- Add a type for invalid contacts with reason


type alias InvalidContact =
    { rowData : List String
    , email : String
    , reason : String
    , rowNumber : Int
    }



-- Update the return type to include both valid and invalid contacts


processCsvToContacts : String -> ColumnMapping -> CarrierMapping -> Result Error { valid : List ProcessedContact, invalid : List InvalidContact }
processCsvToContacts csvContent columnMapping carrierMapping =
    case Csv.Parser.parse { fieldSeparator = ',' } csvContent of
        Ok rows ->
            case List.head rows of
                Just headers ->
                    let
                        -- Get indices for each field
                        getColumnIndex : String -> Maybe Int
                        getColumnIndex columnName =
                            List.indexedMap Tuple.pair headers
                                |> List.filter (\( _, h ) -> h == columnName)
                                |> List.head
                                |> Maybe.map Tuple.first

                        -- Get indices for all required fields
                        indices =
                            { firstName = getColumnIndex columnMapping.firstName
                            , lastName = getColumnIndex columnMapping.lastName
                            , email = getColumnIndex columnMapping.email
                            , phoneNumber = getColumnIndex columnMapping.phoneNumber
                            , currentCarrier = getColumnIndex columnMapping.currentCarrier
                            , effectiveDate = getColumnIndex columnMapping.effectiveDate
                            , birthDate = getColumnIndex columnMapping.birthDate
                            , tobaccoUser = getColumnIndex columnMapping.tobaccoUser
                            , gender = getColumnIndex columnMapping.gender
                            , zipCode = getColumnIndex columnMapping.zipCode
                            , planType = getColumnIndex columnMapping.planType
                            }

                        -- Process each row, categorizing as valid or invalid
                        processRow : List String -> ( Maybe ProcessedContact, Maybe InvalidContact )
                        processRow row =
                            let
                                getValue : Maybe Int -> String
                                getValue maybeIndex =
                                    case maybeIndex of
                                        Just i ->
                                            if i < List.length row then
                                                row |> getAt i |> Maybe.withDefault ""

                                            else
                                                ""

                                        Nothing ->
                                            ""

                                -- Get carrier value and map it to standardized name
                                rawCarrier =
                                    getValue indices.currentCarrier

                                mappedCarrier =
                                    Dict.get rawCarrier carrierMapping.mappings
                                        |> Maybe.withDefault rawCarrier

                                -- Parse tobacco user value
                                parseTobaccoUser : String -> Bool
                                parseTobaccoUser val =
                                    let
                                        lower =
                                            String.toLower (String.trim val)
                                    in
                                    lower == "yes" || lower == "true" || lower == "1" || lower == "y"

                                -- Get email value
                                email =
                                    getValue indices.email

                                -- Check if email is valid
                                emailValid =
                                    isValidEmail email
                            in
                            if not emailValid then
                                ( Nothing
                                , Just
                                    { rowData = row
                                    , email = email
                                    , reason = "Invalid email format"
                                    , rowNumber = 0
                                    }
                                )

                            else
                                ( Just
                                    { firstName = getValue indices.firstName
                                    , lastName = getValue indices.lastName
                                    , email = email
                                    , phoneNumber = getValue indices.phoneNumber |> String.filter Char.isDigit
                                    , currentCarrier = mappedCarrier
                                    , effectiveDate = getValue indices.effectiveDate
                                    , birthDate = getValue indices.birthDate
                                    , tobaccoUser = getValue indices.tobaccoUser |> parseTobaccoUser
                                    , gender = getValue indices.gender
                                    , zipCode = getValue indices.zipCode
                                    , planType = getValue indices.planType
                                    }
                                , Nothing
                                )

                        -- Process all rows except header
                        processed =
                            rows
                                |> List.drop 1
                                |> List.indexedMap
                                    (\index row ->
                                        let
                                            ( maybeContact, maybeInvalidContact ) =
                                                processRow row

                                            updatedInvalidContact =
                                                maybeInvalidContact
                                                    |> Maybe.map (\contact -> { contact | rowNumber = index + 1 })
                                        in
                                        ( maybeContact, updatedInvalidContact )
                                    )

                        -- Separate valid and invalid contacts
                        validContacts =
                            processed
                                |> List.filterMap Tuple.first

                        invalidContacts =
                            processed
                                |> List.filterMap Tuple.second
                    in
                    Ok { valid = validContacts, invalid = invalidContacts }

                Nothing ->
                    Err NoHeaders

        Err _ ->
            Err (ParseError "Failed to parse CSV file")
