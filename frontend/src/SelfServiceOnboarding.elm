port module SelfServiceOnboarding exposing (..)

import Browser
import Browser.Navigation as Nav
import CarrierNaic exposing (Carrier, allCarriers, carrierDecoder, carrierToString, stringToCarrier)
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Task
import Time
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser, oneOf, string)
import Utils.QuoteHeader exposing (viewHeader)



-- PORTS


port saveDebugInfo : String -> Cmd msg


port clearDebugInfo : () -> Cmd msg



-- MODEL


type CarrierChoice
    = HasCarrier Carrier
    | NoneOther


type alias Model =
    { orgId : Maybe String
    , orgSlug : Maybe String
    , logo : Maybe String
    , orgName : Maybe String
    , agentId : Maybe String
    , email : String
    , firstName : String
    , lastName : String
    , zipCode : String
    , dateOfBirth : String
    , gender : String
    , tobacco : Bool
    , phoneNumber : String
    , currentPremium : String
    , currentCarrier : String
    , carrier : Maybe CarrierChoice
    , planType : String
    , optInQuarterlyUpdates : Bool
    , emailReadOnly : Bool
    , isSubmitting : Bool
    , isGeneratingQuote : Bool
    , error : Maybe String
    , success : Bool
    , key : Nav.Key
    , currentDate : Maybe Date
    , state : Maybe String
    , counties : List String
    , selectedCounty : Maybe String
    , isLoadingZipData : Bool
    , zipError : Maybe String
    }



-- INIT


type Route
    = SlugRoute String
    | QueryRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map SlugRoute (Parser.s "self-onboarding" </> string)
        , Parser.map QueryRoute Parser.top
        ]


parseUrl : Url -> Route
parseUrl url =
    Parser.parse routeParser url |> Maybe.withDefault QueryRoute


type alias UrlParams =
    { orgId : Maybe String
    , email : Maybe String
    , hash : Maybe String
    , quoteId : Maybe String
    , agentId : Maybe String
    }


init : Nav.Key -> Url -> ( Model, Cmd Msg )
init key url =
    let
        route =
            parseUrl url

        queryParams =
            url.query
                |> Maybe.map (String.split "&")
                |> Maybe.withDefault []
                |> List.map (String.split "=")
                |> List.filterMap
                    (\parts ->
                        case parts of
                            [ key0, value0 ] ->
                                Just ( key0, Url.percentDecode value0 |> Maybe.withDefault value0 )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList

        getParam name =
            Dict.get name queryParams

        initialModel =
            { orgId = Nothing
            , orgSlug = Nothing
            , logo = Nothing
            , orgName = Nothing
            , agentId = Nothing
            , email = ""
            , firstName = ""
            , lastName = ""
            , zipCode = ""
            , dateOfBirth = ""
            , gender = "M"
            , tobacco = False
            , phoneNumber = ""
            , currentPremium = ""
            , currentCarrier = ""
            , carrier = Nothing
            , planType = ""
            , optInQuarterlyUpdates = True
            , emailReadOnly = False
            , isSubmitting = False
            , isGeneratingQuote = False
            , error = Nothing
            , success = False
            , key = key
            , currentDate = Nothing
            , state = Nothing
            , counties = []
            , selectedCounty = Nothing
            , isLoadingZipData = False
            , zipError = Nothing
            }

        commands =
            [ Task.perform GotCurrentDate Date.today ]

        -- Clear any previous debug info
        clearCmd =
            clearDebugInfo ()
    in
    case route of
        SlugRoute slug ->
            let
                email =
                    getParam "email"

                quoteId =
                    getParam "quoteId"

                agentId =
                    getParam "agentId"

                debugInfo =
                    "Initializing with slug="
                        ++ slug
                        ++ ", email="
                        ++ (email |> Maybe.withDefault "none")
                        ++ ", quoteId="
                        ++ (quoteId |> Maybe.withDefault "none")
                        ++ ", url.query="
                        ++ (url.query |> Maybe.withDefault "none")

                fetchOrgDetailsCmd =
                    let
                        queryParamsStr =
                            url.query |> Maybe.withDefault ""

                        apiUrl =
                            "/api/self-service/"
                                ++ slug
                                ++ (if String.isEmpty queryParamsStr then
                                        ""

                                    else
                                        "?" ++ queryParamsStr
                                   )
                    in
                    Cmd.batch
                        [ saveDebugInfo <| "Fetching org details: " ++ apiUrl
                        , Http.get
                            { url = apiUrl
                            , expect = Http.expectJson GotOrgDetails orgDetailsDecoder
                            }
                        ]

                fetchContactDetailsCmd =
                    case quoteId of
                        Just qid ->
                            fetchContactDetails qid

                        Nothing ->
                            Cmd.none
            in
            ( { initialModel
                | orgSlug = Just slug
                , agentId = agentId
              }
            , Cmd.batch
                (clearCmd
                    :: saveDebugInfo debugInfo
                    :: fetchOrgDetailsCmd
                    :: fetchContactDetailsCmd
                    :: commands
                )
            )

        QueryRoute ->
            -- Handle query parameters for backward compatibility
            let
                params =
                    parseUrlParams url

                orgId =
                    getParam "orgId"

                email =
                    getParam "email"

                hash =
                    getParam "hash"

                quoteId =
                    getParam "quoteId"

                debugInfo =
                    "Initializing with orgId="
                        ++ (orgId |> Maybe.withDefault "none")
                        ++ ", email="
                        ++ (email |> Maybe.withDefault "none")
                        ++ ", quoteId="
                        ++ (quoteId |> Maybe.withDefault "none")

                initCmd =
                    case orgId of
                        Just oid ->
                            -- If we have an org ID, fetch org details which will include contact if found
                            let
                                apiQueryParams =
                                    List.filterMap identity
                                        [ Maybe.map (\e -> ( "email", e )) email
                                        , Maybe.map (\q -> ( "id", q )) quoteId
                                        , Maybe.map (\h -> ( "hash", h )) hash
                                        ]
                                        |> List.map (\( k, v ) -> k ++ "=" ++ Url.percentEncode v)
                                        |> String.join "&"

                                apiUrl =
                                    "/api/self-service/"
                                        ++ oid
                                        ++ (if String.isEmpty apiQueryParams then
                                                ""

                                            else
                                                "?" ++ apiQueryParams
                                           )
                            in
                            Cmd.batch
                                [ saveDebugInfo <| "Fetching org details: " ++ apiUrl
                                , Http.get
                                    { url = apiUrl
                                    , expect = Http.expectJson GotOrgDetails orgDetailsDecoder
                                    }
                                ]

                        Nothing ->
                            Cmd.none

                fetchContactDetailsCmd =
                    case quoteId of
                        Just qid ->
                            fetchContactDetails qid

                        Nothing ->
                            Cmd.none
            in
            ( { initialModel | orgId = orgId }
            , Cmd.batch
                (clearCmd
                    :: saveDebugInfo debugInfo
                    :: initCmd
                    :: fetchContactDetailsCmd
                    :: commands
                )
            )


parseUrlParams : Url -> UrlParams
parseUrlParams url =
    -- This function parses query parameters for backward compatibility
    let
        queryParams =
            url.query
                |> Maybe.map (String.split "&")
                |> Maybe.withDefault []
                |> List.map (String.split "=")
                |> List.filterMap
                    (\parts ->
                        case parts of
                            [ key, value ] ->
                                Just ( key, Url.percentDecode value |> Maybe.withDefault value )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList

        getParam name =
            Dict.get name queryParams
    in
    { orgId = getParam "orgId"
    , email = getParam "email"
    , hash = getParam "hash"
    , quoteId = getParam "id"
    , agentId = getParam "agentId"
    }


fetchContactDetails : String -> Cmd Msg
fetchContactDetails quoteId =
    Http.get
        { url = "/api/quotes/decode/" ++ quoteId
        , expect = Http.expectJson GotContactDetails contactDetailsDecoder
        }


contactDetailsDecoder : Decoder ContactDetails
contactDetailsDecoder =
    Decode.succeed ContactDetails
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "contact" contactDecoder


type alias ContactDetails =
    { success : Bool
    , contact : Contact
    }


fetchOrgDetails : String -> Cmd Msg
fetchOrgDetails slug =
    let
        url =
            "/api/self-service/" ++ slug
    in
    Cmd.batch
        [ saveDebugInfo <| "Fetching org details: " ++ url
        , Http.get
            { url = url
            , expect = Http.expectJson GotOrgDetails orgDetailsDecoder
            }
        ]


fetchZipInfo : String -> Cmd Msg
fetchZipInfo zipCode =
    Http.get
        { url = "/api/zipinfo/" ++ zipCode
        , expect = Http.expectJson GotZipInfo zipInfoDecoder
        }



-- UPDATE


type Msg
    = GotInitResponse (Result Http.Error InitResponse)
    | GotOrgDetails (Result Http.Error OrgDetails)
    | GotContactData (Result Http.Error ContactResponse)
    | UpdateEmail String
    | UpdateFirstName String
    | UpdateLastName String
    | UpdateZipCode String
    | UpdateDateOfBirth String
    | UpdateGender String
    | UpdateTobacco String
    | UpdatePhoneNumber String
    | UpdateCurrentPremium String
    | UpdateCurrentCarrier String
    | UpdateCarrierChoice String
    | UpdatePlanType String
    | UpdateSelectedCounty String
    | SubmitForm
    | GotCurrentDate Date
    | GotZipInfo (Result Http.Error ZipInfo)
    | GotSignupResponse (Result Http.Error SignupResponse)
    | GotQuoteResponse (Result Http.Error QuoteResponse)
    | GoToQuote
    | GotContactDetails (Result Http.Error ContactDetails)


type alias InitResponse =
    { contact : Maybe Contact
    , email : Maybe String
    , emailReadOnly : Bool
    }


type alias OrgDetails =
    { orgId : String
    , orgSlug : String
    , contact : Maybe Contact
    , logo : Maybe String
    , orgName : Maybe String
    }


type alias Contact =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , dateOfBirth : String
    , gender : String
    , tobacco : Bool
    , state : String
    , zipCode : String
    , currentCarrier : Maybe String
    , planType : Maybe String
    , optInQuarterlyUpdates : Bool
    }


type alias ZipInfo =
    { state : String
    , counties : List String
    }


type alias SignupResponse =
    { success : Bool
    , contactId : Int
    , email : String
    }


type alias QuoteResponse =
    { success : Bool
    , contactId : Int
    , quoteId : String
    , redirectUrl : String
    , error : Maybe String
    }


type alias ContactResponse =
    { success : Bool
    , orgSlug : String
    , carrierContracts : List Carrier
    , contact : Contact
    }


zipInfoDecoder : Decoder ZipInfo
zipInfoDecoder =
    Decode.field "success" Decode.bool
        |> Decode.andThen
            (\success ->
                if success then
                    Decode.field "data"
                        (Decode.map2 ZipInfo
                            (Decode.field "state" Decode.string)
                            (Decode.field "counties" (Decode.list Decode.string))
                        )

                else
                    Decode.field "error" Decode.string
                        |> Decode.andThen (\error -> Decode.fail error)
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotOrgDetails result ->
            case result of
                Ok details ->
                    let
                        -- Extract contact info if it exists
                        contact =
                            details.contact

                        debugInfo =
                            "OrgDetails: orgId="
                                ++ details.orgId
                                ++ ", orgSlug="
                                ++ details.orgSlug
                                ++ ", hasContact="
                                ++ (if details.contact /= Nothing then
                                        "true"

                                    else
                                        "false"
                                   )
                                ++ (if details.contact /= Nothing then
                                        ", contactEmail=" ++ (Maybe.map .email contact |> Maybe.withDefault "none")

                                    else
                                        ""
                                   )

                        currentCarrier =
                            Maybe.map .currentCarrier contact
                                |> Maybe.withDefault Nothing
                                |> Maybe.withDefault model.currentCarrier

                        carrier =
                            case stringToCarrier currentCarrier of
                                Just carrierValue ->
                                    Just (HasCarrier carrierValue)

                                Nothing ->
                                    Nothing
                    in
                    ( { model
                        | orgId = Just details.orgId
                        , orgSlug = Just details.orgSlug
                        , orgName = details.orgName
                        , logo = details.logo
                        , email = Maybe.map .email contact |> Maybe.withDefault model.email
                        , firstName = Maybe.map .firstName contact |> Maybe.withDefault model.firstName
                        , lastName = Maybe.map .lastName contact |> Maybe.withDefault model.lastName
                        , phoneNumber = Maybe.map .phone contact |> Maybe.withDefault model.phoneNumber
                        , dateOfBirth = Maybe.map .dateOfBirth contact |> Maybe.withDefault model.dateOfBirth
                        , gender = Maybe.map .gender contact |> Maybe.withDefault model.gender
                        , tobacco = Maybe.map .tobacco contact |> Maybe.withDefault model.tobacco
                        , zipCode = Maybe.map .zipCode contact |> Maybe.withDefault model.zipCode
                        , currentCarrier = currentCarrier
                        , carrier = carrier
                        , planType = Maybe.map .planType contact |> Maybe.withDefault Nothing |> Maybe.withDefault model.planType
                        , optInQuarterlyUpdates = Maybe.map .optInQuarterlyUpdates contact |> Maybe.withDefault model.optInQuarterlyUpdates
                        , error = Nothing
                      }
                    , Cmd.batch
                        [ saveDebugInfo debugInfo

                        -- If we have a zip code from contact, fetch the zip info
                        , case Maybe.map .zipCode contact of
                            Just zip ->
                                if String.length zip == 5 then
                                    fetchZipInfo zip

                                else
                                    Cmd.none

                            Nothing ->
                                Cmd.none
                        ]
                    )

                Err error ->
                    ( { model | error = Just "Organization not found or invalid link." }
                    , saveDebugInfo <| "OrgDetails error: " ++ httpErrorToString error
                    )

        GotInitResponse result ->
            case result of
                Ok response ->
                    let
                        email =
                            response.email |> Maybe.withDefault ""

                        contact =
                            response.contact

                        firstName =
                            contact |> Maybe.map .firstName |> Maybe.withDefault ""

                        lastName =
                            contact |> Maybe.map .lastName |> Maybe.withDefault ""

                        optIn =
                            contact |> Maybe.map .optInQuarterlyUpdates |> Maybe.withDefault False

                        debugInfo =
                            "InitResponse: email="
                                ++ email
                                ++ ", firstName="
                                ++ firstName
                                ++ ", lastName="
                                ++ lastName
                                ++ ", optIn="
                                ++ (if optIn then
                                        "true"

                                    else
                                        "false"
                                   )
                    in
                    ( { model
                        | email = email
                        , firstName = firstName
                        , lastName = lastName
                        , optInQuarterlyUpdates = optIn
                        , emailReadOnly = response.emailReadOnly
                        , error = Nothing
                      }
                    , saveDebugInfo debugInfo
                    )

                Err error ->
                    ( { model | error = Just "Failed to load existing contact details. Please try again." }
                    , saveDebugInfo <| "InitResponse error: " ++ httpErrorToString error
                    )

        UpdateEmail newEmail ->
            ( { model
                | email =
                    if model.emailReadOnly then
                        model.email

                    else
                        newEmail
              }
            , Cmd.none
            )

        UpdateFirstName newFirstName ->
            ( { model | firstName = newFirstName }, Cmd.none )

        UpdateLastName newLastName ->
            ( { model | lastName = newLastName }, Cmd.none )

        UpdateZipCode zip ->
            let
                filteredZip =
                    String.filter Char.isDigit zip |> String.left 5

                cmd =
                    if String.length filteredZip == 5 && filteredZip /= model.zipCode then
                        fetchZipInfo filteredZip

                    else
                        Cmd.none
            in
            ( { model
                | zipCode = filteredZip
                , isLoadingZipData = String.length filteredZip == 5 && filteredZip /= model.zipCode
                , state =
                    if String.length filteredZip /= 5 || (String.length filteredZip == 5 && filteredZip /= model.zipCode) then
                        Nothing

                    else
                        model.state
                , counties =
                    if String.length filteredZip /= 5 || (String.length filteredZip == 5 && filteredZip /= model.zipCode) then
                        []

                    else
                        model.counties
                , selectedCounty =
                    if String.length filteredZip /= 5 || (String.length filteredZip == 5 && filteredZip /= model.zipCode) then
                        Nothing

                    else
                        model.selectedCounty
                , zipError = Nothing
              }
            , cmd
            )

        GotZipInfo result ->
            case result of
                Ok zipInfo ->
                    let
                        -- Always select the first county as default
                        selectedCounty =
                            List.head zipInfo.counties
                    in
                    ( { model
                        | state = Just zipInfo.state
                        , counties = zipInfo.counties
                        , selectedCounty = selectedCounty
                        , isLoadingZipData = False
                        , zipError = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | state = Nothing
                        , counties = []
                        , selectedCounty = Nothing
                        , isLoadingZipData = False
                        , zipError = Just (httpErrorToString error)
                      }
                    , Cmd.none
                    )

        UpdateSelectedCounty county ->
            ( { model | selectedCounty = Just county }, Cmd.none )

        UpdateDateOfBirth dob ->
            ( { model | dateOfBirth = dob }, Cmd.none )

        UpdateGender value ->
            ( { model | gender = value }, Cmd.none )

        UpdateTobacco value ->
            ( { model | tobacco = value == "true" }, Cmd.none )

        UpdatePhoneNumber phone ->
            ( { model | phoneNumber = String.filter Char.isDigit phone |> String.left 10 }, Cmd.none )

        UpdateCurrentPremium value ->
            ( { model | currentPremium = String.filter (\c -> Char.isDigit c || c == '.') value }, Cmd.none )

        UpdateCurrentCarrier carrier ->
            ( { model | currentCarrier = carrier }, Cmd.none )

        UpdateCarrierChoice value ->
            case value of
                "none" ->
                    ( { model
                        | carrier = Just NoneOther
                        , currentCarrier = ""
                      }
                    , Cmd.none
                    )

                _ ->
                    case stringToCarrier value of
                        Just carrierValue ->
                            ( { model
                                | carrier = Just (HasCarrier carrierValue)
                                , currentCarrier = carrierValue |> carrierToString
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

        UpdatePlanType planType ->
            ( { model | planType = planType }, Cmd.none )

        GotCurrentDate date ->
            ( { model | currentDate = Just date }, Cmd.none )

        SubmitForm ->
            if isFormValid model then
                ( { model | isSubmitting = True, error = Nothing }
                , submitForm model
                )

            else
                ( { model | error = Just "Please fill out all required fields" }
                , Cmd.none
                )

        GotSignupResponse result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model
                            | isSubmitting = False
                            , success = True
                            , error = Nothing
                            , isGeneratingQuote = True
                          }
                        , Cmd.batch
                            [ saveDebugInfo <| "SignupResponse: contactId=" ++ String.fromInt response.contactId ++ ", email=" ++ response.email
                            , generateQuote model.orgId response.contactId response.email
                            ]
                        )

                    else
                        ( { model | isSubmitting = False, error = Just "Signup failed. Please try again." }
                        , saveDebugInfo <| "SignupResponse: failed (success=false)"
                        )

                Err error ->
                    ( { model | isSubmitting = False, error = Just "Signup failed. Please try again." }
                    , saveDebugInfo <| "SignupResponse error: " ++ httpErrorToString error
                    )

        GotQuoteResponse result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | isGeneratingQuote = False }
                        , Cmd.batch
                            [ saveDebugInfo <| "QuoteResponse: quoteId=" ++ response.quoteId ++ ", redirectUrl=" ++ response.redirectUrl
                            , Nav.pushUrl model.key (Builder.absolute [ "compare" ] [ Builder.string "id" response.quoteId ])
                            ]
                        )

                    else
                        ( { model | isGeneratingQuote = False, error = Just "Failed to generate quote. Please try again." }
                        , saveDebugInfo <| "QuoteResponse: failed (success=false), error=" ++ (response.error |> Maybe.withDefault "No error message")
                        )

                Err error ->
                    ( { model | isGeneratingQuote = False, error = Just "Failed to generate quote. Please try again." }
                    , saveDebugInfo <| "QuoteResponse error: " ++ httpErrorToString error
                    )

        GoToQuote ->
            if isFormValid model then
                let
                    county =
                        case model.selectedCounty of
                            Just c ->
                                c

                            Nothing ->
                                List.head model.counties
                                    |> Maybe.withDefault ""

                    state =
                        model.state
                            |> Maybe.withDefault ""

                    compareUrl =
                        Builder.absolute [ "compare" ]
                            [ Builder.string "zip" model.zipCode
                            , Builder.string "state" state
                            , Builder.string "county" county
                            , Builder.string "gender" model.gender
                            , Builder.string "tobacco"
                                (if model.tobacco then
                                    "true"

                                 else
                                    "false"
                                )
                            , Builder.string "dateOfBirth" model.dateOfBirth
                            ]
                in
                ( model
                , Nav.pushUrl model.key compareUrl
                )

            else
                ( { model | error = Just "Please fill out all required fields for a quote" }
                , Cmd.none
                )

        GotContactData result ->
            case result of
                Ok response ->
                    let
                        contact =
                            response.contact
                    in
                    ( { model
                        | orgId = Just response.orgSlug
                        , email = contact.email
                        , firstName = contact.firstName
                        , lastName = contact.lastName
                        , phoneNumber = contact.phone
                        , dateOfBirth = contact.dateOfBirth
                        , gender = contact.gender
                        , tobacco = contact.tobacco
                        , zipCode = contact.zipCode
                        , currentCarrier = Maybe.withDefault "" contact.currentCarrier
                        , planType = Maybe.withDefault "" contact.planType
                        , optInQuarterlyUpdates = contact.optInQuarterlyUpdates
                        , error = Nothing
                      }
                    , Cmd.batch
                        [ saveDebugInfo <|
                            "ContactData: success="
                                ++ (if response.success then
                                        "true"

                                    else
                                        "false"
                                   )
                                ++ ", orgSlug="
                                ++ response.orgSlug
                        ]
                    )

                Err error ->
                    ( { model | error = Just "Failed to load contact data. Please try again." }
                    , saveDebugInfo <| "ContactData error: " ++ httpErrorToString error
                    )

        GotContactDetails result ->
            case result of
                Ok response ->
                    ( { model
                        | firstName = response.contact.firstName
                        , lastName = response.contact.lastName
                        , email = response.contact.email
                        , phoneNumber = response.contact.phone
                        , dateOfBirth = response.contact.dateOfBirth
                        , gender = response.contact.gender
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = Just "Failed to load contact details. Please try again." }
                    , saveDebugInfo <| "ContactDetails error: " ++ httpErrorToString error
                    )


isFormValid : Model -> Bool
isFormValid model =
    let
        isValidDate str =
            case String.split "-" str of
                [ year, month, day ] ->
                    String.length year
                        == 4
                        && String.length month
                        == 2
                        && String.length day
                        == 2
                        && String.all Char.isDigit year
                        && String.all Char.isDigit month
                        && String.all Char.isDigit day

                _ ->
                    False
    in
    -- Contact info (required)
    not (String.isEmpty model.email)
        && not (String.isEmpty model.firstName)
        && not (String.isEmpty model.lastName)
        && String.length model.phoneNumber
        == 10
        && model.orgId
        /= Nothing
        -- Quote info (required)
        && String.length model.zipCode
        == 5
        && not (String.isEmpty model.dateOfBirth)
        && isValidDate model.dateOfBirth
        && not (String.isEmpty model.gender)
        -- Tobacco status is now explicitly required
        && (model.tobacco == True || model.tobacco == False)
        -- Carrier selection is required
        && model.carrier
        /= Nothing
        -- Must agree to receive updates
        && model.optInQuarterlyUpdates
        == True


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus statusCode ->
            "Bad status: " ++ String.fromInt statusCode

        Http.BadBody message ->
            "Data error: " ++ message


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    if String.isEmpty phone then
        ""

    else
        let
            digits =
                String.filter Char.isDigit phone
                    |> String.left 10

            len =
                String.length digits
        in
        if len == 0 then
            ""

        else if len <= 3 then
            "(" ++ digits

        else if len <= 6 then
            "(" ++ String.left 3 digits ++ ") " ++ String.dropLeft 3 digits

        else
            "(" ++ String.left 3 digits ++ ") " ++ String.slice 3 6 digits ++ "-" ++ String.dropLeft 6 digits



-- FORM SUBMISSION


submitForm : Model -> Cmd Msg
submitForm model =
    Http.post
        { url = "/api/self-service/signup"
        , body = Http.jsonBody (encodeForm model)
        , expect = Http.expectJson GotSignupResponse signupResponseDecoder
        }


generateQuote : Maybe String -> Int -> String -> Cmd Msg
generateQuote maybeOrgId contactId email =
    case maybeOrgId of
        Just orgId ->
            Http.post
                { url = "/api/self-service/generate-quote"
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "orgId", Encode.string orgId )
                            , ( "contactEmail", Encode.string email )
                            ]
                        )
                , expect = Http.expectJson GotQuoteResponse quoteResponseDecoder
                }

        Nothing ->
            Cmd.none


encodeForm : Model -> Encode.Value
encodeForm model =
    let
        ls0 =
            [ ( "orgId", Encode.string (Maybe.withDefault "" model.orgId) )
            , ( "email", Encode.string model.email )
            , ( "firstName", Encode.string model.firstName )
            , ( "lastName", Encode.string model.lastName )
            , ( "zipCode", Encode.string model.zipCode )
            , ( "dateOfBirth", Encode.string model.dateOfBirth )
            , ( "gender", Encode.string model.gender )
            , ( "tobacco", Encode.bool model.tobacco )
            , ( "phoneNumber", Encode.string model.phoneNumber )
            , ( "currentPremium", Encode.string model.currentPremium )
            , ( "currentCarrier", Encode.string model.currentCarrier )
            , ( "planType", Encode.string model.planType )
            , ( "state", Encode.string (Maybe.withDefault "" model.state) )
            , ( "county", Encode.string (Maybe.withDefault "" model.selectedCounty) )
            , ( "optInQuarterlyUpdates", Encode.bool model.optInQuarterlyUpdates )
            , ( "carrierChoice"
              , case model.carrier of
                    Just (HasCarrier carrier) ->
                        Encode.string (carrierToString carrier)

                    Just NoneOther ->
                        Encode.string "None/Other"

                    Nothing ->
                        Encode.null
              )
            ]

        ls1 =
            case model.agentId of
                Just agentId ->
                    [ ( "agentId", Encode.string agentId ) ]

                Nothing ->
                    []
    in
    Encode.object (ls0 ++ ls1)



-- DECODERS


initResponseDecoder : Decoder InitResponse
initResponseDecoder =
    Decode.map3 InitResponse
        (Decode.maybe (Decode.field "contact" contactDecoder))
        (Decode.maybe (Decode.field "email" Decode.string))
        (Decode.field "emailReadOnly" Decode.bool)


contactDecoder : Decoder Contact
contactDecoder =
    let
        phoneDecoder =
            Decode.oneOf
                [ Decode.field "phone" (Decode.oneOf [ Decode.string, Decode.null "" ])
                , Decode.field "phoneNumber" (Decode.oneOf [ Decode.string, Decode.null "" ])
                , Decode.succeed ""
                ]
    in
    Decode.succeed Contact
        |> Pipeline.required "firstName" (Decode.oneOf [ Decode.string, Decode.null "" ])
        |> Pipeline.required "lastName" (Decode.oneOf [ Decode.string, Decode.null "" ])
        |> Pipeline.required "email" Decode.string
        |> Pipeline.custom phoneDecoder
        |> Pipeline.optional "dateOfBirth" Decode.string ""
        |> Pipeline.optional "gender" Decode.string "M"
        |> Pipeline.optional "tobacco" Decode.bool False
        |> Pipeline.optional "state" Decode.string ""
        |> Pipeline.optional "zipCode" Decode.string ""
        |> Pipeline.optional "currentCarrier" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "planType" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "optInQuarterlyUpdates" Decode.bool True


orgDetailsDecoder : Decoder OrgDetails
orgDetailsDecoder =
    Decode.map5 OrgDetails
        (Decode.field "orgId" Decode.string)
        (Decode.field "orgSlug" Decode.string)
        (Decode.maybe (Decode.field "contact" contactDecoder))
        (Decode.maybe (Decode.field "logo" Decode.string))
        (Decode.maybe (Decode.field "orgName" Decode.string))


signupResponseDecoder : Decoder SignupResponse
signupResponseDecoder =
    Decode.map3 SignupResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "contactId" Decode.int)
        (Decode.field "email" Decode.string)


quoteResponseDecoder : Decoder QuoteResponse
quoteResponseDecoder =
    Decode.map5 QuoteResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "contactId" Decode.int)
        (Decode.field "quoteId" Decode.string)
        (Decode.field "redirectUrl" Decode.string)
        (Decode.maybe (Decode.field "error" Decode.string))


contactResponseDecoder : Decoder ContactResponse
contactResponseDecoder =
    Decode.map4 ContactResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "orgSlug" Decode.string)
        (Decode.field "carrierContracts" (Decode.list carrierDecoder))
        (Decode.field "contact" contactDecoder)


fetchContactData : String -> Cmd Msg
fetchContactData quoteId =
    Http.get
        { url = "/api/quotes/decode/" ++ quoteId
        , expect = Http.expectJson GotContactData contactResponseDecoder
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Get Your Medicare Supplement Quote"
    , body =
        [ viewForm model
        ]
    }


viewForm : Model -> Html Msg
viewForm model =
    div [ class "max-w-4xl mx-auto bg-white rounded-xl shadow-md p-4 sm:p-8" ]
        [ if model.isSubmitting || model.isGeneratingQuote then
            div [ class "fixed inset-0 bg-white flex flex-col items-center justify-center gap-4 text-center" ]
                [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-[#03045E] border-t-transparent" ] []
                , p [ class "text-center text-lg font-medium text-gray-600" ]
                    [ text
                        (if model.isSubmitting then
                            "Submitting your information..."

                         else
                            "Generating your quote..."
                        )
                    ]
                ]

          else if model.orgId == Nothing && model.error == Nothing then
            div [ class "fixed inset-0 bg-white flex flex-col items-center justify-center gap-4 text-center" ]
                [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-[#03045E] border-t-transparent" ] []
                , p [ class "text-center text-lg font-medium text-gray-600" ]
                    [ text "Loading organization details..." ]
                ]

          else
            viewFormStep model
        ]


viewFormStep : Model -> Html Msg
viewFormStep model =
    div []
        [ viewHeader model.logo model.orgName

        {--
        , div [ class "text-center mb-6 px-4 sm:px-6" ]
            [ div [ class "max-w-2xl mx-auto" ]
                [ p [ class "text-[#475467] text-base sm:text-lg italic mb-2" ]
                    [ text "This is the exact form that your clients will go through if we need more info. It can also be used for new lead generation. This form will be white labeled with your branding, logo, and colors to match your agency's style." ]
                ]
            ]
        --}
        , div [ class "text-center mb-6 px-2 sm:px-0" ]
            [ h1 [ class "text-2xl sm:text-3xl font-bold text-[#101828]" ] [ text "Let's Get Some Details" ]
            , p [ class "text-[#475467] mt-2 text-sm sm:text-base" ] [ text "We use this information to get you the most accurate quote for your area." ]
            ]
        , viewCombinedForm model
        , viewError model.error
        , div [ class "flex justify-center mt-8" ]
            [ button
                [ type_ "button"
                , class
                    ("w-48 flex justify-center py-3 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white "
                        ++ (if isFormValid model && not model.isSubmitting then
                                "bg-[#03045E] hover:bg-[#02034e] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#3DBDEC]"

                            else
                                "bg-[#03045E] opacity-50 cursor-not-allowed"
                           )
                    )
                , onClick SubmitForm
                , disabled (model.isSubmitting || not (isFormValid model))
                ]
                [ text
                    (if model.isSubmitting then
                        "Submitting..."

                     else
                        "Submit"
                    )
                ]
            ]
        ]


viewCombinedForm : Model -> Html Msg
viewCombinedForm model =
    div [ class "px-2 sm:px-0 space-y-6 sm:space-y-6" ]
        [ div [ class "grid grid-cols-1 sm:grid-cols-2 gap-4 sm:gap-4" ]
            [ inputField "First Name" "text" model.firstName UpdateFirstName False
            , inputField "Last Name" "text" model.lastName UpdateLastName False
            , inputField "Cell Number *" "tel" (formatPhoneNumber model.phoneNumber) UpdatePhoneNumber False
            , div [ class "mb-0" ]
                [ div [ class "flex items-center" ]
                    [ label [ class "block text-sm font-medium text-[#344054] mb-1" ] [ text "Email Address" ]
                    , span [ class "ml-2 text-xs text-gray-500" ] [ text "(use a unique email for each person)" ]
                    ]
                , input
                    [ type_ "email"
                    , class "w-full px-3 py-2 border border-[#D0D5DD] rounded-md shadow-sm focus:outline-none focus:ring-[#3DBDEC] focus:border-[#3DBDEC]"
                    , placeholder "example@example.com"
                    , Html.Attributes.value model.email
                    , onInput UpdateEmail
                    , disabled model.emailReadOnly
                    , required True
                    ]
                    []
                ]
            ]
        , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-4 sm:gap-4" ]
            [ inputField "Date of Birth" "date" model.dateOfBirth UpdateDateOfBirth False
            , div [ class "mb-0" ]
                [ label [ class "block text-sm font-medium text-[#344054] mb-1" ] [ text "Zip Code" ]
                , input
                    [ type_ "text"
                    , class "w-full px-3 py-2 border border-[#D0D5DD] rounded-md shadow-sm focus:outline-none focus:ring-[#3DBDEC] focus:border-[#3DBDEC]"
                    , Html.Attributes.value model.zipCode
                    , onInput UpdateZipCode
                    , required True
                    ]
                    []
                , if model.isLoadingZipData then
                    div [ class "mt-1 text-sm text-gray-500" ] [ text "Loading location data..." ]

                  else if model.zipError /= Nothing then
                    div [ class "mt-1 text-sm text-red-600" ] [ text (model.zipError |> Maybe.withDefault "Invalid zip code") ]

                  else if model.state /= Nothing then
                    div [ class "mt-1 text-sm text-green-600" ]
                        [ text ("State: " ++ (model.state |> Maybe.withDefault "")) ]

                  else
                    text ""
                ]
            ]
        , if List.length model.counties > 1 then
            div [ class "mb-4" ]
                [ viewCountyDropdown model.counties model.selectedCounty ]

          else
            text ""
        , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-4 sm:gap-4" ]
            [ div [ class "col-span-1" ]
                [ label [ class "block text-sm font-medium text-gray-700 mb-2" ] [ text "Gender" ]
                , div [ class "grid grid-cols-2 gap-2" ]
                    [ label
                        [ class
                            ("flex items-center justify-center px-3 sm:px-4 py-2 rounded-lg border-2 cursor-pointer transition-all text-sm sm:text-base "
                                ++ (if model.gender == "M" then
                                        "border-[#03045E] bg-[#F9F5FF] text-[#03045E]"

                                    else
                                        "border-[#D0D5DD] hover:border-[#3DBDEC] text-[#344054]"
                                   )
                            )
                        ]
                        [ input
                            [ type_ "radio"
                            , value "M"
                            , checked (model.gender == "M")
                            , onInput UpdateGender
                            , class "sr-only"
                            ]
                            []
                        , text "Male"
                        ]
                    , label
                        [ class
                            ("flex items-center justify-center px-3 sm:px-4 py-2 rounded-lg border-2 cursor-pointer transition-all text-sm sm:text-base "
                                ++ (if model.gender == "F" then
                                        "border-[#03045E] bg-[#F9F5FF] text-[#03045E]"

                                    else
                                        "border-[#D0D5DD] hover:border-[#3DBDEC] text-[#344054]"
                                   )
                            )
                        ]
                        [ input
                            [ type_ "radio"
                            , value "F"
                            , checked (model.gender == "F")
                            , onInput UpdateGender
                            , class "sr-only"
                            ]
                            []
                        , text "Female"
                        ]
                    ]
                ]
            , div [ class "col-span-1" ]
                [ label [ class "block text-sm font-medium text-gray-700 mb-2" ] [ text "Do you use tobacco products?" ]
                , div [ class "grid grid-cols-2 gap-2" ]
                    [ label
                        [ class
                            ("flex items-center justify-center px-3 sm:px-4 py-2 rounded-lg border-2 cursor-pointer transition-all text-sm sm:text-base "
                                ++ (if model.tobacco then
                                        "border-[#03045E] bg-[#F9F5FF] text-[#03045E]"

                                    else
                                        "border-[#D0D5DD] hover:border-[#3DBDEC] text-[#344054]"
                                   )
                            )
                        ]
                        [ input
                            [ type_ "radio"
                            , value "true"
                            , checked model.tobacco
                            , onInput UpdateTobacco
                            , class "sr-only"
                            ]
                            []
                        , text "Yes"
                        ]
                    , label
                        [ class
                            ("flex items-center justify-center px-3 sm:px-4 py-2 rounded-lg border-2 cursor-pointer transition-all text-sm sm:text-base "
                                ++ (if not model.tobacco then
                                        "border-[#03045E] bg-[#F9F5FF] text-[#03045E]"

                                    else
                                        "border-[#D0D5DD] hover:border-[#3DBDEC] text-[#344054]"
                                   )
                            )
                        ]
                        [ input
                            [ type_ "radio"
                            , value "false"
                            , checked (not model.tobacco)
                            , onInput UpdateTobacco
                            , class "sr-only"
                            ]
                            []
                        , text "No"
                        ]
                    ]
                ]
            ]
        , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-4 sm:gap-4" ]
            [ div [ class "col-span-1" ]
                [ label [ class "block text-sm font-medium text-[#344054] mb-1" ] [ text "Carrier" ]
                , select
                    [ class "w-full px-3 py-2 border border-[#D0D5DD] rounded-md shadow-sm focus:outline-none focus:ring-[#3DBDEC] focus:border-[#3DBDEC]"
                    , onInput UpdateCarrierChoice
                    , required True
                    ]
                    ([ option
                        [ value "", disabled True, selected (model.carrier == Nothing) ]
                        [ text "Select a carrier" ]
                     , option
                        [ value "none", selected (model.carrier == Just NoneOther) ]
                        [ text "None/Other" ]
                     ]
                        ++ List.map
                            (\carrier ->
                                option
                                    [ value (carrierToString carrier)
                                    , selected (model.carrier == Just (HasCarrier carrier))
                                    ]
                                    [ text (carrierToString carrier) ]
                            )
                            (List.sortBy carrierToString allCarriers)
                    )
                ]
            , div [ class "col-span-1" ] [] -- Empty div to maintain grid layout
            ]

        {--
        , h3 [ class "font-medium text-base sm:text-lg mb-4 text-gray-700" ] [ text "Current Coverage" ]
        , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-4 sm:gap-4" ]
            [ div [ class "col-span-1" ]
                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ] [ text "Current Carrier (optional)" ]
                , select
                    [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-sky-500 focus:border-sky-500"
                    , onInput UpdateCurrentCarrier
                    ]
                    ([ option [ value "" ] [ text "Select Current Carrier" ] ]
                        ++ List.map
                            (\carrier ->
                                option
                                    [ value (carrierToString carrier)
                                    , selected (model.currentCarrier == carrierToString carrier)
                                    ]
                                    [ text (carrierToString carrier) ]
                            )
                            (List.sortBy carrierToString allCarriers)
                        ++ [ option
                                [ value "Other"
                                , selected (model.currentCarrier == "Other")
                                ]
                                [ text "Other" ]
                           ]
                    )
                ]
            , div [ class "col-span-1" ]
                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ] [ text "Current Monthly Premium (optional)" ]
                , div [ class "relative" ]
                    [ div [ class "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none" ]
                        [ span [ class "text-gray-500" ] [ text "$" ] ]
                    , input
                        [ type_ "text"
                        , class "w-full pl-7 px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-sky-500 focus:border-sky-500"
                        , Html.Attributes.value model.currentPremium
                        , onInput UpdateCurrentPremium
                        , Html.Attributes.pattern "[0-9]*"
                        , Html.Attributes.attribute "inputmode" "numeric"
                        , placeholder "0.00"
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "grid grid-cols-1 sm:grid-cols-1 gap-4 sm:gap-4" ]
            [ div [ class "col-span-1" ]
                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ] [ text "Plan Type (optional)" ]
                , select
                    [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-sky-500 focus:border-sky-500"
                    , onInput UpdatePlanType
                    ]
                    [ option [ value "" ] [ text "Select Plan Type" ]
                    , option [ value "G", selected (model.planType == "G") ] [ text "Plan G" ]
                    , option [ value "N", selected (model.planType == "N") ] [ text "Plan N" ]
                    , option [ value "Other", selected (model.planType == "Other") ] [ text "Other" ]
                    ]
                ]
            ]
        , div [ class "mt-6 text-center" ]
            [ p [ class "text-sm text-[#475467]" ]
                [ text "By clicking below, you agree to receive Medicare Supplement plan updates that can help you save money." ]
            ]
        --}
        ]


inputField : String -> String -> String -> (String -> Msg) -> Bool -> Html Msg
inputField labelText inputType inputValue msg isDisabled =
    div [ class "mb-0" ]
        [ label [ class "block text-sm font-medium text-[#344054] mb-1" ] [ text labelText ]
        , input
            [ type_ inputType
            , class "w-full px-3 py-2 border border-[#D0D5DD] rounded-md shadow-sm focus:outline-none focus:ring-[#3DBDEC] focus:border-[#3DBDEC]"
            , placeholder
                (case inputType of
                    "tel" ->
                        "XXX-XXX-XXXX"

                    "email" ->
                        "example@example.com"

                    _ ->
                        ""
                )
            , Html.Attributes.value inputValue
            , onInput msg
            , disabled isDisabled
            , required True
            ]
            []
        ]


viewCountyDropdown : List String -> Maybe String -> Html Msg
viewCountyDropdown counties selectedCounty =
    div [ class "mb-4" ]
        [ label [ class "block text-sm font-medium text-[#344054] mb-1" ]
            [ text "County" ]
        , select
            [ class "w-full px-3 py-2 border border-[#D0D5DD] rounded-md shadow-sm focus:outline-none focus:ring-[#3DBDEC] focus:border-[#3DBDEC]"
            , onInput UpdateSelectedCounty
            , required True
            ]
            (option [ value "", disabled True, selected (selectedCounty == Nothing) ]
                [ text "Select your county" ]
                :: List.map
                    (\county ->
                        option
                            [ value county
                            , selected (selectedCounty == Just county)
                            ]
                            [ text county ]
                    )
                    counties
            )
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ class "mb-4 p-3 bg-red-100 border border-red-400 text-red-700 rounded" ]
                [ text error ]

        Nothing ->
            text ""


viewFormSummary : Model -> Html Msg
viewFormSummary model =
    div [ class "border rounded-md p-3 sm:p-4 mb-4" ]
        [ h3 [ class "font-medium text-base sm:text-lg mb-3" ] [ text "Review Your Information" ]
        , summaryItem "Email" model.email
        , summaryItem "Name" (model.firstName ++ " " ++ model.lastName)
        , summaryItem "Phone" (formatPhoneNumber model.phoneNumber)
        , summaryItem "Zip Code" model.zipCode
        , summaryItem "State"
            (model.state
                |> Maybe.withDefault "Unknown"
            )
        , summaryItem "County"
            (model.selectedCounty
                |> Maybe.withDefault "Unknown"
            )
        , summaryItem "Date of Birth" model.dateOfBirth
        , summaryItem "Gender"
            (if model.gender == "M" then
                "Male"

             else
                "Female"
            )
        , summaryItem "Tobacco User"
            (if model.tobacco then
                "Yes"

             else
                "No"
            )
        , summaryItem "Carrier"
            (case model.carrier of
                Just (HasCarrier carrier) ->
                    carrierToString carrier

                Just NoneOther ->
                    "None/Other"

                Nothing ->
                    "Not selected"
            )
        , if not (String.isEmpty model.currentPremium) then
            summaryItem "Current Premium" ("$" ++ model.currentPremium)

          else
            text ""
        , if not (String.isEmpty model.currentCarrier) then
            summaryItem "Current Carrier" model.currentCarrier

          else
            text ""
        , if not (String.isEmpty model.planType) then
            summaryItem "Plan Type"
                (case model.planType of
                    "G" ->
                        "Plan G"

                    "N" ->
                        "Plan N"

                    "Other" ->
                        "Other"

                    _ ->
                        model.planType
                )

          else
            text ""
        , summaryItem "Agree to Receive Updates"
            (if model.optInQuarterlyUpdates then
                "Yes"

             else
                "No"
            )
        ]


summaryItem : String -> String -> Html Msg
summaryItem label value =
    div [ class "flex justify-between py-1 border-b last:border-b-0 text-sm sm:text-base" ]
        [ span [ class "text-gray-600" ] [ text label ]
        , span [ class "font-medium" ] [ text value ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = \_ url key -> init key url
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = \_ -> GotInitResponse (Err (Http.BadUrl "URL changed"))
        , onUrlRequest = \_ -> GotInitResponse (Err (Http.BadUrl "URL requested"))
        }
