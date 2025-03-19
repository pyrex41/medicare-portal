port module SelfServiceOnboarding exposing (..)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task
import Time
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser, oneOf, string)



-- PORTS


port saveDebugInfo : String -> Cmd msg


port clearDebugInfo : () -> Cmd msg



-- MODEL


type alias Model =
    { orgId : Maybe String
    , orgSlug : Maybe String
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
    , formStep : FormStep
    }


type FormStep
    = SingleStep
    | ConfirmSubmit



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
    }


init : Nav.Key -> Url -> ( Model, Cmd Msg )
init key url =
    let
        route =
            parseUrl url

        initialModel =
            { orgId = Nothing
            , orgSlug = Nothing
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
            , formStep = SingleStep
            }

        commands =
            [ Task.perform GotCurrentDate Date.today ]

        -- Clear any previous debug info
        clearCmd =
            clearDebugInfo ()
    in
    case route of
        SlugRoute slug ->
            ( { initialModel | orgSlug = Just slug }
            , Cmd.batch (clearCmd :: fetchOrgDetails slug :: commands)
            )

        QueryRoute ->
            -- Handle query parameters for backward compatibility
            let
                params =
                    parseUrlParams url

                orgId =
                    params.orgId

                email =
                    params.email

                hash =
                    params.hash

                initCmd =
                    case orgId of
                        Just oid ->
                            let
                                queryParams =
                                    List.filterMap identity
                                        [ Just ( "orgId", oid )
                                        , Maybe.map (\e -> ( "email", e )) email
                                        , Maybe.map (\h -> ( "hash", h )) hash
                                        ]
                                        |> List.map (\( k, v ) -> k ++ "=" ++ Url.percentEncode v)
                                        |> String.join "&"
                            in
                            Http.get
                                { url = "/api/self-service/init?" ++ queryParams
                                , expect = Http.expectJson GotInitResponse initResponseDecoder
                                }

                        Nothing ->
                            Cmd.none
            in
            ( { initialModel | orgId = orgId }
            , Cmd.batch (clearCmd :: initCmd :: commands)
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
    }


fetchOrgDetails : String -> Cmd Msg
fetchOrgDetails slug =
    Http.get
        { url = "/api/self-service/" ++ slug
        , expect = Http.expectJson GotOrgDetails orgDetailsDecoder
        }


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
    | UpdateSelectedCounty String
    | ToggleOptIn Bool
    | SubmitForm
    | GotCurrentDate Date
    | GotZipInfo (Result Http.Error ZipInfo)
    | GotSignupResponse (Result Http.Error SignupResponse)
    | GotQuoteResponse (Result Http.Error QuoteResponse)
    | NextStep
    | PrevStep
    | GoToQuote


type alias InitResponse =
    { contact : Maybe Contact
    , email : Maybe String
    , emailReadOnly : Bool
    }


type alias OrgDetails =
    { orgId : String
    , orgSlug : String
    }


type alias Contact =
    { email : String
    , firstName : String
    , lastName : String
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
                    ( { model | orgId = Just details.orgId, error = Nothing }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Organization not found or invalid link." }
                    , Cmd.none
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
                    in
                    ( { model
                        | email = email
                        , firstName = firstName
                        , lastName = lastName
                        , optInQuarterlyUpdates = optIn
                        , emailReadOnly = response.emailReadOnly
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to load existing contact details. Please try again." }
                    , Cmd.none
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
                    if String.length filteredZip /= 5 then
                        Nothing

                    else
                        model.state
                , counties =
                    if String.length filteredZip /= 5 then
                        []

                    else
                        model.counties
                , selectedCounty =
                    if String.length filteredZip /= 5 then
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

        UpdateCurrentPremium premium ->
            ( { model | currentPremium = premium }, Cmd.none )

        UpdateCurrentCarrier carrier ->
            ( { model | currentCarrier = carrier }, Cmd.none )

        ToggleOptIn newValue ->
            ( { model | optInQuarterlyUpdates = newValue }, Cmd.none )

        GotCurrentDate date ->
            ( { model | currentDate = Just date }, Cmd.none )

        NextStep ->
            let
                nextStep =
                    case model.formStep of
                        SingleStep ->
                            if isFormValid model then
                                ConfirmSubmit

                            else
                                SingleStep

                        ConfirmSubmit ->
                            ConfirmSubmit
            in
            ( { model | formStep = nextStep, error = Nothing }, Cmd.none )

        PrevStep ->
            let
                prevStep =
                    case model.formStep of
                        SingleStep ->
                            SingleStep

                        ConfirmSubmit ->
                            SingleStep
            in
            ( { model | formStep = prevStep, error = Nothing }, Cmd.none )

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
                        ( { model | isSubmitting = False, success = True, error = Nothing, isGeneratingQuote = True }
                        , Cmd.batch
                            [ saveDebugInfo <| "SignupResponse: contactId=" ++ String.fromInt response.contactId ++ ", email=" ++ response.email
                            , generateQuote model.orgId response.contactId response.email
                            ]
                        )

                    else
                        ( { model | isSubmitting = False, error = Just "Signup failed. Please try again." }
                        , saveDebugInfo "SignupResponse: failed (success=false)"
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
                            , Nav.pushUrl model.key (Builder.absolute [ "quote" ] [ Builder.string "id" response.quoteId ])
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


isFormValid : Model -> Bool
isFormValid model =
    -- Contact info (required)
    not (String.isEmpty model.email)
        && not (String.isEmpty model.firstName)
        && not (String.isEmpty model.lastName)
        && model.orgId
        /= Nothing
        -- Quote info (required)
        && String.length model.zipCode
        == 5
        && not (String.isEmpty model.dateOfBirth)
        && not (String.isEmpty model.gender)
        -- Tobacco status is now explicitly required
        && (model.tobacco == True || model.tobacco == False)
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
    Encode.object
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
        , ( "state", Encode.string (Maybe.withDefault "" model.state) )
        , ( "county", Encode.string (Maybe.withDefault "" model.selectedCounty) )
        , ( "optInQuarterlyUpdates", Encode.bool model.optInQuarterlyUpdates )
        ]



-- DECODERS


initResponseDecoder : Decoder InitResponse
initResponseDecoder =
    Decode.map3 InitResponse
        (Decode.maybe (Decode.field "contact" contactDecoder))
        (Decode.maybe (Decode.field "email" Decode.string))
        (Decode.field "emailReadOnly" Decode.bool)


contactDecoder : Decoder Contact
contactDecoder =
    Decode.map4 Contact
        (Decode.field "email" Decode.string)
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.maybe (Decode.field "optInQuarterlyUpdates" Decode.bool)
            |> Decode.map (Maybe.withDefault True)
        )


orgDetailsDecoder : Decoder OrgDetails
orgDetailsDecoder =
    Decode.map2 OrgDetails
        (Decode.field "orgId" Decode.string)
        (Decode.field "orgSlug" Decode.string)


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



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Get Your Medicare Supplement Quote"
    , body =
        [ div [ class "container mx-auto px-4 py-8" ]
            [ h1 [ class "text-center text-2xl font-bold mb-6" ] [ text "Get Your Medicare Supplement Quote" ]
            , viewForm model
            ]
        ]
    }


viewForm : Model -> Html Msg
viewForm model =
    div [ class "max-w-2xl mx-auto bg-white rounded-lg shadow-md p-6" ]
        [ if model.success then
            div []
                [ if model.isGeneratingQuote then
                    div [ class "p-4 mb-4 bg-blue-100 border border-blue-400 text-blue-700 rounded" ]
                        [ text "Your profile has been updated! Generating your quote..." ]

                  else
                    div [ class "p-4 mb-4 bg-green-100 border border-green-400 text-green-700 rounded" ]
                        [ text "Profile updated successfully! Redirecting to your quote..." ]
                ]

          else if model.orgId == Nothing && model.error == Nothing then
            div [ class "p-4 mb-4 bg-yellow-100 border border-yellow-400 text-yellow-700 rounded" ]
                [ text "Loading organization details..." ]

          else
            viewFormStep model
        ]


viewFormStep : Model -> Html Msg
viewFormStep model =
    case model.formStep of
        SingleStep ->
            div []
                [ viewCombinedForm model
                , viewError model.error
                , button
                    [ type_ "button"
                    , class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    , onClick NextStep
                    , disabled (not (isFormValid model))
                    ]
                    [ text "Review" ]
                ]

        ConfirmSubmit ->
            div []
                [ viewFormSummary model
                , viewError model.error
                , div [ class "flex justify-between mt-4" ]
                    [ button
                        [ type_ "button"
                        , class "flex justify-center py-2 px-4 border border-gray-300 rounded-md shadow-sm text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        , onClick PrevStep
                        ]
                        [ text "Back" ]
                    , button
                        [ type_ "button"
                        , class "flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
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
    div []
        [ h3 [ class "font-medium text-lg mb-2" ] [ text "Contact Information" ]
        , inputField "Email" "email" model.email UpdateEmail model.emailReadOnly
        , inputField "First Name" "text" model.firstName UpdateFirstName False
        , inputField "Last Name" "text" model.lastName UpdateLastName False
        , inputField "Phone Number" "tel" (formatPhoneNumber model.phoneNumber) UpdatePhoneNumber False
        , h3 [ class "font-medium text-lg mb-2 mt-6" ] [ text "Quote Information" ]
        , zipCodeField model
        , if List.length model.counties > 1 then
            viewCountyDropdown model.counties model.selectedCounty

          else
            text ""
        , inputField "Date of Birth" "date" model.dateOfBirth UpdateDateOfBirth False
        , formRadioGroup "Gender"
            model.gender
            UpdateGender
            [ ( "M", "Male" ), ( "F", "Female" ) ]
        , formRadioGroup "Tobacco User"
            (if model.tobacco then
                "true"

             else
                "false"
            )
            UpdateTobacco
            [ ( "true", "Yes" ), ( "false", "No" ) ]
        , h3 [ class "font-medium text-lg mb-2 mt-6" ] [ text "Current Coverage (Optional)" ]
        , inputField "Current Premium ($)" "number" model.currentPremium UpdateCurrentPremium False
        , inputField "Current Carrier" "text" model.currentCarrier UpdateCurrentCarrier False
        , div [ class "mt-6 flex justify-center" ]
            [ checkboxField "I agree to receive Medicare Supplement plan updates" model.optInQuarterlyUpdates ToggleOptIn ]
        ]


viewFormSummary : Model -> Html Msg
viewFormSummary model =
    div [ class "border rounded-md p-4 mb-4" ]
        [ h3 [ class "font-medium text-lg mb-3" ] [ text "Review Your Information" ]
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
        , if not (String.isEmpty model.currentPremium) then
            summaryItem "Current Premium" ("$" ++ model.currentPremium)

          else
            text ""
        , if not (String.isEmpty model.currentCarrier) then
            summaryItem "Current Carrier" model.currentCarrier

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
    div [ class "flex justify-between py-1 border-b last:border-b-0" ]
        [ span [ class "text-gray-600" ] [ text label ]
        , span [ class "font-medium" ] [ text value ]
        ]


zipCodeField : Model -> Html Msg
zipCodeField model =
    div [ class "mb-4" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
            [ text "Zip Code" ]
        , input
            [ type_ "text"
            , class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
            , Html.Attributes.value model.zipCode
            , onInput UpdateZipCode
            , Html.Attributes.maxlength 5
            , Html.Attributes.pattern "[0-9]*"
            , required True
            ]
            []
        , if model.isLoadingZipData then
            div [ class "text-xs text-blue-600 mt-1" ]
                [ text "Looking up location..." ]

          else if model.zipError /= Nothing then
            div [ class "text-xs text-red-600 mt-1" ]
                [ text (Maybe.withDefault "Invalid zip code" model.zipError) ]

          else if model.state /= Nothing then
            div [ class "text-xs text-green-600 mt-1" ]
                [ text ("Found: " ++ Maybe.withDefault "" model.state) ]

          else
            text ""
        ]


viewCountyDropdown : List String -> Maybe String -> Html Msg
viewCountyDropdown counties selectedCounty =
    div [ class "mb-4" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
            [ text "County" ]
        , select
            [ class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
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


inputField : String -> String -> String -> (String -> Msg) -> Bool -> Html Msg
inputField labelText inputType inputValue msg isDisabled =
    div [ class "mb-4" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-1" ] [ text labelText ]
        , input
            [ type_ inputType
            , class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
            , Html.Attributes.value inputValue
            , onInput msg
            , disabled isDisabled
            ]
            []
        ]


formRadioGroup : String -> String -> (String -> Msg) -> List ( String, String ) -> Html Msg
formRadioGroup labelText selectedValue msg options =
    div [ class "mb-4" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
            [ text labelText ]
        , div [ class "flex gap-4" ]
            (List.map
                (\( val, txt ) ->
                    label
                        [ class
                            ("flex items-center justify-center px-4 py-2 rounded-lg border-2 cursor-pointer transition-all flex-1 "
                                ++ (if selectedValue == val then
                                        "border-indigo-500 bg-indigo-50 text-indigo-700"

                                    else
                                        "border-gray-200 hover:border-indigo-200"
                                   )
                            )
                        ]
                        [ input
                            [ type_ "radio"
                            , value val
                            , checked (selectedValue == val)
                            , onInput msg
                            , class "sr-only"
                            ]
                            []
                        , text txt
                        ]
                )
                options
            )
        ]


checkboxField : String -> Bool -> (Bool -> Msg) -> Html Msg
checkboxField labelText isChecked toMsg =
    div [ class "mb-4" ]
        [ label [ class "flex items-center" ]
            [ input
                [ type_ "checkbox"
                , class "h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
                , checked isChecked
                , onCheck toMsg
                , required True
                ]
                []
            , span [ class "ml-2 text-sm text-gray-600" ] [ text (labelText ++ " (Required)") ]
            ]
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ class "mb-4 p-3 bg-red-100 border border-red-400 text-red-700 rounded" ]
                [ text error ]

        Nothing ->
            text ""



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
