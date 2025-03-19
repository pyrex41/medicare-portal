module Quote exposing (Model, Msg(..), init, subscriptions, update, view)

import AgeCalc exposing (getAgeNextMonth)
import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Task
import Time
import Url.Builder as Builder
import Url.Parser as Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type alias Model =
    { zipCode : String
    , dateOfBirth : String
    , key : Nav.Key
    , tobacco : Bool
    , gender : String
    , currentDate : Maybe Date
    , quoteId : Maybe String
    , error : Maybe String
    , currentCarrier : Maybe String
    , planType : Maybe String
    , state : Maybe String
    , counties : List String
    , selectedCounty : Maybe String
    , isLoadingZipData : Bool
    , zipError : Maybe String
    , orgId : Maybe String
    }


type Msg
    = UpdateZipCode String
    | UpdateDateOfBirth String
    | UpdateTobacco String
    | UpdateGender String
    | SubmitForm
    | GotCurrentDate Date
    | GotQuoteInfo (Result Http.Error QuoteInfo)
    | GotZipInfo (Result Http.Error ZipInfo)
    | UpdateSelectedCounty String


type alias InitialValues =
    { zipCode : Maybe String
    , dateOfBirth : Maybe String
    , tobacco : Maybe Bool
    , gender : Maybe String
    , quoteId : Maybe String
    , planType : Maybe String
    , orgId : Maybe String
    }


type alias QuoteInfo =
    { zipCode : String
    , dateOfBirth : String
    , tobacco : Bool
    , gender : String
    , currentCarrier : String
    , orgId : String
    }


type alias ZipInfo =
    { state : String
    , counties : List String
    }


init : Nav.Key -> InitialValues -> ( Model, Cmd Msg )
init key initialValues =
    let
        model =
            { zipCode = Maybe.withDefault "" initialValues.zipCode
            , dateOfBirth = Maybe.withDefault "" initialValues.dateOfBirth
            , key = key
            , tobacco = Maybe.withDefault False initialValues.tobacco
            , gender = Maybe.withDefault "M" initialValues.gender
            , currentDate = Nothing
            , quoteId = initialValues.quoteId
            , error = Nothing
            , currentCarrier = Nothing
            , planType = initialValues.planType
            , state = Nothing
            , counties = []
            , selectedCounty = Nothing
            , isLoadingZipData = False
            , zipError = Nothing
            , orgId = initialValues.orgId
            }

        commands =
            [ Task.perform GotCurrentDate Date.today
            , case initialValues.quoteId of
                Just id ->
                    fetchQuoteInfo id

                Nothing ->
                    Cmd.none
            ]
                ++ (if String.length model.zipCode == 5 then
                        [ fetchZipInfo model.zipCode ]

                    else
                        []
                   )
    in
    ( model, Cmd.batch commands )


fetchQuoteInfo : String -> Cmd Msg
fetchQuoteInfo quoteId =
    Http.get
        { url = "/api/quotes/decode/" ++ quoteId
        , expect = Http.expectJson GotQuoteInfo quoteInfoDecoder
        }


fetchZipInfo : String -> Cmd Msg
fetchZipInfo zipCode =
    Http.get
        { url = "/api/zipinfo/" ++ zipCode
        , expect = Http.expectJson GotZipInfo zipInfoDecoder
        }


quoteInfoDecoder : D.Decoder QuoteInfo
quoteInfoDecoder =
    D.field "success" D.bool
        |> D.andThen
            (\success ->
                if success then
                    D.map6 QuoteInfo
                        (D.at [ "contact", "zipCode" ] D.string)
                        (D.at [ "contact", "dateOfBirth" ] D.string)
                        (D.at [ "contact", "tobacco" ] D.bool)
                        (D.at [ "contact", "gender" ] D.string)
                        (D.at [ "contact", "currentCarrier" ] D.string)
                        (D.field "orgId" D.string)

                else
                    D.field "error" D.string
                        |> D.andThen (\error -> D.fail error)
            )


zipInfoDecoder : D.Decoder ZipInfo
zipInfoDecoder =
    D.field "success" D.bool
        |> D.andThen
            (\success ->
                if success then
                    D.field "data"
                        (D.map2 ZipInfo
                            (D.field "state" D.string)
                            (D.field "counties" (D.list D.string))
                        )

                else
                    D.field "error" D.string
                        |> D.andThen (\error -> D.fail error)
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        UpdateDateOfBirth dob ->
            ( { model | dateOfBirth = dob }, Cmd.none )

        UpdateTobacco value ->
            ( { model | tobacco = value == "true" }, Cmd.none )

        UpdateGender value ->
            ( { model | gender = value }, Cmd.none )

        GotCurrentDate date ->
            ( { model | currentDate = Just date }, Cmd.none )

        GotQuoteInfo result ->
            case result of
                Ok quoteInfo ->
                    let
                        updatedModel =
                            { model
                                | zipCode = quoteInfo.zipCode
                                , dateOfBirth = quoteInfo.dateOfBirth
                                , tobacco = quoteInfo.tobacco
                                , gender = quoteInfo.gender
                                , currentCarrier =
                                    if String.isEmpty quoteInfo.currentCarrier then
                                        Nothing

                                    else
                                        Just quoteInfo.currentCarrier
                                , orgId = Just quoteInfo.orgId
                            }

                        cmd =
                            if String.length quoteInfo.zipCode == 5 then
                                fetchZipInfo quoteInfo.zipCode

                            else
                                Cmd.none
                    in
                    ( updatedModel, cmd )

                Err err ->
                    let
                        errorMessage =
                            case err of
                                Http.BadBody errorBody ->
                                    if String.contains "Contact not found" errorBody then
                                        "Quote not found or expired. Please try again or contact support."

                                    else
                                        "Failed to load quote information: " ++ errorBody

                                Http.BadStatus 404 ->
                                    "Quote not found or expired. Please try again or contact support."

                                _ ->
                                    "Failed to load quote information. Please try again."
                    in
                    ( { model | error = Just errorMessage }, Cmd.none )

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

        SubmitForm ->
            if String.length model.zipCode /= 5 then
                ( { model | zipError = Just "Please enter a valid 5-digit zip code" }, Cmd.none )

            else if model.state == Nothing then
                ( { model | zipError = Just "Unable to determine state from zip code" }, Cmd.none )

            else
                let
                    age =
                        case model.currentDate of
                            Just currentDate ->
                                getAgeNextMonth model.dateOfBirth currentDate
                                    |> String.fromInt

                            Nothing ->
                                "65"

                    -- Get the selected county or the first one from the list
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

                    -- If we have a quoteId, use that for navigation first
                    compareUrl =
                        case model.quoteId of
                            Just id ->
                                -- When we have a quoteId, we don't need other parameters
                                Builder.absolute [ "compare" ] [ Builder.string "id" id ]

                            Nothing ->
                                -- Otherwise build the URL with all parameters
                                Builder.absolute [ "compare" ]
                                    ([ Builder.string "zip" model.zipCode
                                     , Builder.string "state" state
                                     , Builder.string "county" county
                                     , Builder.string "gender" model.gender
                                     , Builder.string "tobacco"
                                        (if model.tobacco then
                                            "true"

                                         else
                                            "false"
                                        )
                                     , Builder.string "age" age
                                     , Builder.string "dateOfBirth" model.dateOfBirth
                                     ]
                                        ++ (case model.currentCarrier of
                                                Just carrier ->
                                                    [ Builder.string "currentCarrier" carrier ]

                                                Nothing ->
                                                    []
                                           )
                                        ++ (case model.planType of
                                                Just planType ->
                                                    [ Builder.string "planType" planType ]

                                                Nothing ->
                                                    [ Builder.string "planType" "G" ]
                                            -- Default to G if no plan type provided
                                           )
                                    )
                in
                ( model
                , Nav.pushUrl model.key compareUrl
                )


view : Model -> Browser.Document Msg
view model =
    { title = "Get Your Quote - Medicare Max"
    , body =
        [ div [ class "container mx-auto px-4 py-6 sm:py-8 max-w-xl" ]
            [ h1 [ class "text-2xl sm:text-3xl font-bold text-center mb-4 sm:mb-6" ]
                [ text "Get Your Quote" ]
            , div [ class "flex justify-center mb-6 sm:mb-8" ]
                [ button
                    [ class "flex items-center gap-1 sm:gap-2 px-3 sm:px-4 py-1.5 sm:py-2 rounded-full border border-blue-500 text-blue-500 hover:bg-blue-50 transition-colors text-sm"
                    ]
                    [ span [ class "text-xs sm:text-sm" ] [ text "▶ Video" ]
                    , text "Rates and Plan Options"
                    ]
                ]
            , if model.error /= Nothing || (model.quoteId /= Nothing && String.isEmpty model.zipCode) then
                -- Show error or loading message when we have a quoteId but no data yet
                let
                    message =
                        case model.error of
                            Just error ->
                                div [ class "bg-red-100 border border-red-400 text-red-700 px-3 sm:px-4 py-2 sm:py-3 rounded mb-4 text-sm" ]
                                    [ text error ]

                            Nothing ->
                                if model.quoteId /= Nothing && String.isEmpty model.zipCode then
                                    div [ class "bg-blue-100 border border-blue-400 text-blue-700 px-3 sm:px-4 py-2 sm:py-3 rounded mb-4 text-sm" ]
                                        [ text "Loading your quote information..." ]

                                else
                                    text ""
                in
                div [] [ message, viewFormForManualEntry model ]

              else
                -- Standard form
                Html.form [ onSubmit SubmitForm, class "space-y-4 sm:space-y-6" ]
                    [ viewFormInput "Zip Code" "text" model.zipCode UpdateZipCode True

                    -- Show zip code loading state or error if any
                    , if model.isLoadingZipData then
                        div [ class "text-xs sm:text-sm text-blue-600" ]
                            [ text "Looking up location..." ]

                      else if model.zipError /= Nothing then
                        div [ class "text-xs sm:text-sm text-red-600" ]
                            [ text (Maybe.withDefault "Invalid zip code" model.zipError) ]

                      else
                        text ""

                    -- Only show the county dropdown if there are multiple counties
                    , if List.length model.counties > 1 then
                        viewCountyDropdown model.counties model.selectedCounty

                      else
                        text ""
                    , viewFormInput "Date of Birth" "date" model.dateOfBirth UpdateDateOfBirth True
                    , viewFormRadioGroup "Tobacco User"
                        (if model.tobacco then
                            "true"

                         else
                            "false"
                        )
                        UpdateTobacco
                        [ ( "true", "Yes" ), ( "false", "No" ) ]
                    , viewFormRadioGroup "Gender"
                        model.gender
                        UpdateGender
                        [ ( "M", "Male" ), ( "F", "Female" ) ]
                    , button
                        [ class "w-full bg-purple-600 text-white py-3 sm:py-4 rounded-lg hover:bg-purple-700 transition-colors mt-6 sm:mt-8 text-base sm:text-lg"
                        , type_ "submit"
                        ]
                        [ text "Next" ]
                    ]
            ]
        ]
    }


viewCountyDropdown : List String -> Maybe String -> Html Msg
viewCountyDropdown counties selectedCounty =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-1.5 sm:mb-2" ]
            [ text "County" ]
        , select
            [ class "w-full px-3 sm:px-4 py-2.5 sm:py-3 bg-white border-[2px] sm:border-[2.5px] border-purple-300 rounded-lg text-gray-700 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200 text-sm sm:text-base"
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


viewFormInput : String -> String -> String -> (String -> Msg) -> Bool -> Html Msg
viewFormInput labelText inputType inputValue msg isRequired =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-1.5 sm:mb-2" ]
            [ text labelText ]
        , if inputType == "date" then
            input
                [ type_ inputType
                , class "w-full px-3 sm:px-4 py-2.5 sm:py-3 bg-white border-[2px] sm:border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200 text-sm sm:text-base"
                , Html.Attributes.value inputValue
                , onInput msg
                , required isRequired
                ]
                []

          else if labelText == "Zip Code" then
            input
                [ type_ inputType
                , class "w-full px-3 sm:px-4 py-2.5 sm:py-3 bg-white border-[2px] sm:border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200 text-sm sm:text-base"
                , Html.Attributes.value (formatZipCode inputValue)
                , onInput msg
                , required isRequired
                , Html.Attributes.maxlength 5
                , Html.Attributes.pattern "[0-9]*"
                ]
                []

          else
            input
                [ type_ inputType
                , class "w-full px-3 sm:px-4 py-2.5 sm:py-3 bg-white border-[2px] sm:border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200 text-sm sm:text-base"
                , Html.Attributes.value inputValue
                , onInput msg
                , required isRequired
                ]
                []
        ]


viewFormRadioGroup : String -> String -> (String -> Msg) -> List ( String, String ) -> Html Msg
viewFormRadioGroup labelText selectedValue msg options =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-1.5 sm:mb-2" ]
            [ text labelText ]
        , div [ class "flex flex-col sm:flex-row gap-2 sm:gap-4 w-full" ]
            (List.map
                (\( val, txt ) ->
                    label
                        [ class
                            ("flex items-center justify-center px-3 sm:px-4 py-2 rounded-lg border-2 cursor-pointer transition-all duration-200 flex-1 "
                                ++ (if selectedValue == val then
                                        "border-purple-500 bg-purple-50 text-purple-700"

                                    else
                                        "border-gray-200 hover:border-purple-200"
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


formatZipCode : String -> String
formatZipCode zip =
    String.filter Char.isDigit zip |> String.left 5


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Function to display a form that allows manual entry when quote loading fails


viewFormForManualEntry : Model -> Html Msg
viewFormForManualEntry model =
    div []
        [ p [ class "text-center text-gray-600 mb-6" ]
            [ text "You can manually enter your information below to get a quote." ]
        , Html.form [ onSubmit SubmitForm, class "space-y-4 sm:space-y-6" ]
            [ viewFormInput "Zip Code" "text" model.zipCode UpdateZipCode True
            , viewFormInput "Date of Birth" "date" model.dateOfBirth UpdateDateOfBirth True
            , viewFormRadioGroup "Tobacco User"
                (if model.tobacco then
                    "true"

                 else
                    "false"
                )
                UpdateTobacco
                [ ( "true", "Yes" ), ( "false", "No" ) ]
            , viewFormRadioGroup "Gender"
                model.gender
                UpdateGender
                [ ( "M", "Male" ), ( "F", "Female" ) ]
            , button
                [ class "w-full bg-purple-600 text-white py-3 sm:py-4 rounded-lg hover:bg-purple-700 transition-colors mt-6 sm:mt-8 text-base sm:text-lg"
                , type_ "submit"
                ]
                [ text "Next" ]
            ]
        ]
