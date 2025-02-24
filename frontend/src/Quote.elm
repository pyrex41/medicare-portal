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
    }


type Msg
    = UpdateZipCode String
    | UpdateDateOfBirth String
    | UpdateTobacco String
    | UpdateGender String
    | SubmitForm
    | GotCurrentDate Date
    | GotQuoteInfo (Result Http.Error QuoteInfo)


type alias InitialValues =
    { zipCode : Maybe String
    , dateOfBirth : Maybe String
    , tobacco : Maybe Bool
    , gender : Maybe String
    , quoteId : Maybe String
    }


type alias QuoteInfo =
    { zipCode : String
    , dateOfBirth : String
    , tobacco : Bool
    , gender : String
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
            }

        commands =
            [ Task.perform GotCurrentDate Date.today
            , case initialValues.quoteId of
                Just id ->
                    fetchQuoteInfo id

                Nothing ->
                    Cmd.none
            ]
    in
    ( model, Cmd.batch commands )


fetchQuoteInfo : String -> Cmd Msg
fetchQuoteInfo quoteId =
    Http.get
        { url = "/api/quotes/decode/" ++ quoteId
        , expect = Http.expectJson GotQuoteInfo quoteInfoDecoder
        }


quoteInfoDecoder : D.Decoder QuoteInfo
quoteInfoDecoder =
    D.field "contact"
        (D.map4 QuoteInfo
            (D.field "zipCode" D.string)
            (D.field "dateOfBirth" D.string)
            (D.field "tobacco" D.bool)
            (D.field "gender" D.string)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateZipCode zip ->
            ( { model | zipCode = String.filter Char.isDigit zip |> String.left 5 }, Cmd.none )

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
                    ( { model
                        | zipCode = quoteInfo.zipCode
                        , dateOfBirth = quoteInfo.dateOfBirth
                        , tobacco = quoteInfo.tobacco
                        , gender = quoteInfo.gender
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to load quote information" }, Cmd.none )

        SubmitForm ->
            let
                age =
                    case model.currentDate of
                        Just currentDate ->
                            getAgeNextMonth model.dateOfBirth currentDate
                                |> String.fromInt

                        Nothing ->
                            "65"

                -- Fallback if we somehow don't have current date
                compareUrl =
                    Builder.absolute [ "compare" ]
                        ([ Builder.string "state" "TX"
                         , Builder.string "zip" model.zipCode
                         , Builder.string "county" "Dallas" -- We'll need to look this up based on zip
                         , Builder.string "gender" model.gender
                         , Builder.string "tobacco"
                            (if model.tobacco then
                                "true"

                             else
                                "false"
                            )
                         , Builder.string "age" age
                         , Builder.string "planType" "G"
                         , Builder.string "dateOfBirth" model.dateOfBirth
                         ]
                            ++ (case model.quoteId of
                                    Just id ->
                                        [ Builder.string "id" id ]

                                    Nothing ->
                                        []
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
        [ div [ class "container mx-auto px-4 py-8 max-w-xl" ]
            [ h1 [ class "text-3xl font-bold text-center mb-6" ]
                [ text "Get Your Quote" ]
            , div [ class "flex justify-center mb-8" ]
                [ button
                    [ class "flex items-center gap-2 px-4 py-2 rounded-full border border-blue-500 text-blue-500 hover:bg-blue-50 transition-colors"
                    ]
                    [ span [ class "text-sm" ] [ text "▶ Video" ]
                    , text "Rates and Plan Options"
                    ]
                ]
            , case model.error of
                Just error ->
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded mb-4" ]
                        [ text error ]

                Nothing ->
                    text ""
            , Html.form [ onSubmit SubmitForm, class "space-y-6" ]
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
                    [ class "w-full bg-purple-600 text-white py-4 rounded-lg hover:bg-purple-700 transition-colors mt-8"
                    , type_ "submit"
                    ]
                    [ text "Next" ]
                ]
            ]
        ]
    }


viewFormInput : String -> String -> String -> (String -> Msg) -> Bool -> Html Msg
viewFormInput labelText inputType inputValue msg isRequired =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , if inputType == "date" then
            input
                [ type_ inputType
                , class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
                , Html.Attributes.value inputValue
                , onInput msg
                , required isRequired
                ]
                []

          else if labelText == "Zip Code" then
            input
                [ type_ inputType
                , class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
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
                , class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
                , Html.Attributes.value inputValue
                , onInput msg
                , required isRequired
                ]
                []
        ]


viewFormRadioGroup : String -> String -> (String -> Msg) -> List ( String, String ) -> Html Msg
viewFormRadioGroup labelText selectedValue msg options =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , div [ class "flex gap-4" ]
            (List.map
                (\( val, txt ) ->
                    label
                        [ class
                            ("flex items-center px-4 py-2 rounded-lg border-2 cursor-pointer transition-all duration-200 "
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
