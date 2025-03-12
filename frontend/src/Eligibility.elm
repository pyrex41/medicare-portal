module Eligibility exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Http
import Json.Encode as E
import Url.Builder as Builder



-- TYPES


type alias Question =
    { id : Int
    , text : String
    , answer : Maybe Bool
    }


type alias Model =
    { key : Nav.Key
    , questions : List Question
    , quoteId : Maybe String
    }


type Msg
    = AnswerQuestion Int Bool
    | SubmitAnswers
    | SkipQuestions
    | GotSubmitResponse (Result Http.Error ())



-- INIT


init : Nav.Key -> Maybe String -> ( Model, Cmd Msg )
init key maybeQuoteId =
    ( { key = key
      , questions =
            [ { id = 1
              , text = "Are you currently hospitalized or in a nursing home or assisted living facility; or, are you bedridden or confined to a wheelchair, or require the assistance of motorized mobility aid, or have you had any amputation caused by disease?"
              , answer = Nothing
              }
            , { id = 2
              , text = "Within the past 24 months, have you been hospitalized two or more times, or been confined to a nursing home or assisted living facility?"
              , answer = Nothing
              }
            , { id = 3
              , text = "Have you been advised by a medical professional to have surgery, medical tests, treatment or therapy that has not been performed?"
              , answer = Nothing
              }
            ]
      , quoteId = maybeQuoteId
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SkipQuestions ->
            ( model
            , Nav.pushUrl model.key
                (case model.quoteId of
                    Just id ->
                        "/schedule?id=" ++ id

                    Nothing ->
                        "/schedule"
                )
            )

        AnswerQuestion id answer ->
            ( { model
                | questions =
                    List.map
                        (\q ->
                            if q.id == id then
                                { q | answer = Just answer }

                            else
                                q
                        )
                        model.questions
              }
            , Cmd.none
            )

        GotSubmitResponse result ->
            case result of
                Ok _ ->
                    let
                        anyNo =
                            List.any (\q -> q.answer == Just False) model.questions

                        nextUrl =
                            case model.quoteId of
                                Just id ->
                                    if anyNo then
                                        "/schedule?id=" ++ id ++ "&status=decline"

                                    else
                                        "/schedule?id=" ++ id ++ "&status=accept"

                                Nothing ->
                                    if anyNo then
                                        "/schedule?status=decline"

                                    else
                                        "/schedule?status=accept"
                    in
                    ( model
                    , Nav.pushUrl model.key nextUrl
                    )

                Err _ ->
                    -- On error, still proceed to next page but don't save answers
                    let
                        anyNo =
                            List.any (\q -> q.answer == Just False) model.questions

                        nextUrl =
                            case model.quoteId of
                                Just id ->
                                    if anyNo then
                                        "/schedule?id=" ++ id ++ "&status=decline"

                                    else
                                        "/schedule?id=" ++ id ++ "&status=accept"

                                Nothing ->
                                    if anyNo then
                                        "/schedule?status=decline"

                                    else
                                        "/schedule?status=accept"
                    in
                    ( model
                    , Nav.pushUrl model.key nextUrl
                    )

        SubmitAnswers ->
            let
                allAnswered =
                    List.all (.answer >> (/=) Nothing) model.questions

                submitAnswers =
                    case model.quoteId of
                        Just quoteId ->
                            Http.post
                                { url = "/api/eligibility-answers"
                                , body =
                                    Http.jsonBody <|
                                        E.object
                                            [ ( "quote_id", E.string quoteId )
                                            , ( "answers"
                                              , E.object
                                                    (List.filterMap
                                                        (\q ->
                                                            case q.answer of
                                                                Just ans ->
                                                                    Just ( String.fromInt q.id, E.bool ans )

                                                                Nothing ->
                                                                    Nothing
                                                        )
                                                        model.questions
                                                    )
                                              )
                                            ]
                                , expect = Http.expectWhatever GotSubmitResponse
                                }

                        Nothing ->
                            -- If no quote ID, just proceed without saving
                            Cmd.none
            in
            if allAnswered then
                ( model
                , submitAnswers
                )

            else
                ( model
                , Cmd.none
                )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Underwriting Assessment"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ div [ class "max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-6 sm:py-12" ]
                [ h1 [ class "text-2xl sm:text-3xl font-bold text-center text-gray-900 mb-3 sm:mb-4" ]
                    [ text "Underwriting Assessment" ]
                , p [ class "text-gray-600 text-center mb-8 sm:mb-12 text-sm sm:text-base" ]
                    [ text "In order to qualify for a new Supplemental plan you must past medical underwriting. This is a quick questionnaire to assess the likelihood of you being able to pass." ]
                , Html.form [ onSubmit SubmitAnswers, class "space-y-6 sm:space-y-8" ]
                    (List.map viewQuestion model.questions
                        ++ [ viewSubmitButton model ]
                    )
                , div [ class "text-center mt-4 sm:mt-6" ]
                    [ button
                        [ onClick SkipQuestions
                        , class "text-blue-600 hover:text-blue-800 underline text-sm py-2"
                        , type_ "button"
                        ]
                        [ text "Skip" ]
                    ]
                ]
            ]
        ]
    }


viewQuestion : Question -> Html Msg
viewQuestion question =
    div [ class "space-y-3 sm:space-y-4" ]
        [ p [ class "text-gray-900 text-base sm:text-lg" ]
            [ text question.text ]
        , div [ class "grid grid-cols-2 gap-2 sm:gap-4 max-w-md mx-auto" ]
            [ viewRadioButton question "Yes" True
            , viewRadioButton question "No" False
            ]
        ]


viewRadioButton : Question -> String -> Bool -> Html Msg
viewRadioButton question labelText value =
    label
        [ class
            ("flex items-center justify-center px-4 sm:px-6 py-2.5 sm:py-3 rounded-lg border-[2px] sm:border-[2.5px] cursor-pointer transition-all duration-200 w-full "
                ++ (if question.answer == Just value then
                        "border-blue-500 bg-white text-gray-900"

                    else
                        "border-gray-200 hover:border-blue-200 text-gray-700"
                   )
            )
        , onClick (AnswerQuestion question.id value)
        ]
        [ input
            [ type_ "radio"
            , name ("question-" ++ String.fromInt question.id)
            , checked (question.answer == Just value)
            , class "sr-only"
            ]
            []
        , span [ class "font-medium text-sm sm:text-base" ] [ text labelText ]
        ]


viewSubmitButton : Model -> Html Msg
viewSubmitButton model =
    let
        allAnswered =
            List.all (.answer >> (/=) Nothing) model.questions

        buttonClass =
            "w-full py-3 sm:py-4 rounded-lg text-white font-medium transition-colors duration-200 mt-6 sm:mt-8 text-base sm:text-lg "
                ++ (if allAnswered then
                        "bg-purple-600 hover:bg-purple-700"

                    else
                        "bg-gray-300 cursor-not-allowed"
                   )
    in
    button
        [ class buttonClass
        , type_ "submit"
        , disabled (not allAnswered)
        ]
        [ text "Next" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
