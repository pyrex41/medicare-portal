module Eligibility exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
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
    }


type Msg
    = AnswerQuestion Int Bool
    | SubmitAnswers
    | SkipQuestions



-- INIT


init : Nav.Key -> ( Model, Cmd Msg )
init key =
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
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SkipQuestions ->
            ( model
            , Nav.pushUrl model.key "/quote"
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

        SubmitAnswers ->
            let
                allAnswered =
                    List.all (.answer >> (/=) Nothing) model.questions

                anyYes =
                    List.any (\q -> q.answer == Just True) model.questions

                nextUrl =
                    if allAnswered then
                        if anyYes then
                            "/declined"

                        else
                            "/apply"

                    else
                        "/eligibility"
            in
            ( model
            , Nav.pushUrl model.key nextUrl
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Underwriting Assessment"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ nav [ class "bg-white border-b border-gray-200" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "flex justify-between h-16 items-center" ]
                        [ div [ class "flex-shrink-0" ]
                            [ img [ src "/images/medicare-max-logo.png", class "h-8 w-auto", alt "Medicare Max" ] [] ]
                        , div [ class "flex space-x-8" ]
                            [ a [ href "/", class "text-gray-500 hover:text-gray-900" ] [ text "Start Here" ]
                            , a [ href "/quote", class "text-gray-500 hover:text-gray-900" ] [ text "Get a Quote" ]
                            , a [ href "/compare", class "text-gray-500 hover:text-gray-900" ] [ text "Compare" ]
                            , a [ href "/apply", class "text-gray-500 hover:text-gray-900" ] [ text "Apply" ]
                            , a [ href "/contact", class "text-gray-500 hover:text-gray-900" ] [ text "Contact Us" ]
                            , a [ href "/login", class "text-gray-500 hover:text-gray-900" ] [ text "Login/Register" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-12" ]
                [ h1 [ class "text-3xl font-bold text-center text-gray-900 mb-4" ]
                    [ text "Underwriting Assessment" ]
                , p [ class "text-gray-600 text-center mb-12" ]
                    [ text "In order to qualify for a new Supplemental plan you must past medical underwriting. This is a quick questionnaire to assess the likelihood of you being able to pass." ]
                , Html.form [ onSubmit SubmitAnswers, class "space-y-8" ]
                    (List.map viewQuestion model.questions
                        ++ [ viewSubmitButton model ]
                    )
                , div [ class "text-center mt-6" ]
                    [ button
                        [ onClick SkipQuestions
                        , class "text-blue-600 hover:text-blue-800 underline text-sm"
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
    div [ class "space-y-4" ]
        [ p [ class "text-gray-900 text-lg" ]
            [ text question.text ]
        , div [ class "grid grid-cols-2 gap-4" ]
            [ viewRadioButton question "Yes" True
            , viewRadioButton question "No" False
            ]
        ]


viewRadioButton : Question -> String -> Bool -> Html Msg
viewRadioButton question labelText value =
    label
        [ class
            ("flex items-center justify-center px-6 py-3 rounded-lg border-[2.5px] cursor-pointer transition-all duration-200 w-full "
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
        , span [ class "font-medium" ] [ text labelText ]
        ]


viewSubmitButton : Model -> Html Msg
viewSubmitButton model =
    let
        allAnswered =
            List.all (.answer >> (/=) Nothing) model.questions

        buttonClass =
            "w-full py-4 rounded-lg text-white font-medium transition-colors duration-200 mt-8 "
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
