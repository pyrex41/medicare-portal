module Eligibility exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Encode as E
import Url.Builder as Builder



-- TYPES


type QuestionType
    = MainQuestion
    | FollowUpQuestion Int -- Parent question ID


type alias Question =
    { id : Int
    , text : String
    , questionType : QuestionType
    , answer : Maybe Bool
    , followUpText : Maybe String -- Optional text field for follow-up responses
    }


type alias Model =
    { key : Nav.Key
    , questions : List Question
    , quoteId : Maybe String
    , orgId : String -- Required organization ID
    , isSubmitting : Bool
    , submissionError : Maybe String
    }


type Msg
    = AnswerQuestion Int Bool
    | UpdateFollowUpText Int String
    | SubmitAnswers
    | SkipQuestions
    | GotSubmitResponse (Result Http.Error String)
    | GotTempContactResponse (Result Http.Error String)



-- INIT


init : Nav.Key -> { quoteId : Maybe String, orgId : Maybe String } -> ( Model, Cmd Msg )
init key { quoteId, orgId } =
    let
        -- Extract org ID from quote ID if not provided explicitly
        -- Quote IDs are formatted as "orgId-contactId-hash"
        extractedOrgId =
            case ( orgId, quoteId ) of
                ( Nothing, Just id ) ->
                    -- Try to extract orgId from the quoteId (first part before the first dash)
                    id
                        |> String.split "-"
                        |> List.head
                        |> Maybe.withDefault ""

                ( Just orgIdValue, _ ) ->
                    orgIdValue

                ( Nothing, Nothing ) ->
                    ""
    in
    ( { key = key
      , questions =
            [ -- Heart Health
              { id = 1
              , text = "Heart Health: Have you ever been diagnosed with or treated for any heart condition, including but not limited to heart failure, coronary artery disease, angina, atrial fibrillation, or heart valve problems?"
              , questionType = MainQuestion
              , answer = Nothing
              , followUpText = Nothing
              }
            , { id = 101
              , text = "What specific heart condition(s) have you been diagnosed with?"
              , questionType = FollowUpQuestion 1
              , answer = Nothing
              , followUpText = Just ""
              }
            , { id = 102
              , text = "In the past 2 years, have you been hospitalized, had surgery, or any cardiac procedures (like angioplasty or stenting) for this condition?"
              , questionType = FollowUpQuestion 1
              , answer = Nothing
              , followUpText = Nothing
              }
            , { id = 103
              , text = "Are you currently taking 3 or more medications for this heart condition, or using oxygen or nitroglycerin?"
              , questionType = FollowUpQuestion 1
              , answer = Nothing
              , followUpText = Nothing
              }

            -- Lung Health
            , { id = 2
              , text = "Lung Health: Have you ever been diagnosed with or treated for a chronic lung condition such as COPD, emphysema, chronic bronchitis, or cystic fibrosis?"
              , questionType = MainQuestion
              , answer = Nothing
              , followUpText = Nothing
              }
            , { id = 201
              , text = "What specific lung condition(s) have you been diagnosed with?"
              , questionType = FollowUpQuestion 2
              , answer = Nothing
              , followUpText = Just ""
              }
            , { id = 202
              , text = "Does this condition require you to use oxygen at home or a nebulizer?"
              , questionType = FollowUpQuestion 2
              , answer = Nothing
              , followUpText = Nothing
              }
            , { id = 203
              , text = "In the past year, have you been hospitalized due to lung problems or had frequent (3 or more) respiratory infections requiring antibiotics or steroids?"
              , questionType = FollowUpQuestion 2
              , answer = Nothing
              , followUpText = Nothing
              }

            -- Cancer History
            , { id = 3
              , text = "Cancer History: Have you ever been diagnosed with or treated for cancer, leukemia, or lymphoma (excluding basal cell skin cancer)?"
              , questionType = MainQuestion
              , answer = Nothing
              , followUpText = Nothing
              }
            , { id = 301
              , text = "What type of cancer were you diagnosed with?"
              , questionType = FollowUpQuestion 3
              , answer = Nothing
              , followUpText = Just ""
              }
            , { id = 302
              , text = "When were you initially diagnosed?"
              , questionType = FollowUpQuestion 3
              , answer = Nothing
              , followUpText = Just ""
              }
            , { id = 303
              , text = "Are you currently undergoing active cancer treatment (chemotherapy, radiation, immunotherapy, targeted therapy, or hormone therapy)?"
              , questionType = FollowUpQuestion 3
              , answer = Nothing
              , followUpText = Nothing
              }

            -- Diabetes with Complications
            , { id = 4
              , text = "Diabetes with Complications: Have you ever been diagnosed with diabetes that has resulted in any complications affecting your eyes (retinopathy), nerves (neuropathy), kidneys (nephropathy), circulation (vascular disease), or heart?"
              , questionType = MainQuestion
              , answer = Nothing
              , followUpText = Nothing
              }
            , { id = 401
              , text = "Which complications of diabetes have you experienced? (e.g., Neuropathy, Retinopathy, Kidney Problems, Vascular Disease, Heart Problems)"
              , questionType = FollowUpQuestion 4
              , answer = Nothing
              , followUpText = Just ""
              }
            , { id = 402
              , text = "Do you require insulin to manage your diabetes, and if so, what is your typical daily insulin dosage?"
              , questionType = FollowUpQuestion 4
              , answer = Nothing
              , followUpText = Just ""
              }

            -- Memory or Cognitive Health
            , { id = 5
              , text = "Memory or Cognitive Health: Do you have any concerns about your memory, thinking, or cognitive function, OR have you ever been diagnosed with dementia, Alzheimer's disease, or any other cognitive impairment?"
              , questionType = MainQuestion
              , answer = Nothing
              , followUpText = Nothing
              }
            , { id = 501
              , text = "Have you been formally diagnosed with any cognitive impairment? If yes, what diagnosis?"
              , questionType = FollowUpQuestion 5
              , answer = Nothing
              , followUpText = Just ""
              }
            , { id = 502
              , text = "Do you require assistance with activities of daily living such as remembering medications, managing finances, or personal care due to cognitive impairment?"
              , questionType = FollowUpQuestion 5
              , answer = Nothing
              , followUpText = Nothing
              }

            -- Recent and Current Hospitalizations
            , { id = 6
              , text = "In the past 2 years, have you been hospitalized overnight in a hospital more than two times for any medical condition (excluding planned surgeries)?"
              , questionType = MainQuestion
              , answer = Nothing
              , followUpText = Nothing
              }
            , { id = 7
              , text = "Are you currently hospitalized or waiting on results from a medical test, or been recommended by a doctor to have a medical test performed?"
              , questionType = MainQuestion
              , answer = Nothing
              , followUpText = Nothing
              }
            ]
      , quoteId = quoteId
      , orgId = extractedOrgId
      , isSubmitting = False
      , submissionError = Nothing
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
                (Builder.absolute [ "schedule" ]
                    ([ Builder.string "orgId" model.orgId ]
                        ++ (case model.quoteId of
                                Just id ->
                                    [ Builder.string "id" id ]

                                Nothing ->
                                    []
                           )
                    )
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

        UpdateFollowUpText id text ->
            ( { model
                | questions =
                    List.map
                        (\q ->
                            if q.id == id then
                                { q | followUpText = Just text }

                            else
                                q
                        )
                        model.questions
              }
            , Cmd.none
            )

        GotSubmitResponse result ->
            case result of
                Ok contactId ->
                    let
                        anyYes =
                            List.any (\q -> q.questionType == MainQuestion && q.answer == Just True) model.questions

                        nextUrl =
                            Builder.absolute [ "schedule" ]
                                ([ Builder.string "orgId" model.orgId
                                 , Builder.string "status"
                                    (if anyYes then
                                        "decline"

                                     else
                                        "accept"
                                    )
                                 ]
                                    ++ (case model.quoteId of
                                            Just id ->
                                                [ Builder.string "id" id ]

                                            Nothing ->
                                                [ Builder.string "contactId" contactId ]
                                       )
                                )
                    in
                    ( { model | isSubmitting = False }
                    , Nav.pushUrl model.key nextUrl
                    )

                Err error ->
                    ( { model
                        | isSubmitting = False
                        , submissionError = Just "Failed to save your answers. Please try again."
                      }
                    , Cmd.none
                    )

        GotTempContactResponse result ->
            case result of
                Ok contactId ->
                    -- Now that we have a temporary contact, submit the answers
                    let
                        relevantQuestions =
                            getRelevantQuestions model

                        encodedAnswers =
                            encodeAnswers relevantQuestions
                    in
                    ( model
                    , Http.post
                        { url = "/api/org/" ++ model.orgId ++ "/eligibility-answers"
                        , body =
                            Http.jsonBody <|
                                E.object
                                    [ ( "contact_id", E.string contactId )
                                    , ( "answers", encodedAnswers )
                                    ]
                        , expect = Http.expectString GotSubmitResponse
                        }
                    )

                Err error ->
                    ( { model
                        | isSubmitting = False
                        , submissionError = Just "Failed to create temporary contact. Please try again."
                      }
                    , Cmd.none
                    )

        SubmitAnswers ->
            let
                relevantQuestions =
                    getRelevantQuestions model

                allRelevantQuestionsAnswered =
                    List.all
                        (\q ->
                            case q.followUpText of
                                Just _ ->
                                    True

                                Nothing ->
                                    q.answer /= Nothing
                        )
                        relevantQuestions

                encodedAnswers =
                    encodeAnswers relevantQuestions
            in
            if allRelevantQuestionsAnswered then
                case model.quoteId of
                    Just quoteId ->
                        -- If we have a quote ID, submit answers directly
                        ( { model | isSubmitting = True, submissionError = Nothing }
                        , Http.post
                            { url = "/api/org/" ++ model.orgId ++ "/eligibility-answers"
                            , body =
                                Http.jsonBody <|
                                    E.object
                                        [ ( "quote_id", E.string quoteId )
                                        , ( "answers", encodedAnswers )
                                        ]
                            , expect = Http.expectString GotSubmitResponse
                            }
                        )

                    Nothing ->
                        -- If no quote ID, first create a temporary contact
                        ( { model | isSubmitting = True, submissionError = Nothing }
                        , Http.post
                            { url = "/api/org/" ++ model.orgId ++ "/temp-contact"
                            , body = Http.jsonBody <| E.object [] -- Minimal data for temp contact
                            , expect = Http.expectString GotTempContactResponse
                            }
                        )

            else
                ( model
                , Cmd.none
                )



-- Helper functions


getRelevantQuestions : Model -> List Question
getRelevantQuestions model =
    let
        mainQuestions =
            List.filter (\q -> q.questionType == MainQuestion) model.questions

        followUpQuestions =
            List.filter
                (\q ->
                    case q.questionType of
                        FollowUpQuestion parentId ->
                            let
                                parentQuestion =
                                    List.filter (\p -> p.id == parentId) model.questions
                                        |> List.head
                            in
                            case parentQuestion of
                                Just parent ->
                                    parent.answer == Just True

                                Nothing ->
                                    False

                        MainQuestion ->
                            False
                )
                model.questions
    in
    mainQuestions ++ followUpQuestions


encodeAnswers : List Question -> E.Value
encodeAnswers questions =
    E.object
        (List.filterMap
            (\q ->
                if q.answer == Nothing && q.followUpText == Nothing then
                    Nothing

                else
                    let
                        questionType =
                            case q.questionType of
                                MainQuestion ->
                                    "main"

                                FollowUpQuestion parentId ->
                                    "followup_" ++ String.fromInt parentId

                        answerValue =
                            case q.answer of
                                Just ans ->
                                    E.bool ans

                                Nothing ->
                                    case q.followUpText of
                                        Just text ->
                                            E.string text

                                        Nothing ->
                                            E.null
                    in
                    Just
                        ( String.fromInt q.id
                        , E.object
                            [ ( "question_text", E.string q.text )
                            , ( "question_type", E.string questionType )
                            , ( "answer", answerValue )
                            ]
                        )
            )
            questions
        )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Underwriting Assessment"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ div [ class "max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-6 sm:py-8" ]
                [ h1 [ class "text-2xl sm:text-3xl font-bold text-center text-gray-900 mb-2 sm:mb-3" ]
                    [ text "Underwriting Assessment" ]
                , p [ class "text-gray-600 text-center mb-6 sm:mb-8 text-sm sm:text-base max-w-xl mx-auto" ]
                    [ text "In order to qualify for a new Supplemental plan you must pass medical underwriting. Please answer all questions to the best of your knowledge." ]
                , if model.submissionError /= Nothing then
                    div [ class "mb-4 p-3 bg-red-100 text-red-700 rounded-lg text-sm" ]
                        [ text (Maybe.withDefault "" model.submissionError) ]

                  else
                    text ""
                , Html.form [ onSubmit SubmitAnswers, class "space-y-4 sm:space-y-6" ]
                    (viewQuestionsWithFollowUps model
                        ++ [ viewSubmitButton model ]
                    )
                , div [ class "text-center mt-3 sm:mt-4" ]
                    [ button
                        [ onClick SkipQuestions
                        , class "text-blue-600 hover:text-blue-800 underline text-sm py-2"
                        , type_ "button"
                        , disabled model.isSubmitting
                        ]
                        [ text "Skip" ]
                    ]
                ]
            ]
        ]
    }


viewQuestionsWithFollowUps : Model -> List (Html Msg)
viewQuestionsWithFollowUps model =
    let
        mainQuestions =
            List.filter (\q -> q.questionType == MainQuestion) model.questions
    in
    List.map
        (\mainQ ->
            let
                followUps =
                    List.filter
                        (\q ->
                            case q.questionType of
                                FollowUpQuestion parentId ->
                                    parentId == mainQ.id

                                MainQuestion ->
                                    False
                        )
                        model.questions

                shouldShowFollowUps =
                    mainQ.answer == Just True
            in
            div [ class "mb-6 transition-all duration-200" ]
                [ viewQuestion mainQ
                , if shouldShowFollowUps then
                    div
                        [ class "relative pl-6 mt-2 transition-all duration-300 overflow-hidden" ]
                        [ div [ class "absolute top-0 bottom-0 left-6 w-0.5 bg-blue-300" ] []
                        , div [ class "border border-gray-200 rounded-lg shadow-sm overflow-hidden" ]
                            (List.map viewFollowUpQuestion followUps)
                        ]

                  else
                    text ""
                ]
        )
        mainQuestions


viewQuestion : Question -> Html Msg
viewQuestion question =
    div [ class "border border-gray-300 rounded-lg shadow-md overflow-hidden" ]
        [ div [ class "px-4 py-4 sm:px-5 sm:py-5 bg-white" ]
            [ p [ class "text-gray-900 text-sm sm:text-base font-medium mb-3" ]
                [ text question.text ]
            , div [ class "grid grid-cols-2 gap-2 sm:gap-3" ]
                [ viewRadioButton question "Yes" True
                , viewRadioButton question "No" False
                ]
            ]
        ]


viewFollowUpQuestion : Question -> Html Msg
viewFollowUpQuestion question =
    div
        [ class "px-4 py-3 sm:px-5 sm:py-4 bg-white border-b border-gray-100 last:border-b-0" ]
        [ p [ class "text-gray-700 text-sm sm:text-base mb-2" ]
            [ text question.text ]
        , case question.followUpText of
            Just textValue ->
                div [ class "mt-2" ]
                    [ textarea
                        [ class "w-full p-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500 text-sm"
                        , rows 2
                        , placeholder "Please provide details..."
                        , value textValue
                        , onInput (UpdateFollowUpText question.id)
                        ]
                        []
                    ]

            Nothing ->
                div [ class "grid grid-cols-2 gap-2 sm:gap-3" ]
                    [ viewRadioButton question "Yes" True
                    , viewRadioButton question "No" False
                    ]
        ]


viewRadioButton : Question -> String -> Bool -> Html Msg
viewRadioButton question labelText value =
    label
        [ class
            ("flex items-center justify-center px-3 sm:px-4 py-2 sm:py-2.5 rounded-md border text-sm sm:text-base cursor-pointer transition-all duration-200 w-full "
                ++ (if question.answer == Just value then
                        if value then
                            "border-blue-500 bg-blue-50 text-blue-700 font-medium shadow-sm"

                        else
                            "border-gray-500 bg-gray-50 text-gray-700 font-medium shadow-sm"

                    else
                        "border-gray-200 hover:border-blue-200 hover:bg-gray-50 text-gray-700"
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
        relevantQuestions =
            getRelevantQuestions model

        allRelevantQuestionsAnswered =
            List.all
                (\q ->
                    case q.followUpText of
                        Just _ ->
                            True

                        Nothing ->
                            q.answer /= Nothing
                )
                relevantQuestions

        buttonClass =
            "w-full py-2.5 sm:py-3 rounded-lg text-white font-medium transition-colors duration-200 mt-4 sm:mt-6 text-sm sm:text-base shadow-sm "
                ++ (if allRelevantQuestionsAnswered && not model.isSubmitting then
                        "bg-purple-600 hover:bg-purple-700"

                    else
                        "bg-gray-300 cursor-not-allowed"
                   )
    in
    button
        [ class buttonClass
        , type_ "submit"
        , disabled (not allRelevantQuestionsAnswered || model.isSubmitting)
        ]
        [ if model.isSubmitting then
            text "Submitting..."

          else
            text "Next"
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
