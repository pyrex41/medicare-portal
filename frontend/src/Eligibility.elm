module Eligibility exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import MyIcon
import Url.Builder as Builder
import Utils.QuoteHeader exposing (viewHeader)



-- TYPES


type Color
    = DangerColor
    | BlueColor
    | AmberColor
    | SuccessColor
    | PurpleColor
    | BrandColor


type AnswerType
    = BooleanAnswer (Maybe Bool)
    | TextAnswer String


type alias FollowUpQuestion =
    { id : Int
    , text : String
    , answerType : AnswerType
    }


type alias Question =
    { id : Int
    , icon : Icon
    , title : String
    , text : String
    , color : Color
    , answer : Maybe Bool
    , followUpQuestions : List FollowUpQuestion
    , isExpanded : Bool
    }


type alias Model =
    { key : Nav.Key
    , questions : List Question
    , quoteId : Maybe String
    , orgId : String
    , orgName : Maybe String
    , orgLogo : Maybe String
    , isSubmitting : Bool
    , submissionError : Maybe String
    , isLoading : Bool
    }


type Msg
    = AnswerQuestion Int Bool
    | ToggleExpand Int
    | UpdateFollowUpText Int Int String
    | AnswerFollowUpQuestion Int Int Bool
    | SubmitAnswers
    | SkipQuestions
    | GotSubmitResponse (Result Http.Error ContactResponse)
    | GotTempContactResponse (Result Http.Error String)
    | GotOrgDetails (Result Http.Error OrgDetailsResponse)
    | GotExistingAnswers (Result Http.Error ExistingAnswersResponse)


type alias ContactResponse =
    { contactId : String
    , orgName : String
    , orgLogo : Maybe String
    }


type alias OrgDetailsResponse =
    { orgName : String
    , orgLogo : Maybe String
    }


type Icon
    = HeartIcon
    | LungsIcon
    | AlertCircleIcon
    | DropletsIcon
    | BrainIcon
    | Building2Icon
    | StethoscopeIcon


type alias ExistingAnswersResponse =
    { answers : List ExistingAnswer
    , orgName : String
    , orgLogo : Maybe String
    }


type alias ExistingAnswer =
    { questionId : Int
    , questionText : String
    , questionType : String
    , answer : Bool
    }



-- INIT


defaultQuestions : List Question
defaultQuestions =
    [ { id = 1
      , icon = HeartIcon
      , title = "Heart Health"
      , text = "Have you ever been diagnosed with or treated for any heart condition, including but not limited to heart failure, coronary artery disease, angina, atrial fibrillation, or heart valve problems?"
      , color = DangerColor
      , answer = Nothing
      , isExpanded = False
      , followUpQuestions =
            [ { id = 101
              , text = "What specific heart condition(s) have you been diagnosed with?"
              , answerType = TextAnswer ""
              }
            , { id = 102
              , text = "In the past 2 years, have you been hospitalized, had surgery, or any cardiac procedures (like angioplasty or stenting) for this condition?"
              , answerType = BooleanAnswer Nothing
              }
            , { id = 103
              , text = "Are you currently taking 3 or more medications for this heart condition, or using oxygen or nitroglycerin?"
              , answerType = BooleanAnswer Nothing
              }
            ]
      }
    , { id = 2
      , icon = LungsIcon
      , title = "Lung Health"
      , text = "Have you ever been diagnosed with or treated for a chronic lung condition such as COPD, emphysema, chronic bronchitis, or cystic fibrosis?"
      , color = BlueColor
      , answer = Nothing
      , isExpanded = False
      , followUpQuestions =
            [ { id = 201
              , text = "What specific lung condition(s) have you been diagnosed with?"
              , answerType = TextAnswer ""
              }
            , { id = 202
              , text = "Does this condition require you to use oxygen at home or a nebulizer?"
              , answerType = BooleanAnswer Nothing
              }
            , { id = 203
              , text = "In the past year, have you been hospitalized due to lung problems or had frequent (3 or more) respiratory infections requiring antibiotics or steroids?"
              , answerType = BooleanAnswer Nothing
              }
            ]
      }
    , { id = 3
      , icon = AlertCircleIcon
      , title = "Cancer History"
      , text = "Have you ever been diagnosed with or treated for cancer, leukemia, or lymphoma (excluding basal cell skin cancer)?"
      , color = AmberColor
      , answer = Nothing
      , isExpanded = False
      , followUpQuestions =
            [ { id = 301
              , text = "What type of cancer were you diagnosed with?"
              , answerType = TextAnswer ""
              }
            , { id = 302
              , text = "When were you initially diagnosed?"
              , answerType = TextAnswer ""
              }
            , { id = 303
              , text = "Are you currently undergoing active cancer treatment (chemotherapy, radiation, immunotherapy, targeted therapy, or hormone therapy)?"
              , answerType = BooleanAnswer Nothing
              }
            ]
      }
    , { id = 4
      , icon = DropletsIcon
      , title = "Diabetes with Complications"
      , text = "Have you ever been diagnosed with diabetes that has resulted in any complications affecting your eyes (retinopathy), nerves (neuropathy), kidneys (nephropathy), circulation (vascular disease), or heart?"
      , color = SuccessColor
      , answer = Nothing
      , isExpanded = False
      , followUpQuestions =
            [ { id = 401
              , text = "Which complications of diabetes have you experienced? (e.g., Neuropathy, Retinopathy, Kidney Problems, Vascular Disease, Heart Problems)"
              , answerType = TextAnswer ""
              }
            , { id = 402
              , text = "Do you require insulin to manage your diabetes, and if so, what is your typical daily insulin dosage?"
              , answerType = TextAnswer ""
              }
            ]
      }
    , { id = 5
      , icon = BrainIcon
      , title = "Memory or Cognitive Health"
      , text = "Do you have any concerns about your memory, thinking, or cognitive function, OR have you ever been diagnosed with dementia, Alzheimer's disease, or any other cognitive impairment?"
      , color = PurpleColor
      , answer = Nothing
      , isExpanded = False
      , followUpQuestions =
            [ { id = 501
              , text = "Have you been formally diagnosed with any cognitive impairment? If yes, what diagnosis?"
              , answerType = TextAnswer ""
              }
            , { id = 502
              , text = "Do you require assistance with activities of daily living such as remembering medications, managing finances, or personal care due to cognitive impairment?"
              , answerType = BooleanAnswer Nothing
              }
            ]
      }
    , { id = 6
      , icon = Building2Icon
      , title = "Recent and Current Hospitalizations"
      , text = "In the past 2 years, have you been hospitalized overnight in a hospital more than two times for any medical condition (excluding planned surgeries)?"
      , color = BrandColor
      , answer = Nothing
      , isExpanded = False
      , followUpQuestions = []
      }
    , { id = 7
      , icon = StethoscopeIcon
      , title = "Current Hospitalization or Medical Test"
      , text = "Are you currently hospitalized or waiting on results from a medical test, or been recommended by a doctor to have a medical test performed?"
      , color = BlueColor
      , answer = Nothing
      , isExpanded = False
      , followUpQuestions = []
      }
    ]


init : Nav.Key -> { quoteId : Maybe String, orgId : Maybe String } -> ( Model, Cmd Msg )
init key { quoteId, orgId } =
    let
        extractedOrgId =
            case ( orgId, quoteId ) of
                ( Nothing, Just id ) ->
                    id
                        |> String.split "-"
                        |> List.head
                        |> Maybe.withDefault ""

                ( Just orgIdValue, _ ) ->
                    orgIdValue

                ( Nothing, Nothing ) ->
                    ""

        initialModel =
            { key = key
            , questions = defaultQuestions
            , quoteId = quoteId
            , orgId = extractedOrgId
            , orgName = Nothing
            , orgLogo = Nothing
            , isSubmitting = False
            , submissionError = Nothing
            , isLoading = True
            }

        loadOrgDetails =
            if String.isEmpty extractedOrgId then
                Cmd.none

            else
                Http.get
                    { url = "/api/org/" ++ extractedOrgId ++ "/details"
                    , expect = Http.expectJson GotOrgDetails orgDetailsResponseDecoder
                    }

        loadExistingAnswers =
            case quoteId of
                Just id ->
                    let
                        contactId =
                            String.split "-" id
                                |> List.drop 1
                                |> List.head
                                |> Maybe.withDefault ""
                    in
                    if not (String.isEmpty contactId) then
                        Http.get
                            { url = "/api/org/" ++ extractedOrgId ++ "/eligibility-answers/" ++ contactId
                            , expect = Http.expectJson GotExistingAnswers existingAnswersResponseDecoder
                            }

                    else
                        Cmd.none

                Nothing ->
                    Cmd.none
    in
    ( initialModel
    , Cmd.batch [ loadOrgDetails, loadExistingAnswers ]
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
                                { q
                                    | answer = Just answer
                                    , isExpanded = answer -- Auto-expand when answered Yes
                                }

                            else
                                q
                        )
                        model.questions
              }
            , Cmd.none
            )

        ToggleExpand id ->
            ( { model
                | questions =
                    List.map
                        (\q ->
                            if q.id == id then
                                { q | isExpanded = not q.isExpanded }

                            else
                                q
                        )
                        model.questions
              }
            , Cmd.none
            )

        UpdateFollowUpText questionId answerId text ->
            ( { model
                | questions =
                    List.map
                        (\q ->
                            if q.id == questionId then
                                { q
                                    | followUpQuestions =
                                        List.map
                                            (\f ->
                                                if f.id == answerId then
                                                    { f | answerType = TextAnswer text }

                                                else
                                                    f
                                            )
                                            q.followUpQuestions
                                }

                            else
                                q
                        )
                        model.questions
              }
            , Cmd.none
            )

        AnswerFollowUpQuestion questionId answerId answer ->
            ( { model
                | questions =
                    List.map
                        (\q ->
                            if q.id == questionId then
                                { q
                                    | followUpQuestions =
                                        List.map
                                            (\f ->
                                                if f.id == answerId then
                                                    { f | answerType = BooleanAnswer (Just answer) }

                                                else
                                                    f
                                            )
                                            q.followUpQuestions
                                }

                            else
                                q
                        )
                        model.questions
              }
            , Cmd.none
            )

        GotSubmitResponse result ->
            case result of
                Ok response ->
                    let
                        anyYes =
                            List.any (\q -> q.answer == Just True) model.questions

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
                                                [ Builder.string "contactId" response.contactId ]
                                       )
                                )
                    in
                    ( { model
                        | isSubmitting = False
                        , orgName = Just response.orgName
                        , orgLogo = response.orgLogo
                      }
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
                        , expect = Http.expectJson GotSubmitResponse contactResponseDecoder
                        }
                    )

                Err error ->
                    ( { model
                        | isSubmitting = False
                        , submissionError = Just "Failed to create temporary contact. Please try again."
                      }
                    , Cmd.none
                    )

        GotOrgDetails result ->
            case result of
                Ok response ->
                    ( { model
                        | orgName = Just response.orgName
                        , orgLogo = response.orgLogo
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | isLoading = False }
                    , Cmd.none
                    )

        GotExistingAnswers result ->
            case result of
                Ok response ->
                    let
                        updatedQuestions =
                            List.map
                                (\q ->
                                    case List.filter (\a -> a.questionId == q.id) response.answers of
                                        existingAnswer :: _ ->
                                            { q
                                                | answer = Just existingAnswer.answer
                                                , isExpanded = existingAnswer.answer -- Auto-expand if answered Yes
                                            }

                                        [] ->
                                            q
                                )
                                model.questions
                    in
                    ( { model
                        | questions = updatedQuestions
                        , orgName = Just response.orgName
                        , orgLogo = response.orgLogo
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | isLoading = False }
                    , Cmd.none
                    )

        SubmitAnswers ->
            let
                relevantQuestions =
                    getRelevantQuestions model

                allRelevantQuestionsAnswered =
                    List.all
                        (\q ->
                            case q.followUpQuestions of
                                [] ->
                                    q.answer /= Nothing

                                followUps ->
                                    List.all
                                        (\fq ->
                                            case fq.answerType of
                                                TextAnswer _ ->
                                                    True

                                                BooleanAnswer _ ->
                                                    q.answer /= Nothing
                                        )
                                        followUps
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
                        , submitAnswers model
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
        answeredQuestions =
            List.filter (\q -> q.answer /= Nothing) model.questions
    in
    answeredQuestions


encodeAnswers : List Question -> E.Value
encodeAnswers questions =
    E.object
        (List.filterMap
            (\q ->
                if q.answer == Nothing then
                    Nothing

                else
                    let
                        questionType =
                            case q.color of
                                DangerColor ->
                                    "main"

                                BlueColor ->
                                    "lung_health"

                                AmberColor ->
                                    "cancer_history"

                                SuccessColor ->
                                    "diabetes_with_complications"

                                PurpleColor ->
                                    "memory_or_cognitive_health"

                                BrandColor ->
                                    "recent_and_current_hospitalizations"

                        answerValue =
                            case q.answer of
                                Just ans ->
                                    E.bool ans

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


iconToSvg : Icon -> Color -> Html msg
iconToSvg icon color =
    let
        iconColor =
            case color of
                DangerColor ->
                    "text-[#DC2626]"

                BlueColor ->
                    "text-[#0075F2]"

                AmberColor ->
                    "text-amber-500"

                SuccessColor ->
                    "text-medicare-success"

                PurpleColor ->
                    "text-[#7F56D9]"

                BrandColor ->
                    "text-[#03045E]"
    in
    case icon of
        HeartIcon ->
            div [ class iconColor ] [ MyIcon.heartPulse 24 "currentColor" ]

        LungsIcon ->
            div [ class iconColor ] [ MyIcon.lungs 24 "currentColor" ]

        AlertCircleIcon ->
            div [ class iconColor ] [ MyIcon.activity 24 "currentColor" ]

        DropletsIcon ->
            div [ class iconColor ] [ MyIcon.droplets 24 "currentColor" ]

        BrainIcon ->
            div [ class iconColor ] [ MyIcon.brain 24 "currentColor" ]

        Building2Icon ->
            div [ class iconColor ] [ MyIcon.building2 24 "currentColor" ]

        StethoscopeIcon ->
            div [ class iconColor ] [ MyIcon.stethoscope 24 "currentColor" ]


colorToClasses : Color -> Bool -> { card : String, title : String, indicator : String }
colorToClasses color isActive =
    case color of
        DangerColor ->
            { card =
                if isActive then
                    "bg-[#DC2626]/10"

                else
                    "bg-white"
            , title = "text-[#DC2626]"
            , indicator = "bg-[#DC2626]/10"
            }

        BlueColor ->
            { card =
                if isActive then
                    "bg-[#0075F2]/20"

                else
                    "bg-white"
            , title = "text-[#0075F2]"
            , indicator = "bg-[#0075F2]/10"
            }

        AmberColor ->
            { card =
                if isActive then
                    "bg-amber-50"

                else
                    "bg-white"
            , title = "text-amber-500"
            , indicator = "bg-amber-50"
            }

        SuccessColor ->
            { card =
                if isActive then
                    "bg-medicare-success-light"

                else
                    "bg-white"
            , title = "text-medicare-success"
            , indicator = "bg-medicare-success-light"
            }

        PurpleColor ->
            { card =
                if isActive then
                    "bg-[#7F56D9]/20"

                else
                    "bg-white"
            , title = "text-[#7F56D9]"
            , indicator = "bg-[#7F56D9]/10"
            }

        BrandColor ->
            { card =
                if isActive then
                    "bg-[#03045E]/20"

                else
                    "bg-white"
            , title = "text-[#03045E]"
            , indicator = "bg-[#03045E]/10"
            }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Underwriting Assessment"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ if model.isLoading then
                div [ class "fixed inset-0 bg-white flex flex-col items-center justify-center gap-4 text-center" ]
                    [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-[#03045E] border-t-transparent" ] []
                    , p [ class "text-center text-lg font-medium text-gray-600" ]
                        [ text "Loading assessment..." ]
                    ]

              else
                div [ class "max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-6 sm:py-8" ]
                    [ -- Organization Logo/Name
                      viewHeader model.orgLogo model.orgName
                    , h1 [ class "text-2xl sm:text-3xl font-bold text-center text-gray-900 mb-2 sm:mb-3" ]
                        [ text "Underwriting Assessment" ]
                    , p [ class "text-gray-600 px-5 md:px-0 text-center mb-6 sm:mb-8 text-sm sm:text-base max-w-xl mx-auto" ]
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
    List.map
        (\q ->
            div [ class "mb-6 transition-all duration-200" ]
                [ viewQuestion q ]
        )
        model.questions


viewQuestion : Question -> Html Msg
viewQuestion question =
    let
        colorClasses =
            colorToClasses question.color (question.answer == Just True)

        shouldShowFollowUps =
            question.answer == Just True && question.isExpanded

        hasFollowUps =
            not (List.isEmpty question.followUpQuestions)

        bgColorClass =
            if question.answer == Just True then
                case question.color of
                    DangerColor ->
                        "bg-[#DC2626]/10"

                    BlueColor ->
                        "bg-[#0075F2]/10"

                    AmberColor ->
                        "bg-amber-50"

                    SuccessColor ->
                        "bg-medicare-success-light"

                    PurpleColor ->
                        "bg-[#7F56D9]/10"

                    BrandColor ->
                        "bg-[#03045E]/10"

            else
                "bg-white"
    in
    div
        [ class <|
            "rounded-lg shadow-card overflow-hidden transition-all duration-300 border border-[#DCE2E5] "
                ++ bgColorClass
        ]
        [ div [ class "p-3 sm:p-5" ]
            [ div [ class "flex items-start gap-2 sm:gap-4" ]
                [ div [ class "flex-shrink-0 mt-0.5 sm:mt-1" ]
                    [ iconToSvg question.icon question.color ]
                , div [ class "flex-1 min-w-0" ]
                    [ div [ class "flex justify-between items-start" ]
                        [ h3 [ class <| "font-semibold text-base sm:text-lg mb-1.5 sm:mb-2 " ++ colorClasses.title ]
                            [ text question.title ]
                        ]
                    , p [ class "text-gray-700 text-sm sm:text-base mb-3 sm:mb-4" ]
                        [ text question.text ]
                    , div [ class "flex gap-2 sm:gap-3" ]
                        [ viewAnswerButton question True
                        , viewAnswerButton question False
                        ]
                    ]
                ]
            ]
        , if shouldShowFollowUps then
            div [ class "mt-4 space-y-4 border-t border-[#DCE2E5] pt-4 pb-4 sm:pb-5" ]
                (List.map (\f -> viewFollowUpQuestion f question.id) question.followUpQuestions)

          else
            text ""
        ]


viewFollowUpQuestion : FollowUpQuestion -> Int -> Html Msg
viewFollowUpQuestion followUp parentId =
    div [ class "px-3 sm:px-5" ]
        [ p [ class "text-gray-700 text-sm sm:text-base mb-2" ]
            [ text followUp.text ]
        , case followUp.answerType of
            TextAnswer textValue ->
                div [ class "mt-2" ]
                    [ textarea
                        [ class "w-full p-2.5 sm:p-3 text-sm sm:text-base bg-white border border-[#DCE2E5] rounded-lg focus:ring-2 focus:ring-[#03045E] focus:border-[#03045E]"
                        , rows 2
                        , placeholder "Please provide details..."
                        , value textValue
                        , onInput (\text -> UpdateFollowUpText parentId followUp.id text)
                        ]
                        []
                    ]

            BooleanAnswer answer ->
                div [ class "flex gap-2 sm:gap-3" ]
                    [ button
                        [ type_ "button"
                        , onClick (AnswerFollowUpQuestion parentId followUp.id True)
                        , class <|
                            "flex-1 py-2 sm:py-2.5 px-3 sm:px-4 text-sm sm:text-base rounded-lg font-medium transition-colors border "
                                ++ (if answer == Just True then
                                        "bg-[#03045E] text-white border-[#03045E]"

                                    else
                                        "bg-white text-gray-700 border-[#DCE2E5] hover:bg-gray-50"
                                   )
                        ]
                        [ text "Yes" ]
                    , button
                        [ type_ "button"
                        , onClick (AnswerFollowUpQuestion parentId followUp.id False)
                        , class <|
                            "flex-1 py-2 sm:py-2.5 px-3 sm:px-4 text-sm sm:text-base rounded-lg font-medium transition-colors border "
                                ++ (if answer == Just False then
                                        "bg-gray-900 text-white border-gray-900"

                                    else
                                        "bg-white text-gray-700 border-[#DCE2E5] hover:bg-gray-50"
                                   )
                        ]
                        [ text "No" ]
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


viewAnswerButton : Question -> Bool -> Html Msg
viewAnswerButton question isYes =
    button
        [ type_ "button"
        , onClick (AnswerQuestion question.id isYes)
        , class <|
            "flex-1 py-2 sm:py-2.5 px-3 sm:px-4 text-sm sm:text-base rounded-lg font-medium transition-colors border "
                ++ (if question.answer == Just isYes then
                        if isYes then
                            "bg-[#03045E] text-white border-[#03045E]"

                        else
                            "bg-gray-900 text-white border-gray-900"

                    else
                        "bg-white text-gray-700 border-gray-300 hover:bg-gray-50"
                   )
        ]
        [ text
            (if isYes then
                "Yes"

             else
                "No"
            )
        ]


viewSubmitButton : Model -> Html Msg
viewSubmitButton model =
    let
        -- Check if all main questions are answered
        allMainQuestionsAnswered =
            List.all (\q -> q.answer /= Nothing) model.questions

        -- For any "Yes" answers with follow-up questions, check if all boolean follow-ups are answered
        allRequiredFollowUpsAnswered =
            List.all
                (\q ->
                    case q.answer of
                        Just True ->
                            -- If answered Yes, check follow-up questions
                            List.all
                                (\fq ->
                                    case fq.answerType of
                                        TextAnswer _ ->
                                            -- Text answers are optional
                                            True

                                        BooleanAnswer answer ->
                                            -- Boolean answers must be answered
                                            answer /= Nothing
                                )
                                q.followUpQuestions

                        _ ->
                            -- If not answered Yes, follow-ups don't matter
                            True
                )
                model.questions

        canSubmit =
            allMainQuestionsAnswered && allRequiredFollowUpsAnswered && not model.isSubmitting

        buttonClass =
            "w-full py-2.5 sm:py-3 rounded-lg text-white font-medium transition-colors duration-200 mt-4 sm:mt-6 text-sm sm:text-base shadow-sm "
                ++ (if canSubmit then
                        "bg-[#03045E] hover:bg-[#03045E]/90"

                    else
                        "bg-[#03045E]/70 cursor-not-allowed"
                   )
    in
    button
        [ class buttonClass
        , type_ "submit"
        , disabled (not canSubmit)
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



-- Add this near other decoders


contactResponseDecoder : D.Decoder ContactResponse
contactResponseDecoder =
    D.map3 ContactResponse
        (D.field "contactId" D.string)
        (D.field "orgName" D.string)
        (D.field "orgLogo" (D.nullable D.string))


orgDetailsResponseDecoder : D.Decoder OrgDetailsResponse
orgDetailsResponseDecoder =
    D.map2 OrgDetailsResponse
        (D.field "name" D.string)
        (D.field "logo_data" (D.nullable D.string))


submitAnswers : Model -> Cmd Msg
submitAnswers model =
    Http.post
        { url = "/api/org/" ++ model.orgId ++ "/eligibility-answers"
        , body =
            Http.jsonBody <|
                E.object
                    [ ( "quote_id", E.string (Maybe.withDefault "" model.quoteId) )
                    , ( "answers", encodeAnswers (getRelevantQuestions model) )
                    ]
        , expect = Http.expectJson GotSubmitResponse contactResponseDecoder
        }


existingAnswersResponseDecoder : D.Decoder ExistingAnswersResponse
existingAnswersResponseDecoder =
    D.map3 ExistingAnswersResponse
        (D.field "answers" (D.dict existingAnswerDecoder |> D.map dictToAnswerList))
        (D.field "orgName" D.string)
        (D.field "orgLogo" (D.nullable D.string))


existingAnswerDecoder : D.Decoder ExistingAnswer
existingAnswerDecoder =
    D.map4 ExistingAnswer
        (D.succeed 0)
        -- Will be replaced with the key from the dict
        (D.field "question_text" D.string)
        (D.field "question_type" D.string)
        (D.field "answer" D.bool)


dictToAnswerList : Dict String ExistingAnswer -> List ExistingAnswer
dictToAnswerList dict =
    Dict.toList dict
        |> List.map
            (\( key, answer ) ->
                { answer | questionId = String.toInt key |> Maybe.withDefault 0 }
            )
