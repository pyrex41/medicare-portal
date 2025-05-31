module StageDemoHealth exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type alias Model =
    { tempId : String
    , questions : List Question
    }

type alias Question =
    { id : Int
    , title : String
    , text : String
    , answer : Maybe Bool
    }

type Msg
    = AnswerQuestion Int Bool
    | NavigateToCTA

init : String -> Model
init tempId =
    { tempId = tempId
    , questions = 
        [ { id = 1
          , title = "Heart Problems"
          , text = "Have you been diagnosed with any heart conditions?"
          , answer = Nothing
          }
        , { id = 2
          , title = "Lung Problems"
          , text = "Have you been diagnosed with any lung conditions?"
          , answer = Nothing
          }
        , { id = 3
          , title = "Kidney Disease"
          , text = "Have you been diagnosed with kidney disease?"
          , answer = Nothing
          }
        , { id = 4
          , title = "Diabetes"
          , text = "Have you been diagnosed with diabetes?"
          , answer = Nothing
          }
        ]
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnswerQuestion questionId answer ->
            let
                updateQuestion q =
                    if q.id == questionId then
                        { q | answer = Just answer }
                    else
                        q
            in
            ( { model | questions = List.map updateQuestion model.questions }, Cmd.none )

        NavigateToCTA ->
            ( model, Cmd.none ) -- Navigation handled by Main.elm

view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-white" ]
        [ -- Header
          div [ class "bg-white shadow-sm border-b" ]
            [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6" ]
                [ -- Logo
                  div [ class "flex items-center justify-center mb-4" ]
                    [ img 
                        [ src "/images/medicare-max-logo.png"
                        , alt "Medicare Max"
                        , class "h-10"
                        ]
                        []
                    ]
                , div [ class "text-center" ]
                    [ h1 [ class "text-2xl sm:text-3xl font-bold text-[#141B29]" ] [ text "Health Qualification Questions" ]
                    , p [ class "text-[#475467] mt-2" ] [ text "Please answer these questions to see if you qualify for coverage" ]
                    ]
                ]
            ]
        
        -- Questions
        , div [ class "max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
            [ div [ class "space-y-6" ]
                (List.map viewQuestion model.questions)
            
            -- Navigation buttons
            , div [ class "mt-8 flex justify-between" ]
                [ button
                    [ class "px-6 py-3 text-[#03045E] bg-white border-2 border-[#03045E] rounded-md hover:bg-gray-50 font-semibold transition-colors duration-200"
                    , onClick NavigateToCTA
                    ]
                    [ text "Skip Questions" ]
                , button
                    [ class "px-6 py-3 bg-[#03045E] text-white rounded-md hover:bg-[#1a1f5f] font-semibold transition-colors duration-200"
                    , onClick NavigateToCTA
                    , disabled (not (allQuestionsAnswered model.questions))
                    ]
                    [ text "Next" ]
                ]
            ]
        ]

viewQuestion : Question -> Html Msg
viewQuestion question =
    div [ class "bg-white rounded-lg shadow-lg border border-gray-100 p-6" ]
        [ h3 [ class "text-lg font-semibold text-[#141B29] mb-2" ] [ text question.title ]
        , p [ class "text-[#475467] mb-4" ] [ text question.text ]
        , div [ class "flex space-x-4" ]
            [ button
                [ class (answerButtonClass question.answer True)
                , onClick (AnswerQuestion question.id True)
                ]
                [ text "Yes" ]
            , button
                [ class (answerButtonClass question.answer False)
                , onClick (AnswerQuestion question.id False)
                ]
                [ text "No" ]
            ]
        ]

answerButtonClass : Maybe Bool -> Bool -> String
answerButtonClass currentAnswer buttonValue =
    let
        baseClass = "px-6 py-2 rounded-md font-medium transition-colors "
        selectedClass = 
            case currentAnswer of
                Just answer ->
                    if answer == buttonValue then
                        if buttonValue then
                            "bg-red-600 text-white hover:bg-red-700"
                        else
                            "bg-green-600 text-white hover:bg-green-700"
                    else
                        "bg-gray-200 text-gray-700 hover:bg-gray-300"
                
                Nothing ->
                    "bg-gray-200 text-gray-700 hover:bg-gray-300"
    in
    baseClass ++ selectedClass

allQuestionsAnswered : List Question -> Bool
allQuestionsAnswered questions =
    List.all (\q -> q.answer /= Nothing) questions