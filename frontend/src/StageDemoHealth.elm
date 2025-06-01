module StageDemoHealth exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MyIcon
import Svg
import Svg.Attributes


-- MODEL


type alias Model =
    { tempId : String
    , questions : List Question
    , key : Nav.Key
    }


type alias Question =
    { id : Int
    , title : String
    , text : String
    , answer : Maybe Bool
    , icon : Icon
    , color : Color
    , showFollowUp : Bool
    , followUpQuestion : Maybe String
    , followUpAnswer : Maybe Bool
    }


type Icon
    = HeartIcon
    | LungsIcon
    | AlertCircleIcon
    | DropletsIcon
    | BrainIcon
    | Building2Icon
    | StethoscopeIcon


type Color
    = DangerColor
    | BlueColor
    | AmberColor
    | SuccessColor
    | PurpleColor
    | BrandColor


type Msg
    = AnswerQuestion Int Bool
    | AnswerFollowUp Int Bool
    | NavigateToCTA
    | SkipQuestions


init : String -> Nav.Key -> Model
init tempId key =
    { tempId = tempId
    , key = key
    , questions = 
        [ { id = 1
          , title = "Heart Health"
          , text = "Are you currently experiencing any of the following symptoms: chest pain, shortness of breath, irregular heartbeat, or have you been diagnosed with heart disease, heart failure, or had a heart attack?"
          , answer = Nothing
          , icon = HeartIcon
          , color = DangerColor
          , showFollowUp = False
          , followUpQuestion = Just "Are you currently under treatment or has your doctor said your condition is stable?"
          , followUpAnswer = Nothing
          }
        , { id = 2
          , title = "Lung Health"
          , text = "Do you have any lung conditions such as COPD, emphysema, chronic bronchitis, or require oxygen therapy?"
          , answer = Nothing
          , icon = LungsIcon
          , color = BlueColor
          , showFollowUp = False
          , followUpQuestion = Just "Is your condition well-controlled with medication?"
          , followUpAnswer = Nothing
          }
        , { id = 3
          , title = "Cancer"
          , text = "Have you been diagnosed with or are currently receiving treatment for any type of cancer?"
          , answer = Nothing
          , icon = AlertCircleIcon
          , color = AmberColor
          , showFollowUp = False
          , followUpQuestion = Just "Has it been more than 5 years since your last treatment?"
          , followUpAnswer = Nothing
          }
        , { id = 4
          , title = "Diabetes"
          , text = "Do you have diabetes that requires insulin or has resulted in complications such as neuropathy, retinopathy, or kidney disease?"
          , answer = Nothing
          , icon = DropletsIcon
          , color = SuccessColor
          , showFollowUp = False
          , followUpQuestion = Just "Is your diabetes well-controlled with your current treatment plan?"
          , followUpAnswer = Nothing
          }
        , { id = 5
          , title = "Cognitive Health"
          , text = "Have you been diagnosed with Alzheimer's disease, dementia, or any other cognitive disorder?"
          , answer = Nothing
          , icon = BrainIcon
          , color = PurpleColor
          , showFollowUp = False
          , followUpQuestion = Nothing
          , followUpAnswer = Nothing
          }
        , { id = 6
          , title = "Recent Hospitalizations"
          , text = "Have you been hospitalized or had emergency room visits in the past 12 months?"
          , answer = Nothing
          , icon = Building2Icon
          , color = BrandColor
          , showFollowUp = False
          , followUpQuestion = Just "Were these visits related to a chronic condition?"
          , followUpAnswer = Nothing
          }
        , { id = 7
          , title = "Current Medical Care"
          , text = "Are you currently receiving dialysis, chemotherapy, radiation therapy, or any other ongoing medical treatment?"
          , answer = Nothing
          , icon = StethoscopeIcon
          , color = DangerColor
          , showFollowUp = False
          , followUpQuestion = Nothing
          , followUpAnswer = Nothing
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
                        { q | answer = Just answer, showFollowUp = answer && q.followUpQuestion /= Nothing }
                    else
                        q
            in
            ( { model | questions = List.map updateQuestion model.questions }, Cmd.none )

        AnswerFollowUp questionId answer ->
            let
                updateQuestion q =
                    if q.id == questionId then
                        { q | followUpAnswer = Just answer }
                    else
                        q
            in
            ( { model | questions = List.map updateQuestion model.questions }, Cmd.none )

        NavigateToCTA ->
            ( model, Nav.pushUrl model.key ("/stage-demo/cta/" ++ model.tempId) )

        SkipQuestions ->
            ( model, Nav.pushUrl model.key ("/stage-demo/cta/" ++ model.tempId) )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-[#F6F8F9]" ]
        [ -- Main Content
          div [ class "max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-8 sm:py-12" ]
            [ -- Header
              div [ class "text-center mb-8 sm:mb-10" ]
                [ viewHeader
                , h1 [ class "text-2xl sm:text-3xl font-bold text-[#101828] mt-6" ] [ text "Health Qualification Questions" ]
                , p [ class "text-[#667085] mt-3 text-sm sm:text-base max-w-2xl mx-auto" ] 
                    [ text "Please answer these questions honestly to determine your Medicare Supplement eligibility. Your responses help us find the best plan for your needs." ]
                ]
            
            -- Questions
            , div [ class "space-y-4 sm:space-y-6 mb-8" ]
                (List.map viewQuestion model.questions)
            
            -- Navigation buttons
            , div [ class "flex flex-col sm:flex-row gap-4 mb-8" ]
                [ button
                    [ class "flex-1 px-6 py-3 text-[#03045E] bg-white border-2 border-[#03045E] rounded-lg hover:bg-gray-50 font-semibold transition-colors duration-200"
                    , onClick SkipQuestions
                    ]
                    [ text "Skip Questions" ]
                , button
                    [ class ("flex-1 px-6 py-3 rounded-lg text-white font-semibold transition-colors duration-200 " ++
                        if allQuestionsAnswered model.questions then
                            "bg-[#03045E] hover:bg-[#03045E]/90"
                        else
                            "bg-[#03045E]/70 cursor-not-allowed")
                    , onClick NavigateToCTA
                    , disabled (not (allQuestionsAnswered model.questions))
                    ]
                    [ text "Submit & Continue" ]
                ]
            
            -- Medicare Advantage Section
            , viewMedicareAdvantageSection
            ]
        ]


viewHeader : Html msg
viewHeader =
    div [ class "flex items-center justify-center" ]
        [ img 
            [ src "/images/medicare-max-logo.png"
            , alt "Medicare Max"
            , class "h-16 max-w-[240px] md:max-w-[300px] object-contain px-4"
            ]
            []
        ]


viewQuestion : Question -> Html Msg
viewQuestion question =
    let
        colors = colorToClasses question.color (question.answer == Just True)
        isAnswered = question.answer /= Nothing
    in
    div 
        [ class ("rounded-lg shadow-card overflow-hidden transition-all duration-300 border border-[#DCE2E5] " ++ 
            if isAnswered then colors.card else "bg-white")
        ]
        [ -- Main Question
          div [ class "p-3 sm:p-5" ]
            [ div [ class "flex items-start gap-2 sm:gap-4" ]
                [ -- Icon
                  div [ class "flex-shrink-0 mt-0.5 sm:mt-1" ]
                    [ viewIcon question.icon question.color ]
                , -- Content
                  div [ class "flex-1" ]
                    [ h3 [ class ("font-semibold text-base sm:text-lg mb-1.5 sm:mb-2 " ++ colors.title) ] 
                        [ text question.title ]
                    , p [ class "text-gray-700 text-sm sm:text-base mb-3 sm:mb-4" ] 
                        [ text question.text ]
                    , -- Answer buttons
                      div [ class "flex gap-2 sm:gap-3" ]
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
                ]
            ]
        
        -- Follow-up Question (if applicable)
        , if question.showFollowUp && question.followUpQuestion /= Nothing then
            case question.followUpQuestion of
                Just followUp ->
                    div [ class "border-t border-gray-200 bg-gray-50 p-3 sm:p-5" ]
                        [ p [ class "text-sm sm:text-base text-gray-700 mb-3" ] 
                            [ text followUp ]
                        , div [ class "flex gap-2 sm:gap-3" ]
                            [ button
                                [ class (followUpButtonClass question.followUpAnswer True)
                                , onClick (AnswerFollowUp question.id True)
                                ]
                                [ text "Yes" ]
                            , button
                                [ class (followUpButtonClass question.followUpAnswer False)
                                , onClick (AnswerFollowUp question.id False)
                                ]
                                [ text "No" ]
                            ]
                        ]
                
                Nothing ->
                    text ""
          else
            text ""
        ]


viewIcon : Icon -> Color -> Html msg
viewIcon icon color =
    let
        iconColor = colorToIconClass color
        iconSize = 24  -- Size in pixels
    in
    div [ class iconColor ]
        [ case icon of
            HeartIcon ->
                MyIcon.heartPulse iconSize "currentColor"
            
            LungsIcon ->
                MyIcon.lungs iconSize "currentColor"
            
            AlertCircleIcon ->
                MyIcon.activity iconSize "currentColor"
            
            DropletsIcon ->
                MyIcon.droplets iconSize "currentColor"
            
            BrainIcon ->
                MyIcon.brain iconSize "currentColor"
            
            Building2Icon ->
                MyIcon.building2 iconSize "currentColor"
            
            StethoscopeIcon ->
                MyIcon.stethoscope iconSize "currentColor"
        ]


viewMedicareAdvantageSection : Html Msg
viewMedicareAdvantageSection =
    div [ class "mt-8 bg-white rounded-[10px] border border-[#DCE2E5] shadow-[0_1px_2px_rgba(16,24,40,0.05)]" ]
        [ div [ class "p-4 sm:p-6" ]
            [ div [ class "flex flex-col sm:flex-row items-start sm:items-center gap-4" ]
                [ -- Icon
                  div [ class "w-12 h-12 rounded-full bg-[#F9F5FF] flex items-center justify-center flex-shrink-0" ]
                    [ MyIcon.shieldCheck 24 "#7F56D9" ]
                , -- Content
                  div [ class "flex-1" ]
                    [ h3 [ class "text-lg sm:text-xl font-bold text-[#101828] -tracking-[0.02em] mb-1" ] 
                        [ text "Medicare Advantage Plans Available" ]
                    , p [ class "text-sm sm:text-base text-[#667085]" ] 
                        [ text "Based on your health profile, you may also qualify for Medicare Advantage plans with additional benefits." ]
                    ]
                , -- CTA Button
                  button
                    [ class "bg-[#03045E] text-white px-5 sm:px-4 py-3 sm:py-2 rounded-lg hover:bg-[#02034D] transition-colors text-sm sm:text-base font-medium whitespace-nowrap"
                    ]
                    [ text "Learn More" ]
                ]
            ]
        ]



-- HELPERS


answerButtonClass : Maybe Bool -> Bool -> String
answerButtonClass currentAnswer buttonValue =
    let
        baseClass = "flex-1 py-2 sm:py-2.5 px-3 sm:px-4 text-sm sm:text-base rounded-lg font-medium transition-colors border "
    in
    case currentAnswer of
        Just answer ->
            if answer == buttonValue then
                if buttonValue then
                    baseClass ++ "bg-[#03045E] text-white border-[#03045E]"
                else
                    baseClass ++ "bg-gray-900 text-white border-gray-900"
            else
                baseClass ++ "bg-white text-gray-700 border-gray-300 hover:bg-gray-50"
        
        Nothing ->
            baseClass ++ "bg-white text-gray-700 border-gray-300 hover:bg-gray-50"


followUpButtonClass : Maybe Bool -> Bool -> String
followUpButtonClass currentAnswer buttonValue =
    let
        baseClass = "px-4 py-2 text-sm rounded-lg font-medium transition-colors border "
    in
    case currentAnswer of
        Just answer ->
            if answer == buttonValue then
                baseClass ++ "bg-gray-700 text-white border-gray-700"
            else
                baseClass ++ "bg-white text-gray-700 border-gray-300 hover:bg-gray-50"
        
        Nothing ->
            baseClass ++ "bg-white text-gray-700 border-gray-300 hover:bg-gray-50"


colorToClasses : Color -> Bool -> { card : String, title : String, indicator : String }
colorToClasses color isActive =
    case color of
        DangerColor ->
            if isActive then
                { card = "bg-[#DC2626]/10"
                , title = "text-[#DC2626]"
                , indicator = "bg-[#DC2626]"
                }
            else
                { card = ""
                , title = "text-[#101828]"
                , indicator = "bg-gray-300"
                }
        
        BlueColor ->
            if isActive then
                { card = "bg-[#0075F2]/10"
                , title = "text-[#0075F2]"
                , indicator = "bg-[#0075F2]"
                }
            else
                { card = ""
                , title = "text-[#101828]"
                , indicator = "bg-gray-300"
                }
        
        AmberColor ->
            if isActive then
                { card = "bg-amber-50"
                , title = "text-amber-700"
                , indicator = "bg-amber-500"
                }
            else
                { card = ""
                , title = "text-[#101828]"
                , indicator = "bg-gray-300"
                }
        
        SuccessColor ->
            if isActive then
                { card = "bg-green-50"
                , title = "text-green-700"
                , indicator = "bg-green-500"
                }
            else
                { card = ""
                , title = "text-[#101828]"
                , indicator = "bg-gray-300"
                }
        
        PurpleColor ->
            if isActive then
                { card = "bg-[#7F56D9]/10"
                , title = "text-[#7F56D9]"
                , indicator = "bg-[#7F56D9]"
                }
            else
                { card = ""
                , title = "text-[#101828]"
                , indicator = "bg-gray-300"
                }
        
        BrandColor ->
            if isActive then
                { card = "bg-[#03045E]/10"
                , title = "text-[#03045E]"
                , indicator = "bg-[#03045E]"
                }
            else
                { card = ""
                , title = "text-[#101828]"
                , indicator = "bg-gray-300"
                }


colorToIconClass : Color -> String
colorToIconClass color =
    case color of
        DangerColor ->
            "text-[#DC2626]"
        
        BlueColor ->
            "text-[#0075F2]"
        
        AmberColor ->
            "text-amber-600"
        
        SuccessColor ->
            "text-green-600"
        
        PurpleColor ->
            "text-[#7F56D9]"
        
        BrandColor ->
            "text-[#03045E]"


allQuestionsAnswered : List Question -> Bool
allQuestionsAnswered questions =
    List.all (\q -> q.answer /= Nothing) questions