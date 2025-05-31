module StageDemoCTA exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (svg, path)
import Svg.Attributes exposing (d, fill, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)

type alias Model =
    { tempId : String }

type Msg
    = NoOp

init : String -> Model
init tempId =
    { tempId = tempId }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-[#F9F5FF] flex items-center justify-center px-4" ]
        [ div [ class "max-w-2xl mx-auto text-center" ]
            [ -- Success Icon
              div [ class "mx-auto flex items-center justify-center h-24 w-24 rounded-full bg-green-100 mb-8" ]
                [ svg [ class "h-12 w-12 text-green-600", fill "none", stroke "currentColor", viewBox "0 0 24 24" ]
                    [ path [ strokeLinecap "round", strokeLinejoin "round", strokeWidth "2", d "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" ] []
                    ]
                ]
            
            -- Logo
            , div [ class "mb-8" ]
                [ img 
                    [ src "/images/medicare-max-logo.png"
                    , alt "Medicare Max"
                    , class "h-12 mx-auto"
                    ]
                    []
                ]
            
            -- Heading
            , h1 [ class "text-3xl sm:text-4xl font-bold text-[#141B29] mb-4" ] 
                [ text "Congratulations! You May Qualify!" ]
            
            -- Subheading
            , p [ class "text-xl text-[#475467] mb-8" ] 
                [ text "Based on your answers, you appear to be eligible for the Medicare plans shown." ]
            
            -- Benefits list
            , div [ class "bg-white rounded-lg shadow-lg p-8 mb-8 border border-gray-100" ]
                [ h2 [ class "text-2xl font-semibold text-[#141B29] mb-6" ] 
                    [ text "What's Next?" ]
                , ul [ class "space-y-4 text-left" ]
                    [ benefitItem "Review your personalized plan options with an expert"
                    , benefitItem "Get help comparing coverage and costs"
                    , benefitItem "Learn about available discounts and savings"
                    , benefitItem "Complete your enrollment with professional guidance"
                    ]
                ]
            
            -- CTA Button
            , a 
                [ href "https://calendly.com/your-signup-link"  -- Replace with actual Calendly URL
                , target "_blank"
                , class "inline-flex items-center px-8 py-4 border border-transparent text-lg font-semibold rounded-md shadow-lg text-white bg-[#03045E] hover:bg-[#1a1f5f] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#03045E] transition-all transform hover:scale-105"
                ]
                [ text "Schedule Your Medicare Max Demo"
                , svg [ class "ml-3 -mr-1 h-5 w-5", fill "none", stroke "currentColor", viewBox "0 0 24 24" ]
                    [ path [ strokeLinecap "round", strokeLinejoin "round", strokeWidth "2", d "M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14" ] []
                    ]
                ]
            
            -- Urgency text
            , p [ class "mt-6 text-sm text-gray-600" ]
                [ span [ class "font-semibold" ] [ text "Limited time offer: " ]
                , text "Schedule now to lock in your discounted rates!"
                ]
            ]
        ]

benefitItem : String -> Html msg
benefitItem text_ =
    li [ class "flex items-start" ]
        [ svg [ class "flex-shrink-0 h-6 w-6 text-green-500 mt-0.5", fill "none", stroke "currentColor", viewBox "0 0 24 24" ]
            [ path [ strokeLinecap "round", strokeLinejoin "round", strokeWidth "2", d "M5 13l4 4L19 7" ] []
            ]
        , span [ class "ml-3 text-[#475467]" ] [ text text_ ]
        ]