module StageDemoCTA exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes as HA
import Svg exposing (path, svg)
import Svg.Attributes as SA


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
    div [ HA.class "min-h-screen bg-[#F9F5FF] flex items-center justify-center px-4" ]
        [ div [ HA.class "max-w-2xl mx-auto text-center" ]
            [ -- Success Icon
              div [ HA.class "mx-auto flex items-center justify-center h-24 w-24 rounded-full bg-green-100 mb-8" ]
                [ svg [ SA.class "h-12 w-12 text-green-600", SA.fill "none", SA.stroke "currentColor", SA.viewBox "0 0 24 24" ]
                    [ path [ SA.strokeLinecap "round", SA.strokeLinejoin "round", SA.strokeWidth "2", SA.d "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" ] []
                    ]
                ]

            -- Logo
            , div [ HA.class "mb-8" ]
                [ img
                    [ HA.src "/images/medicare-max-logo.png"
                    , HA.alt "Medicare Max"
                    , HA.class "h-12 mx-auto"
                    ]
                    []
                ]

            -- Heading
            , h1 [ HA.class "text-3xl sm:text-4xl font-bold text-[#141B29] mb-4" ]
                [ text "Congratulations! You May Qualify!" ]

            -- Subheading
            , p [ HA.class "text-xl text-[#475467] mb-8" ]
                [ text "Based on your answers, you appear to be eligible for the Medicare plans shown." ]

            -- Benefits list
            , div [ HA.class "bg-white rounded-lg shadow-lg p-8 mb-8 border border-gray-100" ]
                [ h2 [ HA.class "text-2xl font-semibold text-[#141B29] mb-6" ]
                    [ text "What's Next?" ]
                , ul [ HA.class "space-y-4 text-left" ]
                    [ benefitItem "Review your personalized plan options with an expert"
                    , benefitItem "Get help comparing coverage and costs"
                    , benefitItem "Learn about available discounts and savings"
                    , benefitItem "Complete your enrollment with professional guidance"
                    ]
                ]

            -- CTA Button
            , a
                [ HA.href "https://calendly.com/your-signup-link" -- Replace with actual Calendly URL
                , HA.target "_blank"
                , HA.class "inline-flex items-center px-8 py-4 border border-transparent text-lg font-semibold rounded-md shadow-lg text-white bg-[#03045E] hover:bg-[#1a1f5f] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#03045E] transition-all transform hover:scale-105"
                ]
                [ text "Schedule Your Medicare Max Demo"
                , svg [ SA.class "ml-3 -mr-1 h-5 w-5", SA.fill "none", SA.stroke "currentColor", SA.viewBox "0 0 24 24" ]
                    [ path [ SA.strokeLinecap "round", SA.strokeLinejoin "round", SA.strokeWidth "2", SA.d "M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14" ] []
                    ]
                ]

            -- Urgency text
            , p [ HA.class "mt-6 text-sm text-gray-600" ]
                [ span [ HA.class "font-semibold" ] [ text "Limited time offer: " ]
                , text "Schedule now to lock in your discounted rates!"
                ]
            ]
        ]


benefitItem : String -> Html msg
benefitItem text_ =
    li [ HA.class "flex items-start" ]
        [ svg [ SA.class "flex-shrink-0 h-6 w-6 text-green-500 mt-0.5", SA.fill "none", SA.stroke "currentColor", SA.viewBox "0 0 24 24" ]
            [ path [ SA.strokeLinecap "round", SA.strokeLinejoin "round", SA.strokeWidth "2", SA.d "M5 13l4 4L19 7" ] []
            ]
        , span [ HA.class "ml-3 text-[#475467]" ] [ text text_ ]
        ]
