module StageDemoQuote exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StageDemoData exposing (Plan, standardQuoteG, standardQuoteN)
import Svg exposing (svg, path)
import Svg.Attributes exposing (d, fill, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)

type alias Model =
    { tempId : String
    , planG : Plan
    , planN : Plan
    }

type Msg
    = NavigateToHealth

init : String -> Model
init tempId =
    { tempId = tempId
    , planG = standardQuoteG
    , planN = standardQuoteN
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateToHealth ->
            ( model, Cmd.none ) -- Navigation handled by Main.elm

formatPrice : Float -> String
formatPrice price =
    "$" ++ String.fromFloat (price / 100)

view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-white" ]
        [ -- Header
          div [ class "bg-white shadow-sm border-b" ]
            [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6" ]
                [ -- Logo
                  div [ class "flex items-center justify-center mb-6" ]
                    [ img 
                        [ src "/images/medicare-max-logo.png"
                        , alt "Medicare Max"
                        , class "h-10"
                        ]
                        []
                    ]
                , div [ class "text-center" ]
                    [ h1 [ class "text-2xl sm:text-3xl font-bold text-[#141B29]" ] [ text "Your Personalized Medicare Quote" ]
                    , p [ class "text-[#475467] mt-2" ] [ text "Prepared for: Valued Client" ]
                    , p [ class "text-sm text-[#475467] mt-1" ] [ text "Powered by Medicare Max • 888-888-8888" ]
                    ]
                ]
            ]
        
        -- AI Video Section
        , div [ class "bg-[#F9F5FF] py-12" ]
            [ div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8" ]
                [ h2 [ class "text-xl font-semibold text-[#141B29] mb-4 text-center" ] [ text "Watch Your Personalized Video" ]
                , div [ class "bg-white rounded-lg shadow-lg overflow-hidden" ]
                    [ -- Placeholder for AI video - in real implementation this would be an iframe
                      div [ class "aspect-w-16 aspect-h-9 bg-gray-200" ]
                        [ div [ class "flex items-center justify-center h-96" ]
                            [ div [ class "text-center" ]
                                [ svg [ class "mx-auto h-12 w-12 text-gray-400", fill "none", stroke "currentColor", viewBox "0 0 24 24" ]
                                    [ path [ strokeLinecap "round", strokeLinejoin "round", strokeWidth "2", d "M14.752 11.168l-3.197-2.132A1 1 0 0010 9.87v4.263a1 1 0 001.555.832l3.197-2.132a1 1 0 000-1.664z" ] []
                                    , path [ strokeLinecap "round", strokeLinejoin "round", strokeWidth "2", d "M21 12a9 9 0 11-18 0 9 9 0 0118 0z" ] []
                                    ]
                                , p [ class "mt-2 text-gray-600" ] [ text "AI Video Player" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        
        -- Quote Cards
        , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12" ]
            [ h2 [ class "text-2xl sm:text-3xl font-bold text-[#141B29] mb-8 text-center" ] [ text "Your Best Medicare Supplement Options" ]
            , div [ class "grid md:grid-cols-2 gap-8" ]
                [ -- Plan G Card
                  planCard model.planG True
                , -- Plan N Card
                  planCard model.planN False
                ]
            ]
        
        -- CTA Section
        , div [ class "bg-[#F9FAFB] py-12" ]
            [ div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 text-center" ]
                [ h3 [ class "text-xl font-semibold text-[#141B29] mb-4" ] [ text "Ready to secure your Medicare coverage?" ]
                , button
                    [ class "inline-flex items-center px-6 py-3 border border-transparent text-base font-semibold rounded-md shadow-sm text-white bg-[#03045E] hover:bg-[#1a1f5f] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#03045E] transition-colors duration-200"
                    , onClick NavigateToHealth
                    ]
                    [ text "See if I Qualify" ]
                ]
            ]
        ]

planCard : Plan -> Bool -> Html Msg
planCard plan isPrimary =
    div 
        [ class <| "bg-white rounded-lg shadow-lg overflow-hidden " ++ 
          (if isPrimary then "ring-2 ring-[#03045E]" else "")
        ]
        [ if isPrimary then
            div [ class "bg-[#03045E] text-white text-center py-2 text-sm font-semibold" ]
                [ text "MOST POPULAR" ]
          else
            text ""
        , div [ class "p-6" ]
            [ -- Carrier Logo
              div [ class "flex items-center justify-between mb-4" ]
                [ img 
                    [ src plan.image
                    , alt plan.name
                    , class "h-12 object-contain"
                    ] 
                    []
                , div [ class "text-right" ]
                    [ h3 [ class "text-2xl font-bold text-gray-900" ] [ text ("Plan " ++ plan.planType) ]
                    ]
                ]
            
            -- Pricing
            , div [ class "mb-6" ]
                [ div [ class "flex items-baseline" ]
                    [ span [ class "text-3xl font-bold text-gray-900" ] [ text (formatPrice plan.priceDiscount) ]
                    , span [ class "text-gray-600 ml-2" ] [ text "/month" ]
                    ]
                , if plan.price > plan.priceDiscount then
                    div [ class "mt-1" ]
                        [ span [ class "text-sm text-gray-500 line-through" ] [ text (formatPrice plan.price) ]
                        , span [ class "text-sm text-green-600 ml-2" ] [ text (plan.discountCategory ++ " Discount Applied!") ]
                        ]
                  else
                    text ""
                ]
            
            -- Features
            , div [ class "space-y-3 mb-6" ]
                [ featureItem "Comprehensive coverage"
                , featureItem "No network restrictions"
                , featureItem "Coverage while traveling"
                , if plan.planType == "G" then
                    featureItem "Covers Medicare Part B deductible"
                  else
                    featureItem "Lower premium option"
                ]
            
            -- CTA
            , button
                [ class "w-full py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-semibold text-white bg-[#03045E] hover:bg-[#1a1f5f] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#03045E] transition-colors duration-200"
                , onClick NavigateToHealth
                ]
                [ text "Select This Plan" ]
            ]
        ]

featureItem : String -> Html msg
featureItem text_ =
    div [ class "flex items-start" ]
        [ svg [ class "flex-shrink-0 h-5 w-5 text-green-500 mt-0.5", fill "none", stroke "currentColor", viewBox "0 0 24 24" ]
            [ path [ strokeLinecap "round", strokeLinejoin "round", strokeWidth "2", d "M5 13l4 4L19 7" ] []
            ]
        , span [ class "ml-3 text-gray-700 text-sm" ] [ Html.text text_ ]
        ]