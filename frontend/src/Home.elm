module Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Medicare Max - Automate Client Retention"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ nav [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6" ]
                [ div [ class "flex justify-between items-center" ]
                    [ div [ class "flex items-center" ]
                        [ img
                            [ src "/images/medicare-max-logo.png"
                            , class "h-8 w-auto"
                            , alt "Medicare Max logo"
                            ]
                            []
                        ]
                    , div [ class "flex items-center space-x-4" ]
                        [ a
                            [ href "/login"
                            , class "text-gray-600 hover:text-gray-900 px-4 py-2 text-sm font-medium"
                            ]
                            [ text "Log in" ]
                        , a
                            [ href "/signup"
                            , class "bg-[#0A0F4F] text-white px-4 py-2 rounded-lg text-sm font-medium hover:bg-[#1a1f5f] transition-colors duration-200"
                            ]
                            [ text "Sign up" ]
                        ]
                    ]
                ]
            , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 pt-24 pb-32" ]
                [ div [ class "lg:grid lg:grid-cols-12 lg:gap-8" ]
                    [ div [ class "sm:text-center md:max-w-2xl md:mx-auto lg:col-span-6 lg:text-left" ]
                        [ div [ class "inline-flex items-center space-x-2 bg-[#F4F3FF] rounded-full px-4 py-1.5 mb-8" ]
                            [ span [ class "text-sm font-medium text-[#0A0F4F]" ] [ text "What's new?" ]
                            , span [ class "text-sm text-gray-600" ] [ text "Instantly issue virtual cards" ]
                            ]
                        , h1
                            [ class "text-5xl tracking-tight font-bold text-gray-900 sm:text-6xl md:text-7xl" ]
                            [ text "Automate client retention." ]
                        , p
                            [ class "mt-6 text-lg text-gray-600 leading-relaxed" ]
                            [ text "Automatically engage clients in key moments of their medigap journey, and sit back as they enroll" ]
                        , div [ class "mt-10" ]
                            [ a
                                [ href "/signup"
                                , class "inline-flex items-center px-6 py-3 rounded-lg text-base font-medium text-white bg-[#0A0F4F] hover:bg-[#1a1f5f] transition-colors duration-200"
                                ]
                                [ text "Sign up" ]
                            ]
                        ]
                    , div [ class "mt-16 relative sm:max-w-lg sm:mx-auto lg:mt-0 lg:max-w-none lg:mx-0 lg:col-span-6 lg:flex lg:items-center" ]
                        [ div [ class "relative mx-auto w-full rounded-2xl shadow-xl overflow-hidden bg-gray-100 aspect-[4/3]" ]
                            [ img
                                [ src "/dashboard-preview.png"
                                , class "w-full h-full object-cover"
                                , alt "Dashboard preview"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-16" ]
                [ p [ class "text-center text-gray-600 mb-12" ]
                    [ text "Join 200+ companies retaining their clients" ]
                , div [ class "grid grid-cols-2 gap-8 md:grid-cols-5 items-center justify-items-center" ]
                    [ img [ src "/company-logos/bitshift.svg", class "h-8", alt "Bitshift logo" ] []
                    , img [ src "/company-logos/lightbox.svg", class "h-8", alt "Lightbox logo" ] []
                    , img [ src "/company-logos/greenway.svg", class "h-8", alt "Greenway logo" ] []
                    , img [ src "/company-logos/skyline.svg", class "h-8", alt "Skyline logo" ] []
                    , img [ src "/company-logos/clockwork.svg", class "h-8", alt "Clockwork logo" ] []
                    ]
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
