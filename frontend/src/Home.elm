module Home exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NavigateTo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo path ->
            ( model, Cmd.none )



-- Navigation will be handled by Main.elm
-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Medicare Max - Streamline Your Medicare Business"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ nav [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6" ]
                [ div [ class "flex justify-between items-center" ]
                    [ div [ class "flex items-center" ]
                        [ img [ src "/logo.svg", class "h-8 w-auto", alt "Medicare Max logo" ] []
                        ]
                    , div [ class "flex items-center space-x-4" ]
                        [ button
                            [ class "text-gray-500 hover:text-gray-900 px-3 py-2 rounded-md text-sm font-medium"
                            , onClick (NavigateTo "/login")
                            ]
                            [ text "Log in" ]
                        , button
                            [ class "bg-blue-600 text-white px-4 py-2 rounded-lg text-sm font-medium hover:bg-blue-700 transition-colors duration-200"
                            , onClick (NavigateTo "/signup")
                            ]
                            [ text "Sign up" ]
                        ]
                    ]
                ]
            , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 pt-16 pb-24" ]
                [ div [ class "lg:grid lg:grid-cols-12 lg:gap-8" ]
                    [ div [ class "sm:text-center md:max-w-2xl md:mx-auto lg:col-span-6 lg:text-left" ]
                        [ h1
                            [ class "text-4xl tracking-tight font-bold text-gray-900 sm:text-5xl md:text-6xl" ]
                            [ span [ class "block" ] [ text "Streamline your" ]
                            , span [ class "block text-blue-600" ] [ text "Medicare business" ]
                            ]
                        , p
                            [ class "mt-3 text-base text-gray-500 sm:mt-5 sm:text-xl lg:text-lg xl:text-xl" ]
                            [ text "Manage your Medicare business more efficiently. Track leads, automate follow-ups, and grow your book of business - all in one place." ]
                        , div [ class "mt-8 sm:max-w-lg sm:mx-auto sm:text-center lg:text-left" ]
                            [ button
                                [ class "inline-flex items-center px-6 py-3 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                                , onClick (NavigateTo "/signup")
                                ]
                                [ text "Get started" ]
                            ]
                        ]
                    , div [ class "mt-12 relative sm:max-w-lg sm:mx-auto lg:mt-0 lg:max-w-none lg:mx-0 lg:col-span-6 lg:flex lg:items-center" ]
                        [ div [ class "relative mx-auto w-full rounded-lg shadow-lg lg:max-w-md" ]
                            [ img
                                [ src "/dashboard-preview.png"
                                , class "w-full rounded-lg"
                                , alt "Dashboard preview"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
