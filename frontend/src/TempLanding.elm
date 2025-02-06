module TempLanding exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    {}


type Msg
    = NavigateTo String


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Welcome"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center" ]
            [ div [ class "text-center space-y-8" ]
                [ h1 [ class "text-4xl font-bold text-gray-900" ]
                    [ text "Welcome! You're logged in!" ]
                , div [ class "flex justify-center space-x-4" ]
                    [ button
                        [ class "inline-flex items-center px-4 py-2 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        , onClick (NavigateTo "/dashboard")
                        ]
                        [ text "Go to Dashboard" ]
                    , button
                        [ class "inline-flex items-center px-4 py-2 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        , onClick (NavigateTo "/settings")
                        ]
                        [ text "Go to Settings" ]
                    ]
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
