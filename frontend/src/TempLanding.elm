module TempLanding exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    {}


type Msg
    = NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Welcome"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center" ]
            [ div [ class "text-center" ]
                [ h1 [ class "text-4xl font-bold text-gray-900" ]
                    [ text "Welcome! You're logged in!" ]
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
