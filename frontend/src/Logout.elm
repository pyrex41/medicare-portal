module Logout exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


type alias Model =
    { key : Nav.Key }


type Msg
    = LogoutCompleted (Result Http.Error ())
    | NoOp


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key }
    , Http.post
        { url = "/api/auth/logout"
        , body = Http.emptyBody
        , expect = Http.expectWhatever LogoutCompleted
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogoutCompleted (Ok _) ->
            ( model
            , Nav.load "/"
            )

        LogoutCompleted (Err _) ->
            -- Even if the logout request fails, we'll redirect to home
            ( model
            , Nav.load "/"
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view _ =
    { title = "Logging out..."
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex items-center justify-center" ]
            [ div [ class "animate-spin rounded-full h-8 w-8 border-2 border-purple-500 border-t-transparent" ] []
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
