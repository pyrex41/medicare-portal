module Onboarding exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url exposing (Url)
import Url.Parser.Query as Query
import Utils.UrlStuff exposing (getQueryParams)



-- MODEL


type PaymentStatus
    = ReadyToComplete
    | Continuing
    | Error String


type alias Model =
    { user : User
    , paymentStatus : PaymentStatus
    }


dummyUser : User
dummyUser =
    { firstName = "John"
    , lastName = "Doe"
    , email = "john.doe@example.com"
    }


type alias User =
    { firstName : String
    , lastName : String
    , email : String
    }


init : Nav.Key -> Url -> ( Model, Cmd Msg )
init key url =
    let
        queryParams =
            url |> getQueryParams

        firstName =
            Dict.get "firstName" queryParams

        lastName =
            Dict.get "lastName" queryParams

        email =
            Dict.get "email" queryParams

        maybeUser =
            case ( firstName, lastName, email ) of
                ( Just f, Just l, Just e ) ->
                    Just { firstName = f, lastName = l, email = e }

                _ ->
                    Nothing
    in
    ( { user = maybeUser |> Maybe.withDefault dummyUser
      , paymentStatus = ReadyToComplete
      }
    , case maybeUser of
        Just _ ->
            Cmd.none

        Nothing ->
            Nav.pushUrl key "/signup"
    )



-- UPDATE


type Msg
    = NoOp
    | PaymentCompleted PaymentStatus -- Added to handle custom event from JS


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PaymentCompleted status ->
            ( { model | paymentStatus = status }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Onboarding"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex items-center justify-center py-12 px-4 sm:px-6 lg:px-8" ]
            [ div [ class "max-w-6xl w-full space-y-8 bg-white p-8 rounded-lg shadow-md" ]
                [ div [ class "text-center" ]
                    [ h1 [ class "text-3xl font-bold text-gray-900 mb-4" ] [ text "Welcome to Onboarding" ]
                    , div [ class "mt-4" ]
                        [ p [ class "text-xl font-medium text-gray-700" ]
                            [ text (String.join " " [ model.user.firstName, model.user.lastName ]) ]
                        , p [ class "mt-2 text-gray-600" ]
                            [ text (model.user.email |> Url.percentDecode |> Maybe.withDefault "") ]
                        ]
                    ]
                , case model.paymentStatus of
                    Error error ->
                        div [ class "mt-4 p-3 bg-red-50 border border-red-200 rounded-md" ]
                            [ p [ class "text-red-700" ] [ text "Error" ]
                            , p [ class "mt-2 text-sm text-red-600" ]
                                [ text "If you're using an ad blocker, please disable it for this site as it may interfere with payment processing." ]
                            ]

                    Continuing ->
                        div [ class "mt-4 p-3 bg-blue-50 border border-blue-200 rounded-md" ]
                            [ p [ class "text-blue-700" ] [ text "Resuming your previous setup" ]
                            , p [ class "mt-2 text-sm text-blue-600" ]
                                [ text "If the checkout doesn't appear, please disable any ad blockers for this site." ]
                            ]

                    ReadyToComplete ->
                        node "stripe-checkout"
                            [ attribute "price-id" "price_1RBStWCBUPXAZKNGwpimWl7v" -- Base Subscription price
                            , attribute "metered-price-id" "price_1RBSvJCBUPXAZKNGQ1U9Hl8i" -- Contact Tier price
                            , attribute "return-url" "http://localhost:3000/return" -- Matches your backend config
                            , attribute "first-name" model.user.firstName
                            , attribute "last-name" model.user.lastName
                            , attribute "email" model.user.email
                            ]
                            []
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
