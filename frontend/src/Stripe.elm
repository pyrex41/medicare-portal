module Stripe exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Debug
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


type ApiError
    = NetworkError
    | BadStatus Int String
    | BadPayload String
    | BadUrl String
    | Timeout


type PaymentStatus
    = ReadyToComplete
    | Continuing
    | Error ApiError
    | Loading
    | Success SubscriptionStatus


type alias SubscriptionStatus =
    { isActive : Bool
    , tier : String
    , currentPeriodEnd : Maybe Int
    , cancelAtPeriodEnd : Maybe Bool
    , paymentStatus : String
    }


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
      , paymentStatus = Loading
      }
    , fetchSubscriptionStatus
    )



-- UPDATE


type Msg
    = NoOp
    | PaymentCompleted PaymentStatus
    | GotSubscriptionStatus (Result Http.Error SubscriptionStatus)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PaymentCompleted status ->
            ( { model | paymentStatus = status }
            , fetchSubscriptionStatus
            )

        GotSubscriptionStatus result ->
            case result of
                Ok status ->
                    ( { model | paymentStatus = Success status }, Cmd.none )

                Err httpError ->
                    ( { model | paymentStatus = Error (httpErrorToApiError httpError) }, Cmd.none )



-- HTTP


fetchSubscriptionStatus : Cmd Msg
fetchSubscriptionStatus =
    Http.get
        { url = "/api/stripe/subscription-status"
        , expect = Http.expectJson (Debug.log "API Response" >> GotSubscriptionStatus) subscriptionStatusDecoder
        }


errorResponseDecoder : Decoder String
errorResponseDecoder =
    Decode.field "error" Decode.string


subscriptionStatusDecoder : Decoder SubscriptionStatus
subscriptionStatusDecoder =
    Decode.field "data"
        (Decode.map5 SubscriptionStatus
            (Decode.field "isActive" Decode.bool)
            (Decode.field "tier" Decode.string)
            (Decode.maybe (Decode.field "currentPeriodEnd" Decode.int))
            (Decode.maybe (Decode.field "cancelAtPeriodEnd" Decode.bool))
            (Decode.field "paymentStatus" Decode.string)
        )


httpErrorToApiError : Http.Error -> ApiError
httpErrorToApiError error =
    case error of
        Http.BadUrl url ->
            BadUrl url

        Http.Timeout ->
            Timeout

        Http.NetworkError ->
            NetworkError

        Http.BadStatus status ->
            BadStatus status "Server error occurred"

        Http.BadBody message ->
            BadPayload message


apiErrorToString : ApiError -> String
apiErrorToString error =
    case error of
        NetworkError ->
            "Network error occurred. Please check your connection."

        BadStatus status message ->
            "Server error: " ++ String.fromInt status ++ " - " ++ message

        BadPayload message ->
            "Failed to process server response. "
                ++ (if String.contains "JSON" message then
                        "Please try refreshing the page."

                    else
                        message
                   )

        BadUrl url ->
            "Invalid API URL: " ++ url

        Timeout ->
            "Request timed out. Please try again."



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Onboarding"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex items-center justify-center py-12 px-4 sm:px-6 lg:px-8" ]
            [ div [ class "max-w-6xl w-full space-y-8 bg-white p-8 rounded-lg shadow-md" ]
                [ case model.paymentStatus of
                    Loading ->
                        div [ class "mt-4 p-3 bg-blue-50 border border-blue-200 rounded-md" ]
                            [ p [ class "text-blue-700" ] [ text "Loading subscription status..." ] ]

                    Error apiError ->
                        div [ class "mt-4 p-3 bg-red-50 border border-red-200 rounded-md" ]
                            [ p [ class "text-red-700" ] [ text "Error" ]
                            , p [ class "mt-2 text-sm text-red-600" ]
                                [ text (apiErrorToString apiError)
                                , br [] []
                                , text "If you're using an ad blocker, please disable it for this site as it may interfere with payment processing."
                                ]
                            , button
                                [ class "mt-4 px-4 py-2 bg-red-100 hover:bg-red-200 text-red-700 rounded-md"
                                , onClick (PaymentCompleted Loading)
                                ]
                                [ text "Retry" ]
                            ]

                    Success status ->
                        if status.isActive then
                            div [ class "mt-4 p-3 bg-green-50 border border-green-200 rounded-md" ]
                                [ p [ class "text-green-700 font-medium" ]
                                    [ text ("Active subscription: " ++ status.tier) ]
                                , case status.currentPeriodEnd of
                                    Just periodEnd ->
                                        p [ class "mt-2 text-sm text-green-600" ]
                                            [ text ("Current period ends: " ++ formatUnixTimestamp periodEnd) ]

                                    Nothing ->
                                        text ""
                                , case status.cancelAtPeriodEnd of
                                    Just True ->
                                        p [ class "mt-2 text-sm text-yellow-600" ]
                                            [ text "Your subscription will cancel at the end of the current period" ]

                                    _ ->
                                        text ""
                                ]

                        else
                            node "stripe-checkout"
                                [ attribute "price-id" "price_1RHG4mCBUPXAZKNGem75yV4U" --"price_1RBStWCBUPXAZKNGwpimWl7v"
                                , attribute "metered-price-id" "price_1RHG7kCBUPXAZKNGd5YvIzsw" --"price_1RBSvJCBUPXAZKNGQ1U9Hl8i"
                                , attribute "return-url" "http://localhost:3000/return"
                                , attribute "first-name" model.user.firstName
                                , attribute "last-name" model.user.lastName
                                , attribute "email" model.user.email
                                ]
                                []

                    Continuing ->
                        div [ class "mt-4 p-3 bg-blue-50 border border-blue-200 rounded-md" ]
                            [ p [ class "text-blue-700" ] [ text "Resuming your previous setup" ]
                            , p [ class "mt-2 text-sm text-blue-600" ]
                                [ text "If the checkout doesn't appear, please disable any ad blockers for this site." ]
                            ]

                    ReadyToComplete ->
                        node "stripe-checkout"
                            [ attribute "price-id" "price_1RHG4mCBUPXAZKNGem75yV4U" --"price_1RBStWCBUPXAZKNGwpimWl7v"
                            , attribute "metered-price-id" "price_1RHG7kCBUPXAZKNGd5YvIzsw" --"price_1RBSvJCBUPXAZKNGQ1U9Hl8i"
                            , attribute "return-url" "http://localhost:3000/return"
                            , attribute "first-name" model.user.firstName
                            , attribute "last-name" model.user.lastName
                            , attribute "email" model.user.email
                            ]
                            []
                ]
            ]
        ]
    }



-- HELPERS


formatUnixTimestamp : Int -> String
formatUnixTimestamp timestamp =
    -- Convert Unix timestamp to readable date string
    -- You may want to use a proper date formatting library here
    String.fromInt timestamp



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
