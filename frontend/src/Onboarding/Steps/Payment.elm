module Onboarding.Steps.Payment exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode



-- MODEL


type alias Model =
    { isLoading : Bool
    , error : Maybe String
    , key : Nav.Key
    , orgSlug : String
    , subscriptionDetails : Maybe SubscriptionDetails
    , processingPayment : Bool
    }


type alias SubscriptionDetails =
    { planName : String
    , planPrice : String
    , extraAgents : Int
    , extraAgentsCost : Int
    , extraContacts : Int
    , extraContactsCost : Int
    , totalPrice : String
    , isTrial : Bool
    , trialEndsAt : Maybe String
    }


init : Nav.Key -> String -> ( Model, Cmd Msg )
init key orgSlug =
    ( { isLoading = False
      , error = Nothing
      , key = key
      , orgSlug = orgSlug
      , subscriptionDetails =
            Just
                { planName = "Your Selected Plan"
                , planPrice = "$29/mo"
                , extraAgents = 0
                , extraAgentsCost = 0
                , extraContacts = 0
                , extraContactsCost = 0
                , totalPrice = "$29/mo"
                , isTrial = True
                , trialEndsAt = Just "30 days after signup"
                }
      , processingPayment = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = CompleteOnboarding
    | ProcessPayment
    | GotSubscriptionDetails (Result Http.Error SubscriptionDetails)
    | PaymentProcessed (Result Http.Error String)
    | OnboardingCompleted (Result Http.Error ())
    | NoOp


type OutMsg
    = NoOutMsg
    | Completed
    | ShowError String
    | NavigateToWalkthrough


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        CompleteOnboarding ->
            ( { model | processingPayment = True }
            , Cmd.none
            , NavigateToWalkthrough
            )

        ProcessPayment ->
            ( { model | processingPayment = True }
            , createStripeCheckoutSession model.orgSlug
            , NoOutMsg
            )

        GotSubscriptionDetails result ->
            case result of
                Ok details ->
                    ( { model
                        | subscriptionDetails = Just details
                        , isLoading = False
                      }
                    , Cmd.none
                    , NoOutMsg
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to load subscription details"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to load subscription details"
                    )

        PaymentProcessed result ->
            case result of
                Ok sessionId ->
                    -- Redirect to Stripe checkout
                    ( { model | processingPayment = False }
                    , redirectToStripeCheckout sessionId
                    , NoOutMsg
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to process payment"
                        , processingPayment = False
                      }
                    , Cmd.none
                    , ShowError "Failed to process payment"
                    )

        OnboardingCompleted result ->
            case result of
                Ok _ ->
                    ( { model | isLoading = False, processingPayment = False }
                    , Cmd.none
                    , Completed
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to complete onboarding"
                        , isLoading = False
                        , processingPayment = False
                      }
                    , Cmd.none
                    , ShowError "Failed to complete onboarding"
                    )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Complete Your Account Setup" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "You're almost done! Click 'Continue to Walkthrough' to finish the onboarding process." ]
            ]
        , div [ class "bg-white shadow rounded-lg p-6" ]
            [ div [ class "text-center space-y-6" ]
                [ div [ class "mb-4 text-green-500 text-6xl" ]
                    [ text "✓" ]
                , h2 [ class "text-xl font-medium text-gray-900" ]
                    [ text "All Set!" ]
                , p [ class "text-gray-600" ]
                    [ text "Your account is ready to be created with the details you've provided." ]
                , div [ class "mt-6" ]
                    [ button
                        [ class "px-8 py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                        , onClick CompleteOnboarding
                        , disabled model.processingPayment
                        ]
                        [ if model.processingPayment then
                            div [ class "flex items-center justify-center" ]
                                [ div [ class "mr-2 animate-spin rounded-full h-4 w-4 border-t-2 border-b-2 border-white" ] []
                                , text "Processing..."
                                ]

                          else
                            text "Continue to Walkthrough"
                        ]
                    ]
                ]
            ]
        , if model.error /= Nothing then
            div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                [ text (Maybe.withDefault "" model.error) ]

          else
            text ""
        ]


viewLoading : Html msg
viewLoading =
    div [ class "text-center py-12" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-t-4 border-b-4 border-blue-500 mx-auto" ] []
        , p [ class "mt-4 text-gray-500" ]
            [ text "Loading..." ]
        ]


viewSubscriptionSummary : Model -> Html Msg
viewSubscriptionSummary model =
    case model.subscriptionDetails of
        Just details ->
            div [ class "bg-white shadow rounded-lg overflow-hidden" ]
                [ div [ class "px-6 py-4 border-b border-gray-200" ]
                    [ h2 [ class "text-lg font-medium text-gray-900" ]
                        [ text "Subscription Summary" ]
                    ]
                , div [ class "px-6 py-4" ]
                    [ div [ class "space-y-4" ]
                        [ div [ class "flex justify-between" ]
                            [ span [ class "text-gray-600" ] [ text "Plan" ]
                            , span [ class "font-medium" ] [ text details.planName ]
                            ]
                        , div [ class "flex justify-between" ]
                            [ span [ class "text-gray-600" ] [ text "Base Price" ]
                            , span [ class "font-medium" ] [ text details.planPrice ]
                            ]
                        , if details.extraAgents > 0 then
                            div [ class "flex justify-between" ]
                                [ span [ class "text-gray-600" ]
                                    [ text ("Extra Agents (" ++ String.fromInt details.extraAgents ++ ")") ]
                                , span [ class "font-medium" ]
                                    [ text ("$" ++ String.fromInt details.extraAgentsCost) ]
                                ]

                          else
                            text ""
                        , if details.extraContacts > 0 then
                            div [ class "flex justify-between" ]
                                [ span [ class "text-gray-600" ]
                                    [ text ("Extra Contacts (" ++ String.fromInt details.extraContacts ++ ")") ]
                                , span [ class "font-medium" ]
                                    [ text ("$" ++ String.fromInt details.extraContactsCost) ]
                                ]

                          else
                            text ""
                        , div [ class "pt-4 border-t border-gray-200 flex justify-between" ]
                            [ span [ class "font-medium text-gray-900" ] [ text "Total" ]
                            , span [ class "font-bold text-gray-900" ] [ text details.totalPrice ]
                            ]
                        ]
                    ]
                , if details.isTrial then
                    div [ class "px-6 py-4 bg-blue-50 border-t border-blue-100" ]
                        [ div [ class "flex items-start" ]
                            [ div [ class "flex-shrink-0 pt-0.5" ]
                                [ span [ class "text-blue-500 text-lg" ] [ text "ℹ" ]
                                ]
                            , div [ class "ml-3" ]
                                [ h3 [ class "text-sm font-medium text-blue-800" ]
                                    [ text "Free Trial" ]
                                , div [ class "mt-2 text-sm text-blue-700" ]
                                    [ p []
                                        [ text
                                            (case details.trialEndsAt of
                                                Just date ->
                                                    "Your free trial will end on " ++ date ++ ". No payment is required today."

                                                Nothing ->
                                                    "You're starting with a free trial. No payment is required today."
                                            )
                                        ]
                                    ]
                                ]
                            ]
                        ]

                  else
                    text ""
                ]

        Nothing ->
            div [ class "bg-gray-100 p-6 rounded-lg text-center" ]
                [ text "No subscription details available" ]



-- API CALLS


fetchSubscriptionDetails : String -> Cmd Msg
fetchSubscriptionDetails orgSlug =
    Http.get
        { url = "/api/organizations/" ++ orgSlug ++ "/subscription-details"
        , expect = Http.expectJson GotSubscriptionDetails subscriptionDetailsDecoder
        }


createStripeCheckoutSession : String -> Cmd Msg
createStripeCheckoutSession orgSlug =
    Http.post
        { url = "/api/organizations/" ++ orgSlug ++ "/create-checkout-session"
        , body = Http.jsonBody (encodeCheckoutRequest orgSlug)
        , expect = Http.expectJson PaymentProcessed Decode.string
        }


redirectToStripeCheckout : String -> Cmd Msg
redirectToStripeCheckout sessionId =
    Nav.load ("/api/stripe/redirect-to-checkout?session_id=" ++ sessionId)


completeOnboarding : String -> Cmd Msg
completeOnboarding orgSlug =
    Http.post
        { url = "/api/organizations/complete-onboarding"
        , body = Http.emptyBody -- The parent component will send the full payload
        , expect = Http.expectWhatever OnboardingCompleted
        }



-- DECODERS & ENCODERS


subscriptionDetailsDecoder : Decode.Decoder SubscriptionDetails
subscriptionDetailsDecoder =
    Decode.succeed SubscriptionDetails
        |> Pipeline.required "planName" Decode.string
        |> Pipeline.required "planPrice" Decode.string
        |> Pipeline.required "extraAgents" Decode.int
        |> Pipeline.required "extraAgentsCost" Decode.int
        |> Pipeline.required "extraContacts" Decode.int
        |> Pipeline.required "extraContactsCost" Decode.int
        |> Pipeline.required "totalPrice" Decode.string
        |> Pipeline.required "isTrial" Decode.bool
        |> Pipeline.required "trialEndsAt" (Decode.nullable Decode.string)


encodeCheckoutRequest : String -> Encode.Value
encodeCheckoutRequest orgSlug =
    Encode.object
        [ ( "orgSlug", Encode.string orgSlug )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
