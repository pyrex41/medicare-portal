module VerifyOrganization exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Debug
import Html exposing (Html, button, div, h2, input, label, li, p, span, text, ul)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Encode as Encode


type SetupStep
    = PlanSelection
    | Payment
    | Complete


type alias SubscriptionTier =
    { id : String
    , name : String
    , price : String
    , agentLimit : Int
    , contactLimit : Int
    , features : List String
    }


type alias Model =
    { session : Maybe String
    , orgSlug : String
    , currentStep : SetupStep
    , selectedPlan : Maybe String
    , error : Maybe String
    , tiers : List SubscriptionTier
    , isLoading : Bool
    , key : Nav.Key
    }


type Msg
    = NextStep
    | SelectPlan String
    | SubmitPayment
    | CompleteSetup
    | GotTiers (Result Http.Error (List SubscriptionTier))
    | SubscriptionSaved (Result Http.Error ())
    | NavigateToTempLanding


init : String -> String -> Nav.Key -> ( Model, Cmd Msg )
init orgSlug session key =
    let
        _ =
            Debug.log "Initializing VerifyOrganization"
                { orgSlug = orgSlug
                , session = session
                }
    in
    ( { session = Just session
      , orgSlug = orgSlug
      , currentStep = PlanSelection
      , selectedPlan = Nothing
      , error = Nothing
      , tiers = []
      , isLoading = True
      , key = key
      }
    , fetchSubscriptionTiers
    )


fetchSubscriptionTiers : Cmd Msg
fetchSubscriptionTiers =
    Http.get
        { url = "/api/organizations/subscription-tiers"
        , expect = Http.expectJson GotTiers subscriptionTiersDecoder
        }


saveSubscription : String -> String -> Cmd Msg
saveSubscription orgSlug tierId =
    let
        _ =
            Debug.log "Making subscription request"
                { url = "/api/organizations/" ++ orgSlug ++ "/subscription"
                , tierId = tierId
                }
    in
    Http.post
        { url = "/api/organizations/" ++ orgSlug ++ "/subscription"
        , body = Http.jsonBody (encodeSubscriptionUpdate tierId)
        , expect = Http.expectWhatever SubscriptionSaved
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "VerifyOrganization update" msg
    in
    case msg of
        SubscriptionSaved result ->
            case result of
                Ok _ ->
                    ( model
                    , Nav.pushUrl model.key "/settings/setup"
                    )

                Err _ ->
                    ( { model | error = Just "Failed to update subscription" }
                    , Cmd.none
                    )

        NextStep ->
            case model.currentStep of
                PlanSelection ->
                    case model.selectedPlan of
                        Just planId ->
                            ( { model | currentStep = Payment }
                            , saveSubscription model.orgSlug planId
                            )

                        Nothing ->
                            ( { model | error = Just "Please select a plan" }
                            , Cmd.none
                            )

                Payment ->
                    ( { model | currentStep = Complete }
                    , Nav.pushUrl model.key "/templanding"
                    )

                Complete ->
                    ( model
                    , Nav.pushUrl model.key "/templanding"
                    )

        SelectPlan plan ->
            ( { model | selectedPlan = Just plan }, Cmd.none )

        GotTiers result ->
            case result of
                Ok tiers ->
                    let
                        _ =
                            Debug.log "Got tiers" tiers
                    in
                    ( { model | tiers = tiers, isLoading = False }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            Debug.log "Failed to load tiers" error
                    in
                    ( { model | error = Just "Failed to load subscription tiers", isLoading = False }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Organization Setup"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center py-12 sm:px-6 lg:px-8" ]
            [ div [ class "mt-8 sm:mx-auto sm:w-full sm:max-w-md" ]
                [ div [ class "bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10" ]
                    [ viewStep model ]
                ]
            ]
        ]
    }


viewStep : Model -> Html Msg
viewStep model =
    case model.currentStep of
        PlanSelection ->
            if model.isLoading then
                viewLoading

            else
                viewPlanSelection model

        Payment ->
            div []
                [ h2 [ class "text-2xl font-bold text-gray-900 mb-6" ]
                    [ text "Payment details" ]
                , div [ class "mt-4" ]
                    [ button
                        [ class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                        , onClick NextStep
                        ]
                        [ text "Complete setup" ]
                    ]
                ]

        Complete ->
            div [ class "text-center" ]
                [ div [ class "rounded-full h-12 w-12 bg-green-100 flex items-center justify-center mx-auto" ]
                    [ span [ class "text-green-600 text-xl" ] [ text "✓" ] ]
                , h2 [ class "mt-4 text-2xl font-bold text-gray-900" ]
                    [ text "Setup complete!" ]
                , p [ class "mt-2 text-gray-600" ]
                    [ text "You're all set to start using your organization." ]
                ]


viewLoading : Html Msg
viewLoading =
    div [ class "text-center" ]
        [ div [ class "animate-spin rounded-full h-16 w-16 border-t-4 border-b-4 border-blue-500 mx-auto" ] []
        , p [ class "mt-4 text-gray-500" ]
            [ text "Loading subscription tiers..." ]
        ]


viewPlanSelection : Model -> Html Msg
viewPlanSelection model =
    div []
        [ h2 [ class "text-2xl font-bold text-gray-900 mb-6" ]
            [ text "Choose your plan" ]
        , div [ class "space-y-6" ]
            (List.map
                (\tier ->
                    viewPlanOption
                        tier.id
                        tier.name
                        tier.price
                        tier.features
                        tier.agentLimit
                        tier.contactLimit
                        model.selectedPlan
                )
                model.tiers
            )
        , if model.error /= Nothing then
            div [ class "mt-4 text-red-600" ]
                [ text (Maybe.withDefault "" model.error) ]

          else
            text ""
        , button
            [ class "mt-6 w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
            , onClick NextStep
            ]
            [ text "Continue to payment" ]
        ]


viewPlanOption : String -> String -> String -> List String -> Int -> Int -> Maybe String -> Html Msg
viewPlanOption id name price features agentLimit contactLimit selectedPlan =
    div
        [ class
            ("p-6 border rounded-lg cursor-pointer transition-all "
                ++ (if Just id == selectedPlan then
                        "border-blue-500 ring-2 ring-blue-500 bg-blue-50"

                    else
                        "border-gray-300 hover:border-blue-500 hover:shadow-md"
                   )
            )
        , onClick (SelectPlan id)
        ]
        [ div [ class "flex justify-between items-start" ]
            [ div [ class "space-y-4 flex-grow" ]
                [ div []
                    [ h2 [ class "text-xl font-semibold text-gray-900" ] [ text name ]
                    , p [ class "text-2xl font-bold text-gray-900 mt-2" ] [ text price ]
                    ]
                , div [ class "space-y-2" ]
                    [ div [ class "flex items-center text-gray-600" ]
                        [ text ("Up to " ++ String.fromInt agentLimit ++ " agents") ]
                    , div [ class "flex items-center text-gray-600" ]
                        [ text ("Up to " ++ String.fromInt contactLimit ++ " contacts") ]
                    ]
                , div [ class "mt-4" ]
                    [ p [ class "text-sm font-medium text-gray-900 mb-2" ] [ text "Features:" ]
                    , ul [ class "space-y-2" ]
                        (List.map
                            (\feature ->
                                li [ class "flex items-center text-sm text-gray-600" ]
                                    [ span [ class "text-green-500 mr-2" ] [ text "✓" ]
                                    , text feature
                                    ]
                            )
                            features
                        )
                    ]
                ]
            , if Just id == selectedPlan then
                div [ class "h-6 w-6 text-blue-600" ] [ text "✓" ]

              else
                div [] []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


subscriptionTiersDecoder : Decoder (List SubscriptionTier)
subscriptionTiersDecoder =
    let
        _ =
            Debug.log "Decoding subscription tiers" ()
    in
    field "tiers"
        (list
            (Decode.map6 SubscriptionTier
                (field "id" string)
                (field "name" string)
                (field "price" string)
                (field "agentLimit" int)
                (field "contactLimit" int)
                (field "features" (list string))
            )
        )


encodeSubscriptionUpdate : String -> Encode.Value
encodeSubscriptionUpdate tierId =
    Encode.object
        [ ( "tierId", Encode.string tierId )
        ]
