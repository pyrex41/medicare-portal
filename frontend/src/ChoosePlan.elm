module ChoosePlan exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components.ProgressIndicator
import Debug
import Html exposing (Html, button, div, h1, h2, h3, input, label, li, p, span, text, ul)
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
    | GotSaveResponse (Result Http.Error ())


init : String -> String -> Nav.Key -> ( Model, Cmd Msg )
init orgSlug session key =
    let
        _ =
            Debug.log "Initializing ChoosePlan with orgSlug" orgSlug
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
        url =
            "/api/organizations/" ++ orgSlug ++ "/subscription"

        _ =
            Debug.log "Making subscription request"
                { url = url
                , tierId = tierId
                , orgSlug = orgSlug
                }
    in
    Http.post
        { url = url
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
                    ( { model | error = Nothing }
                    , Nav.pushUrl model.key "/brand-settings"
                    )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                Http.BadStatus 403 ->
                                    "You don't have permission to update this organization"

                                Http.BadStatus 404 ->
                                    "Organization not found"

                                _ ->
                                    "Failed to update subscription. Please try again."
                    in
                    ( { model | error = Just errorMessage }
                    , Cmd.none
                    )

        NextStep ->
            case model.currentStep of
                PlanSelection ->
                    case model.selectedPlan of
                        Just planId ->
                            let
                                _ =
                                    Debug.log "Saving subscription with orgSlug" model.orgSlug
                            in
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

        GotSaveResponse result ->
            case result of
                Ok _ ->
                    ( { model | error = Nothing }
                    , Nav.pushUrl model.key "/brand-settings"
                    )

                Err error ->
                    ( { model
                        | error = Just "Failed to save subscription. Please try again."
                        , isLoading = False
                      }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Choose Plan - Medicare Max"
    , body =
        [ div [ class "flex min-h-screen bg-[#0D1117]" ]
            [ Components.ProgressIndicator.view
                [ { icon = "1"
                  , title = "Choose Plan"
                  , description = "Select your subscription"
                  , isCompleted = False
                  , isActive = True
                  }
                , { icon = "2"
                  , title = "Organization Settings"
                  , description = "Configure your organization"
                  , isCompleted = False
                  , isActive = False
                  }
                , { icon = "3"
                  , title = "Add Team Members"
                  , description = "Invite your team"
                  , isCompleted = False
                  , isActive = False
                  }
                ]
            , div [ class "flex-1 p-8" ]
                [ div [ class "max-w-4xl mx-auto" ]
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
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-white" ]
                [ text "Choose your plan" ]
            , p [ class "text-[#8B8B8B] mt-2" ]
                [ text "Select a plan that best fits your organization's needs" ]
            ]
        , div [ class "grid grid-cols-1 md:grid-cols-3 gap-4" ]
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
            div [ class "mt-4 text-red-500" ]
                [ text (Maybe.withDefault "" model.error) ]

          else
            text ""
        , div [ class "mt-8 flex justify-center" ]
            [ button
                [ class
                    ("px-6 py-3 rounded-lg transition-colors duration-200 "
                        ++ (if model.selectedPlan == Nothing then
                                "bg-[#2563EB]/50 cursor-not-allowed text-white/70"

                            else
                                "bg-[#2563EB] hover:bg-[#1D4ED8] text-white"
                           )
                    )
                , onClick NextStep
                , Html.Attributes.disabled (model.selectedPlan == Nothing)
                ]
                [ text "Select" ]
            ]
        ]


viewPlanOption : String -> String -> String -> List String -> Int -> Int -> Maybe String -> Html Msg
viewPlanOption id name price features agentLimit contactLimit selectedPlan =
    div
        [ class
            ("p-6 rounded-lg cursor-pointer transition-all "
                ++ (if Just id == selectedPlan then
                        "bg-[#2563EB]/10 ring-2 ring-[#2563EB]"

                    else
                        "bg-white/5 hover:bg-black/10"
                   )
            )
        , onClick (SelectPlan id)
        ]
        [ div [ class "space-y-4" ]
            [ div []
                [ h3 [ class "text-xl font-semibold text-white" ] [ text name ]
                , p [ class "text-3xl font-bold text-white mt-2" ] [ text price ]
                ]
            , div [ class "space-y-2 py-4 border-t border-b border-white/10" ]
                [ div [ class "text-[#8B8B8B]" ]
                    [ text ("Up to " ++ String.fromInt agentLimit ++ " agents") ]
                , div [ class "text-[#8B8B8B]" ]
                    [ text ("Up to " ++ String.fromInt contactLimit ++ " contacts") ]
                ]
            , div [ class "mt-4" ]
                [ p [ class "text-sm font-medium text-white mb-2" ] [ text "Features:" ]
                , ul [ class "space-y-2" ]
                    (List.map
                        (\feature ->
                            li [ class "flex items-center text-sm text-[#8B8B8B]" ]
                                [ span [ class "text-[#059669] mr-2" ] [ text "✓" ]
                                , text feature
                                ]
                        )
                        features
                    )
                ]
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
