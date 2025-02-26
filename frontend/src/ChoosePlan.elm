module ChoosePlan exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components.SetupLayout as SetupLayout
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
    in
    Http.post
        { url = url
        , body = Http.jsonBody (encodeSubscriptionUpdate tierId)
        , expect = Http.expectWhatever SubscriptionSaved
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubscriptionSaved result ->
            case result of
                Ok _ ->
                    ( { model | error = Nothing }
                    , case model.selectedPlan of
                        Just planId ->
                            Nav.pushUrl model.key ("/setup/settings?plan=" ++ planId)

                        Nothing ->
                            Nav.pushUrl model.key "/setup/settings"
                    )

                Err error ->
                    ( { model | error = Just "Failed to save subscription" }
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
                    ( { model | tiers = tiers, isLoading = False }
                    , Cmd.none
                    )

                Err error ->
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


view : Model -> Browser.Document Msg
view model =
    { title = "Choose Plan - Medicare Max"
    , body =
        [ SetupLayout.view SetupLayout.PlanSelection
            (case model.selectedPlan of
                Just "basic" ->
                    True

                _ ->
                    False
            )
            [ if model.isLoading then
                viewLoading

              else
                viewPlanSelection model
            ]
        ]
    }


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
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Choose your plan" ]
            , p [ class "text-gray-600 mt-2" ]
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
                                "bg-[#2563EB]/50 cursor-not-allowed text-white"

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
                        "bg-gray-50 hover:bg-gray-100"
                   )
            )
        , onClick (SelectPlan id)
        ]
        [ div [ class "space-y-4" ]
            [ div []
                [ h3 [ class "text-xl font-semibold text-gray-900" ] [ text name ]
                , p [ class "text-3xl font-bold text-gray-900 mt-2" ]
                    [ text
                        (if id == "enterprise" then
                            "Contact Us"

                         else
                            price
                        )
                    ]
                ]
            , div [ class "space-y-2 py-4 border-t border-b border-gray-200" ]
                [ div [ class "text-gray-600" ]
                    [ text
                        (if agentLimit == -1 then
                            "Unlimited agents"

                         else
                            "Up to " ++ String.fromInt agentLimit ++ " agents"
                        )
                    ]
                , div [ class "text-gray-600" ]
                    [ text
                        (if contactLimit == -1 then
                            "Unlimited contacts"

                         else
                            "Up to " ++ String.fromInt contactLimit ++ " contacts"
                        )
                    ]
                ]
            , div [ class "mt-4" ]
                [ p [ class "text-sm font-medium text-gray-900 mb-2" ] [ text "Features:" ]
                , ul [ class "space-y-2" ]
                    (List.map
                        (\feature ->
                            li [ class "flex items-center text-sm text-gray-600" ]
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
