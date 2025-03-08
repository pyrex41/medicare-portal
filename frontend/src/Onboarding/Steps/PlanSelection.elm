module Onboarding.Steps.PlanSelection exposing
    ( Model
    , Msg
    , OutMsg(..)
    , fetchSubscriptionTiers
    , init
    , subscriptions
    , update
    , view
    )

import Basics
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field)
import Json.Encode as Encode
import Process
import Task



-- MODEL


type alias SubscriptionTier =
    { id : String
    , name : String
    , price : String
    , agentLimit : Int
    , contactLimit : Int
    , features : List String
    }


type alias Model =
    { selectedPlan : Maybe String
    , extraAgents : Int
    , extraContacts : Int
    , tiers : List SubscriptionTier
    , isLoading : Bool
    , error : Maybe String
    , key : Nav.Key
    , orgSlug : String
    , session : String
    }


type alias SubscriptionResponse =
    { success : Bool
    , message : String
    , orgSlug : Maybe String
    }


init : Nav.Key -> String -> String -> ( Model, Cmd Msg )
init key orgSlug session =
    ( { selectedPlan = Nothing
      , extraAgents = 0
      , extraContacts = 0
      , tiers = []
      , isLoading = True
      , error = Nothing
      , key = key
      , orgSlug = orgSlug
      , session = session
      }
    , Cmd.batch
        [ fetchSubscriptionTiers
        , -- Add a timeout to clear loading state after 5 seconds
          Process.sleep 5000
            |> Task.perform (\_ -> LoadingTimeout)
        ]
    )



-- UPDATE


type Msg
    = SelectPlan String
    | SetExtraAgents String
    | SetExtraContacts String
    | GotTiers (Result Http.Error (List SubscriptionTier))
    | SubscriptionSaved (Result Http.Error SubscriptionResponse)
    | NextStepClicked
    | LoadingTimeout
    | NoOp


type OutMsg
    = NoOutMsg
    | SelectedPlan String
    | NextStep
    | ShowError String


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        SelectPlan planId ->
            ( { model | selectedPlan = Just planId }
            , Cmd.none
            , SelectedPlan planId
            )

        SetExtraAgents value ->
            let
                extraAgents =
                    String.toInt value |> Maybe.withDefault 0
            in
            ( { model | extraAgents = extraAgents }
            , Cmd.none
            , NoOutMsg
            )

        SetExtraContacts value ->
            let
                extraContacts =
                    String.toInt value |> Maybe.withDefault 0
            in
            ( { model | extraContacts = extraContacts }
            , Cmd.none
            , NoOutMsg
            )

        GotTiers result ->
            case result of
                Ok tiers ->
                    ( { model | tiers = tiers, isLoading = False }
                    , Cmd.none
                    , NoOutMsg
                    )

                Err _ ->
                    ( { model | error = Just "Failed to load subscription tiers", isLoading = False }
                    , Cmd.none
                    , ShowError "Failed to load subscription tiers"
                    )

        SubscriptionSaved result ->
            case result of
                Ok response ->
                    case response.orgSlug of
                        Just newOrgSlug ->
                            ( { model | orgSlug = newOrgSlug }
                            , Cmd.none
                            , NextStep
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            , NextStep
                            )

                Err _ ->
                    ( { model | error = Just "Failed to save subscription", isLoading = False }
                    , Cmd.none
                    , ShowError "Failed to save subscription"
                    )

        NextStepClicked ->
            case model.selectedPlan of
                Just planId ->
                    if planId == "enterprise" then
                        -- For Enterprise, redirect to the Enterprise form
                        ( model
                        , Cmd.none
                        , SelectedPlan "enterprise"
                        )

                    else
                        -- Instead of saving the subscription now, just proceed to the next step
                        -- We'll save everything at the end of the onboarding flow
                        ( model
                        , Cmd.none
                        , NextStep
                        )

                Nothing ->
                    ( { model | error = Just "Please select a plan" }
                    , Cmd.none
                    , ShowError "Please select a plan"
                    )

        LoadingTimeout ->
            -- If we're still loading after the timeout, clear the loading state
            if model.isLoading then
                ( { model | isLoading = False, error = Just "Could not load subscription tiers. Please try refreshing the page." }
                , Cmd.none
                , ShowError "Could not load subscription tiers. Please try refreshing the page."
                )

            else
                ( model, Cmd.none, NoOutMsg )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Choose your plan" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Select a plan that best fits your organization's needs" ]
            ]
        , if model.isLoading then
            viewLoading

          else
            div []
                [ div [ class "grid grid-cols-1 md:grid-cols-3 gap-4" ]
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
                , if canAddExtraResources model.selectedPlan then
                    viewExtraResources model

                  else
                    text ""
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
                        , onClick NextStepClicked
                        , disabled (model.selectedPlan == Nothing)
                        ]
                        [ text "Continue" ]
                    ]
                ]
        ]


viewLoading : Html msg
viewLoading =
    div [ class "text-center" ]
        [ div [ class "animate-spin rounded-full h-16 w-16 border-t-4 border-b-4 border-blue-500 mx-auto" ] []
        , p [ class "mt-4 text-gray-500" ]
            [ text "Loading subscription tiers..." ]
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
                [ if id /= "enterprise" then
                    div [ class "text-gray-600" ]
                        [ text
                            (if id == "pro" then
                                "5+ agent seats"

                             else if agentLimit == -1 then
                                "Unlimited agent seats"

                             else if agentLimit == 1 then
                                "1 agent seat"

                             else
                                "Up to " ++ String.fromInt agentLimit ++ " agent seats"
                            )
                        ]

                  else
                    div [ class "text-gray-600" ]
                        [ text "Custom agent seats" ]
                , if id /= "enterprise" then
                    div [ class "text-gray-600" ]
                        [ text
                            (if id == "pro" then
                                "5,000+ clients"

                             else if contactLimit == -1 then
                                "Unlimited clients"

                             else if contactLimit == 1000 then
                                "1,000 clients"

                             else
                                "Up to " ++ String.fromInt contactLimit ++ " clients"
                            )
                        ]

                  else
                    div [ class "text-gray-600" ]
                        [ text "Unlimited clients" ]
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


viewExtraResources : Model -> Html Msg
viewExtraResources model =
    div [ class "mt-8 p-4 bg-gray-50 rounded-lg border border-gray-200" ]
        [ h3 [ class "text-lg font-semibold text-gray-900 mb-4" ]
            [ text "Additional Resources" ]
        , div [ class "grid grid-cols-1 md:grid-cols-2 gap-6" ]
            [ div [ class "space-y-2" ]
                [ label [ class "block text-sm font-medium text-gray-700" ]
                    [ text "Extra Agents" ]
                , p [ class "text-xs text-gray-500" ]
                    [ text "Add more agent seats beyond your plan's included limit ($20/agent seat/month)" ]
                , div [ class "flex items-center" ]
                    [ button
                        [ class "bg-gray-200 px-3 py-1 rounded-l-md hover:bg-gray-300"
                        , onClick (SetExtraAgents (String.fromInt (Basics.max 0 (model.extraAgents - 1))))
                        ]
                        [ text "-" ]
                    , input
                        [ type_ "number"
                        , class "w-16 text-center border-y border-gray-200 py-1"
                        , value (String.fromInt model.extraAgents)
                        , onInput SetExtraAgents
                        ]
                        []
                    , button
                        [ class "bg-gray-200 px-3 py-1 rounded-r-md hover:bg-gray-300"
                        , onClick (SetExtraAgents (String.fromInt (model.extraAgents + 1)))
                        ]
                        [ text "+" ]
                    , span [ class "ml-2 text-sm font-medium" ]
                        [ text ("$" ++ String.fromInt (model.extraAgents * 20) ++ "/mo") ]
                    ]
                ]
            , div [ class "space-y-2" ]
                [ label [ class "block text-sm font-medium text-gray-700" ]
                    [ text "Extra Clients" ]
                , p [ class "text-xs text-gray-500" ]
                    [ text "Add more clients beyond your plan's included limit ($50/5,000 clients/month)" ]
                , div [ class "flex items-center" ]
                    [ button
                        [ class "bg-gray-200 px-3 py-1 rounded-l-md hover:bg-gray-300"
                        , onClick (SetExtraContacts (String.fromInt (Basics.max 0 (model.extraContacts - 5000))))
                        ]
                        [ text "-" ]
                    , input
                        [ type_ "number"
                        , class "w-20 text-center border-y border-gray-200 py-1"
                        , value (String.fromInt model.extraContacts)
                        , onInput SetExtraContacts
                        , Html.Attributes.step "5000"
                        ]
                        []
                    , button
                        [ class "bg-gray-200 px-3 py-1 rounded-r-md hover:bg-gray-300"
                        , onClick (SetExtraContacts (String.fromInt (model.extraContacts + 5000)))
                        ]
                        [ text "+" ]
                    , span [ class "ml-2 text-sm font-medium" ]
                        [ text ("$" ++ String.fromInt (model.extraContacts // 5000 * 50) ++ "/mo") ]
                    ]
                ]
            ]
        ]



-- HELPERS


canAddExtraResources : Maybe String -> Bool
canAddExtraResources selectedPlan =
    case selectedPlan of
        Just plan ->
            plan == "pro"

        Nothing ->
            False



-- API CALLS


fetchSubscriptionTiers : Cmd Msg
fetchSubscriptionTiers =
    Http.get
        { url = "/api/organizations/subscription-tiers"
        , expect = Http.expectJson GotTiers subscriptionTiersDecoder
        }


saveSubscription : String -> String -> Int -> Int -> Cmd Msg
saveSubscription orgSlug tierId extraAgents extraContacts =
    -- If orgSlug is empty, use a different endpoint for new organization signup
    if String.isEmpty (String.trim orgSlug) then
        Http.post
            { url = "/api/organizations/signup/subscription"
            , body = Http.jsonBody (encodeSubscriptionUpdate tierId extraAgents extraContacts)
            , expect = Http.expectJson SubscriptionSaved subscriptionResponseDecoder
            }

    else
        Http.post
            { url = "/api/organizations/" ++ orgSlug ++ "/subscription"
            , body = Http.jsonBody (encodeSubscriptionUpdate tierId extraAgents extraContacts)
            , expect = Http.expectJson SubscriptionSaved subscriptionResponseDecoder
            }



-- DECODERS & ENCODERS


subscriptionTiersDecoder : Decoder (List SubscriptionTier)
subscriptionTiersDecoder =
    field "tiers"
        (Decode.list
            (Decode.map6 SubscriptionTier
                (field "id" Decode.string)
                (field "name" Decode.string)
                (field "price" Decode.string)
                (field "agentLimit" Decode.int)
                (field "contactLimit" Decode.int)
                (field "features" (Decode.list Decode.string))
            )
        )


encodeSubscriptionUpdate : String -> Int -> Int -> Encode.Value
encodeSubscriptionUpdate tierId extraAgents extraContacts =
    Encode.object
        [ ( "tierId", Encode.string tierId )
        , ( "extraAgents", Encode.int extraAgents )
        , ( "extraContacts", Encode.int extraContacts )
        ]



-- Add decoder for the subscription response


subscriptionResponseDecoder : Decoder SubscriptionResponse
subscriptionResponseDecoder =
    Decode.map3 SubscriptionResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "message" Decode.string)
        (Decode.maybe (Decode.field "orgSlug" Decode.string))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
