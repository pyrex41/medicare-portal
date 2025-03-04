module ChoosePlan exposing (Model, Msg(..), init, subscriptions, update, view, viewChangePlan)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components.SetupLayout as SetupLayout
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
    , extraAgents : Int
    , extraContacts : Int
    , error : Maybe String
    , tiers : List SubscriptionTier
    , isLoading : Bool
    , key : Nav.Key
    , isProcessingPayment : Bool
    , showChangePlan : Bool
    , currentTier : Maybe String
    , currentAgentLimit : Int
    , currentContactLimit : Int
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
    | SetExtraAgents String
    | SetExtraContacts String
    | StripeCheckoutReady (Result Http.Error String)
    | ProcessPayment
    | CancelPayment
    | GotCurrentSubscription (Result Http.Error { tierId : String, agentLimit : Int, contactLimit : Int })


init : String -> String -> Nav.Key -> Bool -> ( Model, Cmd Msg )
init orgSlug session key showChangePlan =
    ( { session = Just session
      , orgSlug = orgSlug
      , currentStep = PlanSelection
      , selectedPlan = Nothing
      , extraAgents = 0
      , extraContacts = 0
      , error = Nothing
      , tiers = []
      , isLoading = True
      , key = key
      , isProcessingPayment = False
      , showChangePlan = showChangePlan
      , currentTier = Nothing
      , currentAgentLimit = 0
      , currentContactLimit = 0
      }
    , Cmd.batch
        [ fetchSubscriptionTiers
        , if showChangePlan then
            fetchCurrentSubscription orgSlug

          else
            Cmd.none
        ]
    )


fetchCurrentSubscription : String -> Cmd Msg
fetchCurrentSubscription orgSlug =
    let
        url =
            "/api/organizations/" ++ orgSlug ++ "/subscription"

        _ =
            Debug.log "Fetching subscription from URL" url
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotCurrentSubscription currentSubscriptionDecoder
        }


fetchSubscriptionTiers : Cmd Msg
fetchSubscriptionTiers =
    Http.get
        { url = "/api/organizations/subscription-tiers"
        , expect = Http.expectJson GotTiers subscriptionTiersDecoder
        }


saveSubscription : String -> String -> Int -> Int -> Cmd Msg
saveSubscription orgSlug tierId extraAgents extraContacts =
    let
        url =
            "/api/organizations/" ++ orgSlug ++ "/subscription"
    in
    Http.post
        { url = url
        , body = Http.jsonBody (encodeSubscriptionUpdate tierId extraAgents extraContacts)
        , expect = Http.expectWhatever SubscriptionSaved
        }


createStripeCheckoutSession : String -> String -> Int -> Int -> Cmd Msg
createStripeCheckoutSession orgSlug tierId extraAgents extraContacts =
    let
        url =
            "/api/stripe/create-checkout-session"
    in
    Http.post
        { url = url
        , body = Http.jsonBody (encodeStripeCheckoutRequest orgSlug tierId extraAgents extraContacts)
        , expect = Http.expectJson StripeCheckoutReady (field "sessionId" string)
        }


redirectToStripeCheckout : String -> Cmd Msg
redirectToStripeCheckout sessionId =
    -- Use the session ID to redirect to Stripe Checkout
    -- In a real implementation, this would likely be a port to JavaScript
    -- For this demo, we're just simulating with a navigation
    Nav.load ("/api/stripe/redirect-to-checkout?session_id=" ++ sessionId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubscriptionSaved result ->
            case result of
                Ok _ ->
                    ( { model | error = Nothing }
                    , if model.showChangePlan then
                        -- For change plan, just reload the current page to show updated info
                        Nav.reload

                      else
                        -- For initial setup, continue to next step
                        case model.selectedPlan of
                            Just planId ->
                                Nav.pushUrl model.key ("/setup/settings?plan=" ++ planId)

                            Nothing ->
                                Nav.pushUrl model.key "/setup/settings"
                    )

                Err error ->
                    ( { model | error = Just "Failed to save subscription", isProcessingPayment = False }
                    , Cmd.none
                    )

        NextStep ->
            case model.currentStep of
                PlanSelection ->
                    case model.selectedPlan of
                        Just planId ->
                            if planId == "enterprise" then
                                -- For Enterprise, redirect to contact form
                                ( model
                                , Nav.pushUrl model.key "/enterprise-contact"
                                )

                            else if model.showChangePlan then
                                -- For change plan, we go directly to payment
                                ( { model | currentStep = Payment, isProcessingPayment = True }
                                , createStripeCheckoutSession model.orgSlug planId model.extraAgents model.extraContacts
                                )

                            else
                                -- For initial setup, save subscription then go to next page
                                ( { model | currentStep = Payment }
                                , saveSubscription model.orgSlug planId model.extraAgents model.extraContacts
                                )

                        Nothing ->
                            ( { model | error = Just "Please select a plan" }
                            , Cmd.none
                            )

                Payment ->
                    ( { model | currentStep = Complete }
                    , if model.showChangePlan then
                        Nav.pushUrl model.key "/dashboard"

                      else
                        Nav.pushUrl model.key "/templanding"
                    )

                Complete ->
                    ( model
                    , if model.showChangePlan then
                        Nav.pushUrl model.key "/dashboard"

                      else
                        Nav.pushUrl model.key "/templanding"
                    )

        SelectPlan plan ->
            ( { model | selectedPlan = Just plan }, Cmd.none )

        SetExtraAgents value ->
            let
                extraAgents =
                    String.toInt value |> Maybe.withDefault 0
            in
            ( { model | extraAgents = extraAgents }, Cmd.none )

        SetExtraContacts value ->
            let
                extraContacts =
                    String.toInt value |> Maybe.withDefault 0
            in
            ( { model | extraContacts = extraContacts }, Cmd.none )

        GotTiers result ->
            case result of
                Ok tiers ->
                    let
                        -- Log all tiers
                        _ =
                            Debug.log "All tiers from API" tiers

                        -- Look for Pro plans specifically
                        proPlans =
                            List.filter (\t -> t.id == "pro") tiers

                        _ =
                            Debug.log "Pro plans found" proPlans

                        -- Look for $99 Pro plan
                        proPlans99 =
                            List.filter (\t -> t.id == "pro" && t.price == "$99/mo") tiers

                        _ =
                            Debug.log "$99 Pro plans found" proPlans99

                        -- Apply filter
                        filteredTiers =
                            filterTiers tiers

                        -- Log filtered tiers
                        _ =
                            Debug.log "Filtered tiers" filteredTiers
                    in
                    ( { model
                        | tiers = filteredTiers
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        errorString =
                            case error of
                                Http.BadUrl url ->
                                    "Bad URL: " ++ url

                                Http.Timeout ->
                                    "Request timed out"

                                Http.NetworkError ->
                                    "Network error"

                                Http.BadStatus statusCode ->
                                    "Bad status: " ++ String.fromInt statusCode

                                Http.BadBody message ->
                                    "Bad body: " ++ message

                        _ =
                            Debug.log "Failed to load tiers" errorString
                    in
                    ( { model | error = Just "Failed to load subscription tiers", isLoading = False }
                    , Cmd.none
                    )

        GotCurrentSubscription result ->
            case result of
                Ok subscription ->
                    -- Log successful subscription load in console using Debug.log
                    let
                        _ =
                            Debug.log "Successfully loaded subscription"
                                { tierId = subscription.tierId
                                , agentLimit = subscription.agentLimit
                                , contactLimit = subscription.contactLimit
                                }
                    in
                    ( { model
                        | currentTier = Just subscription.tierId
                        , currentAgentLimit = subscription.agentLimit
                        , currentContactLimit = subscription.contactLimit
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err error ->
                    -- Log the error in console using Debug.log
                    let
                        errorString =
                            case error of
                                Http.BadUrl url ->
                                    "Bad URL: " ++ url

                                Http.Timeout ->
                                    "Request timed out"

                                Http.NetworkError ->
                                    "Network error"

                                Http.BadStatus statusCode ->
                                    "Bad status: " ++ String.fromInt statusCode

                                Http.BadBody message ->
                                    "Bad body: " ++ message

                        _ =
                            Debug.log "Failed to load subscription" errorString
                    in
                    ( { model | error = Just "Failed to load current subscription", isLoading = False }
                    , Cmd.none
                    )

        StripeCheckoutReady result ->
            case result of
                Ok sessionId ->
                    ( model
                    , redirectToStripeCheckout sessionId
                    )

                Err error ->
                    ( { model | error = Just "Failed to create payment session", isProcessingPayment = False }
                    , Cmd.none
                    )

        ProcessPayment ->
            case model.selectedPlan of
                Just planId ->
                    let
                        _ =
                            Debug.log "ProcessPayment with plan" planId

                        _ =
                            Debug.log "showChangePlan" model.showChangePlan

                        _ =
                            Debug.log "hasChanges" (hasChanges model)
                    in
                    if planId == "enterprise" then
                        -- For Enterprise plans, always redirect to contact form
                        let
                            _ =
                                Debug.log "Redirecting to enterprise contact" "/enterprise-contact"
                        in
                        ( model
                        , Nav.pushUrl model.key "/enterprise-contact"
                        )

                    else if model.showChangePlan && not (hasChanges model) then
                        -- No changes, show an error (only for non-enterprise plans)
                        ( { model | error = Just "No changes made to your subscription." }
                        , Cmd.none
                        )

                    else
                        -- Process payment for changes
                        ( { model | isProcessingPayment = True }
                        , createStripeCheckoutSession model.orgSlug planId model.extraAgents model.extraContacts
                        )

                Nothing ->
                    ( { model | error = Just "Please select a plan" }
                    , Cmd.none
                    )

        CancelPayment ->
            ( { model | isProcessingPayment = False }
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
    { title =
        if model.showChangePlan then
            "Change Plan - Medicare Max"

        else
            "Choose Plan - Medicare Max"
    , body =
        [ if model.showChangePlan then
            viewChangePlan model

          else
            SetupLayout.view SetupLayout.PlanSelection
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


viewChangePlan : Model -> Html Msg
viewChangePlan model =
    div [ class "container mx-auto py-8 px-4" ]
        [ div [ class "space-y-8" ]
            [ div [ class "mb-8" ]
                [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                    [ text "Change Your Plan" ]
                , p [ class "text-gray-600 mt-2" ]
                    [ text "Modify your subscription to better fit your organization's needs" ]
                ]
            , case model.currentTier of
                Just currentTierId ->
                    div [ class "p-4 bg-blue-50 rounded-lg border border-blue-200 mb-8" ]
                        [ h3 [ class "text-lg font-semibold text-gray-900" ]
                            [ text "Current Plan" ]
                        , p [ class "text-sm text-gray-600" ]
                            [ text
                                ("You are currently on the "
                                    ++ (model.tiers
                                            |> List.filter (\t -> t.id == currentTierId)
                                            |> List.head
                                            |> Maybe.map .name
                                            |> Maybe.withDefault currentTierId
                                       )
                                    ++ " plan with "
                                    ++ String.fromInt model.currentAgentLimit
                                    ++ " agents and "
                                    ++ String.fromInt model.currentContactLimit
                                    ++ " contacts."
                                )
                            ]
                        ]

                Nothing ->
                    text ""
            , if model.isProcessingPayment then
                div [ class "text-center py-8" ]
                    [ div [ class "animate-spin rounded-full h-16 w-16 border-t-4 border-b-4 border-blue-500 mx-auto" ] []
                    , p [ class "mt-4 text-gray-500" ]
                        [ text "Preparing payment session..." ]
                    , button
                        [ class "mt-4 px-4 py-2 bg-gray-200 hover:bg-gray-300 rounded"
                        , onClick CancelPayment
                        ]
                        [ text "Cancel" ]
                    ]

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
                        div [ class "mt-8 p-4 bg-gray-50 rounded-lg border border-gray-200" ]
                            [ h3 [ class "text-lg font-semibold text-gray-900 mb-4" ]
                                [ text "Additional Resources" ]
                            , div [ class "grid grid-cols-1 md:grid-cols-2 gap-6" ]
                                [ div [ class "space-y-2" ]
                                    [ label [ class "block text-sm font-medium text-gray-700" ]
                                        [ text "Extra Agents" ]
                                    , p [ class "text-xs text-gray-500" ]
                                        [ text "Add more agents beyond your plan's included limit ($20/agent/month)" ]
                                    , div [ class "flex items-center" ]
                                        [ button
                                            [ class "bg-gray-200 px-3 py-1 rounded-l-md hover:bg-gray-300"
                                            , onClick (SetExtraAgents (String.fromInt (max 0 (model.extraAgents - 1))))
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
                                        [ text "Extra Contacts" ]
                                    , p [ class "text-xs text-gray-500" ]
                                        [ text "Add more contacts beyond your plan's included limit ($50/5,000 contacts/month)" ]
                                    , div [ class "flex items-center" ]
                                        [ button
                                            [ class "bg-gray-200 px-3 py-1 rounded-l-md hover:bg-gray-300"
                                            , onClick (SetExtraContacts (String.fromInt (max 0 (model.extraContacts - 5000))))
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

                      else
                        text ""
                    , if model.error /= Nothing then
                        div [ class "mt-4 text-red-500" ]
                            [ text (Maybe.withDefault "" model.error) ]

                      else
                        text ""
                    , div [ class "mt-8 flex justify-center space-x-4" ]
                        [ button
                            [ class
                                ("px-6 py-3 rounded-lg transition-colors duration-200 "
                                    ++ (if model.selectedPlan == Nothing || (model.showChangePlan && not (hasChanges model)) then
                                            "bg-[#2563EB]/50 cursor-not-allowed text-white"

                                        else
                                            "bg-[#2563EB] hover:bg-[#1D4ED8] text-white"
                                       )
                                )
                            , onClick
                                (if model.showChangePlan then
                                    ProcessPayment

                                 else
                                    NextStep
                                )
                            , Html.Attributes.disabled (model.selectedPlan == Nothing || (model.showChangePlan && not (hasChanges model)))
                            ]
                            [ text
                                (if model.showChangePlan then
                                    "Change Plan"

                                 else
                                    "Select"
                                )
                            ]
                        , if model.showChangePlan then
                            button
                                [ class "px-6 py-3 rounded-lg bg-gray-200 hover:bg-gray-300 text-gray-800"
                                , onClick NavigateToTempLanding
                                ]
                                [ text "Cancel" ]

                          else
                            text ""
                        ]
                    ]
            ]
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
        , if canAddExtraResources model.selectedPlan then
            div [ class "mt-8 p-4 bg-gray-50 rounded-lg border border-gray-200" ]
                [ h3 [ class "text-lg font-semibold text-gray-900 mb-4" ]
                    [ text "Additional Resources" ]
                , div [ class "grid grid-cols-1 md:grid-cols-2 gap-6" ]
                    [ div [ class "space-y-2" ]
                        [ label [ class "block text-sm font-medium text-gray-700" ]
                            [ text "Extra Agents" ]
                        , p [ class "text-xs text-gray-500" ]
                            [ text "Add more agents beyond your plan's included limit ($20/agent/month)" ]
                        , div [ class "flex items-center" ]
                            [ button
                                [ class "bg-gray-200 px-3 py-1 rounded-l-md hover:bg-gray-300"
                                , onClick (SetExtraAgents (String.fromInt (max 0 (model.extraAgents - 1))))
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
                            [ text "Extra Contacts" ]
                        , p [ class "text-xs text-gray-500" ]
                            [ text "Add more contacts beyond your plan's included limit ($50/5,000 contacts/month)" ]
                        , div [ class "flex items-center" ]
                            [ button
                                [ class "bg-gray-200 px-3 py-1 rounded-l-md hover:bg-gray-300"
                                , onClick (SetExtraContacts (String.fromInt (max 0 (model.extraContacts - 5000))))
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
                [ if id /= "enterprise" then
                    div [ class "text-gray-600" ]
                        [ text
                            (if id == "pro" then
                                "Starts with up to " ++ String.fromInt agentLimit ++ " agents"

                             else if agentLimit == -1 then
                                "Unlimited agents"

                             else
                                "Up to " ++ String.fromInt agentLimit ++ " agents"
                            )
                        ]

                  else
                    text ""
                , if id /= "enterprise" then
                    div [ class "text-gray-600" ]
                        [ text
                            (if id == "pro" then
                                "Starts with up to " ++ String.fromInt contactLimit ++ " contacts"

                             else if contactLimit == -1 then
                                "Unlimited contacts"

                             else
                                "Up to " ++ String.fromInt contactLimit ++ " contacts"
                            )
                        ]

                  else
                    text ""
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


currentSubscriptionDecoder : Decoder { tierId : String, agentLimit : Int, contactLimit : Int }
currentSubscriptionDecoder =
    let
        -- Try to decode from a success field first (for API format consistency)
        successDecoder =
            field "success" Decode.bool
                |> Decode.andThen
                    (\success ->
                        if success then
                            -- If success is true, look for the fields at the top level
                            Decode.map3
                                (\tierId agentLimit contactLimit ->
                                    { tierId = tierId
                                    , agentLimit = agentLimit
                                    , contactLimit = contactLimit
                                    }
                                )
                                (field "tierId" string)
                                (field "agentLimit" int)
                                (field "contactLimit" int)

                        else
                            -- If success is false, fail with the error message
                            field "error" string
                                |> Decode.andThen (\err -> Decode.fail err)
                    )

        -- Try the direct decoder as a fallback
        directDecoder =
            Decode.map3
                (\tierId agentLimit contactLimit ->
                    { tierId = tierId
                    , agentLimit = agentLimit
                    , contactLimit = contactLimit
                    }
                )
                (field "tierId" string)
                (field "agentLimit" int)
                (field "contactLimit" int)
    in
    -- Try to use the success wrapper first, fall back to direct decoder
    Decode.oneOf [ successDecoder, directDecoder ]


encodeSubscriptionUpdate : String -> Int -> Int -> Encode.Value
encodeSubscriptionUpdate tierId extraAgents extraContacts =
    Encode.object
        [ ( "tierId", Encode.string tierId )
        , ( "extraAgents", Encode.int extraAgents )
        , ( "extraContacts", Encode.int extraContacts )
        ]


encodeStripeCheckoutRequest : String -> String -> Int -> Int -> Encode.Value
encodeStripeCheckoutRequest orgSlug tierId extraAgents extraContacts =
    Encode.object
        [ ( "orgSlug", Encode.string orgSlug )
        , ( "tierId", Encode.string tierId )
        , ( "extraAgents", Encode.int extraAgents )
        , ( "extraContacts", Encode.int extraContacts )
        ]


filterTiers : List SubscriptionTier -> List SubscriptionTier
filterTiers tiers =
    let
        -- Keep only the $99 Pro plan (filter out any other Pro plans)
        filteredTiers =
            tiers
                |> List.filter
                    (\tier ->
                        -- Keep Basic and Enterprise tiers
                        tier.id
                            == "basic"
                            || tier.id
                            == "enterprise"
                            || -- Keep only the $99 Pro plan
                               (tier.id == "pro" && tier.price == "$99/mo")
                    )

        -- Make sure we have the base tiers
        hasBasic =
            List.any (\t -> t.id == "basic") filteredTiers

        hasPro =
            List.any (\t -> t.id == "pro") filteredTiers

        hasEnterprise =
            List.any (\t -> t.id == "enterprise") filteredTiers

        -- Default tiers to add if missing
        defaultBasic =
            SubscriptionTier "basic" "Basic" "$49/mo" 1 1000 [ "1 agent", "Up to 1,000 contacts", "Email support" ]

        defaultPro =
            SubscriptionTier "pro" "Pro" "$99/mo" 5 10000 [ "5 agents", "Up to 10,000 contacts", "Email & chat support", "Advanced analytics" ]

        defaultEnterprise =
            SubscriptionTier "enterprise" "Enterprise" "Contact Us" 10 20000 [ "10+ agents", "Unlimited contacts", "Priority support", "Custom solutions", "Dedicated account manager" ]

        -- Add default tiers if missing
        tiersWithDefaults =
            (if not hasBasic then
                [ defaultBasic ]

             else
                []
            )
                ++ (if not hasPro then
                        [ defaultPro ]

                    else
                        []
                   )
                ++ (if not hasEnterprise then
                        [ defaultEnterprise ]

                    else
                        []
                   )
                ++ filteredTiers
    in
    tiersWithDefaults


canAddExtraResources : Maybe String -> Bool
canAddExtraResources selectedPlan =
    case selectedPlan of
        Just plan ->
            plan == "pro"

        -- Only Pro plan can add extra resources
        Nothing ->
            False



-- Add this function after filterTiers function


hasChanges : Model -> Bool
hasChanges model =
    let
        -- Check if the plan has changed
        planChanged =
            case ( model.currentTier, model.selectedPlan ) of
                ( Just currentTier, Just selectedTier ) ->
                    currentTier /= selectedTier

                _ ->
                    False

        -- Check if resources have changed
        resourcesChanged =
            model.extraAgents > 0 || model.extraContacts > 0
    in
    planChanged || resourcesChanged
