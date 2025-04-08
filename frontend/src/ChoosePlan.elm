port module ChoosePlan exposing (Model, Msg(..), init, subscriptions, update, view, viewChangePlan)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components.LimitBanner as LimitBanner exposing (LimitWarning(..))
import Components.SetupLayout as SetupLayout
import Html exposing (Html, button, div, h1, h2, h3, h4, input, label, li, node, p, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Encode as Encode
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, viewBox)


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


type alias CustomTierPricing =
    { price : String
    , agentLimit : Int
    , tierName : String
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
    , showTrialBanner : Bool
    , customContactCount : String
    , isLoadingCustomTier : Bool
    , customTierPricing : Maybe CustomTierPricing
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
    | ConfirmPlan
    | GotConfirmation (Result Http.Error { success : Bool, redirectUrl : String })
    | NoOp
    | NavigateTo String
    | CloseBanner
    | SetCustomContactCount String
    | CalculateCustomTier
    | GotCustomTierPricing (Result Http.Error CustomTierPricing)
    | SelectCustomTier
    | StripeTableSelection String


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
      , showTrialBanner = True
      , customContactCount = ""
      , isLoadingCustomTier = False
      , customTierPricing = Nothing
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
                            if model.showChangePlan then
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
            ( { model | selectedPlan = Just plan }
            , Cmd.none
            )

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
                        -- Look for Pro plans specifically
                        proPlans =
                            List.filter (\t -> t.id == "pro") tiers

                        -- Look for $99 Pro plan
                        proPlans99 =
                            List.filter (\t -> t.id == "pro" && t.price == "$99/mo") tiers

                        -- Apply filter
                        filteredTiers =
                            filterTiers tiers
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
                    in
                    ( { model | error = Just "Failed to load subscription tiers", isLoading = False }
                    , Cmd.none
                    )

        GotCurrentSubscription result ->
            case result of
                Ok subscription ->
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
                    if model.showChangePlan && not (hasChanges model) then
                        -- No changes, show an error
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

        ConfirmPlan ->
            let
                plan =
                    model.selectedPlan |> Maybe.withDefault "basic"
            in
            ( { model | isLoading = True }
            , Http.post
                { url = "/api/choose-plan"
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "plan", Encode.string plan )
                            , ( "orgSlug", Encode.string model.orgSlug )
                            ]
                        )
                , expect = Http.expectJson GotConfirmation confirmationDecoder
                }
            )

        GotConfirmation (Ok response) ->
            if response.success then
                ( model
                , Nav.pushUrl model.key response.redirectUrl
                )

            else
                ( { model | isLoading = False, error = Just "Failed to update plan. Please try again." }
                , Cmd.none
                )

        GotConfirmation (Err _) ->
            ( { model | isLoading = False, error = Just "Failed to connect to server. Please try again." }
            , Cmd.none
            )

        NavigateTo url ->
            ( model
            , Nav.pushUrl model.key url
            )

        CloseBanner ->
            ( { model | showTrialBanner = False }, Cmd.none )

        SetCustomContactCount value ->
            ( { model | customContactCount = value }, Cmd.none )

        CalculateCustomTier ->
            ( { model | isLoadingCustomTier = True }, calculateCustomTier model )

        GotCustomTierPricing result ->
            case result of
                Ok pricing ->
                    ( { model | customTierPricing = Just pricing, isLoadingCustomTier = False }, Cmd.none )

                Err error ->
                    ( { model | error = Just "Failed to calculate custom tier pricing", isLoadingCustomTier = False }, Cmd.none )

        SelectCustomTier ->
            ( model, selectCustomTier model )

        StripeTableSelection priceId ->
            ( { model | selectedPlan = Just priceId, error = Nothing }
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
        [ if model.showTrialBanner then
            getPlanLimitBanner model

          else
            text ""
        , if model.showChangePlan then
            -- Change Plan is not part of setup flow, but a standalone page
            -- Return just the content portion which will be wrapped by Main.elm
            viewChangePlan model

          else
            -- This is the setup flow which uses a different layout
            SetupLayout.view SetupLayout.PlanSelection
                -- For plan selection, we determine basic vs pro based on what the user has selected
                (case model.selectedPlan of
                    Just "basic" ->
                        True

                    _ ->
                        False
                )
                0
                -- Using 0 for PlanSelection as it's the first step
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
                                    ++ " agent seats and "
                                    ++ String.fromInt model.currentContactLimit
                                    ++ " clients."
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

                    -- Add custom tier calculator section
                    , div [ class "mt-8 p-6 bg-gray-50 rounded-lg border border-gray-200" ]
                        [ h3 [ class "text-lg font-semibold text-gray-900 mb-4" ]
                            [ text "Need a custom plan?" ]
                        , p [ class "text-gray-600 mb-4" ]
                            [ text "Enter your expected number of contacts to calculate a custom tier." ]
                        , div [ class "flex items-end space-x-4" ]
                            [ div [ class "flex-grow" ]
                                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                                    [ text "Number of Contacts" ]
                                , input
                                    [ type_ "number"
                                    , class "w-full px-4 py-2 border border-gray-300 rounded-md"
                                    , value model.customContactCount
                                    , onInput SetCustomContactCount
                                    , Html.Attributes.placeholder "Enter contact count (e.g. 15000)"
                                    ]
                                    []
                                ]
                            , button
                                [ class "px-4 py-2 rounded-md bg-blue-600 text-white hover:bg-blue-700"
                                , onClick CalculateCustomTier
                                ]
                                [ if model.isLoadingCustomTier then
                                    text "Calculating..."

                                  else
                                    text "Calculate"
                                ]
                            ]

                        -- Display custom tier result if available
                        , case model.customTierPricing of
                            Just pricing ->
                                div [ class "mt-6 p-4 bg-white rounded-lg border border-blue-200" ]
                                    [ div [ class "flex items-center justify-between mb-3" ]
                                        [ h4 [ class "text-lg font-semibold text-gray-900" ]
                                            [ text pricing.tierName ]
                                        , div [ class "rounded-full px-3 py-1 text-sm font-medium bg-blue-50 text-blue-700" ]
                                            [ text pricing.price ]
                                        ]
                                    , p [ class "text-gray-600 mb-3" ]
                                        [ text ("Up to " ++ String.fromInt pricing.contactLimit ++ " contacts with " ++ String.fromInt pricing.agentLimit ++ " agent accounts") ]
                                    , div [ class "mt-3" ]
                                        [ ul [ class "space-y-2" ]
                                            (List.map viewFeature pricing.features)
                                        ]
                                    , div [ class "mt-4" ]
                                        [ button
                                            [ class "px-4 py-2 rounded-md bg-blue-600 text-white hover:bg-blue-700 w-full"
                                            , onClick (SelectPlan ("tier-custom-" ++ String.fromInt pricing.contactLimit))
                                            ]
                                            [ text "Select This Plan" ]
                                        ]
                                    ]

                            Nothing ->
                                text ""
                        ]
                    , if canAddExtraResources model.selectedPlan then
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
                                        [ text "Extra Clients" ]
                                    , p [ class "text-xs text-gray-500" ]
                                        [ text "Add more clients beyond your plan's included limit ($50/5,000 clients/month)" ]
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
                [ text "Select a plan that fits your organization's needs" ]
            ]
        , div [ class "w-full" ]
            [ -- Embedded Stripe pricing table
              node "stripe-pricing-table"
                [ attribute "pricing-table-id" "prctbl_1RAfz9CBUPXAZKNG0EyV8bRU"
                , attribute "publishable-key" "pk_test_51Qyh7RCBUPXAZKNGAvsWikdxCCaV1R9Vc79IgPqCul8AJsln69ABDQZE0zzOtOlH5rqrlw2maRebndvPl8xDaIVl00Nn2OOBCX"
                ]
                []
            , div [ class "mt-4 p-4 bg-gray-50 rounded-lg border border-gray-200" ]
                [ h3 [ class "text-lg font-semibold text-gray-900 mb-2" ]
                    [ text "Usage-Based Pricing" ]
                , p [ class "text-gray-600" ]
                    [ text "Our subscription includes the following:" ]
                , ul [ class "list-disc list-inside mt-2 space-y-1 text-gray-600" ]
                    [ li [] [ text "Base plan includes 500 contacts" ]
                    , li [] [ text "$40 for each additional 500 contacts used" ]
                    , li [] [ text "You will only be billed for the contacts you actually have" ]
                    , li [] [ text "We'll automatically adjust your billing as your contact count changes" ]
                    ]
                ]
            ]

        -- Add a button to continue after selection
        , div [ class "mt-8 flex justify-center" ]
            [ button
                [ class
                    ("px-6 py-3 rounded-lg transition-colors duration-200 "
                        ++ (if model.selectedPlan == Nothing then
                                "bg-blue-400 cursor-not-allowed text-white"

                            else
                                "bg-blue-600 hover:bg-blue-700 text-white"
                           )
                    )
                , onClick NextStep
                , disabled (model.selectedPlan == Nothing)
                ]
                [ text "Continue" ]
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
                                "Starts with up to " ++ String.fromInt agentLimit ++ " agent seats"

                             else if agentLimit == -1 then
                                "Unlimited agent seats"

                             else
                                "Up to " ++ String.fromInt agentLimit ++ " agent seats"
                            )
                        ]

                  else
                    text ""
                , if id /= "enterprise" then
                    div [ class "text-gray-600" ]
                        [ text
                            (if id == "pro" then
                                "Starts with up to " ++ String.fromInt contactLimit ++ " clients"

                             else if contactLimit == -1 then
                                "Unlimited clients"

                             else
                                "Up to " ++ String.fromInt contactLimit ++ " clients"
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
                            li [ class "flex items-start" ]
                                [ span [ class "text-[#059669] mr-2" ] [ text "✓" ]
                                , text feature
                                ]
                        )
                        features
                    )
                ]
            ]
        ]


viewFeature : String -> Html Msg
viewFeature feature =
    li [ class "flex items-start" ]
        [ div [ class "flex-shrink-0 h-5 w-5 text-green-500" ]
            [ viewSmallCheckIcon ]
        , div [ class "ml-3 text-sm text-gray-500" ]
            [ text feature ]
        ]


viewCheckIcon : Html Msg
viewCheckIcon =
    svg
        [ Svg.Attributes.class "h-5 w-5"
        , viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ fillRule "evenodd"
            , clipRule "evenodd"
            , d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
            ]
            []
        ]


viewSmallCheckIcon : Html Msg
viewSmallCheckIcon =
    svg
        [ Svg.Attributes.class "h-4 w-4"
        , viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ fillRule "evenodd"
            , clipRule "evenodd"
            , d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
            ]
            []
        ]


calculateCustomTier : Model -> Cmd Msg
calculateCustomTier model =
    case String.toInt model.customContactCount of
        Just contactCount ->
            if contactCount > 0 then
                Http.get
                    { url = "/api/subscription/calculate-tier/" ++ String.fromInt contactCount
                    , expect = Http.expectJson GotCustomTierPricing customTierPricingDecoder
                    }

            else
                Cmd.none

        Nothing ->
            Cmd.none


customTierPricingDecoder : Decoder CustomTierPricing
customTierPricingDecoder =
    field "success" Decode.bool
        |> Decode.andThen
            (\success ->
                if success then
                    field "pricing"
                        (Decode.map5 CustomTierPricing
                            (field "price" string)
                            (field "agentLimit" int)
                            (field "tierName" string)
                            (field "contactLimit" int)
                            (field "features" (list string))
                        )

                else
                    Decode.fail "API returned error"
            )


selectCustomTier : Model -> Cmd Msg
selectCustomTier model =
    case model.customTierPricing of
        Just pricing ->
            -- Create a custom tier ID based on the contact limit
            let
                customTierId =
                    "tier-custom-" ++ String.fromInt pricing.contactLimit
            in
            -- Simulate selecting this plan
            Cmd.none

        Nothing ->
            Cmd.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    stripeTableSelected StripeTableSelection


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
    -- No need to filter out enterprise options with the new contact-based pricing model
    tiers


canAddExtraResources : Maybe String -> Bool
canAddExtraResources selectedPlan =
    case selectedPlan of
        Just plan ->
            plan == "pro"

        -- Only Pro plan can add extra resources
        Nothing ->
            False


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


confirmationDecoder : Decoder { success : Bool, redirectUrl : String }
confirmationDecoder =
    Decode.map2 (\success redirectUrl -> { success = success, redirectUrl = redirectUrl })
        (Decode.field "success" Decode.bool)
        (Decode.field "redirectUrl" Decode.string)


getPlanLimitBanner : Model -> Html Msg
getPlanLimitBanner model =
    -- When user is on trial plan
    if model.currentAgentLimit > 0 && model.extraAgents > model.currentAgentLimit then
        LimitBanner.viewLimitBanner
            (AgentLimit (model.currentAgentLimit + model.extraAgents) model.currentAgentLimit)
            CloseBanner
        -- When user is on basic plan (which only allows 1 agent)

    else if model.currentTier == Just "basic" then
        LimitBanner.viewLimitBanner
            (CustomWarning
                "Basic Plan Limitations"
                "Your current Basic plan only supports 1 agent. Please upgrade to a higher tier plan to add more agents."
            )
            CloseBanner
        -- When approaching contact limit (subscription data from API)

    else if model.currentContactLimit > 0 && model.extraContacts >= (model.currentContactLimit * 1 // 10) then
        LimitBanner.viewLimitBanner
            (ContactLimit (model.currentContactLimit + model.extraContacts) model.currentContactLimit)
            CloseBanner
        -- Default for new users or when no specific warning is needed

    else
        LimitBanner.viewLimitBanner
            (TrialEnding "June 15, 2024")
            CloseBanner


viewPlan : Maybe String -> SubscriptionTier -> Html Msg
viewPlan selectedPlan tier =
    let
        isSelected =
            selectedPlan == Just tier.id

        selectedClass =
            if isSelected then
                "border-[#03045E] ring-2 ring-[#03045E]"

            else
                "border-gray-200 hover:border-gray-300"
    in
    div
        [ class ("border rounded-lg p-6 cursor-pointer " ++ selectedClass)
        , onClick (SelectPlan tier.id)
        ]
        [ -- Plan header with tier name
          div [ class "flex items-center justify-between" ]
            [ div [ class "flex items-center" ]
                [ h3 [ class "text-lg font-semibold text-gray-900" ]
                    [ text tier.name ]
                , if isSelected then
                    -- Checkmark for selected plan
                    div [ class "ml-2 text-[#03045E]" ]
                        [ viewCheckIcon ]

                  else
                    text ""
                ]
            , div [ class "rounded-full px-3 py-1 text-sm font-medium bg-blue-50 text-blue-700" ]
                [ text tier.price ]
            ]

        -- Limits
        , div [ class "mt-4 space-y-3" ]
            [ div [ class "flex items-start" ]
                [ div [ class "flex-shrink-0 h-5 w-5 text-green-500" ]
                    [ viewSmallCheckIcon ]
                , div [ class "ml-3 text-sm text-gray-500" ]
                    [ text ("Up to " ++ String.fromInt tier.contactLimit ++ " contacts") ]
                ]
            , div [ class "flex items-start" ]
                [ div [ class "flex-shrink-0 h-5 w-5 text-green-500" ]
                    [ viewSmallCheckIcon ]
                , div [ class "ml-3 text-sm text-gray-500" ]
                    [ text ("Up to " ++ String.fromInt tier.agentLimit ++ " agents") ]
                ]
            ]

        -- Features
        , div [ class "mt-6" ]
            [ h4 [ class "text-sm font-medium text-gray-900" ]
                [ text "Includes:" ]
            , ul [ class "mt-2 space-y-2" ]
                (List.map viewFeature tier.features)
            ]
        ]



-- Add this port for the embedded Stripe pricing table


port stripeTableSelected : (String -> msg) -> Sub msg
