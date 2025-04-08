module Subscription exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, viewBox)
import Time



-- MODEL


type alias Model =
    { subscriptionData : Maybe SubscriptionData
    , paymentMethods : List PaymentMethod
    , billingHistory : List BillingRecord
    , isLoading : Bool
    , error : Maybe String
    , activeTab : Tab
    , contactCount : Int
    , autoUpgradeLimit : Int
    , upgradeModalOpen : Bool
    , selectedTier : Int
    }


type Tab
    = PlanTab
    | PaymentTab
    | BillingTab


type alias SubscriptionData =
    { id : String
    , status : String
    , tier : Int
    , contactLimit : Int
    , currentPeriodEnd : String
    , cancelAtPeriodEnd : Bool
    , features : List String
    }


type alias PaymentMethod =
    { id : String
    , brand : String
    , last4 : String
    , expiryMonth : Int
    , expiryYear : Int
    , isDefault : Bool
    }


type alias BillingRecord =
    { id : String
    , date : String
    , amount : Float
    , status : String
    , description : String
    , invoiceUrl : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { subscriptionData = Nothing
      , paymentMethods = []
      , billingHistory = []
      , isLoading = True
      , error = Nothing
      , activeTab = PlanTab
      , contactCount = 0
      , autoUpgradeLimit = 0
      , upgradeModalOpen = False
      , selectedTier = 1
      }
    , Cmd.batch
        [ fetchSubscriptionData
        , fetchPaymentMethods
        , fetchBillingHistory
        ]
    )



-- UPDATE


type Msg
    = GotSubscriptionData (Result Http.Error SubscriptionDataResponse)
    | GotPaymentMethods (Result Http.Error (List PaymentMethod))
    | GotBillingHistory (Result Http.Error (List BillingRecord))
    | ChangeTier Int
    | UpgradeTier Int
    | UpgradeTierResult (Result Http.Error UpgradeResponse)
    | AddPaymentMethod
    | RemovePaymentMethod String
    | SetDefaultPaymentMethod String
    | PaymentMethodUpdated (Result Http.Error (List PaymentMethod))
    | DownloadInvoice String
    | ChangeTab Tab
    | OpenUpgradeModal Int
    | CloseUpgradeModal
    | SetAutoUpgradeLimit Int
    | AutoUpgradeLimitUpdated (Result Http.Error AutoUpgradeResponse)
    | CreateCheckoutSession Int
    | GotCheckoutSession (Result Http.Error CheckoutResponse)
    | NoOp


type alias SubscriptionDataResponse =
    { subscription : Maybe SubscriptionData
    , contactCount : Int
    , autoUpgradeLimit : Int
    }


type alias UpgradeResponse =
    { success : Bool
    , previousTier : Maybe Int
    , newTier : Maybe Int
    , contactLimit : Maybe Int
    , message : Maybe String
    }


type alias AutoUpgradeResponse =
    { success : Bool
    , autoUpgradeLimit : Int
    }


type alias CheckoutResponse =
    { clientSecret : String
    , subscriptionId : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSubscriptionData (Ok response) ->
            ( { model
                | subscriptionData = response.subscription
                , contactCount = response.contactCount
                , autoUpgradeLimit = response.autoUpgradeLimit
                , isLoading = False
              }
            , Cmd.none
            )

        GotSubscriptionData (Err _) ->
            ( { model | error = Just "Failed to load subscription data", isLoading = False }, Cmd.none )

        GotPaymentMethods (Ok methods) ->
            ( { model | paymentMethods = methods, isLoading = False }, Cmd.none )

        GotPaymentMethods (Err _) ->
            ( { model | error = Just "Failed to load payment methods", isLoading = False }, Cmd.none )

        GotBillingHistory (Ok history) ->
            ( { model | billingHistory = history, isLoading = False }, Cmd.none )

        GotBillingHistory (Err _) ->
            ( { model | error = Just "Failed to load billing history", isLoading = False }, Cmd.none )

        ChangeTier tier ->
            ( { model | selectedTier = tier }, Cmd.none )

        OpenUpgradeModal tier ->
            ( { model | upgradeModalOpen = True, selectedTier = tier }, Cmd.none )

        CloseUpgradeModal ->
            ( { model | upgradeModalOpen = False }, Cmd.none )

        UpgradeTier tier ->
            ( { model | isLoading = True, upgradeModalOpen = False }
            , upgradeTier tier
            )

        UpgradeTierResult (Ok response) ->
            if response.success then
                case model.subscriptionData of
                    Just subscription ->
                        let
                            updatedSubscription =
                                { subscription
                                    | tier = Maybe.withDefault subscription.tier response.newTier
                                    , contactLimit = Maybe.withDefault subscription.contactLimit response.contactLimit
                                }
                        in
                        ( { model
                            | subscriptionData = Just updatedSubscription
                            , isLoading = False
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        -- Should not happen, but handle it gracefully
                        ( { model | isLoading = False }
                        , fetchSubscriptionData
                        )

            else
                ( { model
                    | error = Just (Maybe.withDefault "Failed to upgrade subscription" response.message)
                    , isLoading = False
                  }
                , Cmd.none
                )

        UpgradeTierResult (Err _) ->
            ( { model | error = Just "Failed to upgrade subscription", isLoading = False }, Cmd.none )

        AddPaymentMethod ->
            -- In a real implementation, this would open a Stripe form or similar
            ( model, Cmd.none )

        RemovePaymentMethod id ->
            ( { model | isLoading = True }, removePaymentMethod id )

        SetDefaultPaymentMethod id ->
            ( { model | isLoading = True }, setDefaultPaymentMethod id )

        PaymentMethodUpdated (Ok methods) ->
            ( { model | paymentMethods = methods, isLoading = False }, Cmd.none )

        PaymentMethodUpdated (Err _) ->
            ( { model | error = Just "Failed to update payment method", isLoading = False }, Cmd.none )

        DownloadInvoice url ->
            -- This would trigger a download in a real implementation
            ( model, Cmd.none )

        ChangeTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        SetAutoUpgradeLimit limit ->
            ( { model | isLoading = True }
            , updateAutoUpgradeLimit limit
            )

        AutoUpgradeLimitUpdated (Ok response) ->
            if response.success then
                ( { model
                    | autoUpgradeLimit = response.autoUpgradeLimit
                    , isLoading = False
                  }
                , Cmd.none
                )

            else
                ( { model
                    | error = Just "Failed to update auto-upgrade limit"
                    , isLoading = False
                  }
                , Cmd.none
                )

        AutoUpgradeLimitUpdated (Err _) ->
            ( { model
                | error = Just "Failed to update auto-upgrade limit"
                , isLoading = False
              }
            , Cmd.none
            )

        CreateCheckoutSession tier ->
            ( { model | isLoading = True }
            , createCheckoutSession tier
            )

        GotCheckoutSession (Ok response) ->
            -- In a real implementation, this would redirect to Stripe checkout
            -- For now, we just update the state and let JavaScript handle it
            ( model, Cmd.none )

        GotCheckoutSession (Err _) ->
            ( { model
                | error = Just "Failed to create checkout session"
                , isLoading = False
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Subscription & Payments"
    , body =
        [ div [ class "min-h-screen bg-gray-50" ]
            [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
                [ h1 [ class "text-2xl font-semibold text-[#03045E] mb-6" ]
                    [ text "Subscription & Payments" ]
                , if model.isLoading then
                    viewLoading

                  else
                    div []
                        [ viewTabs model
                        , viewTabContent model
                        ]
                , if model.upgradeModalOpen then
                    viewUpgradeModal model

                  else
                    text ""
                ]
            ]
        ]
    }


viewTabs : Model -> Html Msg
viewTabs model =
    div [ class "border-b border-gray-200 mb-6" ]
        [ div [ class "flex -mb-px" ]
            [ viewTab "Subscription Plan" (model.activeTab == PlanTab) (ChangeTab PlanTab)
            , viewTab "Payment Methods" (model.activeTab == PaymentTab) (ChangeTab PaymentTab)
            , viewTab "Billing History" (model.activeTab == BillingTab) (ChangeTab BillingTab)
            ]
        ]


viewTab : String -> Bool -> Msg -> Html Msg
viewTab label isActive msg =
    button
        [ class
            ("px-4 py-2 font-medium text-sm "
                ++ (if isActive then
                        "border-b-2 border-[#03045E] text-[#03045E]"

                    else
                        "text-gray-500 hover:text-gray-700 hover:border-gray-300"
                   )
            )
        , onClick msg
        ]
        [ text label ]


viewTabContent : Model -> Html Msg
viewTabContent model =
    case model.activeTab of
        PlanTab ->
            viewSubscriptionPlan model

        PaymentTab ->
            viewPaymentMethods model

        BillingTab ->
            viewBillingHistory model


viewLoading : Html Msg
viewLoading =
    div [ class "flex justify-center items-center h-64" ]
        [ div [ class "animate-spin rounded-full h-8 w-8 border-4 border-[#03045E] border-t-transparent" ] [] ]


viewSubscriptionPlan : Model -> Html Msg
viewSubscriptionPlan model =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ case model.subscriptionData of
            Just subscription ->
                div [ class "space-y-6" ]
                    [ div [ class "flex justify-between items-center" ]
                        [ div []
                            [ h2 [ class "text-lg font-medium text-gray-900" ]
                                [ text "Current Plan" ]
                            , div [ class "mt-1 text-sm text-gray-500" ]
                                [ text "Contact-based subscription" ]
                            ]
                        , div [ class "px-3 py-1 bg-blue-50 text-blue-700 rounded-full text-sm font-medium" ]
                            [ text subscription.status ]
                        ]
                    , div [ class "border-t border-gray-200 pt-4" ]
                        [ div [ class "flex items-baseline" ]
                            [ span [ class "text-3xl font-bold text-gray-900" ]
                                [ text ("Tier " ++ String.fromInt subscription.tier) ]
                            , span [ class "ml-3 text-gray-500" ]
                                [ text (String.fromInt subscription.contactLimit ++ " contacts") ]
                            ]
                        , div [ class "mt-1 text-sm text-gray-500" ]
                            [ text ("Current usage: " ++ String.fromInt model.contactCount ++ " contacts") ]
                        , div [ class "mt-1 text-sm text-gray-500" ]
                            [ text ("Next billing date: " ++ subscription.currentPeriodEnd) ]
                        ]
                    , div [ class "border-t border-gray-200 pt-4" ]
                        [ h3 [ class "text-md font-medium text-gray-900 mb-2" ]
                            [ text "Features" ]
                        , ul [ class "space-y-2" ]
                            (List.map
                                (\feature ->
                                    li [ class "flex items-center text-gray-600" ]
                                        [ div [ class "mr-2 text-green-500" ]
                                            [ viewCheckIcon ]
                                        , text feature
                                        ]
                                )
                                subscription.features
                            )
                        ]
                    , div [ class "border-t border-gray-200 pt-4" ]
                        [ h3 [ class "text-md font-medium text-gray-900 mb-3" ]
                            [ text "Auto-Upgrade Settings" ]
                        , div [ class "flex items-center space-x-4" ]
                            [ div [ class "text-sm text-gray-700" ]
                                [ text "Automatically upgrade when contacts exceed limit up to: " ]
                            , select
                                [ class "form-select rounded-md border-gray-300 text-sm"
                                , onInput (\value -> SetAutoUpgradeLimit (Maybe.withDefault 0 (String.toInt value)))
                                , value (String.fromInt model.autoUpgradeLimit)
                                ]
                                [ option [ value "0" ] [ text "Disabled (require manual approval)" ]
                                , option [ value "1000" ] [ text "1,000 contacts" ]
                                , option [ value "1500" ] [ text "1,500 contacts" ]
                                , option [ value "2000" ] [ text "2,000 contacts" ]
                                , option [ value "2500" ] [ text "2,500 contacts" ]
                                , option [ value "5000" ] [ text "5,000 contacts" ]
                                ]
                            ]
                        ]
                    , div [ class "border-t border-gray-200 pt-4" ]
                        [ h3 [ class "text-md font-medium text-gray-900 mb-3" ]
                            [ text "Upgrade Your Plan" ]
                        , div [ class "grid grid-cols-3 gap-4" ]
                            (List.map
                                (\tier ->
                                    viewTierCard
                                        tier
                                        (tier * 500)
                                        (if tier == 1 then
                                            60

                                         else
                                            60 + ((tier - 1) * 40)
                                        )
                                        subscription.tier
                                )
                                [ 1, 2, 3, 4, 5, 10 ]
                            )
                        ]
                    ]

            Nothing ->
                div [ class "space-y-6" ]
                    [ div [ class "text-center text-gray-500 py-8" ]
                        [ text "No subscription active. Subscribe to start using the service." ]
                    , div [ class "grid grid-cols-3 gap-4" ]
                        (List.map
                            (\tier ->
                                viewTierCard
                                    tier
                                    (tier * 500)
                                    (if tier == 1 then
                                        60

                                     else
                                        60 + ((tier - 1) * 40)
                                    )
                                    0
                            )
                            [ 1, 2, 3, 4, 5, 10 ]
                        )
                    ]
        ]


viewTierCard : Int -> Int -> Int -> Int -> Html Msg
viewTierCard tier contactLimit price currentTier =
    let
        isCurrentTier =
            tier == currentTier
    in
    div
        [ class
            ("border rounded-lg p-4 "
                ++ (if isCurrentTier then
                        "border-[#03045E] bg-blue-50"

                    else
                        "border-gray-200"
                   )
            )
        ]
        [ div [ class "flex justify-between items-center mb-2" ]
            [ h4 [ class "font-medium text-gray-900" ]
                [ text ("Tier " ++ String.fromInt tier) ]
            , if isCurrentTier then
                span [ class "px-2 py-1 bg-[#03045E] text-white text-xs rounded-full" ]
                    [ text "Current" ]

              else
                text ""
            ]
        , div [ class "flex items-baseline mb-2" ]
            [ span [ class "text-xl font-bold text-gray-900" ]
                [ text ("$" ++ String.fromInt price) ]
            , span [ class "ml-1 text-gray-500 text-sm" ]
                [ text "/month" ]
            ]
        , div [ class "text-sm text-gray-600 mb-4" ]
            [ text (String.fromInt contactLimit ++ " contacts") ]
        , if isCurrentTier then
            button
                [ class "w-full px-3 py-2 text-sm font-medium text-gray-500 bg-gray-100 rounded-md cursor-not-allowed"
                , disabled True
                ]
                [ text "Current Plan" ]

          else if currentTier == 0 then
            button
                [ class "w-full px-3 py-2 text-sm font-medium text-white bg-[#03045E] rounded-md hover:bg-opacity-90"
                , onClick (CreateCheckoutSession tier)
                ]
                [ text "Subscribe" ]

          else if tier > currentTier then
            button
                [ class "w-full px-3 py-2 text-sm font-medium text-white bg-[#03045E] rounded-md hover:bg-opacity-90"
                , onClick (OpenUpgradeModal tier)
                ]
                [ text "Upgrade" ]

          else
            button
                [ class "w-full px-3 py-2 text-sm font-medium text-gray-500 bg-gray-100 rounded-md cursor-not-allowed"
                , disabled True
                ]
                [ text "Lower Tier" ]
        ]


viewUpgradeModal : Model -> Html Msg
viewUpgradeModal model =
    let
        currentTier =
            case model.subscriptionData of
                Just sub ->
                    sub.tier

                Nothing ->
                    0

        currentLimit =
            case model.subscriptionData of
                Just sub ->
                    sub.contactLimit

                Nothing ->
                    0

        newLimit =
            model.selectedTier * 500

        price =
            if model.selectedTier == 1 then
                60

            else
                60 + ((model.selectedTier - 1) * 40)

        currentPrice =
            if currentTier == 1 then
                60

            else
                60 + ((currentTier - 1) * 40)

        priceDifference =
            price - currentPrice
    in
    div [ class "fixed inset-0 bg-gray-500 bg-opacity-75 flex items-center justify-center z-50" ]
        [ div [ class "bg-white rounded-lg max-w-lg w-full mx-4" ]
            [ div [ class "p-6" ]
                [ h3 [ class "text-lg font-medium text-gray-900 mb-4" ]
                    [ text "Upgrade Subscription" ]
                , p [ class "text-gray-600 mb-4" ]
                    [ text ("You are upgrading from Tier " ++ String.fromInt currentTier ++ " (" ++ String.fromInt currentLimit ++ " contacts) to Tier " ++ String.fromInt model.selectedTier ++ " (" ++ String.fromInt newLimit ++ " contacts).") ]
                , p [ class "text-gray-600 mb-4" ]
                    [ text ("Your monthly price will increase by $" ++ String.fromInt priceDifference ++ ", from $" ++ String.fromInt currentPrice ++ " to $" ++ String.fromInt price ++ ".") ]
                , p [ class "text-gray-600 mb-6" ]
                    [ text "This change will take effect immediately, and you will be charged a prorated amount for the remainder of your billing cycle." ]
                , div [ class "flex justify-end space-x-4" ]
                    [ button
                        [ class "px-4 py-2 text-sm font-medium text-gray-700 bg-gray-100 rounded-md hover:bg-gray-200"
                        , onClick CloseUpgradeModal
                        ]
                        [ text "Cancel" ]
                    , button
                        [ class "px-4 py-2 text-sm font-medium text-white bg-[#03045E] rounded-md hover:bg-opacity-90"
                        , onClick (UpgradeTier model.selectedTier)
                        ]
                        [ text "Confirm Upgrade" ]
                    ]
                ]
            ]
        ]


viewPaymentMethods : Model -> Html Msg
viewPaymentMethods model =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ div [ class "flex justify-between items-center mb-4" ]
            [ h2 [ class "text-lg font-medium text-gray-900" ]
                [ text "Payment Methods" ]
            , button
                [ class "px-4 py-2 text-sm font-medium text-white bg-[#03045E] rounded-md hover:bg-opacity-90"
                , onClick AddPaymentMethod
                ]
                [ text "Add Payment Method" ]
            ]
        , if List.isEmpty model.paymentMethods then
            div [ class "text-center text-gray-500 py-8" ]
                [ text "No payment methods added yet" ]

          else
            div [ class "space-y-4" ]
                (List.map viewPaymentMethod model.paymentMethods)
        ]


viewPaymentMethod : PaymentMethod -> Html Msg
viewPaymentMethod method =
    div [ class "border rounded-lg p-4 flex justify-between items-center" ]
        [ div [ class "flex items-center" ]
            [ div [ class "flex-shrink-0 w-10 h-6 bg-gray-100 rounded flex items-center justify-center mr-3" ]
                [ text (String.left 1 method.brand) ]
            , div []
                [ div [ class "text-gray-900" ]
                    [ text (method.brand ++ " •••• " ++ method.last4) ]
                , div [ class "text-sm text-gray-500" ]
                    [ text ("Expires " ++ String.fromInt method.expiryMonth ++ "/" ++ String.fromInt method.expiryYear) ]
                ]
            ]
        , div [ class "flex items-center space-x-2" ]
            [ if method.isDefault then
                span [ class "px-2 py-1 bg-green-100 text-green-800 text-xs rounded-full" ]
                    [ text "Default" ]

              else
                button
                    [ class "text-sm text-blue-600 hover:text-blue-800"
                    , onClick (SetDefaultPaymentMethod method.id)
                    ]
                    [ text "Set Default" ]
            , button
                [ class "text-sm text-red-600 hover:text-red-800"
                , onClick (RemovePaymentMethod method.id)
                ]
                [ text "Remove" ]
            ]
        ]


viewBillingHistory : Model -> Html Msg
viewBillingHistory model =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium text-gray-900 mb-4" ]
            [ text "Billing History" ]
        , if List.isEmpty model.billingHistory then
            div [ class "text-center text-gray-500 py-8" ]
                [ text "No billing history available" ]

          else
            div [ class "overflow-x-auto" ]
                [ table [ class "min-w-full divide-y divide-gray-200" ]
                    [ thead [ class "bg-gray-50" ]
                        [ tr []
                            [ th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ]
                                [ text "Date" ]
                            , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ]
                                [ text "Description" ]
                            , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ]
                                [ text "Amount" ]
                            , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ]
                                [ text "Status" ]
                            , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ]
                                [ text "Actions" ]
                            ]
                        ]
                    , tbody [ class "bg-white divide-y divide-gray-200" ]
                        (List.map viewBillingRecord model.billingHistory)
                    ]
                ]
        ]


viewBillingRecord : BillingRecord -> Html Msg
viewBillingRecord record =
    tr []
        [ td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500" ]
            [ text record.date ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ]
            [ text record.description ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500" ]
            [ text ("$" ++ String.fromFloat record.amount) ]
        , td [ class "px-6 py-4 whitespace-nowrap" ]
            [ span
                [ class
                    ("px-2 py-1 text-xs rounded-full "
                        ++ (case record.status of
                                "Paid" ->
                                    "bg-green-100 text-green-800"

                                "Failed" ->
                                    "bg-red-100 text-red-800"

                                "Pending" ->
                                    "bg-yellow-100 text-yellow-800"

                                _ ->
                                    "bg-gray-100 text-gray-800"
                           )
                    )
                ]
                [ text record.status ]
            ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500" ]
            [ case record.invoiceUrl of
                Just url ->
                    button
                        [ class "text-blue-600 hover:text-blue-800"
                        , onClick (DownloadInvoice url)
                        ]
                        [ text "Download" ]

                Nothing ->
                    text "—"
            ]
        ]



-- Helper Functions and Icons


viewCheckIcon : Html msg
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


viewSmallCheckIcon : Html msg
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



-- HTTP


fetchSubscriptionData : Cmd Msg
fetchSubscriptionData =
    Http.get
        { url = "/api/subscription/status"
        , expect = Http.expectJson GotSubscriptionData subscriptionDataDecoder
        }


subscriptionDataDecoder : Decoder SubscriptionDataResponse
subscriptionDataDecoder =
    Decode.succeed SubscriptionDataResponse
        |> Pipeline.required "subscription" (Decode.nullable subscriptionDecoder)
        |> Pipeline.required "contactCount" Decode.int
        |> Pipeline.required "autoUpgradeLimit" Decode.int


subscriptionDecoder : Decoder SubscriptionData
subscriptionDecoder =
    Decode.succeed SubscriptionData
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "status" Decode.string
        |> Pipeline.required "tier" Decode.int
        |> Pipeline.required "contactLimit" Decode.int
        |> Pipeline.required "currentPeriodEnd" Decode.string
        |> Pipeline.required "cancelAtPeriodEnd" Decode.bool
        |> Pipeline.hardcoded [ "Contact-based pricing", "Email scheduling", "Analytics dashboard" ]


fetchPaymentMethods : Cmd Msg
fetchPaymentMethods =
    Http.get
        { url = "/api/payment-methods"
        , expect = Http.expectJson GotPaymentMethods (Decode.list paymentMethodDecoder)
        }


paymentMethodDecoder : Decoder PaymentMethod
paymentMethodDecoder =
    Decode.succeed PaymentMethod
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "brand" Decode.string
        |> Pipeline.required "last4" Decode.string
        |> Pipeline.required "expiryMonth" Decode.int
        |> Pipeline.required "expiryYear" Decode.int
        |> Pipeline.required "isDefault" Decode.bool


fetchBillingHistory : Cmd Msg
fetchBillingHistory =
    Http.get
        { url = "/api/billing-history"
        , expect = Http.expectJson GotBillingHistory (Decode.list billingRecordDecoder)
        }


billingRecordDecoder : Decoder BillingRecord
billingRecordDecoder =
    Decode.succeed BillingRecord
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "date" Decode.string
        |> Pipeline.required "amount" Decode.float
        |> Pipeline.required "status" Decode.string
        |> Pipeline.required "description" Decode.string
        |> Pipeline.optional "invoiceUrl" (Decode.nullable Decode.string) Nothing


upgradeTier : Int -> Cmd Msg
upgradeTier tier =
    Http.post
        { url = "/api/subscription/upgrade"
        , body = Http.jsonBody (Encode.object [ ( "newTier", Encode.int tier ) ])
        , expect = Http.expectJson UpgradeTierResult upgradeResponseDecoder
        }


upgradeResponseDecoder : Decoder UpgradeResponse
upgradeResponseDecoder =
    Decode.succeed UpgradeResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.optional "previousTier" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "newTier" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "contactLimit" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "message" (Decode.nullable Decode.string) Nothing


updateAutoUpgradeLimit : Int -> Cmd Msg
updateAutoUpgradeLimit limit =
    Http.post
        { url = "/api/subscription/auto-upgrade"
        , body = Http.jsonBody (Encode.object [ ( "autoUpgradeLimit", Encode.int limit ) ])
        , expect = Http.expectJson AutoUpgradeLimitUpdated autoUpgradeLimitDecoder
        }


autoUpgradeLimitDecoder : Decoder AutoUpgradeResponse
autoUpgradeLimitDecoder =
    Decode.succeed AutoUpgradeResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "autoUpgradeLimit" Decode.int


createCheckoutSession : Int -> Cmd Msg
createCheckoutSession tier =
    Http.post
        { url = "/api/subscription/checkout"
        , body = Http.jsonBody (Encode.object [ ( "contactTier", Encode.int tier ) ])
        , expect = Http.expectJson GotCheckoutSession checkoutResponseDecoder
        }


checkoutResponseDecoder : Decoder CheckoutResponse
checkoutResponseDecoder =
    Decode.succeed CheckoutResponse
        |> Pipeline.required "clientSecret" Decode.string
        |> Pipeline.required "subscriptionId" Decode.string


removePaymentMethod : String -> Cmd Msg
removePaymentMethod id =
    Http.post
        { url = "/api/payment-methods/remove"
        , body = Http.jsonBody (Encode.object [ ( "id", Encode.string id ) ])
        , expect = Http.expectJson PaymentMethodUpdated (Decode.list paymentMethodDecoder)
        }


setDefaultPaymentMethod : String -> Cmd Msg
setDefaultPaymentMethod id =
    Http.post
        { url = "/api/payment-methods/set-default"
        , body = Http.jsonBody (Encode.object [ ( "id", Encode.string id ) ])
        , expect = Http.expectJson PaymentMethodUpdated (Decode.list paymentMethodDecoder)
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
