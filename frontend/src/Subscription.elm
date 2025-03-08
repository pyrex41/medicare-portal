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
    }


type Tab
    = PlanTab
    | PaymentTab
    | BillingTab


type alias SubscriptionData =
    { plan : String
    , status : String
    , nextBillingDate : String
    , price : Float
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
      }
    , Cmd.batch
        [ fetchSubscriptionData
        , fetchPaymentMethods
        , fetchBillingHistory
        ]
    )



-- UPDATE


type Msg
    = GotSubscriptionData (Result Http.Error SubscriptionData)
    | GotPaymentMethods (Result Http.Error (List PaymentMethod))
    | GotBillingHistory (Result Http.Error (List BillingRecord))
    | ChangePlan String
    | ChangePlanResult (Result Http.Error SubscriptionData)
    | AddPaymentMethod
    | RemovePaymentMethod String
    | SetDefaultPaymentMethod String
    | PaymentMethodUpdated (Result Http.Error (List PaymentMethod))
    | DownloadInvoice String
    | ChangeTab Tab
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSubscriptionData (Ok data) ->
            ( { model | subscriptionData = Just data, isLoading = False }, Cmd.none )

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

        ChangePlan plan ->
            ( { model | isLoading = True }, changeSubscriptionPlan plan )

        ChangePlanResult (Ok data) ->
            ( { model | subscriptionData = Just data, isLoading = False }, Cmd.none )

        ChangePlanResult (Err _) ->
            ( { model | error = Just "Failed to change subscription plan", isLoading = False }, Cmd.none )

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
                                [ text "You can change your subscription plan at any time" ]
                            ]
                        , div [ class "px-3 py-1 bg-blue-50 text-blue-700 rounded-full text-sm font-medium" ]
                            [ text subscription.status ]
                        ]
                    , div [ class "border-t border-gray-200 pt-4" ]
                        [ div [ class "flex items-baseline" ]
                            [ span [ class "text-3xl font-bold text-gray-900" ]
                                [ text ("$" ++ String.fromFloat subscription.price) ]
                            , span [ class "ml-1 text-gray-500" ]
                                [ text "/month" ]
                            ]
                        , div [ class "mt-1 text-sm text-gray-500" ]
                            [ text ("Next billing date: " ++ subscription.nextBillingDate) ]
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
                            [ text "Available Plans" ]
                        , div [ class "grid grid-cols-3 gap-4" ]
                            [ viewPlanCard "Basic" 29.99 [ "Up to 5 agents", "Email notifications", "Basic reporting" ] subscription.plan
                            , viewPlanCard "Professional" 49.99 [ "Up to 15 agents", "All Basic features", "Advanced reporting", "API access" ] subscription.plan
                            , viewPlanCard "Enterprise" 99.99 [ "Unlimited agents", "All Professional features", "Dedicated support", "Custom integrations" ] subscription.plan
                            ]
                        ]
                    ]

            Nothing ->
                div [ class "text-center text-gray-500 py-8" ]
                    [ text "No subscription data available" ]
        ]


viewPlanCard : String -> Float -> List String -> String -> Html Msg
viewPlanCard plan price features currentPlan =
    let
        isCurrentPlan =
            plan == currentPlan
    in
    div
        [ class
            ("border rounded-lg p-4 "
                ++ (if isCurrentPlan then
                        "border-[#03045E] bg-blue-50"

                    else
                        "border-gray-200"
                   )
            )
        ]
        [ div [ class "flex justify-between items-center mb-2" ]
            [ h4 [ class "font-medium text-gray-900" ]
                [ text plan ]
            , if isCurrentPlan then
                span [ class "px-2 py-1 bg-[#03045E] text-white text-xs rounded-full" ]
                    [ text "Current" ]

              else
                text ""
            ]
        , div [ class "flex items-baseline mb-4" ]
            [ span [ class "text-xl font-bold text-gray-900" ]
                [ text ("$" ++ String.fromFloat price) ]
            , span [ class "ml-1 text-gray-500 text-sm" ]
                [ text "/month" ]
            ]
        , ul [ class "text-sm space-y-1 mb-4" ]
            (List.map
                (\feature ->
                    li [ class "flex items-start" ]
                        [ div [ class "mr-1 text-green-500 mt-0.5 flex-shrink-0" ]
                            [ viewSmallCheckIcon ]
                        , text feature
                        ]
                )
                features
            )
        , if isCurrentPlan then
            button
                [ class "w-full px-3 py-2 text-sm font-medium text-gray-500 bg-gray-100 rounded-md cursor-not-allowed"
                , disabled True
                ]
                [ text "Current Plan" ]

          else
            button
                [ class "w-full px-3 py-2 text-sm font-medium text-white bg-[#03045E] rounded-md hover:bg-opacity-90"
                , onClick (ChangePlan plan)
                ]
                [ text "Switch to Plan" ]
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
                            , th [ class "px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider" ]
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
        [ td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ]
            [ text record.date ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ]
            [ text record.description ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ]
            [ text ("$" ++ String.fromFloat record.amount) ]
        , td [ class "px-6 py-4 whitespace-nowrap" ]
            [ span
                [ class
                    ("px-2 py-1 text-xs rounded-full "
                        ++ (if record.status == "Paid" then
                                "bg-green-100 text-green-800"

                            else if record.status == "Failed" then
                                "bg-red-100 text-red-800"

                            else
                                "bg-yellow-100 text-yellow-800"
                           )
                    )
                ]
                [ text record.status ]
            ]
        , td [ class "px-6 py-4 whitespace-nowrap text-right text-sm font-medium" ]
            [ case record.invoiceUrl of
                Just url ->
                    button
                        [ class "text-blue-600 hover:text-blue-900"
                        , onClick (DownloadInvoice url)
                        ]
                        [ text "Download" ]

                Nothing ->
                    text ""
            ]
        ]



-- Helper Functions and Icons


viewCheckIcon : Html msg
viewCheckIcon =
    svg [ viewBox "0 0 20 20", fill "currentColor", class "h-5 w-5" ]
        [ path [ fillRule "evenodd", d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z", clipRule "evenodd" ] []
        ]


viewSmallCheckIcon : Html msg
viewSmallCheckIcon =
    svg [ viewBox "0 0 20 20", fill "currentColor", class "h-4 w-4" ]
        [ path [ fillRule "evenodd", d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z", clipRule "evenodd" ] []
        ]



-- HTTP


fetchSubscriptionData : Cmd Msg
fetchSubscriptionData =
    -- In a real implementation, this would call an API
    -- For now, return mock data
    Cmd.none


fetchPaymentMethods : Cmd Msg
fetchPaymentMethods =
    -- In a real implementation, this would call an API
    -- For now, return mock data
    Cmd.none


fetchBillingHistory : Cmd Msg
fetchBillingHistory =
    -- In a real implementation, this would call an API
    -- For now, return mock data
    Cmd.none


changeSubscriptionPlan : String -> Cmd Msg
changeSubscriptionPlan plan =
    -- In a real implementation, this would call an API
    -- For now, just return a success
    Cmd.none


removePaymentMethod : String -> Cmd Msg
removePaymentMethod id =
    -- In a real implementation, this would call an API
    Cmd.none


setDefaultPaymentMethod : String -> Cmd Msg
setDefaultPaymentMethod id =
    -- In a real implementation, this would call an API
    Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
