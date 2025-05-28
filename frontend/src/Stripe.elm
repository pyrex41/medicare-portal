module Stripe exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Url exposing (Url)



-- MODEL


type alias Model =
    { planInfo : RemoteData PlanInfo
    , paymentMethod : RemoteData (Maybe PaymentMethod)
    , invoices : RemoteData (List Invoice)
    , showEditPaymentModal : Bool
    , downloadingInvoiceId : Maybe String
    }


type RemoteData a
    = NotAsked
    | Loading
    | Success a
    | Failure Http.Error


type alias PlanInfo =
    { name : String
    , contactCount : Int
    , pricePerMonth : Float
    , totalClients : Int
    }


type alias PaymentMethod =
    { id : String
    , type_ : String
    , last4 : String
    , brand : String
    , expiryMonth : Int
    , expiryYear : Int
    , email : String
    }


type alias Invoice =
    { id : String
    , number : String
    , date : String
    , status : InvoiceStatus
    , amount : Float
    , currency : String
    , plan : String
    , pdfUrl : Maybe String
    }


type InvoiceStatus
    = Pending
    | Paid
    | Failed


init : Nav.Key -> Url -> ( Model, Cmd Msg )
init key url =
    ( { planInfo = Loading
      , paymentMethod = Loading
      , invoices = Loading
      , showEditPaymentModal = False
      , downloadingInvoiceId = Nothing
      }
    , Cmd.batch
        [ fetchPlanInfo
        , fetchPaymentMethod
        , fetchInvoices
        ]
    )



-- UPDATE


type Msg
    = GotPlanInfo (Result Http.Error PlanInfo)
    | GotPaymentMethod (Result Http.Error (Maybe PaymentMethod))
    | GotInvoices (Result Http.Error (List Invoice))
    | ClickedEditPayment
    | CloseEditPaymentModal
    | ClickedDownloadInvoice String
    | GotInvoiceDownloadUrl (Result Http.Error String)
    | ClickedAnnualPlan


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPlanInfo result ->
            ( { model | planInfo = remoteDataFromResult result }, Cmd.none )

        GotPaymentMethod result ->
            ( { model | paymentMethod = remoteDataFromResult result }, Cmd.none )

        GotInvoices result ->
            ( { model | invoices = remoteDataFromResult result }, Cmd.none )

        ClickedEditPayment ->
            ( { model | showEditPaymentModal = True }, Cmd.none )

        CloseEditPaymentModal ->
            ( { model | showEditPaymentModal = False }, Cmd.none )

        ClickedDownloadInvoice invoiceId ->
            ( { model | downloadingInvoiceId = Just invoiceId }
            , downloadInvoice invoiceId
            )

        GotInvoiceDownloadUrl result ->
            case result of
                Ok url ->
                    ( { model | downloadingInvoiceId = Nothing }
                    , Nav.load url
                    )

                Err _ ->
                    ( { model | downloadingInvoiceId = Nothing }, Cmd.none )

        ClickedAnnualPlan ->
            -- TODO: Implement annual plan upgrade
            ( model, Cmd.none )


remoteDataFromResult : Result Http.Error a -> RemoteData a
remoteDataFromResult result =
    case result of
        Ok data ->
            Success data

        Err error ->
            Failure error



-- HTTP


fetchPlanInfo : Cmd Msg
fetchPlanInfo =
    Http.get
        { url = "/api/billing/plan"
        , expect = Http.expectJson GotPlanInfo planInfoDecoder
        }


fetchPaymentMethod : Cmd Msg
fetchPaymentMethod =
    Http.get
        { url = "/api/billing/payment-method"
        , expect = Http.expectJson GotPaymentMethod paymentMethodDecoder
        }


fetchInvoices : Cmd Msg
fetchInvoices =
    Http.get
        { url = "/api/billing/invoices"
        , expect = Http.expectJson GotInvoices invoicesDecoder
        }


downloadInvoice : String -> Cmd Msg
downloadInvoice invoiceId =
    Http.get
        { url = "/api/billing/invoices/" ++ invoiceId ++ "/download"
        , expect = Http.expectJson GotInvoiceDownloadUrl downloadUrlDecoder
        }


planInfoDecoder : Decoder PlanInfo
planInfoDecoder =
    Decode.field "data"
        (Decode.map4 PlanInfo
            (Decode.field "name" Decode.string)
            (Decode.field "contactCount" Decode.int)
            (Decode.field "pricePerMonth" Decode.float)
            (Decode.field "totalClients" Decode.int)
        )


paymentMethodDecoder : Decoder (Maybe PaymentMethod)
paymentMethodDecoder =
    Decode.field "data"
        (Decode.nullable
            (Decode.map7 PaymentMethod
                (Decode.field "id" Decode.string)
                (Decode.field "type" Decode.string)
                (Decode.field "last4" Decode.string)
                (Decode.field "brand" Decode.string)
                (Decode.field "expiryMonth" Decode.int)
                (Decode.field "expiryYear" Decode.int)
                (Decode.field "email" Decode.string)
            )
        )


invoicesDecoder : Decoder (List Invoice)
invoicesDecoder =
    Decode.field "data" (Decode.list invoiceDecoder)


invoiceDecoder : Decoder Invoice
invoiceDecoder =
    Decode.map8 Invoice
        (Decode.field "id" Decode.string)
        (Decode.field "number" Decode.string)
        (Decode.field "date" Decode.string)
        (Decode.field "status" invoiceStatusDecoder)
        (Decode.field "amount" Decode.float)
        (Decode.field "currency" Decode.string)
        (Decode.field "plan" Decode.string)
        (Decode.maybe (Decode.field "pdfUrl" Decode.string))


invoiceStatusDecoder : Decoder InvoiceStatus
invoiceStatusDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "pending" ->
                        Decode.succeed Pending

                    "paid" ->
                        Decode.succeed Paid

                    "failed" ->
                        Decode.succeed Failed

                    _ ->
                        Decode.fail ("Unknown invoice status: " ++ str)
            )


downloadUrlDecoder : Decoder String
downloadUrlDecoder =
    Decode.at [ "data", "url" ] Decode.string



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Billing"
    , body =
        [ div [ class "min-h-screen bg-gray-50" ]
            [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
                [ h1 [ class "text-3xl font-bold text-gray-900 mb-8" ] [ text "Billing" ]
                , div [ class "space-y-6" ]
                    [ viewPlanSettings model.planInfo
                    , viewPaymentMethod model.paymentMethod
                    , viewInvoices model.invoices model.downloadingInvoiceId
                    ]
                ]
            ]
        , if model.showEditPaymentModal then
            viewEditPaymentModal

          else
            text ""
        ]
    }


viewPlanSettings : RemoteData PlanInfo -> Html Msg
viewPlanSettings planInfo =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ div [ class "mb-4" ]
            [ h2 [ class "text-lg font-semibold text-gray-900" ] [ text "Plan Settings" ]
            , p [ class "text-sm text-gray-500" ] [ text "Update payment methods and see past invoices" ]
            ]
        , case planInfo of
            Loading ->
                div [ class "animate-pulse" ]
                    [ div [ class "h-4 bg-gray-200 rounded w-1/4 mb-2" ] []
                    , div [ class "h-8 bg-gray-200 rounded w-1/3" ] []
                    ]

            Success info ->
                div []
                    [ div [ class "flex items-baseline justify-between mb-4" ]
                        [ div []
                            [ h3 [ class "text-xl font-medium text-gray-900" ] [ text info.name ]
                            , p [ class "text-sm text-gray-500" ] [ text "Simply charged per contact, each month" ]
                            ]
                        , div [ class "text-right" ]
                            [ span [ class "text-3xl font-bold text-gray-900" ] [ text ("$" ++ String.fromFloat info.pricePerMonth) ]
                            , span [ class "text-gray-500 ml-1" ] [ text "per month" ]
                            ]
                        ]
                    , div [ class "mb-4" ]
                        [ div [ class "flex justify-between text-sm mb-1" ]
                            [ span [ class "text-gray-600" ] [ text (String.fromInt info.contactCount ++ " of " ++ String.fromInt info.totalClients ++ " Clients") ]
                            ]
                        , div [ class "w-full bg-gray-200 rounded-full h-2" ]
                            [ div
                                [ class "bg-blue-600 h-2 rounded-full"
                                , style "width" (String.fromFloat (toFloat info.contactCount / toFloat info.totalClients * 100) ++ "%")
                                ]
                                []
                            ]
                        ]
                    , button
                        [ class "text-blue-600 hover:text-blue-700 text-sm font-medium flex items-center"
                        , onClick ClickedAnnualPlan
                        ]
                        [ text "Want an Annual Plan?"
                        , svg [ class "ml-1 w-4 h-4", SvgAttr.fill "none", SvgAttr.stroke "currentColor", SvgAttr.viewBox "0 0 24 24" ]
                            [ path [ SvgAttr.strokeLinecap "round", SvgAttr.strokeLinejoin "round", SvgAttr.strokeWidth "2", SvgAttr.d "M9 5l7 7-7 7" ] []
                            ]
                        ]
                    ]

            Failure _ ->
                div [ class "text-red-600" ] [ text "Failed to load plan information" ]

            NotAsked ->
                text ""
        ]


viewPaymentMethod : RemoteData (Maybe PaymentMethod) -> Html Msg
viewPaymentMethod paymentMethod =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ div [ class "mb-4" ]
            [ h2 [ class "text-lg font-semibold text-gray-900" ] [ text "Payment method" ]
            , p [ class "text-sm text-gray-500" ] [ text "Change how you pay for your plan." ]
            ]
        , case paymentMethod of
            Loading ->
                div [ class "animate-pulse" ]
                    [ div [ class "h-12 bg-gray-200 rounded" ] [] ]

            Success maybePm ->
                case maybePm of
                    Just pm ->
                        div [ class "flex items-center justify-between p-4 border border-gray-200 rounded-lg" ]
                            [ div [ class "flex items-center" ]
                                [ viewCardIcon pm.brand
                                , div [ class "ml-3" ]
                                    [ p [ class "text-sm font-medium text-gray-900" ]
                                        [ text (formatCardBrand pm.brand ++ " ending in " ++ pm.last4) ]
                                    , p [ class "text-sm text-gray-500" ]
                                        [ text ("Expiry " ++ String.padLeft 2 '0' (String.fromInt pm.expiryMonth) ++ "/" ++ String.fromInt pm.expiryYear) ]
                                    , p [ class "text-sm text-gray-500" ] [ text pm.email ]
                                    ]
                                ]
                            , button
                                [ class "text-blue-600 hover:text-blue-700 text-sm font-medium"
                                , onClick ClickedEditPayment
                                ]
                                [ text "Edit" ]
                            ]

                    Nothing ->
                        div [ class "text-center py-4" ]
                            [ p [ class "text-gray-500 mb-2" ] [ text "No payment method on file" ]
                            , button
                                [ class "text-blue-600 hover:text-blue-700 text-sm font-medium"
                                , onClick ClickedEditPayment
                                ]
                                [ text "Add payment method" ]
                            ]

            Failure _ ->
                div [ class "text-red-600" ] [ text "Failed to load payment method" ]

            NotAsked ->
                text ""
        ]


viewInvoices : RemoteData (List Invoice) -> Maybe String -> Html Msg
viewInvoices invoices downloadingId =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-semibold text-gray-900 mb-4" ] [ text "Invoices" ]
        , p [ class "text-sm text-gray-500 mb-4" ] [ text "Click Download on the far right to receive a pdf of the invoice." ]
        , case invoices of
            Loading ->
                div [ class "animate-pulse space-y-2" ]
                    [ div [ class "h-12 bg-gray-200 rounded" ] []
                    , div [ class "h-12 bg-gray-200 rounded" ] []
                    , div [ class "h-12 bg-gray-200 rounded" ] []
                    ]

            Success invoiceList ->
                if List.isEmpty invoiceList then
                    div [ class "text-center py-8 text-gray-500" ] [ text "No invoices yet" ]

                else
                    div [ class "overflow-x-auto" ]
                        [ table [ class "min-w-full divide-y divide-gray-200" ]
                            [ thead [ class "bg-gray-50" ]
                                [ tr []
                                    [ th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Invoice" ]
                                    , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Billing date" ]
                                    , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Status" ]
                                    , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Amount" ]
                                    , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Plan" ]
                                    , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "" ]
                                    ]
                                ]
                            , tbody [ class "bg-white divide-y divide-gray-200" ]
                                (List.map (viewInvoiceRow downloadingId) invoiceList)
                            ]
                        ]

            Failure _ ->
                div [ class "text-red-600" ] [ text "Failed to load invoices" ]

            NotAsked ->
                text ""
        ]


viewInvoiceRow : Maybe String -> Invoice -> Html Msg
viewInvoiceRow downloadingId invoice =
    tr []
        [ td [ class "px-6 py-4 whitespace-nowrap" ]
            [ div [ class "flex items-center" ]
                [ svg [ class "w-5 h-5 text-gray-400 mr-2", SvgAttr.fill "none", SvgAttr.stroke "currentColor", SvgAttr.viewBox "0 0 24 24" ]
                    [ path [ SvgAttr.strokeLinecap "round", SvgAttr.strokeLinejoin "round", SvgAttr.strokeWidth "2", SvgAttr.d "M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" ] []
                    ]
                , span [ class "text-sm text-gray-900" ] [ text ("Invoice " ++ invoice.number) ]
                ]
            ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500" ]
            [ text (formatDate invoice.date) ]
        , td [ class "px-6 py-4 whitespace-nowrap" ]
            [ viewInvoiceStatus invoice.status ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ]
            [ text (invoice.currency ++ " $" ++ String.fromFloat invoice.amount) ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500" ]
            [ text invoice.plan ]
        , td [ class "px-6 py-4 whitespace-nowrap text-right text-sm font-medium" ]
            [ if downloadingId == Just invoice.id then
                span [ class "text-gray-400" ] [ text "Downloading..." ]

              else
                button
                    [ class "text-blue-600 hover:text-blue-900"
                    , onClick (ClickedDownloadInvoice invoice.id)
                    ]
                    [ text "Download" ]
            ]
        ]


viewInvoiceStatus : InvoiceStatus -> Html msg
viewInvoiceStatus status =
    case status of
        Pending ->
            span [ class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800" ]
                [ span [ class "w-2 h-2 bg-yellow-400 rounded-full mr-1.5" ] []
                , text "Pending"
                ]

        Paid ->
            span [ class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800" ]
                [ span [ class "w-2 h-2 bg-green-400 rounded-full mr-1.5" ] []
                , text "Paid"
                ]

        Failed ->
            span [ class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800" ]
                [ span [ class "w-2 h-2 bg-red-400 rounded-full mr-1.5" ] []
                , text "Failed"
                ]


viewCardIcon : String -> Html msg
viewCardIcon brand =
    let
        iconClass =
            "w-10 h-6 flex items-center justify-center rounded border"

        brandClass =
            case String.toLower brand of
                "visa" ->
                    "bg-blue-50 border-blue-200 text-blue-600"

                "mastercard" ->
                    "bg-red-50 border-red-200 text-red-600"

                "amex" ->
                    "bg-blue-50 border-blue-200 text-blue-600"

                _ ->
                    "bg-gray-50 border-gray-200 text-gray-600"
    in
    div [ class (iconClass ++ " " ++ brandClass) ]
        [ text (String.toUpper (String.left 4 brand)) ]


viewEditPaymentModal : Html Msg
viewEditPaymentModal =
    div [ class "fixed inset-0 bg-gray-500 bg-opacity-75 flex items-center justify-center z-50" ]
        [ div [ class "bg-white rounded-lg p-6 max-w-md w-full" ]
            [ div [ class "flex justify-between items-center mb-4" ]
                [ h3 [ class "text-lg font-medium text-gray-900" ] [ text "Update Payment Method" ]
                , button
                    [ class "text-gray-400 hover:text-gray-500"
                    , onClick CloseEditPaymentModal
                    ]
                    [ text "✕" ]
                ]
            , p [ class "text-sm text-gray-500 mb-4" ]
                [ text "This will open Stripe's secure payment form to update your payment method." ]
            , div [ class "flex justify-end space-x-3" ]
                [ button
                    [ class "px-4 py-2 border border-gray-300 rounded-md text-sm font-medium text-gray-700 hover:bg-gray-50"
                    , onClick CloseEditPaymentModal
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "px-4 py-2 bg-blue-600 text-white rounded-md text-sm font-medium hover:bg-blue-700"
                    ]
                    [ text "Continue to Stripe" ]
                ]
            ]
        ]



-- HELPERS


formatCardBrand : String -> String
formatCardBrand brand =
    case String.toLower brand of
        "visa" ->
            "Visa"

        "mastercard" ->
            "Mastercard"

        "amex" ->
            "American Express"

        _ ->
            brand


formatDate : String -> String
formatDate isoDate =
    -- Simple date formatting - you might want to use a proper date library
    String.left 10 isoDate



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
