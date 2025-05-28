module Stripe exposing (Model, Msg, init, subscriptions, update, view)

-- Alias for Svg.Attributes

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Svg exposing (path, svg)
import Svg.Attributes as SA
import Url exposing (Url)



-- MODEL


type alias Model =
    { planInfo : RemoteData PlanInfo
    , paymentMethod : RemoteData (Maybe PaymentMethod)
    , invoices : RemoteData (List Invoice)
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
    , totalClients : Int -- Overall limit
    , priceDescription : String
    , basePrice : Maybe Float -- Made Maybe for decoder convenience
    , contactsIncluded : Maybe Int
    , pricePerAdditionalContact : Maybe Float
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
        , expect = Http.expectJson GotPaymentMethod maybePaymentMethodDecoder
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
        (Decode.map7 PlanInfo
            (Decode.field "name" Decode.string)
            (Decode.field "contactCount" Decode.int)
            (Decode.field "totalClients" Decode.int)
            (Decode.field "priceDescription" Decode.string)
            (Decode.maybe (Decode.field "basePrice" Decode.float))
            -- Handling potentially missing fields
            (Decode.maybe (Decode.field "contactsIncluded" Decode.int))
            (Decode.maybe (Decode.field "pricePerAdditionalContact" Decode.float))
        )


paymentMethodDecoder : Decoder PaymentMethod
paymentMethodDecoder =
    Decode.map7 PaymentMethod
        (Decode.field "id" Decode.string)
        (Decode.field "type" Decode.string)
        (Decode.field "last4" Decode.string)
        (Decode.field "brand" Decode.string)
        (Decode.field "expiryMonth" Decode.int)
        (Decode.field "expiryYear" Decode.int)
        (Decode.field "email" Decode.string)


maybePaymentMethodDecoder : Decoder (Maybe PaymentMethod)
maybePaymentMethodDecoder =
    Decode.field "data" (Decode.nullable paymentMethodDecoder)


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
                    -- Main container for vertical spacing
                    [ div [ class "grid grid-cols-1 lg:grid-cols-2 gap-6" ]
                        -- Row for Plan Settings and Payment Method
                        [ viewPlanSettings model.planInfo
                        , viewPaymentMethod model.paymentMethod
                        ]
                    , viewInvoices model.invoices model.downloadingInvoiceId -- Invoices below, takes full width due to parent flow
                    ]
                ]
            ]
        ]
    }


viewPlanSettings : RemoteData PlanInfo -> Html Msg
viewPlanSettings planInfo =
    div [ class "bg-white shadow rounded-lg p-6 h-full flex flex-col" ]
        [ case planInfo of
            Loading ->
                div [ class "animate-pulse flex-grow flex flex-col justify-between" ]
                    [ div []
                        [ div [ class "flex items-center justify-between" ]
                            [ div [ class "h-6 bg-gray-200 rounded w-1/3" ] []
                            , div [ class "h-10 bg-gray-200 rounded w-1/4" ] []
                            ]
                        , div [ class "h-4 bg-gray-200 rounded w-3/4 mt-3" ] []
                        ]
                    , div [ class "h-4 bg-gray-200 rounded w-1/2 mt-4" ] []
                    ]

            Success info ->
                div [ class "flex-grow flex flex-col justify-between" ]
                    [ div []
                        [ div [ class "flex justify-between items-start" ]
                            [ div [ class "flex items-center gap-x-2" ]
                                [ h3 [ class "text-lg font-semibold text-gray-900" ] [ text info.name ]
                                , span [ class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800" ]
                                    [ text
                                        (String.fromInt info.contactCount
                                            ++ (if info.contactCount == 1 then
                                                    " Contact"

                                                else
                                                    " Contacts"
                                               )
                                        )
                                    ]
                                ]
                            , div [ class "text-right" ]
                                [ case info.basePrice of
                                    Just bp ->
                                        p [ class "text-4xl font-bold text-gray-900" ] [ text ("$" ++ String.fromFloat bp) ]

                                    Nothing ->
                                        p [ class "text-4xl font-bold text-gray-900" ] [ text "-" ]
                                , p [ class "text-sm text-gray-500" ] [ text "per month" ]
                                ]
                            ]
                        , p [ class "text-sm text-gray-500 mt-1" ] [ text (formatPriceDescriptionShort info) ]
                        ]
                    , div [ class "mt-4" ]
                        [ text "" ]
                    ]

            Failure httpError ->
                div [ class "text-red-600" ] [ text "Failed to load plan information. Please try again later." ]

            NotAsked ->
                text ""
        ]


viewPaymentMethod : RemoteData (Maybe PaymentMethod) -> Html Msg
viewPaymentMethod paymentMethodData =
    div [ class "bg-white shadow rounded-lg p-6 h-full flex flex-col" ]
        [ div [ class "mb-4" ]
            [ h2 [ class "text-lg font-semibold text-gray-900" ] [ text "Payment method" ]
            , p [ class "text-sm text-gray-500" ] [ text "Change how you pay for your plan." ]
            ]
        , div [ class "flex-grow flex flex-col justify-center items-center" ]
            [ case paymentMethodData of
                Loading ->
                    div [ class "animate-pulse w-full" ]
                        [ div [ class "flex items-center space-x-4 mb-4" ]
                            [ div [ class "h-10 w-16 bg-gray-200 rounded" ] []
                            , div [ class "flex-1 space-y-2 py-1" ]
                                [ div [ class "h-4 bg-gray-200 rounded w-3/4" ] []
                                , div [ class "h-4 bg-gray-200 rounded w-1/2" ] []
                                ]
                            ]
                        , div [ class "h-4 bg-gray-200 rounded w-3/4 mb-6" ] []
                        , div [ class "h-10 bg-blue-200 rounded w-1/2 mx-auto" ] []
                        ]

                Success maybePm ->
                    div [ class "w-full text-center" ]
                        [ case maybePm of
                            Just pm ->
                                div [ class "mb-6" ]
                                    [ div [ class "flex items-center justify-center space-x-3" ]
                                        [ viewCardIcon pm.brand
                                        , div [ class "text-sm text-left" ]
                                            [ p [ class "font-medium text-gray-700" ] [ text (formatCardBrand pm.brand ++ " ending in " ++ pm.last4) ]
                                            , p [ class "text-gray-500" ] [ text ("Expiry " ++ String.fromInt pm.expiryMonth ++ "/" ++ String.fromInt pm.expiryYear) ]
                                            ]
                                        ]
                                    , if String.isEmpty pm.email then
                                        text ""

                                      else
                                        p [ class "text-sm text-gray-500 mt-3" ] [ text pm.email ]
                                    ]

                            Nothing ->
                                text ""
                        , a
                            [ class "inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                            , href "https://billing.stripe.com/p/login/cNidR88q09BV2d32OA7Zu00"
                            , target "_blank"
                            ]
                            [ text "Manage Payment Method"
                            , svg [ SA.class "ml-2 -mr-0.5 h-4 w-4", SA.viewBox "0 0 20 20", SA.fill "currentColor", SA.stroke "currentColor", SA.strokeWidth "1" ]
                                [ path [ SA.fillRule "evenodd", SA.d "M6 14l8-8m0 0H6m8 0v8", SA.clipRule "evenodd" ] []
                                ]
                            ]
                        ]

                Failure _ ->
                    div [ class "text-red-600" ] [ text "Failed to load payment method information." ]

                NotAsked ->
                    text ""
            ]
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
                    p [ class "text-gray-500" ] [ text "No invoices yet." ]

                else
                    div [ class "overflow-x-auto" ]
                        [ table [ class "min-w-full divide-y divide-gray-200" ]
                            [ thead [ class "bg-gray-50" ]
                                [ tr []
                                    [ th [ scope "col", class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Invoice" ]
                                    , th [ scope "col", class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Date" ]
                                    , th [ scope "col", class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Status" ]
                                    , th [ scope "col", class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Amount" ]
                                    , th [ scope "col", class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Plan" ]
                                    , th [ scope "col", class "relative px-6 py-3" ] [ span [ class "sr-only" ] [ text "Download" ] ]
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
                [ svg [ SA.class "w-5 h-5 text-gray-400 mr-2", SA.fill "none", SA.stroke "currentColor", SA.viewBox "0 0 24 24" ]
                    [ path [ SA.strokeLinecap "round", SA.strokeLinejoin "round", SA.strokeWidth "2", SA.d "M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" ] []
                    ]
                , button
                    [ class "text-sm text-blue-600 hover:text-blue-900 hover:underline focus:outline-none"
                    , onClick (ClickedDownloadInvoice invoice.id)
                    , disabled (downloadingId == Just invoice.id) -- Disable if this invoice is downloading
                    ]
                    [ text ("Invoice " ++ invoice.number) ]
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


formatPriceDescription : PlanInfo -> String
formatPriceDescription info =
    case ( info.basePrice, info.contactsIncluded, info.pricePerAdditionalContact ) of
        ( Just bp, Just ci, Just ppac ) ->
            "$" ++ String.fromFloat bp ++ "/month for first " ++ String.fromInt ci ++ " contacts, then $" ++ String.fromFloat ppac ++ "/contact."

        ( Just bp, Nothing, Nothing ) ->
            -- Only base price known
            "$" ++ String.fromFloat bp ++ "/month"

        _ ->
            info.priceDescription


formatPriceDescriptionShort : PlanInfo -> String
formatPriceDescriptionShort info =
    if String.isEmpty info.priceDescription then
        "Simply charged per contact, each month"

    else if String.contains "$" info.priceDescription && String.contains "/month" info.priceDescription && String.contains "contacts" info.priceDescription then
        info.priceDescription

    else
        "Simply charged per contact, each month"



-- Fallback to the raw description from backend
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
