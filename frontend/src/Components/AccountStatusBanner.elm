module Components.AccountStatusBanner exposing (AccountStatusDetails, view)

import Html exposing (Html, a, button, div, h3, p, span, text)
import Html.Attributes exposing (attribute, class, href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder, bool, int, string)
import Json.Decode.Pipeline as Pipeline
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr exposing (clipRule, d, fill, fillRule, viewBox)



-- Account status types


type alias AccountStatus =
    String


type alias AccountStatusDetails =
    { status : AccountStatus
    , message : String
    , organizationId : Int
    , organizationName : String
    , organizationSlug : String
    , subscriptionTier : String
    , subscriptionStatus : String
    , agentLimit : Int
    , contactLimit : Int
    , currentAgentCount : Int
    , currentContactCount : Int
    , billingCycleEnd : Maybe String
    , paymentFailureCount : Int
    , paymentCompleted : Bool
    }



-- Decoder for account status details


accountStatusDetailsDecoder : Decoder AccountStatusDetails
accountStatusDetailsDecoder =
    Decode.succeed AccountStatusDetails
        |> Pipeline.required "status" string
        |> Pipeline.required "message" string
        |> Pipeline.required "organizationId" int
        |> Pipeline.required "organizationName" string
        |> Pipeline.required "organizationSlug" string
        |> Pipeline.required "subscriptionTier" string
        |> Pipeline.required "subscriptionStatus" string
        |> Pipeline.required "agentLimit" int
        |> Pipeline.required "contactLimit" int
        |> Pipeline.required "currentAgentCount" int
        |> Pipeline.required "currentContactCount" int
        |> Pipeline.optional "billingCycleEnd" (Decode.nullable string) Nothing
        |> Pipeline.required "paymentFailureCount" int
        |> Pipeline.required "paymentCompleted" bool



-- Function to determine banner color class based on status


getBannerColorClass : AccountStatus -> String
getBannerColorClass status =
    case status of
        "warning" ->
            "bg-yellow-50 border-yellow-200 text-yellow-800"

        "error" ->
            "bg-red-50 border-red-200 text-red-800"

        "success" ->
            "bg-green-50 border-green-200 text-green-800"

        "info" ->
            "bg-blue-50 border-blue-200 text-blue-800"

        _ ->
            "bg-gray-50 border-gray-200 text-gray-800"



-- Function to determine icon color class based on status


getIconColorClass : AccountStatus -> String
getIconColorClass status =
    case status of
        "warning" ->
            "text-yellow-600"

        "error" ->
            "text-red-600"

        "success" ->
            "text-green-600"

        "info" ->
            "text-blue-600"

        _ ->
            "text-gray-600"



-- Function to determine button color class based on status


getButtonColorClass : AccountStatus -> String
getButtonColorClass status =
    case status of
        "warning" ->
            "bg-yellow-100 hover:bg-yellow-200 text-yellow-700"

        "error" ->
            "bg-red-100 hover:bg-red-200 text-red-700"

        "success" ->
            "bg-green-100 hover:bg-green-200 text-green-700"

        "info" ->
            "bg-blue-100 hover:bg-blue-200 text-blue-700"

        _ ->
            "bg-gray-100 hover:bg-gray-200 text-gray-700"



-- Main view function for the account status banner


view : Maybe AccountStatusDetails -> msg -> Html msg
view maybeStatus closeMsg =
    case maybeStatus of
        Just status ->
            div
                [ class ("p-3 sm:p-4 mb-3 sm:mb-4 border rounded-lg flex flex-col sm:flex-row sm:items-start sm:justify-between " ++ getBannerColorClass status.status)
                , attribute "role" "alert"
                ]
                [ div [ class "flex items-start" ]
                    [ div [ class "flex-shrink-0" ]
                        [ div [ class ("p-1 rounded-full mr-2 sm:mr-3 " ++ getIconColorClass status.status) ]
                            [ svg
                                [ SvgAttr.class "w-4 h-4 sm:w-5 sm:h-5"
                                , SvgAttr.fill "currentColor"
                                , SvgAttr.viewBox "0 0 20 20"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    , div [ class "flex-grow pr-8 sm:pr-0" ]
                        [ div [ class "font-medium text-sm sm:text-base" ] [ text (getStatusTitle status.status) ]
                        , p [ class "text-xs sm:text-sm" ] [ text status.message ]
                        , viewLimitsInfo status
                        ]
                    ]
                , button
                    [ class ("p-1.5 rounded-lg absolute top-2 right-2 sm:static " ++ getButtonColorClass status.status)
                    , onClick closeMsg
                    ]
                    [ svg
                        [ SvgAttr.class "w-4 h-4"
                        , SvgAttr.fill "currentColor"
                        , SvgAttr.viewBox "0 0 20 20"
                        ]
                        [ path
                            [ SvgAttr.fillRule "evenodd"
                            , SvgAttr.d "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                            , SvgAttr.clipRule "evenodd"
                            ]
                            []
                        ]
                    ]
                ]

        Nothing ->
            div [] []



-- Helper function to display resource limits information


viewLimitsInfo : AccountStatusDetails -> Html msg
viewLimitsInfo status =
    if status.currentAgentCount >= status.agentLimit || status.currentContactCount >= status.contactLimit then
        div [ class "mt-1 sm:mt-2 text-xs sm:text-sm" ]
            [ if status.currentAgentCount >= status.agentLimit then
                div [ class "mb-1" ]
                    [ text ("Agents: " ++ String.fromInt status.currentAgentCount ++ "/" ++ String.fromInt status.agentLimit ++ " ")
                    , a [ href "/change-plan", class "underline" ] [ text "Upgrade" ]
                    ]

              else
                text ""
            , if status.currentContactCount >= status.contactLimit then
                div []
                    [ text ("Contacts: " ++ String.fromInt status.currentContactCount ++ "/" ++ String.fromInt status.contactLimit ++ " ")
                    , a [ href "/change-plan", class "underline" ] [ text "Upgrade" ]
                    ]

              else
                text ""
            ]

    else
        text ""



-- Helper function to get a human-readable status title


getStatusTitle : AccountStatus -> String
getStatusTitle status =
    case status of
        "warning" ->
            "Warning"

        "error" ->
            "Error"

        "success" ->
            "Success"

        "info" ->
            "Information"

        "limit_reached" ->
            "Resource Limit Reached"

        "payment_failed" ->
            "Payment Failed"

        "subscription_expiring" ->
            "Subscription Expiring"

        _ ->
            "Notice"
