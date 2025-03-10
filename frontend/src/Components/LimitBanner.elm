module Components.LimitBanner exposing (LimitWarning(..), Model, Msg, init, update, view, viewLimitBanner)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline as Pipeline
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, viewBox)


type LimitWarning
    = AgentLimit Int Int -- currentCount, maxAllowed
    | ContactLimit Int Int -- currentCount, maxAllowed
    | TrialEnding String -- date when trial ends
    | CustomWarning String String -- title, message


type alias Model =
    { warning : Maybe LimitWarning
    , limits : Maybe LimitInfo
    , error : Maybe String
    }


type alias LimitInfo =
    { tierId : String
    , tierName : String
    , agentLimit : Int
    , contactLimit : Int
    }


type Msg
    = GotLimits (Result Http.Error LimitInfo)
    | CloseBanner


init : ( Model, Cmd Msg )
init =
    ( { warning = Nothing
      , limits = Nothing
      , error = Nothing
      }
    , fetchLimits GotLimits
    )


fetchLimits : (Result Http.Error LimitInfo -> msg) -> Cmd msg
fetchLimits toMsg =
    Http.request
        { method = "GET"
        , url = "/api/organizations/my-subscription"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg limitInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


limitInfoDecoder : Decoder LimitInfo
limitInfoDecoder =
    Decode.succeed LimitInfo
        |> Pipeline.required "tierId" string
        |> Pipeline.required "tierName" string
        |> Pipeline.required "agentLimit" int
        |> Pipeline.required "contactLimit" int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLimits (Ok limitInfo) ->
            ( { model | limits = Just limitInfo }, Cmd.none )

        GotLimits (Err error) ->
            ( { model | error = Just (Debug.toString error) }, Cmd.none )

        CloseBanner ->
            ( { model | warning = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.warning of
        Nothing ->
            text ""

        Just warning ->
            viewLimitBanner warning CloseBanner


viewLimitBanner : LimitWarning -> msg -> Html msg
viewLimitBanner warning closeMsg =
    let
        ( title, message, upgradeText ) =
            case warning of
                AgentLimit current max ->
                    ( "Notice"
                    , "Your account has " ++ String.fromInt current ++ " agents, but your plan only allows for " ++ String.fromInt max ++ ". Please remove some agents or upgrade your plan."
                    , "Upgrade"
                    )

                ContactLimit current max ->
                    ( "Notice"
                    , "Your account has " ++ String.fromInt current ++ " contacts, but your plan only allows for " ++ String.fromInt max ++ ". Please upgrade your plan to add more contacts."
                    , "Upgrade"
                    )

                TrialEnding date ->
                    ( "Trial Ending Soon"
                    , "Your trial will end on " ++ date ++ ". Please upgrade your plan to continue using all features."
                    , "Upgrade"
                    )

                CustomWarning customTitle customMessage ->
                    ( customTitle
                    , customMessage
                    , "Upgrade"
                    )
    in
    div [ class "bg-amber-50 border-l-4 border-amber-400 p-4 mb-6" ]
        [ div [ class "flex justify-between" ]
            [ div [ class "flex" ]
                [ div [ class "flex-shrink-0" ]
                    [ div [ class "h-5 w-5 text-amber-400" ]
                        [ svg [ viewBox "0 0 20 20", fill "currentColor" ]
                            [ path [ fillRule "evenodd", d "M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z", clipRule "evenodd" ] []
                            ]
                        ]
                    ]
                , div [ class "ml-3" ]
                    [ p [ class "text-sm text-amber-700 font-medium" ]
                        [ text title ]
                    , p [ class "text-sm text-amber-700 mt-1" ]
                        [ text message ]
                    ]
                ]
            , div [ class "flex items-center" ]
                [ a [ href "/change-plan", class "mr-4 text-sm font-medium text-amber-700 underline hover:text-amber-600" ]
                    [ text upgradeText ]
                , button
                    [ class "rounded-md text-amber-500 hover:bg-amber-100 focus:outline-none focus:ring-2 focus:ring-amber-500"
                    , onClick closeMsg
                    ]
                    [ span [ class "sr-only" ] [ text "Dismiss" ]
                    , div [ class "h-5 w-5" ]
                        [ svg [ viewBox "0 0 20 20", fill "currentColor" ]
                            [ path [ fillRule "evenodd", d "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z", clipRule "evenodd" ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]
