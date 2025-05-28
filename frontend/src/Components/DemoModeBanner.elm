module Components.DemoModeBanner exposing (Model, Msg(..), init, update, view)

import Date exposing (Date, Unit(..))
import Html exposing (Html, button, div, h3, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import MyIcon
import Task
import Time exposing (Posix)



-- MODEL


type alias Model =
    { demoMode : Bool
    , orgCreateDate : Maybe Date
    , today : Maybe Date
    , showConfirmModal : Bool
    , isSubmitting : Bool
    , error : Maybe String
    }


init : Bool -> Maybe Date -> ( Model, Cmd Msg )
init demoMode orgCreateDate =
    ( { demoMode = demoMode
      , orgCreateDate = orgCreateDate
      , today = Nothing
      , showConfirmModal = False
      , isSubmitting = False
      , error = Nothing
      }
    , now
    )


now : Cmd Msg
now =
    Task.perform (Just >> SetTime) Time.now



-- UPDATE


type Msg
    = ClickExitDemo
    | CancelExitDemo
    | ConfirmExitDemo
    | DemoModeUpdated (Result Http.Error { success : Bool })
    | SetTime (Maybe Posix)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickExitDemo ->
            ( { model | showConfirmModal = True, error = Nothing }, Cmd.none )

        CancelExitDemo ->
            ( { model | showConfirmModal = False }, Cmd.none )

        ConfirmExitDemo ->
            ( { model | isSubmitting = True, error = Nothing }
            , exitDemoMode
            )

        DemoModeUpdated (Ok response) ->
            if response.success then
                ( { model | demoMode = False, isSubmitting = False, showConfirmModal = False }, Cmd.none )

            else
                ( { model | isSubmitting = False, error = Just "Failed to exit demo mode. Please try again." }, Cmd.none )

        DemoModeUpdated (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.BadUrl _ ->
                            "Invalid URL"

                        Http.Timeout ->
                            "Request timed out"

                        Http.NetworkError ->
                            "Network error"

                        Http.BadStatus statusCode ->
                            if statusCode == 403 then
                                "Only administrators can exit demo mode"

                            else
                                "Server error: " ++ String.fromInt statusCode

                        Http.BadBody message ->
                            "Failed to parse response: " ++ message
            in
            ( { model | isSubmitting = False, error = Just errorMsg }, Cmd.none )

        SetTime (Just posix) ->
            let
                today =
                    Date.fromPosix Time.utc posix
            in
            ( { model | today = Just today }, Cmd.none )

        SetTime Nothing ->
            ( { model | today = Nothing }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- HTTP


exitDemoMode : Cmd Msg
exitDemoMode =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/api/organizations/exit-demo-mode"
        , body = Http.emptyBody
        , expect = Http.expectJson DemoModeUpdated responseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


responseDecoder : Decode.Decoder { success : Bool }
responseDecoder =
    Decode.map (\success -> { success = success })
        (Decode.field "success" Decode.bool)



-- VIEW


view : Model -> Html Msg
view model =
    if model.demoMode then
        Html.div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ viewBanner model
            , if model.showConfirmModal then
                viewConfirmModal model

              else
                Html.text ""
            ]

    else
        Html.text ""


viewBanner : Model -> Html Msg
viewBanner model =
    let
        daysLeft =
            daysLeftInTrial model.orgCreateDate model.today
    in
    div [ class "p-3 sm:p-4 border rounded-lg bg-[#F6F1FF] border-[#E9D7FE] text-[#53389E] flex items-center justify-between" ]
        [ div [ class "flex items-start" ]
            [ Html.span [ class "inline-flex items-center justify-center h-6 w-6 rounded-full text-[#7F56D9] mt-0.5 mr-3" ]
                [ MyIcon.purpleIcon 18 "#53389E" ]
            , div []
                [ Html.span [ class "font-semibold text-sm leading-tight" ]
                    [ text ("Trial Period - " ++ String.fromInt daysLeft ++ " days remaining") ]
                , Html.div [ class "mt-1 text-xs leading-tight" ]
                    [ text "Your account is currently in a trial period. You can upload contacts, add agents, and see scheduled quote sends." ]
                , Html.div [ class "mt-1 text-xs leading-tight font-semibold" ]
                    [ text "Please note: emails will not be sent during your trial period. To start sending quotes click \"Go Live\"." ]
                ]
            ]
        , button
            [ class "ml-6 px-4 py-2 bg-[#1C1361] hover:bg-[#53389E] text-white rounded-md text-xs font-medium transition-colors"
            , onClick ClickExitDemo
            ]
            [ text "Go Live" ]
        ]


viewConfirmModal : Model -> Html Msg
viewConfirmModal model =
    div [ class "fixed inset-0 bg-black/50 flex items-center justify-center z-50" ]
        [ div [ class "bg-white rounded-lg p-6 max-w-md w-full mx-4" ]
            [ h3 [ class "text-lg font-medium text-gray-900 mb-3" ]
                [ text "Go Live?" ]
            , p [ class "text-sm text-gray-600 mb-5" ]
                [ text "Once you go live, your emails will be delivered to actual recipients and you will be charged based on usage for all contacts in your organization. This action cannot be undone." ]
            , if model.error /= Nothing then
                div [ class "mb-4 p-2 bg-red-50 border border-red-200 rounded text-red-700 text-sm" ]
                    [ text (Maybe.withDefault "An error occurred" model.error) ]

              else
                text ""
            , div [ class "flex justify-end space-x-3" ]
                [ button
                    [ class "px-4 py-2 text-gray-700 bg-gray-100 hover:bg-gray-200 rounded-md text-sm font-medium transition-colors"
                    , onClick CancelExitDemo
                    , Html.Attributes.disabled model.isSubmitting
                    ]
                    [ text "Cancel" ]
                , if model.isSubmitting then
                    button
                        [ class "px-4 py-2 text-white bg-blue-600 rounded-md text-sm font-medium transition-colors flex items-center"
                        , Html.Attributes.disabled True
                        ]
                        [ div [ class "mr-2 animate-spin h-4 w-4 border-2 border-white border-t-transparent rounded-full" ] []
                        , text "Processing..."
                        ]

                  else
                    button
                        [ class "px-4 py-2 text-white bg-blue-600 hover:bg-blue-700 rounded-md text-sm font-medium transition-colors"
                        , onClick ConfirmExitDemo
                        ]
                        [ text "Go Live" ]
                ]
            ]
        ]



-- HELPERS


daysLeftInTrial : Maybe Date -> Maybe Date -> Int



-- Calculates the number of days left in the 30-day trial.


daysLeftInTrial maybeOrgCreateDate maybeToday =
    case ( maybeOrgCreateDate, maybeToday ) of
        ( Just orgCreate, Just today ) ->
            let
                daysElapsed =
                    Date.diff Days orgCreate today

                daysLeft =
                    30 - daysElapsed
            in
            if daysLeft < 0 then
                0

            else
                daysLeft

        _ ->
            30
