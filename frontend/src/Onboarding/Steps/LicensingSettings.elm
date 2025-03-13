module Onboarding.Steps.LicensingSettings exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode



-- MODEL


type alias Model =
    { carrierContracts : List String
    , useSmartSendForGI : Bool
    , isLoading : Bool
    , error : Maybe String
    , key : Nav.Key
    , orgSlug : String
    , expandedSections : List String
    }


init : Nav.Key -> String -> ( Model, Cmd Msg )
init key orgSlug =
    ( { carrierContracts = []
      , useSmartSendForGI = True
      , isLoading = True
      , error = Nothing
      , key = key
      , orgSlug = orgSlug
      , expandedSections = [ "Carrier Contracts", "Guaranteed Issue Settings" ]
      }
    , fetchLicensingSettings orgSlug
    )



-- UPDATE


type Msg
    = AddCarrierContract String
    | RemoveCarrierContract String
    | ToggleSection String
    | ToggleAllCarriers Bool
    | ToggleSmartSendForGI Bool
    | NextStepClicked
    | GotLicensingSettings (Result Http.Error LicensingSettingsResponse)
    | LicensingSettingsSaved (Result Http.Error LicensingSettingsUpdateResponse)
    | NoOp


type OutMsg
    = NoOutMsg
    | NextStep
    | ShowError String


type alias LicensingSettingsResponse =
    { carrierContracts : List String
    , useSmartSendForGI : Bool
    }


type alias LicensingSettingsUpdateResponse =
    { success : Bool
    , message : String
    , isBasicPlan : Bool
    }


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        AddCarrierContract carrier ->
            let
                newModel =
                    if List.member carrier model.carrierContracts then
                        model

                    else
                        { model | carrierContracts = carrier :: model.carrierContracts }
            in
            ( newModel, Cmd.none, NoOutMsg )

        RemoveCarrierContract carrier ->
            let
                newModel =
                    { model | carrierContracts = List.filter (\x -> x /= carrier) model.carrierContracts }
            in
            ( newModel, Cmd.none, NoOutMsg )

        ToggleSection title ->
            let
                newExpandedSections =
                    if List.member title model.expandedSections then
                        List.filter ((/=) title) model.expandedSections

                    else
                        title :: model.expandedSections
            in
            ( { model | expandedSections = newExpandedSections }, Cmd.none, NoOutMsg )

        ToggleAllCarriers checked ->
            let
                newModel =
                    { model
                        | carrierContracts =
                            if checked then
                                allCarriers

                            else
                                []
                    }
            in
            ( newModel, Cmd.none, NoOutMsg )

        ToggleSmartSendForGI useSmartSend ->
            ( { model | useSmartSendForGI = useSmartSend }, Cmd.none, NoOutMsg )

        NextStepClicked ->
            if List.isEmpty model.carrierContracts then
                ( { model | error = Just "Please select at least one carrier contract" }
                , Cmd.none
                , ShowError "Please select at least one carrier contract"
                )

            else
                ( { model | isLoading = True }
                , saveLicensingSettings model
                , NoOutMsg
                )

        GotLicensingSettings result ->
            case result of
                Ok response ->
                    ( { model
                        | carrierContracts = response.carrierContracts
                        , useSmartSendForGI = response.useSmartSendForGI
                        , isLoading = False
                      }
                    , Cmd.none
                    , NoOutMsg
                    )

                Err error ->
                    ( { model
                        | error = Just "Failed to load licensing settings"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to load licensing settings"
                    )

        LicensingSettingsSaved result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | isLoading = False }
                        , Cmd.none
                        , NextStep
                        )

                    else
                        ( { model
                            | error = Just response.message
                            , isLoading = False
                          }
                        , Cmd.none
                        , ShowError response.message
                        )

                Err error ->
                    ( { model
                        | error = Just "Failed to save licensing settings"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to save licensing settings"
                    )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Licensing & Carriers" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Configure your carrier contracts" ]
            ]
        , if model.isLoading && List.isEmpty model.carrierContracts then
            viewLoading

          else
            div [ class "space-y-6" ]
                [ viewExpandableSection "Carrier Contracts"
                    (viewCarriersGrid model)
                    model.expandedSections
                , viewGISettings model
                , if model.error /= Nothing then
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , div [ class "flex justify-center" ]
                    [ button
                        [ class "px-6 py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                        , onClick NextStepClicked
                        , disabled (List.isEmpty model.carrierContracts || model.isLoading)
                        , title
                            (if List.isEmpty model.carrierContracts then
                                "Please select at least one carrier contract"

                             else
                                ""
                            )
                        ]
                        [ if model.isLoading then
                            div [ class "flex items-center justify-center" ]
                                [ div [ class "animate-spin mr-2 h-4 w-4 border-t-2 border-b-2 border-white rounded-full" ] []
                                , text "Saving..."
                                ]

                          else
                            text "Continue"
                        ]
                    ]
                ]
        ]


viewLoading : Html msg
viewLoading =
    div [ class "text-center py-12" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-t-4 border-b-4 border-blue-500 mx-auto" ] []
        , p [ class "mt-4 text-gray-500" ]
            [ text "Loading..." ]
        ]


viewExpandableSection : String -> Html Msg -> List String -> Html Msg
viewExpandableSection title content expandedSections =
    let
        isExpanded =
            List.member title expandedSections
    in
    div [ class "bg-white shadow rounded-lg overflow-hidden" ]
        [ button
            [ class "w-full px-6 py-4 text-left flex justify-between items-center hover:bg-gray-50"
            , onClick (ToggleSection title)
            ]
            [ h2 [ class "text-lg font-medium" ] [ text title ]
            , div
                [ class "transform transition-transform"
                , classList [ ( "rotate-180", isExpanded ) ]
                ]
                [ text "▼" ]
            ]
        , div
            [ class "px-6 pb-6"
            , classList [ ( "hidden", not isExpanded ) ]
            ]
            [ content ]
        ]


viewCarriersGrid : Model -> Html Msg
viewCarriersGrid model =
    div []
        [ div [ class "mb-4 flex items-center" ]
            [ checkbox "Select All Carriers"
                (List.length model.carrierContracts == List.length allCarriers)
                ToggleAllCarriers
            ]
        , div [ class "grid grid-cols-2 sm:grid-cols-3 gap-4" ]
            (List.map
                (\carrier ->
                    checkbox carrier
                        (List.member carrier model.carrierContracts)
                        (\checked ->
                            if checked then
                                AddCarrierContract carrier

                            else
                                RemoveCarrierContract carrier
                        )
                )
                allCarriers
            )
        ]


viewGISettings : Model -> Html Msg
viewGISettings model =
    div [ class "bg-white shadow rounded-lg overflow-hidden" ]
        [ div [ class "px-6 py-4 border-b border-gray-200" ]
            [ h2 [ class "text-lg font-medium" ] [ text "Guaranteed Issue Settings" ]
            ]
        , div [ class "p-6 space-y-6" ]
            [ div [ class "mb-4 p-4 bg-blue-50 border border-blue-100 rounded-md" ]
                [ div [ class "flex items-start" ]
                    [ div [ class "flex items-center h-5" ]
                        [ input
                            [ type_ "checkbox"
                            , checked model.useSmartSendForGI
                            , onCheck ToggleSmartSendForGI
                            , class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
                            ]
                            []
                        ]
                    , div [ class "ml-3 text-sm" ]
                        [ label [ class "font-medium text-gray-700" ]
                            [ text "Use SmartSend for Guaranteed Issue" ]
                        , p [ class "text-gray-500 mt-1" ]
                            [ text "When enabled, SmartSend will automatically identify which carrier combinations offer full compensation for Guaranteed Issue (GI) policies." ]
                        ]
                    ]
                ]
            , div [ class "mt-4 p-4 bg-gray-50 rounded-md" ]
                [ h3 [ class "text-lg font-medium text-gray-900 mb-2" ]
                    [ text "How SmartSend Works" ]
                , p [ class "text-gray-600" ]
                    [ text "SmartSend analyzes each carrier to determine which ones offer full carrier compensation for Guaranteed Issue policies. This helps maximize your commissions while ensuring your quotes are always compliant with the latest carrier regulations." ]
                ]
            ]
        ]


checkbox : String -> Bool -> (Bool -> msg) -> Html msg
checkbox labelText isChecked onToggle =
    Html.label [ class "flex items-center space-x-3" ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            , onCheck onToggle
            , class "h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
            ]
            []
        , span [ class "text-gray-700" ] [ text labelText ]
        ]


th : List (Attribute msg) -> List (Html msg) -> Html msg
th attributes children =
    Html.th attributes children


td : List (Attribute msg) -> List (Html msg) -> Html msg
td attributes children =
    Html.td attributes children



-- HELPERS
-- API CALLS


fetchLicensingSettings : String -> Cmd Msg
fetchLicensingSettings _ =
    Http.get
        { url = "/api/onboarding/settings"
        , expect = Http.expectJson GotLicensingSettings licensingSettingsDecoder
        }


saveLicensingSettings : Model -> Cmd Msg
saveLicensingSettings model =
    Http.post
        { url = "/api/onboarding/licensing-settings"
        , body = Http.jsonBody (encodeLicensingSettings model)
        , expect = Http.expectJson LicensingSettingsSaved licensingSettingsUpdateResponseDecoder
        }



-- DECODERS & ENCODERS


licensingSettingsDecoder : Decode.Decoder LicensingSettingsResponse
licensingSettingsDecoder =
    Decode.field "licensingSettingsModel"
        (Decode.succeed LicensingSettingsResponse
            |> Pipeline.required "carrierContracts" (Decode.list Decode.string)
            |> Pipeline.required "useSmartSendForGI" Decode.bool
        )


licensingSettingsUpdateResponseDecoder : Decode.Decoder LicensingSettingsUpdateResponse
licensingSettingsUpdateResponseDecoder =
    Decode.succeed LicensingSettingsUpdateResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "message" Decode.string
        |> Pipeline.required "isBasicPlan" Decode.bool


encodeLicensingSettings : Model -> Encode.Value
encodeLicensingSettings model =
    Encode.object
        [ ( "carrierContracts", Encode.list Encode.string model.carrierContracts )
        , ( "useSmartSendForGI", Encode.bool model.useSmartSendForGI )
        ]



-- CONSTANTS


allCarriers : List String
allCarriers =
    [ "Aetna"
    , "Humana"
    , "UnitedHealthcare"
    , "Cigna"
    , "Aflac"
    , "Allstate"
    , "Mutual of Omaha"
    , "Ace Chubb"
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
