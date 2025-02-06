module Settings exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (expectJson, jsonBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Model =
    { settings : Maybe Settings
    , status : Status
    }


type alias Settings =
    { stateLicenses : List String
    , carrierContracts : List String
    , stateCarrierSettings : StateCarrierSettings
    , emailSendBirthday : Bool
    , emailSendPolicyAnniversary : Bool
    , emailSendAep : Bool
    , smartSendEnabled : Bool
    }


type alias StateCarrierSettings =
    { state : String
    , carrier : String
    , active : Bool
    , targetGI : Bool
    }


type Status
    = Loading
    | Loaded
    | Saving
    | Error String


type Msg
    = GotSettings (Result Http.Error Settings)
    | SaveSettings
    | SettingsSaved (Result Http.Error Settings)
    | ToggleEmailBirthday Bool
    | ToggleEmailAnniversary Bool
    | ToggleEmailAep Bool
    | ToggleSmartSend Bool
    | AddStateLicense String
    | RemoveStateLicense String
    | AddCarrierContract String
    | RemoveCarrierContract String
    | UpdateStateCarrierSetting String String Bool Bool


init : () -> ( Model, Cmd Msg )
init _ =
    ( { settings = Nothing
      , status = Loading
      }
    , fetchSettings
    )


fetchSettings : Cmd Msg
fetchSettings =
    Http.get
        { url = "/api/settings"
        , expect = Http.expectJson GotSettings settingsDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSettings result ->
            case result of
                Ok settings ->
                    ( { model | settings = Just settings, status = Loaded }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | status = Error "Failed to load settings" }
                    , Cmd.none
                    )

        SaveSettings ->
            case model.settings of
                Just settings ->
                    ( { model | status = Saving }
                    , saveSettings settings
                    )

                Nothing ->
                    ( model, Cmd.none )

        SettingsSaved result ->
            case result of
                Ok settings ->
                    ( { model | settings = Just settings, status = Loaded }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | status = Error "Failed to save settings" }
                    , Cmd.none
                    )

        ToggleEmailBirthday value ->
            updateSettings model (\s -> { s | emailSendBirthday = value })

        ToggleEmailAnniversary value ->
            updateSettings model (\s -> { s | emailSendPolicyAnniversary = value })

        ToggleEmailAep value ->
            updateSettings model (\s -> { s | emailSendAep = value })

        ToggleSmartSend value ->
            updateSettings model (\s -> { s | smartSendEnabled = value })

        AddStateLicense state ->
            updateSettings model
                (\s ->
                    { s
                        | stateLicenses =
                            if List.member state s.stateLicenses then
                                s.stateLicenses

                            else
                                state :: s.stateLicenses
                    }
                )

        RemoveStateLicense state ->
            updateSettings model
                (\s ->
                    { s
                        | stateLicenses =
                            List.filter (\x -> x /= state) s.stateLicenses
                    }
                )

        AddCarrierContract carrier ->
            updateSettings model
                (\s ->
                    { s
                        | carrierContracts =
                            if List.member carrier s.carrierContracts then
                                s.carrierContracts

                            else
                                carrier :: s.carrierContracts
                    }
                )

        RemoveCarrierContract carrier ->
            updateSettings model
                (\s ->
                    { s
                        | carrierContracts =
                            List.filter (\x -> x /= carrier) s.carrierContracts
                    }
                )

        UpdateStateCarrierSetting state carrier active targetGI ->
            updateSettings model
                (\s ->
                    { s
                        | stateCarrierSettings =
                            { state = state
                            , carrier = carrier
                            , active = active
                            , targetGI = targetGI
                            }
                    }
                )


updateSettings : Model -> (Settings -> Settings) -> ( Model, Cmd Msg )
updateSettings model updateFn =
    case model.settings of
        Just settings ->
            let
                newSettings =
                    updateFn settings
            in
            ( { model | settings = Just newSettings }
            , saveSettings newSettings
            )

        Nothing ->
            ( model, Cmd.none )


saveSettings : Settings -> Cmd Msg
saveSettings settings =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/settings"
        , body = jsonBody (encodeSettings settings)
        , expect = expectJson SettingsSaved settingsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


view : Model -> Browser.Document Msg
view model =
    { title = "Settings"
    , body =
        [ div [ class "min-h-screen bg-gray-50" ]
            [ div [ class "max-w-7xl mx-auto py-6 sm:px-6 lg:px-8" ]
                [ h1 [ class "text-2xl font-semibold text-gray-900 mb-6" ]
                    [ text "Agent Settings" ]
                , viewContent model
                ]
            ]
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    case model.status of
        Loading ->
            div [] [ text "Loading..." ]

        Error error ->
            div [ class "text-red-600" ] [ text error ]

        _ ->
            case model.settings of
                Just settings ->
                    div [ class "space-y-6" ]
                        [ viewEmailSettings settings
                        , viewLicensesSection settings
                        , viewCarriersSection settings
                        , viewStateCarrierSettings settings
                        ]

                Nothing ->
                    div [] [ text "No settings found" ]


viewEmailSettings : Settings -> Html Msg
viewEmailSettings settings =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium mb-4" ] [ text "Email Settings" ]
        , div [ class "space-y-4" ]
            [ checkbox "Send birthday emails" settings.emailSendBirthday ToggleEmailBirthday
            , checkbox "Send policy anniversary emails" settings.emailSendPolicyAnniversary ToggleEmailAnniversary
            , checkbox "Send AEP emails" settings.emailSendAep ToggleEmailAep
            , checkbox "Enable smart send" settings.smartSendEnabled ToggleSmartSend
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


viewLicensesSection : Settings -> Html Msg
viewLicensesSection settings =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium mb-4" ] [ text "State Licenses" ]
        , div [ class "space-y-4" ]
            [ div [ class "flex flex-wrap gap-2" ]
                (List.map viewLicenseTag settings.stateLicenses)
            , viewAddLicense
            ]
        ]


viewLicenseTag : String -> Html Msg
viewLicenseTag state =
    div [ class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800" ]
        [ text state
        , button
            [ onClick (RemoveStateLicense state)
            , class "ml-1 text-blue-600 hover:text-blue-800"
            ]
            [ text "×" ]
        ]


viewAddLicense : Html Msg
viewAddLicense =
    div [ class "mt-4" ]
        [ select
            [ onInput AddStateLicense
            , class "mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
            ]
            (option [ value "" ] [ text "Add state license..." ]
                :: List.map (\state -> option [ value state ] [ text state ])
                    [ "AL"
                    , "AK"
                    , "AZ"
                    , "AR"
                    , "CA"
                    , "CO"
                    , "CT"
                    , "DE"
                    , "FL"
                    , "GA"
                    , "HI"
                    , "ID"
                    , "IL"
                    , "IN"
                    , "IA"
                    , "KS"
                    , "KY"
                    , "LA"
                    , "ME"
                    , "MD"
                    , "MA"
                    , "MI"
                    , "MN"
                    , "MS"
                    , "MO"
                    , "MT"
                    , "NE"
                    , "NV"
                    , "NH"
                    , "NJ"
                    , "NM"
                    , "NY"
                    , "NC"
                    , "ND"
                    , "OH"
                    , "OK"
                    , "OR"
                    , "PA"
                    , "RI"
                    , "SC"
                    , "SD"
                    , "TN"
                    , "TX"
                    , "UT"
                    , "VT"
                    , "VA"
                    , "WA"
                    , "WV"
                    , "WI"
                    , "WY"
                    ]
            )
        ]


viewCarriersSection : Settings -> Html Msg
viewCarriersSection settings =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium mb-4" ] [ text "Carrier Contracts" ]
        , div [ class "space-y-4" ]
            [ div [ class "flex flex-wrap gap-2" ]
                (List.map viewCarrierTag settings.carrierContracts)
            , viewAddCarrier
            ]
        ]


viewCarrierTag : String -> Html Msg
viewCarrierTag carrier =
    div [ class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800" ]
        [ text carrier
        , button
            [ onClick (RemoveCarrierContract carrier)
            , class "ml-1 text-green-600 hover:text-green-800"
            ]
            [ text "×" ]
        ]


viewAddCarrier : Html Msg
viewAddCarrier =
    div [ class "mt-4" ]
        [ select
            [ onInput AddCarrierContract
            , class "mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
            ]
            (option [ value "" ] [ text "Add carrier contract..." ]
                :: List.map (\carrier -> option [ value carrier ] [ text carrier ])
                    [ "Aetna"
                    , "Humana"
                    , "UnitedHealthcare"
                    , "Cigna"
                    , "WellCare"
                    , "Anthem"
                    , "BlueCross BlueShield"
                    ]
            )
        ]


viewStateCarrierSettings : Settings -> Html Msg
viewStateCarrierSettings settings =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium mb-4" ] [ text "State & Carrier Settings" ]
        , div [ class "space-y-6" ]
            (List.map
                (\state ->
                    div [ class "border-t pt-4" ]
                        [ h3 [ class "text-md font-medium mb-2" ] [ text ("State: " ++ state) ]
                        , div [ class "space-y-4" ]
                            (List.map
                                (\carrier ->
                                    viewStateCarrierRow state carrier settings.stateCarrierSettings
                                )
                                settings.carrierContracts
                            )
                        ]
                )
                settings.stateLicenses
            )
        ]


viewStateCarrierRow : String -> String -> StateCarrierSettings -> Html Msg
viewStateCarrierRow state carrier settings =
    let
        isActive =
            settings.state == state && settings.carrier == carrier && settings.active

        targetGI =
            settings.state == state && settings.carrier == carrier && settings.targetGI
    in
    div [ class "flex items-center justify-between" ]
        [ div [ class "flex items-center space-x-4" ]
            [ text carrier
            , checkbox "Active"
                isActive
                (\active -> UpdateStateCarrierSetting state carrier active targetGI)
            , checkbox "Target GI"
                targetGI
                (\gi -> UpdateStateCarrierSetting state carrier isActive gi)
            ]
        ]



-- Encoders and Decoders


settingsDecoder : Decoder Settings
settingsDecoder =
    Decode.map7 Settings
        (Decode.field "stateLicenses" (Decode.list Decode.string))
        (Decode.field "carrierContracts" (Decode.list Decode.string))
        (Decode.field "stateCarrierSettings" stateCarrierSettingsDecoder)
        (Decode.field "emailSendBirthday" Decode.bool)
        (Decode.field "emailSendPolicyAnniversary" Decode.bool)
        (Decode.field "emailSendAep" Decode.bool)
        (Decode.field "smartSendEnabled" Decode.bool)


stateCarrierSettingsDecoder : Decoder StateCarrierSettings
stateCarrierSettingsDecoder =
    Decode.map4 StateCarrierSettings
        (Decode.field "state" Decode.string)
        (Decode.field "carrier" Decode.string)
        (Decode.field "active" Decode.bool)
        (Decode.field "targetGI" Decode.bool)


encodeSettings : Settings -> Encode.Value
encodeSettings settings =
    Encode.object
        [ ( "stateLicenses", Encode.list Encode.string settings.stateLicenses )
        , ( "carrierContracts", Encode.list Encode.string settings.carrierContracts )
        , ( "stateCarrierSettings", encodeStateCarrierSettings settings.stateCarrierSettings )
        , ( "emailSendBirthday", Encode.bool settings.emailSendBirthday )
        , ( "emailSendPolicyAnniversary", Encode.bool settings.emailSendPolicyAnniversary )
        , ( "emailSendAep", Encode.bool settings.emailSendAep )
        , ( "smartSendEnabled", Encode.bool settings.smartSendEnabled )
        ]


encodeStateCarrierSettings : StateCarrierSettings -> Encode.Value
encodeStateCarrierSettings settings =
    Encode.object
        [ ( "state", Encode.string settings.state )
        , ( "carrier", Encode.string settings.carrier )
        , ( "active", Encode.bool settings.active )
        , ( "targetGI", Encode.bool settings.targetGI )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
