module Settings exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (expectJson, jsonBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import StateRegions exposing (Region(..), getRegionStates, regionToString)
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, viewBox)



-- Constants


allStates : List String
allStates =
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
    , "DC"
    ]


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


type Carrier
    = Aetna
    | Humana
    | UnitedHealthcare
    | Cigna
    | Aflac
    | Allstate
    | MutualOfOmaha
    | AceChubb



-- Add new type for GI selection mode


type GISelectionMode
    = GIAll
    | GINone
    | GIRecommended


type alias InitFlags =
    { isSetup : Bool
    , key : Nav.Key
    }


type alias Model =
    { orgSettings : Maybe Settings
    , status : Status
    , expandedSections : List String
    , recommendedGICombos : List StateCarrierSetting
    , isSetup : Bool
    , key : Nav.Key
    }


type alias StateCarrierSetting =
    { state : String
    , carrier : String
    , active : Bool
    , targetGI : Bool
    }


type alias Settings =
    { stateLicenses : List String
    , carrierContracts : List String
    , stateCarrierSettings : List StateCarrierSetting
    , allowAgentSettings : Bool
    , emailSendBirthday : Bool
    , emailSendPolicyAnniversary : Bool
    , emailSendAep : Bool
    , smartSendEnabled : Bool
    }


type Status
    = Loading
    | Loaded
    | Saving
    | Error String


type Msg
    = GotSettings (Result Http.Error SettingsResponse)
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
    | ToggleSection String
    | ToggleAllStates Bool
    | ToggleAllCarriers Bool
    | ApplyGISelection GISelectionMode
    | GotRecommendedGICombos (Result Http.Error (List StateCarrierSetting))
    | ToggleAllowAgentSettings Bool
    | FinishSetup
    | SelectCommonStates Region


type alias SettingsResponse =
    { orgSettings : Settings
    , canEditOrgSettings : Bool
    }


init : InitFlags -> ( Model, Cmd Msg )
init flags =
    ( { orgSettings = Nothing
      , status = Loading
      , expandedSections = []
      , recommendedGICombos = []
      , isSetup = flags.isSetup
      , key = flags.key
      }
    , Cmd.batch
        [ fetchSettings
        , fetchRecommendedGICombos
        ]
    )


fetchSettings : Cmd Msg
fetchSettings =
    let
        _ =
            Debug.log "Fetching settings" ()
    in
    Http.get
        { url = "/api/settings"
        , expect = Http.expectJson GotSettings settingsDecoder
        }


fetchRecommendedGICombos : Cmd Msg
fetchRecommendedGICombos =
    let
        _ =
            Debug.log "Fetching GI recommendations" ()
    in
    Http.get
        { url = "/api/settings/gi-recommendations"
        , expect = Http.expectJson GotRecommendedGICombos recommendationsDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSettings result ->
            case result of
                Ok response ->
                    ( { model | orgSettings = Just response.orgSettings, status = Loaded }
                    , Cmd.none
                    )

                Err error ->
                    let
                        errorMsg =
                            case error of
                                Http.BadUrl url ->
                                    "Bad URL: " ++ url

                                Http.Timeout ->
                                    "Request timed out"

                                Http.NetworkError ->
                                    "Network error"

                                Http.BadStatus status ->
                                    "Bad status: " ++ String.fromInt status

                                Http.BadBody message ->
                                    let
                                        _ =
                                            Debug.log "Decoder error" message
                                    in
                                    "Bad body: " ++ message
                    in
                    ( { model | status = Error errorMsg }
                    , Cmd.none
                    )

        SaveSettings ->
            ( { model | status = Saving }
            , Cmd.none
            )

        SettingsSaved result ->
            case result of
                Ok settings ->
                    ( { model | orgSettings = Just settings, status = Loaded }
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
                        , stateCarrierSettings =
                            List.filter (\setting -> setting.state /= state) s.stateCarrierSettings
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
                        , stateCarrierSettings =
                            List.filter (\setting -> setting.carrier /= carrier) s.stateCarrierSettings
                    }
                )

        UpdateStateCarrierSetting state carrier active targetGI ->
            updateSettings model
                (\s ->
                    let
                        existingSetting =
                            List.filter
                                (\setting ->
                                    setting.state == state && setting.carrier == carrier
                                )
                                s.stateCarrierSettings
                                |> List.head

                        newSettings =
                            case existingSetting of
                                Just _ ->
                                    List.map
                                        (\setting ->
                                            if setting.state == state && setting.carrier == carrier then
                                                { setting | active = active, targetGI = targetGI }

                                            else
                                                setting
                                        )
                                        s.stateCarrierSettings

                                Nothing ->
                                    { state = state
                                    , carrier = carrier
                                    , active = active
                                    , targetGI = targetGI
                                    }
                                        :: s.stateCarrierSettings
                    in
                    { s | stateCarrierSettings = newSettings }
                )

        ToggleSection title ->
            ( { model
                | expandedSections =
                    if List.member title model.expandedSections then
                        List.filter ((/=) title) model.expandedSections

                    else
                        title :: model.expandedSections
              }
            , Cmd.none
            )

        ToggleAllStates checked ->
            case model.orgSettings of
                Just settings ->
                    let
                        newSettings =
                            { settings
                                | stateLicenses =
                                    if checked then
                                        allStates

                                    else
                                        []
                                , stateCarrierSettings =
                                    if checked then
                                        settings.stateCarrierSettings

                                    else
                                        []

                                -- Clear all state/carrier settings when deselecting all states
                            }
                    in
                    ( { model | orgSettings = Just newSettings }
                    , saveSettings newSettings
                    )

                Nothing ->
                    ( model, Cmd.none )

        ToggleAllCarriers checked ->
            case model.orgSettings of
                Just settings ->
                    let
                        newSettings =
                            { settings
                                | carrierContracts =
                                    if checked then
                                        allCarriers

                                    else
                                        []
                                , stateCarrierSettings =
                                    if checked then
                                        settings.stateCarrierSettings

                                    else
                                        []

                                -- Clear all state/carrier settings when deselecting all carriers
                            }
                    in
                    ( { model | orgSettings = Just newSettings }
                    , saveSettings newSettings
                    )

                Nothing ->
                    ( model, Cmd.none )

        ApplyGISelection mode ->
            case model.orgSettings of
                Just settings ->
                    let
                        newSettings =
                            case mode of
                                GIAll ->
                                    { settings
                                        | stateCarrierSettings =
                                            List.map
                                                (\setting -> { setting | targetGI = True })
                                                settings.stateCarrierSettings
                                    }

                                GINone ->
                                    { settings
                                        | stateCarrierSettings =
                                            List.map
                                                (\setting -> { setting | targetGI = False })
                                                settings.stateCarrierSettings
                                    }

                                GIRecommended ->
                                    { settings
                                        | stateCarrierSettings =
                                            List.map
                                                (\setting ->
                                                    { setting
                                                        | targetGI =
                                                            List.any
                                                                (\rec ->
                                                                    rec.state == setting.state && rec.carrier == setting.carrier
                                                                )
                                                                model.recommendedGICombos
                                                    }
                                                )
                                                settings.stateCarrierSettings
                                    }
                    in
                    ( { model | orgSettings = Just newSettings }
                    , saveSettings newSettings
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotRecommendedGICombos result ->
            case result of
                Ok combos ->
                    let
                        _ =
                            Debug.log "Received GI recommendations" combos
                    in
                    ( { model | recommendedGICombos = combos }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            Debug.log "Error getting GI recommendations" error
                    in
                    ( { model | status = Error "Failed to load GI recommendations" }
                    , Cmd.none
                    )

        ToggleAllowAgentSettings value ->
            updateSettings model (\s -> { s | allowAgentSettings = value })

        FinishSetup ->
            ( model
            , Nav.pushUrl model.key "/add-agents"
            )

        SelectCommonStates region ->
            updateSettings model
                (\s ->
                    { s | stateLicenses = s.stateLicenses ++ getRegionStates region }
                )


updateSettings : Model -> (Settings -> Settings) -> ( Model, Cmd Msg )
updateSettings model updateFn =
    case model.orgSettings of
        Just settings ->
            let
                newSettings =
                    updateFn settings
            in
            ( { model | orgSettings = Just newSettings }
            , saveSettings newSettings
            )

        Nothing ->
            ( model, Cmd.none )


saveSettings : Settings -> Cmd Msg
saveSettings settings =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/settings/org"
        , body = jsonBody (encodeSettings (settings |> Debug.log "settings"))
        , expect = expectJson SettingsSaved settingsObjectDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


encodeSettings : Settings -> Encode.Value
encodeSettings settings =
    Encode.object
        [ ( "stateLicenses", Encode.list Encode.string settings.stateLicenses )
        , ( "carrierContracts", Encode.list Encode.string settings.carrierContracts )
        , ( "stateCarrierSettings", Encode.list stateCarrierSettingEncoder settings.stateCarrierSettings )
        , ( "allowAgentSettings", Encode.bool settings.allowAgentSettings )
        , ( "emailSendBirthday", Encode.bool settings.emailSendBirthday )
        , ( "emailSendPolicyAnniversary", Encode.bool settings.emailSendPolicyAnniversary )
        , ( "emailSendAep", Encode.bool settings.emailSendAep )
        , ( "smartSendEnabled", Encode.bool settings.smartSendEnabled )
        ]


stateCarrierSettingEncoder : StateCarrierSetting -> Encode.Value
stateCarrierSettingEncoder setting =
    Encode.object
        [ ( "state", Encode.string setting.state )
        , ( "carrier", Encode.string setting.carrier )
        , ( "active", Encode.bool setting.active )
        , ( "targetGI", Encode.bool setting.targetGI )
        ]


view : Model -> Browser.Document Msg
view model =
    { title =
        if model.isSetup then
            "Organization Setup - Settings"

        else
            "Settings"
    , body =
        [ div [ class "min-h-screen bg-gray-50" ]
            [ div [ class "max-w-7xl mx-auto py-6 sm:px-6 lg:px-8" ]
                [ if model.isSetup then
                    viewSetupHeader

                  else
                    viewNormalHeader
                , viewSettingsContent model.orgSettings True model.expandedSections
                , viewBottomBar model
                ]
            ]
        ]
    }


viewSetupHeader : Html Msg
viewSetupHeader =
    div [ class "mb-8 text-center" ]
        [ h1 [ class "text-3xl font-bold text-gray-900" ]
            [ text "Set Up Your Organization" ]
        , p [ class "mt-2 text-gray-600" ]
            [ text "Configure your organization's settings to get started" ]
        ]


viewNormalHeader : Html Msg
viewNormalHeader =
    h1 [ class "text-2xl font-semibold text-gray-900 mb-6" ]
        [ text "Organization Settings" ]


viewBottomBar : Model -> Html Msg
viewBottomBar model =
    div
        [ class """fixed bottom-0 left-0 right-0 bg-white border-t border-gray-200 
                  px-4 py-4 sm:px-6 lg:px-8 flex justify-between items-center"""
        ]
        [ button
            [ class """px-4 py-2 text-sm font-medium text-gray-700 bg-white 
                      border border-gray-300 rounded-md hover:bg-gray-50"""
            , onClick SaveSettings
            ]
            [ text "Save Changes" ]
        , if model.isSetup then
            button
                [ class """px-4 py-2 text-sm font-medium text-white bg-blue-600 
                          rounded-md hover:bg-blue-700 ml-4"""
                , onClick FinishSetup
                ]
                [ text "Next: Add Agents" ]

          else
            text ""
        ]


viewSettingsContent : Maybe Settings -> Bool -> List String -> Html Msg
viewSettingsContent maybeSettings canEdit expandedSections =
    case maybeSettings of
        Just settings ->
            div [ class "space-y-6" ]
                [ div [ class "bg-white shadow rounded-lg p-6" ]
                    [ h2 [ class "text-lg font-medium mb-4" ] [ text "Organization Settings" ]
                    , div [ class "space-y-4" ]
                        [ checkbox "Allow agents to customize their own settings"
                            settings.allowAgentSettings
                            ToggleAllowAgentSettings
                        ]
                    ]
                , viewEmailSettings settings
                , viewExpandableSection "State Licenses"
                    (viewLicensesGrid settings)
                    expandedSections
                , viewExpandableSection "Carrier Contracts"
                    (viewCarriersGrid settings)
                    expandedSections
                , viewExpandableSection "State & Carrier Settings"
                    (viewStateCarrierGrid settings)
                    expandedSections
                ]

        Nothing ->
            div [ class "text-gray-500 italic" ]
                [ text "Using organization settings" ]


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


viewLicensesGrid : Settings -> Html Msg
viewLicensesGrid settings =
    div []
        [ div [ class "mb-4 space-y-2" ]
            [ div [ class "flex gap-4" ]
                -- Container for both label groups
                [ div []
                    -- Batch Select group
                    [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                        [ text "Batch Select" ]
                    , div [ class "flex gap-2" ]
                        [ button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                            , onClick (ToggleAllStates True)
                            ]
                            [ text "Select All" ]
                        , button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                            , onClick (ToggleAllStates False)
                            ]
                            [ text "Clear All" ]
                        ]
                    ]
                , div []
                    -- By Region: group
                    [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                        [ text "By Region:" ]
                    , div [ class "flex gap-2" ]
                        (List.map
                            (\region ->
                                button
                                    [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50"
                                    , onClick (SelectCommonStates region)
                                    ]
                                    [ text (regionToString region) ]
                            )
                            StateRegions.allRegions
                        )
                    ]
                ]
            ]
        , div [ class "grid grid-cols-5 gap-4" ]
            (List.map
                (\state ->
                    checkbox state
                        (List.member state settings.stateLicenses)
                        (\checked ->
                            if checked then
                                AddStateLicense state

                            else
                                RemoveStateLicense state
                        )
                )
                allStates
            )
        ]


viewCarriersGrid : Settings -> Html Msg
viewCarriersGrid settings =
    div []
        [ div [ class "mb-4 flex items-center" ]
            [ checkbox "Select All Carriers"
                (List.length settings.carrierContracts == List.length allCarriers)
                ToggleAllCarriers
            ]
        , div [ class "grid grid-cols-3 gap-4" ]
            (List.map
                (\carrier ->
                    checkbox carrier
                        (List.member carrier settings.carrierContracts)
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


viewStateCarrierGrid : Settings -> Html Msg
viewStateCarrierGrid settings =
    if List.isEmpty settings.stateLicenses || List.isEmpty settings.carrierContracts then
        div [ class "text-gray-500 italic p-4" ]
            [ text "Please select at least one state license and one carrier contract to configure their settings." ]

    else
        div []
            [ div [ class "mb-6" ]
                [ h3 [ class "text-sm font-medium text-gray-700 mb-2" ]
                    [ text "Guaranteed Issue Settings" ]
                , div [ class "flex space-x-2" ]
                    [ button
                        [ class "px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50"
                        , onClick (ApplyGISelection GIAll)
                        ]
                        [ text "Select All" ]
                    , button
                        [ class "px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50"
                        , onClick (ApplyGISelection GINone)
                        ]
                        [ text "Clear All" ]
                    , div [ class "relative group" ]
                        [ button
                            [ class "px-4 py-2 text-sm font-medium text-indigo-700 bg-white border border-indigo-300 rounded-md hover:bg-indigo-50 flex items-center"
                            , onClick (ApplyGISelection GIRecommended)
                            ]
                            [ text "Apply Recommended"
                            , div
                                [ class "ml-2 w-5 h-5 rounded-full bg-indigo-50 flex items-center justify-center text-indigo-700" ]
                                [ text "i" ]
                            ]
                        , div
                            [ class """absolute bottom-full mb-2 p-4 bg-white text-indigo-700 text-sm rounded-lg shadow-lg 
                                      invisible group-hover:visible opacity-0 group-hover:opacity-100 transition-opacity
                                      -translate-x-1/2 left-1/2 w-96 border border-indigo-100 z-50"""
                            ]
                            [ text """Include state/carriers with full compensation for GI policies. Always verify with your own contracts and commission schedules.""" ]
                        ]
                    ]
                ]
            , div [ class "overflow-x-auto" ]
                [ table [ class "min-w-full divide-y divide-gray-200" ]
                    [ thead
                        [ class "bg-gray-50" ]
                        [ tr []
                            (th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider sticky left-0 bg-gray-50 z-10" ]
                                [ text "State" ]
                                :: List.map
                                    (\carrier ->
                                        th [ class "px-6 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider min-w-[120px]" ]
                                            [ text carrier ]
                                    )
                                    settings.carrierContracts
                            )
                        ]
                    , tbody [ class "bg-white divide-y divide-gray-200" ]
                        (List.indexedMap
                            (\index state ->
                                tr [ classList [ ( "bg-gray-50", modBy 2 index == 0 ) ] ]
                                    (td [ class "px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900 sticky left-0 bg-inherit z-10" ]
                                        [ text state ]
                                        :: List.map
                                            (\carrier ->
                                                let
                                                    setting =
                                                        findStateCarrierSetting settings state carrier
                                                in
                                                td [ class "px-6 py-4 whitespace-nowrap text-sm text-center" ]
                                                    [ div [ class "flex flex-col items-start space-y-2 w-[120px] mx-auto" ]
                                                        [ div [ class "w-full" ]
                                                            [ label [ class "flex items-center cursor-pointer w-full" ]
                                                                [ div [ class "relative w-4 h-4 mr-2 flex-shrink-0" ]
                                                                    [ input
                                                                        [ type_ "checkbox"
                                                                        , checked setting.active
                                                                        , onCheck
                                                                            (\active ->
                                                                                UpdateStateCarrierSetting state
                                                                                    carrier
                                                                                    active
                                                                                    (if active then
                                                                                        setting.targetGI

                                                                                     else
                                                                                        False
                                                                                    )
                                                                            )
                                                                        , class "absolute w-0 h-0 opacity-0" -- Hide the actual checkbox
                                                                        ]
                                                                        []
                                                                    , div
                                                                        [ class "w-4 h-4 border rounded transition-colors duration-200 flex items-center justify-center"
                                                                        , classList
                                                                            [ ( "bg-green-600 border-green-600", setting.active )
                                                                            , ( "border-gray-300", not setting.active )
                                                                            ]
                                                                        ]
                                                                        [ if setting.active then
                                                                            div [ class "text-white text-xs" ] [ text "✓" ]

                                                                          else
                                                                            text ""
                                                                        ]
                                                                    ]
                                                                , div [ class "flex-grow text-left" ] [ text "Active" ]
                                                                ]
                                                            ]
                                                        , div [ class "w-full" ]
                                                            [ label
                                                                [ class "flex items-center w-full"
                                                                , classList
                                                                    [ ( "cursor-pointer", setting.active )
                                                                    , ( "cursor-not-allowed opacity-50", not setting.active )
                                                                    ]
                                                                ]
                                                                [ div [ class "relative w-4 h-4 mr-2 flex-shrink-0" ]
                                                                    [ input
                                                                        [ type_ "checkbox"
                                                                        , checked setting.targetGI
                                                                        , onCheck (\targetGI -> UpdateStateCarrierSetting state carrier setting.active targetGI)
                                                                        , class "absolute w-0 h-0 opacity-0" -- Hide the actual checkbox
                                                                        , disabled (not setting.active)
                                                                        ]
                                                                        []
                                                                    , div
                                                                        [ class "w-4 h-4 border rounded transition-colors duration-200 flex items-center justify-center"
                                                                        , classList
                                                                            [ ( "bg-blue-600 border-blue-600", setting.targetGI && setting.active )
                                                                            , ( "border-gray-300", not setting.targetGI || not setting.active )
                                                                            ]
                                                                        ]
                                                                        [ if setting.targetGI && setting.active then
                                                                            div [ class "text-white text-xs" ] [ text "✓" ]

                                                                          else
                                                                            text ""
                                                                        ]
                                                                    ]
                                                                , div [ class "flex-grow text-left" ] [ text "GI" ]
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                            )
                                            settings.carrierContracts
                                    )
                            )
                            settings.stateLicenses
                        )
                    ]
                ]
            ]



-- Helper functions for state/carrier grid


isStateCarrierActive : Settings -> String -> String -> Bool
isStateCarrierActive settings state carrier =
    settings.stateCarrierSettings
        |> List.filter (\s -> s.state == state && s.carrier == carrier)
        |> List.head
        |> Maybe.map .active
        |> Maybe.withDefault True


isStateCarrierTargetGI : Settings -> String -> String -> Bool
isStateCarrierTargetGI settings state carrier =
    settings.stateCarrierSettings
        |> List.filter (\s -> s.state == state && s.carrier == carrier)
        |> List.head
        |> Maybe.map .targetGI
        |> Maybe.withDefault False


hasDefaultSettings : Settings -> Bool
hasDefaultSettings settings =
    List.all
        (\setting -> setting.active && not setting.targetGI)
        settings.stateCarrierSettings


findStateCarrierSetting : Settings -> String -> String -> StateCarrierSetting
findStateCarrierSetting settings state carrier =
    settings.stateCarrierSettings
        |> List.filter (\s -> s.state == state && s.carrier == carrier)
        |> List.head
        |> Maybe.withDefault
            { state = state
            , carrier = carrier
            , active = True
            , targetGI = False
            }


viewStateCarrierCell : StateCarrierSetting -> Html Msg
viewStateCarrierCell setting =
    div [ class "flex flex-col items-center gap-1" ]
        [ label [ class "flex items-center gap-2 cursor-pointer" ]
            [ input
                [ type_ "checkbox"
                , checked setting.active
                , onCheck (\active -> UpdateStateCarrierSetting setting.state setting.carrier active setting.targetGI)
                , class "h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
                ]
                []
            , span [ class "text-sm text-gray-600" ] [ text "Active" ]
            ]
        , label [ class "flex items-center gap-2 cursor-pointer" ]
            [ input
                [ type_ "checkbox"
                , checked setting.targetGI
                , onCheck (\targetGI -> UpdateStateCarrierSetting setting.state setting.carrier setting.active targetGI)
                , class "h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
                ]
                []
            , span [ class "text-sm text-gray-600" ] [ text "GI" ]
            ]
        ]



-- Encoders and Decoders


settingsDecoder : Decoder SettingsResponse
settingsDecoder =
    let
        _ =
            Debug.log "Running settingsDecoder" ()
    in
    Decode.map2 SettingsResponse
        (Decode.field "orgSettings" settingsObjectDecoder)
        (Decode.field "canEditOrgSettings" Decode.bool)


settingsObjectDecoder : Decoder Settings
settingsObjectDecoder =
    let
        _ =
            Debug.log "Running settingsObjectDecoder" ()

        stateCarrierSettingsDecoder =
            Decode.field "stateCarrierSettings" <|
                Decode.oneOf
                    [ Decode.list stateCarrierSettingDecoder
                    , Decode.null []
                    ]
    in
    Decode.map8 Settings
        (Decode.field "stateLicenses" (Decode.list Decode.string))
        (Decode.field "carrierContracts" (Decode.list Decode.string))
        stateCarrierSettingsDecoder
        (Decode.field "allowAgentSettings" Decode.bool)
        (Decode.field "emailSendBirthday" Decode.bool)
        (Decode.field "emailSendPolicyAnniversary" Decode.bool)
        (Decode.field "emailSendAep" Decode.bool)
        (Decode.field "smartSendEnabled" Decode.bool)


stateCarrierSettingDecoder : Decoder StateCarrierSetting
stateCarrierSettingDecoder =
    Decode.map4 StateCarrierSetting
        (Decode.field "state" Decode.string)
        (Decode.field "carrier" Decode.string)
        (Decode.field "active" Decode.bool)
        (Decode.field "targetGI" Decode.bool)


recommendationsDecoder : Decoder (List StateCarrierSetting)
recommendationsDecoder =
    let
        _ =
            Debug.log "Running recommendationsDecoder" ()
    in
    Decode.list
        (Decode.map4 StateCarrierSetting
            (Decode.field "state" Decode.string)
            (Decode.field "carrier" Decode.string)
            (Decode.field "active" Decode.bool)
            (Decode.field "targetGI" Decode.bool)
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


tab : String -> Bool -> Bool -> msg -> Html msg
tab label isActive isDisabled msg =
    button
        [ class "px-3 py-2 font-medium text-sm rounded-md -mb-px"
        , classList
            [ ( "text-indigo-600 border-indigo-500 border-b-2", isActive )
            , ( "text-gray-500 hover:text-gray-700 hover:border-gray-300 border-transparent border-b-2"
              , not isActive && not isDisabled
              )
            , ( "text-gray-400 cursor-not-allowed", isDisabled )
            ]
        , onClick msg
        , disabled isDisabled
        ]
        [ text label ]
