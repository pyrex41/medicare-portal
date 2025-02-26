module Settings exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Components.ProgressIndicator
import Components.SetupLayout as SetupLayout
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (expectJson, jsonBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import StateRegions exposing (Region(..), getRegionStates, regionToString)
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, viewBox)
import Task



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
    , currentUser : Maybe CurrentUser
    , planType : String
    }


type alias CurrentUser =
    { id : String
    , email : String
    , isAdmin : Bool
    , isAgent : Bool
    , organizationSlug : String
    , organizationId : String
    }


type alias Model =
    { orgSettings : Maybe Settings
    , status : Status
    , expandedSections : List String
    , recommendedGICombos : List StateCarrierSetting
    , isSetup : Bool
    , key : Nav.Key
    , currentUser : Maybe CurrentUser
    , isLoading : Bool
    , isSaving : Bool
    , planType : String
    , error : Maybe String
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
    , brandName : String
    , primaryColor : String
    , secondaryColor : String
    , logo : Maybe String
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
    | UpdateBrandName String
    | UpdatePrimaryColor String
    | UpdateSecondaryColor String
    | UploadLogo
    | GotLogo File
    | GotLogoUrl String
    | LogoUploaded (Result Http.Error String)
    | NoOp
    | OrgFinalized (Result Http.Error ())


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
      , currentUser = flags.currentUser
      , isLoading = True
      , isSaving = False
      , planType = flags.planType
      , error = Nothing
      }
    , Cmd.batch
        [ fetchSettings
        , fetchRecommendedGICombos
        ]
    )


fetchSettings : Cmd Msg
fetchSettings =
    Http.get
        { url = "/api/settings"
        , expect = Http.expectJson GotSettings settingsDecoder
        }


fetchRecommendedGICombos : Cmd Msg
fetchRecommendedGICombos =
    Http.get
        { url = "/api/settings/gi-recommendations"
        , expect = Http.expectJson GotRecommendedGICombos recommendationsDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotSettings result ->
            case result of
                Ok response ->
                    ( { model | orgSettings = Just response.orgSettings, status = Loaded, isLoading = False }
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
                                    "Bad body: " ++ message
                    in
                    ( { model | status = Error errorMsg, isLoading = False }
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
                    if List.member state s.stateLicenses then
                        s

                    else
                        let
                            newStateCarrierSettings =
                                List.concat
                                    [ s.stateCarrierSettings
                                    , List.map
                                        (\carrier ->
                                            { state = state
                                            , carrier = carrier
                                            , active = True
                                            , targetGI = False
                                            }
                                        )
                                        s.carrierContracts
                                    ]
                        in
                        { s
                            | stateLicenses = state :: s.stateLicenses
                            , stateCarrierSettings = newStateCarrierSettings
                        }
                )

        RemoveStateLicense state ->
            updateSettings model
                (\s ->
                    { s
                        | stateLicenses = List.filter (\x -> x /= state) s.stateLicenses
                        , stateCarrierSettings = List.filter (\setting -> setting.state /= state) s.stateCarrierSettings
                    }
                )

        AddCarrierContract carrier ->
            updateSettings model
                (\s ->
                    if List.member carrier s.carrierContracts then
                        s

                    else
                        let
                            newStateCarrierSettings =
                                List.concat
                                    [ s.stateCarrierSettings
                                    , List.map
                                        (\state ->
                                            { state = state
                                            , carrier = carrier
                                            , active = True
                                            , targetGI = False
                                            }
                                        )
                                        s.stateLicenses
                                    ]
                        in
                        { s
                            | carrierContracts = carrier :: s.carrierContracts
                            , stateCarrierSettings = newStateCarrierSettings
                        }
                )

        RemoveCarrierContract carrier ->
            updateSettings model
                (\s ->
                    { s
                        | carrierContracts = List.filter (\x -> x /= carrier) s.carrierContracts
                        , stateCarrierSettings = List.filter (\setting -> setting.carrier /= carrier) s.stateCarrierSettings
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
                                        -- Create settings for all state/carrier combinations
                                        List.concatMap
                                            (\state ->
                                                List.map
                                                    (\carrier ->
                                                        { state = state
                                                        , carrier = carrier
                                                        , active = True
                                                        , targetGI = False
                                                        }
                                                    )
                                                    settings.carrierContracts
                                            )
                                            allStates

                                    else
                                        []
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
                                        -- Create settings for all state/carrier combinations
                                        List.concatMap
                                            (\state ->
                                                List.map
                                                    (\carrier ->
                                                        { state = state
                                                        , carrier = carrier
                                                        , active = True
                                                        , targetGI = False
                                                        }
                                                    )
                                                    allCarriers
                                            )
                                            settings.stateLicenses

                                    else
                                        []
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
                                            List.concatMap
                                                (\state ->
                                                    List.map
                                                        (\carrier ->
                                                            { state = state
                                                            , carrier = carrier
                                                            , active = True
                                                            , targetGI = True
                                                            }
                                                        )
                                                        settings.carrierContracts
                                                )
                                                settings.stateLicenses
                                    }

                                GINone ->
                                    { settings
                                        | stateCarrierSettings =
                                            List.concatMap
                                                (\state ->
                                                    List.map
                                                        (\carrier ->
                                                            { state = state
                                                            , carrier = carrier
                                                            , active = True
                                                            , targetGI = False
                                                            }
                                                        )
                                                        settings.carrierContracts
                                                )
                                                settings.stateLicenses
                                    }

                                GIRecommended ->
                                    { settings
                                        | stateCarrierSettings =
                                            List.concatMap
                                                (\state ->
                                                    List.map
                                                        (\carrier ->
                                                            { state = state
                                                            , carrier = carrier
                                                            , active = True
                                                            , targetGI =
                                                                List.any
                                                                    (\rec ->
                                                                        rec.state == state && rec.carrier == carrier
                                                                    )
                                                                    model.recommendedGICombos
                                                            }
                                                        )
                                                        settings.carrierContracts
                                                )
                                                settings.stateLicenses
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
                    ( { model | recommendedGICombos = combos }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | status = Error "Failed to load GI recommendations" }
                    , Cmd.none
                    )

        ToggleAllowAgentSettings value ->
            updateSettings model (\s -> { s | allowAgentSettings = value })

        FinishSetup ->
            case model.currentUser of
                Just user ->
                    if model.planType == "basic" then
                        ( { model | isLoading = True }
                        , finalizeOrganization user.organizationSlug
                        )

                    else
                        ( model
                        , Nav.pushUrl model.key "/dashboard"
                        )

                Nothing ->
                    ( model
                    , Nav.pushUrl model.key "/dashboard"
                    )

        OrgFinalized result ->
            case result of
                Ok _ ->
                    ( { model | isLoading = False }
                    , Nav.pushUrl model.key "/dashboard"
                    )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                Http.BadStatus 500 ->
                                    "Failed to set up your organization's database. Please contact support at help@medicaremax.com and we'll help you get started."

                                Http.BadBody message ->
                                    message ++ "\nPlease contact support at help@medicaremax.com and we'll help you get started."

                                _ ->
                                    "An unexpected error occurred. Please contact support at help@medicaremax.com and we'll help you get started."
                    in
                    ( { model
                        | isLoading = False
                        , error = Just errorMessage
                      }
                    , Cmd.none
                    )

        SelectCommonStates region ->
            updateSettings model
                (\s ->
                    { s | stateLicenses = s.stateLicenses ++ getRegionStates region }
                )

        UpdateBrandName name ->
            updateSettings model (\s -> { s | brandName = name })

        UpdatePrimaryColor color ->
            updateSettings model (\s -> { s | primaryColor = color })

        UpdateSecondaryColor color ->
            updateSettings model (\s -> { s | secondaryColor = color })

        UploadLogo ->
            ( model
            , Select.file [ "image/png", "image/jpeg" ] GotLogo
            )

        GotLogo file ->
            ( model
            , Task.perform GotLogoUrl (File.toUrl file)
            )

        GotLogoUrl url ->
            updateSettings model (\s -> { s | logo = Just url })

        LogoUploaded result ->
            case result of
                Ok url ->
                    updateSettings model (\s -> { s | logo = Just url })

                Err error ->
                    ( { model | status = Error "Failed to upload logo" }, Cmd.none )


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
        , body = jsonBody (encodeSettings settings)
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
        , ( "brandName", Encode.string settings.brandName )
        , ( "primaryColor", Encode.string settings.primaryColor )
        , ( "secondaryColor", Encode.string settings.secondaryColor )
        , ( "logo", Maybe.withDefault Encode.null (Maybe.map Encode.string settings.logo) )
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
        [ if model.isSetup then
            SetupLayout.view SetupLayout.OrganizationSetup
                (model.planType == "basic")
                [ if model.isLoading then
                    viewLoading

                  else
                    viewSettings model
                ]

          else
            div [ class "min-h-screen bg-gray-50" ]
                [ viewHeader
                , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
                    [ if model.isLoading then
                        viewLoading

                      else
                        viewSettings model
                    ]
                ]
        ]
    }


viewSetupHeader : Html Msg
viewSetupHeader =
    div [ class "mb-8" ]
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
        [ class """sticky bottom-0 left-0 right-0 bg-white border-t border-gray-200 
                  px-4 py-4 sm:px-6 lg:px-8 flex justify-between items-center
                  mt-8"""
        ]
        [ case model.error of
            Just errorMsg ->
                div [ class "text-red-600 text-sm max-w-xl" ]
                    [ text errorMsg ]

            Nothing ->
                text ""
        , button
            [ class """px-4 py-2 text-sm font-medium text-white bg-blue-600 
                      rounded-md hover:bg-blue-700"""
            , onClick FinishSetup
            ]
            [ text "Next: Go to Dashboard" ]
        ]


viewSettingsContent : Maybe Settings -> Bool -> List String -> String -> Html Msg
viewSettingsContent maybeSettings canEdit expandedSections planType =
    case maybeSettings of
        Just settings ->
            div [ class "space-y-6" ]
                [ div [ class "bg-white shadow rounded-lg p-6" ]
                    [ div [ class "flex justify-between items-center mb-4" ]
                        [ h2 [ class "text-lg font-medium" ] [ text "Organization Settings" ]
                        , div [ class "px-3 py-1 bg-gray-100 rounded-full text-sm text-gray-600" ]
                            [ text (String.toUpper planType ++ " Plan") ]
                        ]
                    , if planType /= "basic" then
                        div [ class "space-y-4" ]
                            [ checkbox "Allow agents to customize their own settings"
                                settings.allowAgentSettings
                                ToggleAllowAgentSettings
                            ]

                      else
                        text ""
                    ]
                , viewBrandSettings settings
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


viewBrandSettings : Settings -> Html Msg
viewBrandSettings settings =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium mb-4" ] [ text "Agency Settings" ]
        , div [ class "space-y-6" ]
            [ div [ class "space-y-4" ]
                [ viewFormGroup "Agency Name"
                    (input
                        [ type_ "text"
                        , class "w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-purple-500 focus:border-purple-500"
                        , value settings.brandName
                        , onInput UpdateBrandName
                        ]
                        []
                    )
                , viewFormGroup "Primary Color"
                    (div [ class "flex items-center space-x-4" ]
                        [ input
                            [ type_ "color"
                            , class "w-16 h-10 p-1 border border-gray-300 rounded"
                            , value settings.primaryColor
                            , onInput UpdatePrimaryColor
                            ]
                            []
                        , input
                            [ type_ "text"
                            , class "flex-1 px-4 py-2 border border-gray-300 rounded-md focus:ring-purple-500 focus:border-purple-500"
                            , value settings.primaryColor
                            , onInput UpdatePrimaryColor
                            ]
                            []
                        ]
                    )
                , viewFormGroup "Secondary Color"
                    (div [ class "flex items-center space-x-4" ]
                        [ input
                            [ type_ "color"
                            , class "w-16 h-10 p-1 border border-gray-300 rounded"
                            , value settings.secondaryColor
                            , onInput UpdateSecondaryColor
                            ]
                            []
                        , input
                            [ type_ "text"
                            , class "flex-1 px-4 py-2 border border-gray-300 rounded-md focus:ring-purple-500 focus:border-purple-500"
                            , value settings.secondaryColor
                            , onInput UpdateSecondaryColor
                            ]
                            []
                        ]
                    )
                , viewFormGroup "Logo"
                    (div [ class "flex items-center space-x-4" ]
                        [ case settings.logo of
                            Just logoUrl ->
                                div [ class "flex items-center space-x-4" ]
                                    [ img
                                        [ src logoUrl
                                        , class "h-16 w-16 object-contain border border-gray-200 rounded"
                                        ]
                                        []
                                    , button
                                        [ class "px-4 py-2 text-sm text-purple-600 hover:text-purple-800"
                                        , onClick UploadLogo
                                        ]
                                        [ text "Change Logo" ]
                                    ]

                            Nothing ->
                                button
                                    [ class "px-4 py-2 text-sm text-purple-600 hover:text-purple-800 border border-purple-200 rounded"
                                    , onClick UploadLogo
                                    ]
                                    [ text "Upload Logo" ]
                        ]
                    )
                ]
            ]
        ]


viewFormGroup : String -> Html Msg -> Html Msg
viewFormGroup labelText content =
    div [ class "mb-4" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , content
        ]


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
                            , div [ class "ml-2 w-5 h-5 rounded-full bg-indigo-50 flex items-center justify-center text-indigo-700" ]
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
                    [ thead [ class "bg-gray-50" ]
                        [ tr []
                            (th [ class "px-3 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider sticky left-0 bg-gray-50 z-10 w-16" ]
                                [ text "State" ]
                                :: List.map
                                    (\carrier ->
                                        th [ class "px-3 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider w-24" ]
                                            [ text carrier ]
                                    )
                                    settings.carrierContracts
                            )
                        ]
                    , tbody [ class "bg-white divide-y divide-gray-200" ]
                        (List.indexedMap
                            (\index state ->
                                tr [ classList [ ( "bg-gray-50", modBy 2 index == 0 ) ] ]
                                    (td [ class "px-3 py-4 whitespace-nowrap text-sm font-medium text-gray-900 sticky left-0 bg-inherit z-10" ]
                                        [ text state ]
                                        :: List.map
                                            (\carrier ->
                                                let
                                                    setting =
                                                        findStateCarrierSetting settings state carrier
                                                in
                                                td [ class "px-3 py-2 whitespace-nowrap text-sm text-center" ]
                                                    [ div [ class "flex flex-col items-start space-y-1 w-20 mx-auto" ]
                                                        [ div [ class "w-full" ]
                                                            [ label [ class "flex items-center cursor-pointer w-full" ]
                                                                [ div [ class "relative w-4 h-4 mr-1 shrink-0" ]
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
                                                                        , class "absolute w-0 h-0 opacity-0"
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
                                                                , span [ class "text-xs ml-1" ] [ text "Active" ]
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
                                                                [ div [ class "relative w-4 h-4 mr-1 shrink-0" ]
                                                                    [ input
                                                                        [ type_ "checkbox"
                                                                        , checked setting.targetGI
                                                                        , onCheck (\targetGI -> UpdateStateCarrierSetting state carrier setting.active targetGI)
                                                                        , class "absolute w-0 h-0 opacity-0"
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
                                                                , span [ class "text-xs ml-1" ] [ text "GI" ]
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
        boolDecoder =
            Decode.oneOf
                [ Decode.bool
                , Decode.map (\n -> n == 1) Decode.int
                ]
    in
    Decode.field "success" Decode.bool
        |> Decode.andThen
            (\success ->
                if success then
                    Decode.map2 SettingsResponse
                        (Decode.field "orgSettings" settingsObjectDecoder)
                        (Decode.field "canEditOrgSettings" boolDecoder)

                else
                    Decode.fail "Settings request was not successful"
            )


settingsObjectDecoder : Decoder Settings
settingsObjectDecoder =
    let
        stateCarrierSettingsDecoder =
            Decode.field "stateCarrierSettings" <|
                Decode.oneOf
                    [ Decode.list stateCarrierSettingDecoder
                    , Decode.null []
                    ]
    in
    Decode.succeed Settings
        |> Pipeline.required "stateLicenses" (Decode.list Decode.string)
        |> Pipeline.required "carrierContracts" (Decode.list Decode.string)
        |> Pipeline.custom stateCarrierSettingsDecoder
        |> Pipeline.required "allowAgentSettings" Decode.bool
        |> Pipeline.required "emailSendBirthday" Decode.bool
        |> Pipeline.required "emailSendPolicyAnniversary" Decode.bool
        |> Pipeline.required "emailSendAep" Decode.bool
        |> Pipeline.required "smartSendEnabled" Decode.bool
        |> Pipeline.optional "brandName" Decode.string ""
        |> Pipeline.optional "primaryColor" Decode.string "#6B46C1"
        |> Pipeline.optional "secondaryColor" Decode.string "#9F7AEA"
        |> Pipeline.optional "logo" (Decode.nullable Decode.string) Nothing


stateCarrierSettingDecoder : Decoder StateCarrierSetting
stateCarrierSettingDecoder =
    Decode.map4 StateCarrierSetting
        (Decode.field "state" Decode.string)
        (Decode.field "carrier" Decode.string)
        (Decode.field "active" Decode.bool)
        (Decode.field "targetGI" Decode.bool)


recommendationsDecoder : Decoder (List StateCarrierSetting)
recommendationsDecoder =
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


viewNavLink : String -> String -> Html Msg
viewNavLink label path =
    a
        [ class "text-gray-700 hover:text-gray-900 hover:bg-gray-50 group flex items-center px-3 py-2 text-sm font-medium rounded-md"
        , href path
        ]
        [ text label ]


viewNavigation : Model -> Html Msg
viewNavigation model =
    nav []
        [ case model.currentUser of
            Just user ->
                if user.isAdmin then
                    viewNavLink "Manage Agents" "/agents"

                else
                    text ""

            Nothing ->
                text ""
        ]


viewLoading : Html msg
viewLoading =
    div [ class "flex justify-center items-center h-64" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-purple-500 border-t-transparent" ] []
        ]


viewHeader : Html msg
viewHeader =
    nav [ class "bg-white border-b border-gray-200" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex justify-between h-16" ]
                [ div [ class "flex" ]
                    [ div [ class "flex-shrink-0 flex items-center" ]
                        [ h1 [ class "text-xl font-semibold text-purple-600" ]
                            [ text "Organization Settings" ]
                        ]
                    ]
                ]
            ]
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    div [ class "space-y-8" ]
        [ viewSettingsContent model.orgSettings True model.expandedSections model.planType
        , viewBottomBar model
        ]


finalizeOrganization : String -> Cmd Msg
finalizeOrganization orgSlug =
    Http.post
        { url = "/api/organizations/" ++ orgSlug ++ "/setup-database"
        , body = Http.emptyBody
        , expect = Http.expectWhatever OrgFinalized
        }
