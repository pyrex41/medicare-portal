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
import Ports
import StateRegions exposing (Region(..), getRegionStates, regionToString)
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, viewBox)
import Task



-- Constants


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



-- Add new type for deactivated carrier-state pairs


type alias DeactivatedPair =
    { carrier : String
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
    , selectedCarrier : Maybe String
    , loadedCarriers : List String
    , linkCopied : Bool
    , selfOnboardingUrl : Maybe String
    , logo : Maybe String
    }


type alias StateCarrierSetting =
    { carrier : String
    , active : Bool
    , targetGI : Bool
    }


type alias Settings =
    { carrierContracts : List String
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
    | AddCarrierContract String
    | RemoveCarrierContract String
    | UpdateStateCarrierSetting String Bool Bool
    | ToggleSection String
    | ToggleAllCarriers Bool
    | ApplyGISelection GISelectionMode
    | GotRecommendedGICombos (Result Http.Error (List StateCarrierSetting))
    | ToggleAllowAgentSettings Bool
    | FinishSetup
    | UpdateBrandName String
    | UpdatePrimaryColor String
    | UpdateSecondaryColor String
    | UploadLogo
    | GotLogo File
    | GotLogoUrl String
    | LogoUploaded (Result Http.Error String)
    | NoOp
    | OrgFinalized (Result Http.Error ())
    | SelectCarrier String
    | GotCarriers (Result Http.Error (List String))
    | CopySelfOnboardingLink
    | LinkCopied Bool
    | GotSelfOnboardingUrl (Result Http.Error SelfOnboardingUrlResponse)


type alias SettingsResponse =
    { orgSettings : Settings
    , logo : Maybe String
    , canEditOrgSettings : Bool
    }


type alias SelfOnboardingUrlResponse =
    { selfOnboardingUrl : String
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
      , selectedCarrier = Nothing
      , loadedCarriers = []
      , linkCopied = False
      , selfOnboardingUrl = Nothing
      , logo = Nothing
      }
    , Cmd.batch
        [ fetchSettings
        , fetchRecommendedGICombos
        , fetchCarriers
        , fetchSelfOnboardingUrl
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


fetchCarriers : Cmd Msg
fetchCarriers =
    Http.get
        { url = "/api/settings/carriers"
        , expect = Http.expectJson GotCarriers (Decode.list (Decode.field "name" Decode.string))
        }


fetchSelfOnboardingUrl : Cmd Msg
fetchSelfOnboardingUrl =
    let
        slug =
            -- This is a placeholder, we'll get the actual slug in the backend
            "latest"
    in
    Http.get
        { url = "/api/self-service/" ++ slug
        , expect = Http.expectJson GotSelfOnboardingUrl selfOnboardingUrlDecoder
        }


selfOnboardingUrlDecoder : Decoder SelfOnboardingUrlResponse
selfOnboardingUrlDecoder =
    Decode.map SelfOnboardingUrlResponse
        (Decode.field "selfOnboardingUrl" Decode.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotSettings result ->
            case result of
                Ok response ->
                    ( { model
                        | orgSettings = Just response.orgSettings
                        , logo = response.logo
                        , status = Loaded
                        , isLoading = False
                      }
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

        GotCarriers result ->
            case result of
                Ok carriers ->
                    ( { model | loadedCarriers = carriers }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

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

        AddCarrierContract carrier ->
            updateSettings model
                (\s ->
                    if List.member carrier s.carrierContracts then
                        s

                    else
                        { s
                            | carrierContracts = carrier :: s.carrierContracts
                            , stateCarrierSettings = List.map (\setting -> { setting | active = True }) s.stateCarrierSettings
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

        UpdateStateCarrierSetting carrier active targetGI ->
            updateSettings model
                (\s ->
                    let
                        existingSetting =
                            List.filter
                                (\setting ->
                                    setting.carrier == carrier
                                )
                                s.stateCarrierSettings
                                |> List.head

                        newSettings =
                            case existingSetting of
                                Just _ ->
                                    List.map
                                        (\setting ->
                                            if setting.carrier == carrier then
                                                { setting | active = active, targetGI = targetGI }

                                            else
                                                setting
                                        )
                                        s.stateCarrierSettings

                                Nothing ->
                                    { carrier = carrier
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

        ToggleAllCarriers checked ->
            case model.orgSettings of
                Just settings ->
                    let
                        carriersToUse =
                            if List.isEmpty model.loadedCarriers then
                                allCarriers

                            else
                                model.loadedCarriers

                        newSettings =
                            { settings
                                | carrierContracts =
                                    if checked then
                                        carriersToUse

                                    else
                                        []
                                , stateCarrierSettings =
                                    if checked then
                                        List.map
                                            (\carrier ->
                                                { carrier = carrier
                                                , active = True
                                                , targetGI = False
                                                }
                                            )
                                            carriersToUse

                                    else
                                        []
                            }
                    in
                    ( { model | orgSettings = Just newSettings }
                    , saveSettings { settings = Just newSettings, logo = model.logo }
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
                                                (\carrier ->
                                                    { carrier = carrier
                                                    , active = True
                                                    , targetGI = True
                                                    }
                                                )
                                                settings.carrierContracts
                                    }

                                GINone ->
                                    { settings
                                        | stateCarrierSettings =
                                            List.map
                                                (\carrier ->
                                                    { carrier = carrier
                                                    , active = True
                                                    , targetGI = False
                                                    }
                                                )
                                                settings.carrierContracts
                                    }

                                GIRecommended ->
                                    { settings
                                        | stateCarrierSettings =
                                            List.map
                                                (\carrier ->
                                                    { carrier = carrier
                                                    , active = True
                                                    , targetGI =
                                                        List.any
                                                            (\rec -> rec.carrier == carrier)
                                                            model.recommendedGICombos
                                                    }
                                                )
                                                settings.carrierContracts
                                    }
                    in
                    ( { model | orgSettings = Just newSettings }
                    , saveSettings { settings = Just newSettings, logo = model.logo }
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

        SelectCarrier carrier ->
            ( { model | selectedCarrier = Just carrier }, Cmd.none )

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
            case model.orgSettings of
                Just settings ->
                    ( { model | logo = Just url, orgSettings = Just { settings | logo = Just url } }
                    , saveSettings { settings = Just { settings | logo = Just url }, logo = Just url }
                    )

                Nothing ->
                    ( model, Cmd.none )

        LogoUploaded result ->
            case result of
                Ok url ->
                    updateSettings model (\s -> { s | logo = Just url })

                Err _ ->
                    ( { model | status = Error "Failed to upload logo" }, Cmd.none )

        CopySelfOnboardingLink ->
            let
                slug =
                    model.currentUser
                        |> Maybe.map .organizationSlug
                        |> Maybe.withDefault ""

                url =
                    model.selfOnboardingUrl
                        |> Maybe.withDefault ("http://localhost:5173/self-onboarding/" ++ slug)

                -- Command to copy link to clipboard using ports
                copyCmd =
                    Ports.copyToClipboard url
            in
            ( { model | linkCopied = False }, copyCmd )

        LinkCopied success ->
            ( { model | linkCopied = success }, Cmd.none )

        GotSelfOnboardingUrl result ->
            case result of
                Ok response ->
                    ( { model | selfOnboardingUrl = Just response.selfOnboardingUrl }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


updateSettings : Model -> (Settings -> Settings) -> ( Model, Cmd Msg )
updateSettings model updateFn =
    case model.orgSettings of
        Just settings ->
            let
                newSettings =
                    updateFn settings
            in
            ( { model | orgSettings = Just newSettings }
            , saveSettings { settings = Just newSettings, logo = model.logo }
            )

        Nothing ->
            ( model, Cmd.none )


saveSettings : { settings : Maybe Settings, logo : Maybe String } -> Cmd Msg
saveSettings { settings, logo } =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/settings/org"
        , body = jsonBody (Encode.object [ ( "settings", Maybe.withDefault Encode.null (Maybe.map encodeSettings settings) ), ( "logo", Maybe.withDefault Encode.null (Maybe.map Encode.string logo) ) ])
        , expect = expectJson SettingsSaved settingsObjectDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


encodeSettings : Settings -> Encode.Value
encodeSettings settings =
    Encode.object
        [ ( "carrierContracts", Encode.list Encode.string settings.carrierContracts )
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
        [ ( "carrier", Encode.string setting.carrier )
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
                5
                [ if model.isLoading then
                    viewLoading

                  else
                    viewSettings model
                ]

          else
            div [ class "min-h-screen bg-gray-50" ]
                [ viewHeader
                , div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-4 sm:py-8" ]
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
    h1 [ class "text-2xl font-semibold text-[#03045E] mb-6" ]
        [ text "Organization Settings" ]


viewBottomBar : Model -> Html Msg
viewBottomBar model =
    div
        [ class """sticky bottom-0 left-0 right-0 bg-white border-t border-gray-200 
                  px-4 py-4 sm:px-6 lg:px-8 flex justify-end items-center
                  mt-8 max-w-4xl mx-auto"""
        ]
        [ case model.error of
            Just errorMsg ->
                div [ class "text-red-600 text-sm max-w-xl" ]
                    [ text errorMsg ]

            Nothing ->
                text ""
        ]


viewSettingsContent : Maybe Settings -> Bool -> List String -> String -> Model -> Html Msg
viewSettingsContent maybeSettings canEdit expandedSections planType model =
    case maybeSettings of
        Just settings ->
            div [ class "space-y-6" ]
                [ viewBrandSettings settings model
                , viewEmailSettings settings
                , viewSelfOnboardingLink model
                , viewExpandableSection "Carrier Contracts"
                    (viewCarriersGrid settings model)
                    expandedSections
                , viewExpandableSection "State & Carrier Settings"
                    (viewStateCarrierGrid settings model)
                    expandedSections
                ]

        Nothing ->
            div [ class "text-gray-500 italic" ]
                [ text "Using organization settings" ]


viewBrandSettings : Settings -> Model -> Html Msg
viewBrandSettings settings model =
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
                        [ case model.logo of
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
            [ checkbox "Enable smart send" settings.smartSendEnabled ToggleSmartSend
            ]
        ]


viewSelfOnboardingLink : Model -> Html Msg
viewSelfOnboardingLink model =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium mb-4" ] [ text "Self Onboarding" ]
        , div [ class "space-y-4" ]
            [ p [ class "mb-4 text-gray-600" ]
                [ text "Share this link with companies that need to self-onboard into your platform." ]
            , div [ class "flex items-center space-x-3" ]
                [ let
                    slug =
                        model.currentUser
                            |> Maybe.map .organizationSlug
                            |> Maybe.withDefault ""

                    selfOnboardingUrl =
                        model.selfOnboardingUrl
                            |> Maybe.withDefault ("/self-onboarding/" ++ slug)
                  in
                  div [ class "flex-1 flex items-center space-x-4" ]
                    [ input
                        [ type_ "text"
                        , class "flex-1 px-4 py-2 border border-gray-300 rounded-md focus:ring-purple-500 focus:border-purple-500 bg-gray-50"
                        , value selfOnboardingUrl
                        , readonly True
                        ]
                        []
                    , button
                        [ class "px-4 py-2 text-sm bg-purple-600 text-white rounded hover:bg-purple-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-purple-500"
                        , onClick CopySelfOnboardingLink
                        ]
                        [ text
                            (if model.linkCopied then
                                "Copied!"

                             else
                                "Copy Link"
                            )
                        ]
                    ]
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


viewCarriersGrid : Settings -> Model -> Html Msg
viewCarriersGrid settings model =
    let
        carriersToUse =
            if List.isEmpty model.loadedCarriers then
                allCarriers

            else
                model.loadedCarriers
    in
    div []
        [ div [ class "mb-4 flex items-center" ]
            [ checkbox "Select All Carriers"
                (List.length settings.carrierContracts == List.length carriersToUse)
                ToggleAllCarriers
            ]
        , div [ class "grid grid-cols-2 sm:grid-cols-3 gap-4" ]
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
                carriersToUse
            )
        ]


viewStateCarrierGrid : Settings -> Model -> Html Msg
viewStateCarrierGrid settings model =
    let
        carriersToUse =
            if List.isEmpty model.loadedCarriers then
                allCarriers

            else
                model.loadedCarriers
    in
    div []
        [ div [ class "mb-6" ]
            [ h3 [ class "text-sm font-medium text-gray-700 mb-2" ]
                [ text "SmartSend for Guaranteed Issue" ]
            , div [ class "flex items-start p-4 bg-blue-50 rounded-md mb-6" ]
                [ div [ class "flex items-center h-5" ]
                    [ input
                        [ type_ "checkbox"
                        , checked settings.smartSendEnabled
                        , onCheck ToggleSmartSend
                        , class "h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
                        ]
                        []
                    ]
                , div [ class "ml-3 text-sm" ]
                    [ label [ class "font-medium text-gray-700" ]
                        [ text "Use SmartSend for Guaranteed Issue" ]
                    , p [ class "text-gray-500 mt-1" ]
                        [ text "When enabled, SmartSend will automatically identify which carriers offer full compensation for Guaranteed Issue (GI) policies." ]
                    ]
                ]
            , div [ class "mt-4 p-4 bg-gray-50 rounded-md mb-8" ]
                [ h3 [ class "text-md font-medium text-gray-900 mb-2" ]
                    [ text "How SmartSend Works" ]
                , p [ class "text-gray-600" ]
                    [ text "SmartSend analyzes each carrier to determine which ones offer full carrier compensation for Guaranteed Issue policies. This helps maximize your commissions while ensuring your quotes are always compliant with the latest carrier regulations." ]
                ]
            ]
        ]


option : List (Attribute msg) -> List (Html msg) -> Html msg
option attributes children =
    Html.option attributes children



-- Helper functions for state/carrier grid


hasDefaultSettings : Settings -> Bool
hasDefaultSettings settings =
    List.all
        (\setting -> setting.active && not setting.targetGI)
        settings.stateCarrierSettings


findStateCarrierSetting : Settings -> String -> StateCarrierSetting
findStateCarrierSetting settings carrier =
    settings.stateCarrierSettings
        |> List.filter (\s -> s.carrier == carrier)
        |> List.head
        |> Maybe.withDefault
            { carrier = carrier
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
                , onCheck (\active -> UpdateStateCarrierSetting setting.carrier active setting.targetGI)
                , class "h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
                ]
                []
            , span [ class "text-sm text-gray-600" ] [ text "Active" ]
            ]
        , label [ class "flex items-center gap-2 cursor-pointer" ]
            [ input
                [ type_ "checkbox"
                , checked setting.targetGI
                , onCheck (\targetGI -> UpdateStateCarrierSetting setting.carrier setting.active targetGI)
                , class "h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
                ]
                []
            , span [ class "text-xs ml-1" ] [ text "GI" ]
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
                    Decode.map3 SettingsResponse
                        (Decode.field "orgSettings" settingsObjectDecoder)
                        (Decode.field "logo" (Decode.nullable Decode.string))
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
    Decode.map3 StateCarrierSetting
        (Decode.field "carrier" Decode.string)
        (Decode.field "active" Decode.bool)
        (Decode.field "targetGI" Decode.bool)


recommendationsDecoder : Decoder (List StateCarrierSetting)
recommendationsDecoder =
    Decode.list
        (Decode.map3 StateCarrierSetting
            (Decode.field "carrier" Decode.string)
            (Decode.field "active" Decode.bool)
            (Decode.field "targetGI" Decode.bool)
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.onCopyResult LinkCopied


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
        [ div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8" ]
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
        [ case model.orgSettings of
            Just settings ->
                div [ class "space-y-6" ]
                    [ viewBrandSettings settings model
                    , viewEmailSettings settings
                    , viewSelfOnboardingLink model
                    , viewExpandableSection "Carrier Contracts"
                        (viewCarriersGrid settings model)
                        model.expandedSections
                    , viewExpandableSection "State & Carrier Settings"
                        (viewStateCarrierGrid settings model)
                        model.expandedSections
                    ]

            Nothing ->
                div [ class "text-gray-500 italic" ]
                    [ text "Loading settings..." ]
        , viewBottomBar model
        ]


finalizeOrganization : String -> Cmd Msg
finalizeOrganization orgSlug =
    Http.post
        { url = "/api/organizations/" ++ orgSlug ++ "/setup-database"
        , body = Http.emptyBody
        , expect = Http.expectWhatever OrgFinalized
        }
