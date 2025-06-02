module Settings exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Components.ProgressIndicator
import Components.SetupLayout as SetupLayout
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, preventDefaultOn)
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
    , "Mutual of Omaha"
    ]


type Carrier
    = Aetna
    | Humana
    | UnitedHealthcare
    | Cigna
    | Aflac
    | MutualOfOmaha



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
    , hover : Bool
    , uploadingLogo : Bool
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
    , orgSignature : Bool
    , phone : String
    , redirectUrl : String
    , signature : String
    , forceOrgSenderDetails : Bool
    }


type alias SaveResponse =
    { success : Bool
    , error : Maybe String
    }


type Status
    = Loading
    | Loaded
    | Saving
    | Error String


type Msg
    = GotSettings (Result Http.Error SettingsResponse)
    | SaveSettings
    | SettingsSaved (Result Http.Error SaveResponse)
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
    | UpdatePhone String
    | UpdateRedirectUrl String
    | UpdateSignature String
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
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | UpdateForceOrgSenderDetails Bool


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
      , hover = False
      , uploadingLogo = False
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
    Http.get
        { url = "/api/self-service-info/"
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
                Ok response ->
                    ( { model
                        | status =
                            if response.success then
                                Loaded

                            else
                                Error (response.error |> Maybe.withDefault "Failed to save settings")
                      }
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

        UpdatePhone phone ->
            updateSettings model (\s -> { s | phone = phone })

        UpdateRedirectUrl url ->
            updateSettings model (\s -> { s | redirectUrl = url })

        UpdateSignature signature ->
            updateSettings model (\s -> { s | signature = signature })

        UploadLogo ->
            ( model
            , Select.file [ "image/png", "image/jpeg" ] GotLogo
            )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        GotFiles file files ->
            ( { model | hover = False, uploadingLogo = True }, Task.perform GotLogoUrl (File.toUrl file) )

        GotLogo file ->
            ( { model | uploadingLogo = True }, Task.perform GotLogoUrl (File.toUrl file) )

        GotLogoUrl url ->
            case model.orgSettings of
                Just settings ->
                    ( { model | logo = Just url, uploadingLogo = False, orgSettings = Just { settings | logo = Just url } }
                    , saveSettings { settings = Just { settings | logo = Just url }, logo = Just url }
                    )

                Nothing ->
                    ( { model | logo = Just url, uploadingLogo = False }, Cmd.none )

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

        UpdateForceOrgSenderDetails value ->
            updateSettings model (\s -> { s | forceOrgSenderDetails = value })


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
        , expect = expectJson SettingsSaved saveResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveResponseDecoder : Decoder SaveResponse
saveResponseDecoder =
    Decode.map2 SaveResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "error" (Decode.nullable Decode.string))


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
        , ( "name", Encode.string settings.brandName )
        , ( "primaryColor", Encode.string settings.primaryColor )
        , ( "secondaryColor", Encode.string settings.secondaryColor )
        , ( "logo", Maybe.withDefault Encode.null (Maybe.map Encode.string settings.logo) )
        , ( "orgSignature", Encode.bool settings.orgSignature )
        , ( "phone", Encode.string settings.phone )
        , ( "redirectUrl", Encode.string settings.redirectUrl )
        , ( "signature", Encode.string settings.signature )
        , ( "forceOrgSenderDetails", Encode.bool settings.forceOrgSenderDetails )
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
            div [ class "min-h-screen bg-white" ]
                [ viewHeader
                , div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 pt-2 sm:pt-4 pb-16" ]
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


viewSettings : Model -> Html Msg
viewSettings model =
    div [ class "space-y-12" ]
        [ case model.orgSettings of
            Just settings ->
                div [ class "space-y-12" ]
                    [ viewOrganizationDetails settings model
                    , viewCarriersOffered settings model
                    , viewSelfOnboardingLink model
                    , viewDefaultSenderSettings settings
                    ]

            Nothing ->
                div [ class "text-gray-500 italic" ]
                    [ text "Loading settings..." ]
        ]


viewOrganizationDetails : Settings -> Model -> Html Msg
viewOrganizationDetails settings model =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium text-gray-900 mb-6" ] [ text "Organization Details" ]
        , p [ class "text-sm text-gray-500 mb-6" ]
            [ text "If the Organization is at any point selected to be the sender, the following details will be used." ]
        , div [ class "space-y-6" ]
            [ div [ class "space-y-4" ]
                [ viewFormGroup "Agency Name"
                    (input
                        [ type_ "text"
                        , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base"
                        , value settings.brandName
                        , onInput UpdateBrandName
                        , placeholder "Example Biz"
                        ]
                        []
                    )
                , viewFormGroup "Phone number"
                    (div [ class "flex" ]
                        [ span [ class "inline-flex items-center px-3 py-2 rounded-l-md border border-r-0 border-gray-300 bg-gray-50 text-gray-500 shadow-sm" ]
                            [ text "US" ]
                        , input
                            [ type_ "tel"
                            , class "flex-1 px-3 py-2 rounded-r-md border border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base focus:z-10"
                            , value settings.phone
                            , onInput UpdatePhone
                            , placeholder "+1 (555) 000-0000"
                            ]
                            []
                        ]
                    )
                , viewFormGroup "Calendar Booking Link (optional)"
                    (div []
                        [ input
                            [ type_ "text"
                            , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base"
                            , value settings.redirectUrl
                            , onInput UpdateRedirectUrl
                            , placeholder "example.biz"
                            ]
                            []
                        , p [ class "text-gray-500 text-xs mt-1" ]
                            [ text "If provided, this link will be used as one of the options a client may select to connect with your agency. Traditionally this would be a Calendly link, Acuity link, etc." ]
                        ]
                    )
                , viewFormGroup "Email & SMS Signature or Sign Off"
                    (input
                        [ class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base"
                        , value settings.signature
                        , onInput UpdateSignature
                        , placeholder
                            ("The Team at "
                                ++ (if String.isEmpty settings.brandName then
                                        "Example Biz"

                                    else
                                        settings.brandName
                                   )
                            )
                        ]
                        []
                    )
                , viewFormGroup "Organization Logo"
                    (div []
                        [ if model.uploadingLogo then
                            div [ class "flex justify-center items-center h-32" ]
                                [ div [ class "animate-spin rounded-full h-8 w-8 border-2 border-purple-500 border-t-transparent" ] []
                                ]

                          else
                            div
                                [ class "border-2 border-dashed border-gray-300 rounded-lg p-6 flex flex-col items-center justify-center"
                                , classList [ ( "bg-purple-50 border-purple-300", model.hover ) ]
                                , hijackOn "dragenter" (Decode.succeed DragEnter)
                                , hijackOn "dragover" (Decode.succeed DragEnter)
                                , hijackOn "dragleave" (Decode.succeed DragLeave)
                                , hijackOn "drop" dropDecoder
                                ]
                                [ case model.logo of
                                    Just logoUrl ->
                                        div [ class "flex flex-col items-center space-y-4" ]
                                            [ img
                                                [ src logoUrl
                                                , class "h-24 w-auto object-contain"
                                                ]
                                                []
                                            , button
                                                [ class "px-4 py-2 text-sm text-purple-600 hover:text-purple-800 border border-purple-200 rounded"
                                                , onClick UploadLogo
                                                ]
                                                [ text "Change Logo" ]
                                            ]

                                    Nothing ->
                                        div [ class "flex flex-col items-center space-y-4 text-center" ]
                                            [ svg
                                                [ Svg.Attributes.class "h-10 w-10 text-gray-400"
                                                , viewBox "0 0 20 20"
                                                , fill "currentColor"
                                                ]
                                                [ path
                                                    [ fillRule "evenodd"
                                                    , d "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z"
                                                    , clipRule "evenodd"
                                                    ]
                                                    []
                                                ]
                                            , div []
                                                [ p [ class "text-gray-500" ] [ text "Drag and drop your logo here" ]
                                                , p [ class "text-gray-400 text-xs" ] [ text "or" ]
                                                , button
                                                    [ class "mt-2 px-4 py-2 text-sm text-purple-600 hover:text-purple-800 border border-purple-200 rounded"
                                                    , onClick UploadLogo
                                                    ]
                                                    [ text "Upload Logo" ]
                                                ]
                                            ]
                                ]
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
        [ h2 [ class "text-lg font-medium text-gray-900 mb-2" ] [ text "Organization Self-Onboarding Link" ]
        , p [ class "text-sm text-gray-500 mb-6" ]
            [ text "Share this link with clients or non-clients to gather missing information or capture new leads to your book of business." ]
        , div [ class "space-y-4" ]
            [ p [ class "text-sm font-medium text-gray-700 mb-2" ]
                [ text "Please note: when a user uses the link and fills out the form they will be added to your book without having a specified agent. To designate an agent for a new contact share the agent's self-onboarding link." ]
            , div [ class "mt-4" ]
                [ let
                    slug =
                        model.currentUser
                            |> Maybe.map .organizationSlug
                            |> Maybe.withDefault ""

                    selfOnboardingUrl =
                        model.selfOnboardingUrl
                            |> Maybe.withDefault ("medicaremax.ai/self-onboarding/" ++ slug)
                  in
                  div [ class "flex items-center space-x-2" ]
                    [ svg [ Svg.Attributes.class "h-5 w-5 text-gray-400", Svg.Attributes.viewBox "0 0 20 20", Svg.Attributes.fill "currentColor" ]
                        [ path [ Svg.Attributes.fillRule "evenodd", Svg.Attributes.d "M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z", Svg.Attributes.clipRule "evenodd" ] []
                        ]
                    , input
                        [ type_ "text"
                        , class "flex-1 px-3 py-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500 bg-gray-50 text-gray-500"
                        , value selfOnboardingUrl
                        , readonly True
                        ]
                        []
                    , button
                        [ class "px-4 py-2 text-sm bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                        , onClick CopySelfOnboardingLink
                        ]
                        [ text "Copy Link" ]
                    ]
                ]
            ]
        ]


viewDefaultSenderSettings : Settings -> Html Msg
viewDefaultSenderSettings settings =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium text-gray-900 mb-2" ] [ text "Default Sender Settings" ]
        , p [ class "text-sm text-gray-500 mb-6" ]
            [ text "When communication is sent to your book of business you can set the default sender settings for the organization." ]
        , div [ class "grid grid-cols-1 md:grid-cols-2 gap-4" ]
            [ -- Organization Only Card
              div
                [ class
                    ("relative rounded-lg border-2 p-6 cursor-pointer transition-all "
                        ++ (if settings.forceOrgSenderDetails then
                                "border-blue-500 bg-blue-50"

                            else
                                "border-gray-200 hover:border-gray-300 bg-white"
                           )
                    )
                , onClick (UpdateForceOrgSenderDetails True)
                ]
                [ div [ class "flex items-start" ]
                    [ div [ class "flex items-center h-5" ]
                        [ input
                            [ type_ "radio"
                            , name "senderSettings"
                            , checked settings.forceOrgSenderDetails
                            , class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300"
                            ]
                            []
                        ]
                    , div [ class "ml-3" ]
                        [ label [ class "font-medium text-gray-900" ] [ text "Organization Only" ]
                        , p [ class "text-sm text-gray-500 mt-1" ]
                            [ text "When this option is selected the Organization Details above will be used for the signature, phone number, and scheduling link if applicable." ]
                        , p [ class "text-sm text-gray-500 mt-3" ]
                            [ text "The Agent will not get an option to use their information." ]
                        , div [ class "mt-4 text-xs text-gray-400" ]
                            [ div [ class "mb-1" ] [ text "Recommended For:" ]
                            , div [] [ text "Agencies with a customer care or retention team." ]
                            ]
                        ]
                    ]
                ]
            , -- Agent's Choice Card
              div
                [ class
                    ("relative rounded-lg border-2 p-6 cursor-pointer transition-all "
                        ++ (if not settings.forceOrgSenderDetails then
                                "border-blue-500 bg-blue-50"

                            else
                                "border-gray-200 hover:border-gray-300 bg-white"
                           )
                    )
                , onClick (UpdateForceOrgSenderDetails False)
                ]
                [ div [ class "flex items-start" ]
                    [ div [ class "flex items-center h-5" ]
                        [ input
                            [ type_ "radio"
                            , name "senderSettings"
                            , checked (not settings.forceOrgSenderDetails)
                            , class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300"
                            ]
                            []
                        ]
                    , div [ class "ml-3" ]
                        [ label [ class "font-medium text-gray-900" ] [ text "Agent Details" ]
                        , p [ class "text-sm text-gray-500 mt-1" ]
                            [ text "When this option is selected the Agent will have the choice to use their own name or the organization's name for the signature, phone number, and scheduling link (if applicable)." ]
                        , div [ class "mt-4 text-xs text-gray-400" ]
                            [ div [ class "mb-1" ] [ text "Recommended For:" ]
                            , div [] [ text "Agencies with agents who provide ongoing support for servicing existing clients." ]
                            ]
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


viewCarriersOffered : Settings -> Model -> Html Msg
viewCarriersOffered settings model =
    let
        carriersToUse =
            if List.isEmpty model.loadedCarriers then
                allCarriers

            else
                model.loadedCarriers
    in
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-medium text-gray-900 mb-6" ] [ text "Carriers Offered" ]
        , p [ class "text-sm text-gray-500 mb-6" ]
            [ text "Select the carriers you would like offered to your clients. This will be set for all agents within the organization." ]
        , div []
            [ div [ class "mb-4" ]
                [ checkbox "Select to Offer All Carriers"
                    (List.length settings.carrierContracts == List.length carriersToUse)
                    ToggleAllCarriers
                ]
            , div [ class "grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-4" ]
                (List.map
                    (\carrier ->
                        checkbox carrier
                            (safeMember carrier settings.carrierContracts)
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
        , div [ class "mt-6 text-sm text-gray-600" ]
            [ p [ class "mb-2" ] [ text "We're adding new carriers every month!" ]
            , p []
                [ text "Don't see a carrier you want? Email us with carriers and states that you would like to see added to the platform at "
                , Html.a [ Html.Attributes.href "mailto:information@medicaremax.com", class "text-indigo-600 hover:text-indigo-800" ] [ text "information@medicaremax.com" ]
                , text "."
                ]
            ]
        ]


viewCarriersGrid : Settings -> Model -> Html Msg
viewCarriersGrid settings model =
    viewCarriersOffered settings model


safeMember : String -> List String -> Bool
safeMember carrier carriers =
    let
        func : String -> String
        func c =
            c
                |> String.toLower
                |> String.trim
                |> String.replace " " ""
    in
    carriers
        |> List.map func
        |> List.member (func carrier)


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
                        [ text "When enabled, SmartSend will automatically identify which carriers offer full carrier compensation for Guaranteed Issue (GI) policies." ]
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
        |> Pipeline.optional "allowAgentSettings" Decode.bool True
        |> Pipeline.optional "emailSendBirthday" Decode.bool False
        |> Pipeline.optional "emailSendPolicyAnniversary" Decode.bool False
        |> Pipeline.optional "emailSendAep" Decode.bool False
        |> Pipeline.optional "smartSendEnabled" Decode.bool True
        |> Pipeline.optional "brandName" Decode.string ""
        |> Pipeline.optional "primaryColor" Decode.string "#6B46C1"
        |> Pipeline.optional "secondaryColor" Decode.string "#9F7AEA"
        |> Pipeline.optional "logo" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "orgSignature" Decode.bool False
        |> Pipeline.optional "phone" Decode.string ""
        |> Pipeline.optional "redirectUrl" Decode.string ""
        |> Pipeline.optional "signature" Decode.string ""
        |> Pipeline.optional "forceOrgSenderDetails" Decode.bool False


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
    nav [ class "bg-white" ]
        [ div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex justify-between h-16" ]
                [ div [ class "flex" ]
                    [ div [ class "flex-shrink-0 flex items-center" ]
                        [ h1 [ class "text-2xl font-semibold" ]
                            [ text "Organization Settings" ]
                        ]
                    ]
                ]
            ]
        ]


finalizeOrganization : String -> Cmd Msg
finalizeOrganization orgSlug =
    Http.post
        { url = "/api/organizations/" ++ orgSlug ++ "/setup-database"
        , body = Http.emptyBody
        , expect = Http.expectWhatever OrgFinalized
        }



-- Helper functions for drag and drop


dropDecoder : Decode.Decoder Msg
dropDecoder =
    Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore GotFiles File.decoder)


hijackOn : String -> Decode.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
