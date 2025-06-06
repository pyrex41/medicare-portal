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
    { stateLicenses : List String
    , carrierContracts : List String
    , stateCarrierSettings : List StateCarrierSetting
    , isLoading : Bool
    , error : Maybe String
    , key : Nav.Key
    , orgSlug : String
    , expandedSections : List String
    , useRecommendedGISettings : Bool
    , contactOutreachDelayYears : Int
    , outreachTypes : OutreachTypes
    , failedUnderwritingOutreach : FailedUnderwritingOutreach
    }


type alias StateCarrierSetting =
    { state : String
    , carrier : String
    , active : Bool
    , targetGI : Bool
    }


type alias OutreachTypes =
    { birthday : Bool
    , enrollmentAnniversary : Bool
    , scheduleIncrease : Bool
    , aep : Bool
    }


type alias FailedUnderwritingOutreach =
    { enabled : Bool
    , frequency : String -- "annual" for now
    , timing : String -- "birthday", "enrollmentAnniversary", "aep", "scheduleIncrease"
    }


init : Nav.Key -> String -> ( Model, Cmd Msg )
init key orgSlug =
    ( { stateLicenses = []
      , carrierContracts = []
      , stateCarrierSettings = []
      , isLoading = False
      , error = Nothing
      , key = key
      , orgSlug = orgSlug
      , expandedSections = [ "State Licenses", "Carrier Contracts", "State & Carrier Settings" ]
      , useRecommendedGISettings = True
      , contactOutreachDelayYears = 1
      , outreachTypes = { birthday = True, enrollmentAnniversary = True, scheduleIncrease = True, aep = True }
      , failedUnderwritingOutreach = { enabled = False, frequency = "annual", timing = "birthday" }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddStateLicense String
    | RemoveStateLicense String
    | AddCarrierContract String
    | RemoveCarrierContract String
    | UpdateStateCarrierSetting String String Bool Bool
    | ToggleSection String
    | ToggleAllStates Bool
    | ToggleAllCarriers Bool
    | ToggleRecommendedGISettings Bool
    | NextStepClicked
    | GotLicensingSettings (Result Http.Error LicensingSettingsResponse)
    | LicensingSettingsSaved (Result Http.Error ())
    | NoOp
    | UpdateContactOutreachDelayYears Int
    | ToggleOutreachType String Bool
    | ToggleFailedUnderwritingOutreach Bool
    | UpdateFailedUnderwritingTiming String


type OutMsg
    = NoOutMsg
    | NextStep
    | ShowError String


type alias LicensingSettingsResponse =
    { stateLicenses : List String
    , carrierContracts : List String
    , stateCarrierSettings : List StateCarrierSetting
    }


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        AddStateLicense state ->
            let
                newModel =
                    if List.member state model.stateLicenses then
                        model

                    else
                        let
                            newStateCarrierSettings =
                                List.concat
                                    [ model.stateCarrierSettings
                                    , List.map
                                        (\carrier ->
                                            { state = state
                                            , carrier = carrier
                                            , active = True
                                            , targetGI = model.useRecommendedGISettings
                                            }
                                        )
                                        model.carrierContracts
                                    ]
                        in
                        { model
                            | stateLicenses = state :: model.stateLicenses
                            , stateCarrierSettings = newStateCarrierSettings
                        }
            in
            ( newModel, Cmd.none, NoOutMsg )

        RemoveStateLicense state ->
            let
                newModel =
                    { model
                        | stateLicenses = List.filter (\x -> x /= state) model.stateLicenses
                        , stateCarrierSettings = List.filter (\setting -> setting.state /= state) model.stateCarrierSettings
                    }
            in
            ( newModel, Cmd.none, NoOutMsg )

        AddCarrierContract carrier ->
            let
                newModel =
                    if List.member carrier model.carrierContracts then
                        model

                    else
                        let
                            newStateCarrierSettings =
                                List.concat
                                    [ model.stateCarrierSettings
                                    , List.map
                                        (\state ->
                                            { state = state
                                            , carrier = carrier
                                            , active = True
                                            , targetGI = model.useRecommendedGISettings
                                            }
                                        )
                                        model.stateLicenses
                                    ]
                        in
                        { model
                            | carrierContracts = carrier :: model.carrierContracts
                            , stateCarrierSettings = newStateCarrierSettings
                        }
            in
            ( newModel, Cmd.none, NoOutMsg )

        RemoveCarrierContract carrier ->
            let
                newModel =
                    { model
                        | carrierContracts = List.filter (\x -> x /= carrier) model.carrierContracts
                        , stateCarrierSettings = List.filter (\setting -> setting.carrier /= carrier) model.stateCarrierSettings
                    }
            in
            ( newModel, Cmd.none, NoOutMsg )

        UpdateStateCarrierSetting state carrier active targetGI ->
            let
                existingSetting =
                    List.filter
                        (\setting ->
                            setting.state == state && setting.carrier == carrier
                        )
                        model.stateCarrierSettings
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
                                model.stateCarrierSettings

                        Nothing ->
                            { state = state
                            , carrier = carrier
                            , active = active
                            , targetGI = targetGI
                            }
                                :: model.stateCarrierSettings

                newModel =
                    { model | stateCarrierSettings = newSettings }
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

        ToggleAllStates checked ->
            let
                newModel =
                    { model
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
                                            model.carrierContracts
                                    )
                                    allStates

                            else
                                []
                    }
            in
            ( newModel, Cmd.none, NoOutMsg )

        ToggleAllCarriers checked ->
            let
                newModel =
                    { model
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
                                    model.stateLicenses

                            else
                                []
                    }
            in
            ( newModel, Cmd.none, NoOutMsg )

        ToggleRecommendedGISettings useRecommended ->
            let
                newModel =
                    { model | useRecommendedGISettings = useRecommended }

                -- If turning on recommended settings, update all state carrier settings
                updatedSettings =
                    if useRecommended then
                        -- Apply recommended settings to all state/carrier combinations
                        List.map
                            (\setting ->
                                { setting | targetGI = setting.active }
                            )
                            model.stateCarrierSettings

                    else
                        -- Keep current settings
                        model.stateCarrierSettings
            in
            ( { newModel | stateCarrierSettings = updatedSettings }
            , Cmd.none
            , NoOutMsg
            )

        NextStepClicked ->
            -- Instead of making API calls, just move to the next step
            -- All data will be collected and submitted in the final step
            ( model, Cmd.none, NextStep )

        GotLicensingSettings result ->
            case result of
                Ok response ->
                    ( { model
                        | stateLicenses = response.stateLicenses
                        , carrierContracts = response.carrierContracts
                        , stateCarrierSettings = response.stateCarrierSettings
                        , isLoading = False
                      }
                    , Cmd.none
                    , NoOutMsg
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to load licensing settings"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to load licensing settings"
                    )

        LicensingSettingsSaved result ->
            case result of
                Ok _ ->
                    ( { model | isLoading = False }
                    , Cmd.none
                    , NextStep
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to save licensing settings"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to save licensing settings"
                    )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )

        UpdateContactOutreachDelayYears years ->
            ( { model | contactOutreachDelayYears = years }, Cmd.none, NoOutMsg )

        ToggleOutreachType typeName isActive ->
            let
                updatedOutreachTypes =
                    case typeName of
                        "birthday" ->
                            { model.outreachTypes | birthday = isActive }

                        "enrollmentAnniversary" ->
                            { model.outreachTypes | enrollmentAnniversary = isActive }

                        "scheduleIncrease" ->
                            { model.outreachTypes | scheduleIncrease = isActive }

                        "aep" ->
                            { model.outreachTypes | aep = isActive }

                        _ ->
                            model.outreachTypes
            in
            ( { model | outreachTypes = updatedOutreachTypes }, Cmd.none, NoOutMsg )

        ToggleFailedUnderwritingOutreach isActive ->
            ( { model | failedUnderwritingOutreach = { model.failedUnderwritingOutreach | enabled = isActive } }, Cmd.none, NoOutMsg )

        UpdateFailedUnderwritingTiming timing ->
            ( { model | failedUnderwritingOutreach = { model.failedUnderwritingOutreach | timing = timing } }, Cmd.none, NoOutMsg )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Licensing & Carriers" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Configure your state licenses and carrier contracts" ]
            ]
        , div [ class "space-y-6" ]
            [ viewExpandableSection "State Licenses"
                (viewLicensesGrid model)
                model.expandedSections
            , viewExpandableSection "Carrier Contracts"
                (viewCarriersGrid model)
                model.expandedSections
            , viewExpandableSection "State & Carrier Settings"
                (viewStateCarrierGrid model)
                model.expandedSections
            , viewExpandableSection "Outreach Settings"
                (viewOutreachSettings model)
                model.expandedSections
            , if model.error /= Nothing then
                div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                    [ text (Maybe.withDefault "" model.error) ]

              else
                text ""
            , div [ class "flex justify-end" ]
                [ button
                    [ class "px-6 py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                    , onClick NextStepClicked
                    ]
                    [ text "Continue" ]
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


viewLicensesGrid : Model -> Html Msg
viewLicensesGrid model =
    div []
        [ div [ class "mb-4 flex items-center" ]
            [ checkbox "Select All States"
                (List.length model.stateLicenses == List.length allStates)
                ToggleAllStates
            ]
        , div [ class "grid grid-cols-5 gap-4" ]
            (List.map
                (\state ->
                    checkbox state
                        (List.member state model.stateLicenses)
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


viewCarriersGrid : Model -> Html Msg
viewCarriersGrid model =
    div []
        [ div [ class "mb-4 flex items-center" ]
            [ checkbox "Select All Carriers"
                (List.length model.carrierContracts == List.length allCarriers)
                ToggleAllCarriers
            ]
        , div [ class "grid grid-cols-3 gap-4" ]
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


viewStateCarrierGrid : Model -> Html Msg
viewStateCarrierGrid model =
    if List.isEmpty model.stateLicenses || List.isEmpty model.carrierContracts then
        div [ class "text-gray-500 italic p-4 bg-yellow-50 border border-yellow-200 rounded-md" ]
            [ div [ class "font-medium text-yellow-800 mb-1" ]
                [ text "Setup Needed" ]
            , p []
                [ text "You need to select at least one state license and one carrier contract above before you can configure their settings." ]
            , if List.isEmpty model.stateLicenses && List.isEmpty model.carrierContracts then
                div [ class "mt-2" ]
                    [ text "Please select both state licenses and carrier contracts from the sections above." ]

              else if List.isEmpty model.stateLicenses then
                div [ class "mt-2" ]
                    [ text "Please select at least one state license from the State Licenses section above." ]

              else
                div [ class "mt-2" ]
                    [ text "Please select at least one carrier contract from the Carrier Contracts section above." ]
            ]

    else
        div []
            [ div [ class "mb-4 p-3 bg-blue-50 border border-blue-100 rounded-md" ]
                [ checkbox "Use recommended GI settings (applies GI to all active carrier combinations)"
                    model.useRecommendedGISettings
                    ToggleRecommendedGISettings
                , if model.useRecommendedGISettings then
                    p [ class "text-sm text-blue-600 mt-2" ]
                        [ text "With recommended settings, all active carrier combinations will automatically be configured for GI (Guaranteed Issue)." ]

                  else
                    p [ class "text-sm text-blue-600 mt-2" ]
                        [ text "Configure GI settings individually for each state and carrier combination below." ]
                ]
            , if not model.useRecommendedGISettings then
                div [ class "overflow-x-auto" ]
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
                                        model.carrierContracts
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
                                                            findStateCarrierSetting model state carrier
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
                                                model.carrierContracts
                                        )
                                )
                                model.stateLicenses
                            )
                        ]
                    ]

              else
                div [ class "p-4 bg-gray-50 rounded-md" ]
                    [ p [ class "text-gray-600 text-center" ]
                        [ text "Guaranteed Issue (GI) will be automatically applied to all active state and carrier combinations." ]
                    ]
            ]


viewOutreachSettings : Model -> Html Msg
viewOutreachSettings model =
    div [ class "space-y-6" ]
        [ -- Contact Outreach Delay
          div [ class "space-y-2" ]
            [ label [ class "block text-sm font-medium text-gray-700" ]
                [ text "Contact Outreach Delay" ]
            , select
                [ class "w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                , onInput (\value -> UpdateContactOutreachDelayYears (String.toInt value |> Maybe.withDefault 1))
                ]
                [ option [ value "1", selected (model.contactOutreachDelayYears == 1) ] [ text "1 year" ]
                , option [ value "2", selected (model.contactOutreachDelayYears == 2) ] [ text "2 years" ]
                , option [ value "3", selected (model.contactOutreachDelayYears == 3) ] [ text "3 years" ]
                ]
            , p [ class "text-sm text-gray-500" ]
                [ text "How long to wait before reaching out to contacts" ]
            ]
        , -- Outreach Types
          div [ class "space-y-3" ]
            [ label [ class "block text-sm font-medium text-gray-700" ]
                [ text "Outreach Types" ]
            , checkbox "Birthday outreach"
                model.outreachTypes.birthday
                (\checked -> ToggleOutreachType "birthday" checked)
            , checkbox "Enrollment anniversary outreach"
                model.outreachTypes.enrollmentAnniversary
                (\checked -> ToggleOutreachType "enrollmentAnniversary" checked)
            , checkbox "Schedule increase outreach"
                model.outreachTypes.scheduleIncrease
                (\checked -> ToggleOutreachType "scheduleIncrease" checked)
            , checkbox "Annual enrollment period (AEP) outreach"
                model.outreachTypes.aep
                (\checked -> ToggleOutreachType "aep" checked)
            , p [ class "text-sm text-gray-500" ]
                [ text "At least one outreach type must be selected" ]
            ]
        , -- Failed Underwriting Outreach
          div [ class "space-y-3" ]
            [ checkbox "Reduce outreach frequency to once per year for contacts who failed underwriting"
                model.failedUnderwritingOutreach.enabled
                ToggleFailedUnderwritingOutreach
            , if model.failedUnderwritingOutreach.enabled then
                div [ class "ml-6 space-y-2" ]
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "When to send annual outreach:" ]
                    , select
                        [ class "w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                        , onInput UpdateFailedUnderwritingTiming
                        ]
                        [ option [ value "birthday", selected (model.failedUnderwritingOutreach.timing == "birthday") ] [ text "Birthday" ]
                        , option [ value "enrollmentAnniversary", selected (model.failedUnderwritingOutreach.timing == "enrollmentAnniversary") ] [ text "Enrollment Anniversary" ]
                        , option [ value "aep", selected (model.failedUnderwritingOutreach.timing == "aep") ] [ text "AEP" ]
                        , option [ value "scheduleIncrease", selected (model.failedUnderwritingOutreach.timing == "scheduleIncrease") ] [ text "Schedule Increase" ]
                        ]
                    ]

              else
                text ""
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


findStateCarrierSetting : Model -> String -> String -> StateCarrierSetting
findStateCarrierSetting model state carrier =
    model.stateCarrierSettings
        |> List.filter (\s -> s.state == state && s.carrier == carrier)
        |> List.head
        |> Maybe.withDefault
            { state = state
            , carrier = carrier
            , active = True
            , targetGI = False
            }



-- API CALLS


fetchLicensingSettings : String -> Cmd Msg
fetchLicensingSettings orgSlug =
    Http.get
        { url = "/api/organizations/" ++ orgSlug ++ "/licensing-settings"
        , expect = Http.expectJson GotLicensingSettings licensingSettingsDecoder
        }


saveLicensingSettings : Model -> Cmd Msg
saveLicensingSettings model =
    Http.post
        { url = "/api/organizations/" ++ model.orgSlug ++ "/licensing-settings"
        , body = Http.jsonBody (encodeLicensingSettings model)
        , expect = Http.expectWhatever LicensingSettingsSaved
        }



-- DECODERS & ENCODERS


licensingSettingsDecoder : Decode.Decoder LicensingSettingsResponse
licensingSettingsDecoder =
    Decode.succeed LicensingSettingsResponse
        |> Pipeline.required "stateLicenses" (Decode.list Decode.string)
        |> Pipeline.required "carrierContracts" (Decode.list Decode.string)
        |> Pipeline.required "stateCarrierSettings" (Decode.list stateCarrierSettingDecoder)


stateCarrierSettingDecoder : Decode.Decoder StateCarrierSetting
stateCarrierSettingDecoder =
    Decode.map4 StateCarrierSetting
        (Decode.field "state" Decode.string)
        (Decode.field "carrier" Decode.string)
        (Decode.field "active" Decode.bool)
        (Decode.field "targetGI" Decode.bool)


encodeLicensingSettings : Model -> Encode.Value
encodeLicensingSettings model =
    let
        -- If using recommended settings, ensure all active combinations have targetGI set to true
        processedSettings =
            if model.useRecommendedGISettings then
                List.map
                    (\setting ->
                        { setting | targetGI = setting.active }
                    )
                    model.stateCarrierSettings

            else
                model.stateCarrierSettings
    in
    Encode.object
        [ ( "stateLicenses", Encode.list Encode.string model.stateLicenses )
        , ( "carrierContracts", Encode.list Encode.string model.carrierContracts )
        , ( "stateCarrierSettings", Encode.list stateCarrierSettingEncoder processedSettings )
        , ( "useRecommendedGISettings", Encode.bool model.useRecommendedGISettings )
        ]


stateCarrierSettingEncoder : StateCarrierSetting -> Encode.Value
stateCarrierSettingEncoder setting =
    Encode.object
        [ ( "state", Encode.string setting.state )
        , ( "carrier", Encode.string setting.carrier )
        , ( "active", Encode.bool setting.active )
        , ( "targetGI", Encode.bool setting.targetGI )
        ]



-- CONSTANTS


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
