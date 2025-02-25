module AddAgent exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Components.ProgressIndicator
import Components.SetupLayout as SetupLayout
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser, chompIf, chompWhile, end, succeed, symbol)
import StateRegions exposing (Region(..), getRegionStates, regionToString)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, viewBox)
import Time



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


type alias Model =
    { email : String
    , firstName : String
    , lastName : String
    , rawPhone : String
    , displayPhone : String
    , isAdmin : Bool
    , isAgent : Bool
    , carriers : List String
    , stateLicenses : List String
    , error : Maybe String
    , isSetup : Bool
    , key : Nav.Key
    , isLoading : Bool
    , agents : List Agent
    , showAddForm : Bool
    , currentUser : Maybe CurrentUser
    , isLoadingForAgent : Maybe String
    , orgSettings : Maybe Settings
    , pendingSave : Maybe String
    , planType : String
    }


type alias User =
    { id : String
    , email : String
    , firstName : String
    , lastName : String
    , isAdmin : Bool
    , isAgent : Bool
    , phone : String
    }


type alias Agent =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    , isAdmin : Bool
    , isAgent : Bool
    , carriers : List String
    , stateLicenses : List String
    , expanded : Bool
    }


type alias CurrentUser =
    { id : String
    , email : String
    , firstName : String
    , lastName : String
    , isAdmin : Bool
    , isAgent : Bool
    , phone : String
    }


type Msg
    = NoOp
    | UpdateEmail String
    | UpdateFirstName String
    | UpdateLastName String
    | UpdatePhone String
    | ToggleAdmin String
    | ToggleAgent String
    | UpdateField String String
    | SaveAgent
    | AgentSaved (Result Http.Error ())
    | NavigateTo String
    | CloseModal
    | ShowModal
    | ToggleCarrier String Bool
    | ToggleState String Bool
    | SelectAllCarriers Bool
    | SelectAllStates Bool
    | DeleteAgent String
    | AgentDeleted (Result Http.Error ())
    | FinishSetup
    | SelectCommonStates Region
    | LoadFromOrg
    | GotOrgSettings (Result Http.Error Settings)
    | AddAnotherAgent
    | CancelAddAgent
    | RemoveAgent String
    | FetchAgents
    | GotAgents (Result Http.Error (List Agent))
    | GotCurrentUser (Result Http.Error CurrentUserResponse)
    | ToggleAgentRole String String
    | UpdateAgentField String String String
    | ToggleAgentExpanded String
    | UpdateAgentCarrier String String Bool
    | UpdateAgentState String String Bool
    | SelectAllStatesForAgent String Bool
    | SelectCommonStatesForAgent String Region
    | LoadFromOrgForAgent String
    | GotOrgSettingsForAgent String (Result Http.Error Settings)
    | SelectAllCarriersForAgent String Bool
    | SaveAgentDetails String
    | AgentDetailsSaved String (Result Http.Error ())
    | DebounceSaveAgent String
    | EditAgent Agent


type alias CurrentUserResponse =
    { success : Bool
    , user : Maybe User
    }


init : Bool -> Nav.Key -> Maybe CurrentUser -> String -> ( Model, Cmd Msg )
init isSetup key currentUser planType =
    ( { email = ""
      , firstName = ""
      , lastName = ""
      , rawPhone = ""
      , displayPhone = ""
      , isAdmin = False
      , isAgent = False
      , carriers = []
      , stateLicenses = []
      , error = Nothing
      , isSetup = isSetup
      , key = key
      , isLoading = True
      , agents = []
      , showAddForm = False
      , currentUser = currentUser
      , isLoadingForAgent = Nothing
      , orgSettings = Nothing
      , pendingSave = Nothing
      , planType = planType
      }
    , fetchAgents
    )


view : Model -> Browser.Document Msg
view model =
    { title =
        if model.isSetup then
            "Add Team Members"

        else
            "Manage Agents"
    , body =
        [ if model.isSetup then
            SetupLayout.view SetupLayout.AgentSetup
                (model.planType == "basic")
                [ div [ class "max-w-3xl mx-auto pb-24" ]
                    [ viewSetupHeader model
                    , viewAgentsList model
                    , if model.showAddForm then
                        div [ class "bg-white shadow rounded-lg p-6 space-y-6 mt-6" ]
                            [ viewBasicInfo model
                            , viewWritingNumbers model
                            , viewStateLicenses model
                            ]

                      else
                        viewAddAgentButton
                    ]
                , viewBottomBar model
                ]

          else
            div [ class "min-h-screen bg-gray-50" ]
                [ viewHeader
                , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8 pb-24" ]
                    [ viewAgentsList model
                    , if model.showAddForm then
                        div [ class "bg-white shadow rounded-lg p-6 space-y-6 mt-6" ]
                            [ viewBasicInfo model
                            , viewWritingNumbers model
                            , viewStateLicenses model
                            ]

                      else
                        viewAddAgentButton
                    ]
                , viewBottomBar model
                ]
        ]
    }


viewSetupHeader : Model -> Html Msg
viewSetupHeader model =
    div [ class "mb-8" ]
        [ h1 [ class "text-3xl font-bold text-gray-900" ]
            [ text "Add Your First Agent" ]
        , p [ class "mt-2 text-gray-600" ]
            [ text "Set up your first agent to get started" ]
        ]


viewNormalHeader : Html Msg
viewNormalHeader =
    h1 [ class "text-2xl font-semibold text-gray-900 mb-6" ]
        [ text "Add Agent" ]


viewHeader : Html msg
viewHeader =
    nav [ class "bg-white border-b border-gray-200" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex justify-between h-16" ]
                [ div [ class "flex" ]
                    [ div [ class "flex-shrink-0 flex items-center" ]
                        [ h1 [ class "text-xl font-semibold text-purple-600" ]
                            [ text "Manage Agents" ]
                        ]
                    ]
                ]
            ]
        ]


viewBasicInfo : Model -> Html Msg
viewBasicInfo model =
    div [ class "space-y-6" ]
        [ div [ class "border-b border-gray-200 pb-4" ]
            [ h2 [ class "text-lg font-medium text-gray-900" ]
                [ text "Add New Agent" ]
            , p [ class "mt-1 text-sm text-gray-500" ]
                [ text "Fill in the agent's information below" ]
            ]
        , div [ class "space-y-4" ]
            [ div [ class "grid grid-cols-2 gap-4" ]
                [ div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "First Name" ]
                    , input
                        ([ type_ "text"
                         , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
                         , value
                            (if isAdminBecomingAgent model then
                                case model.currentUser of
                                    Just user ->
                                        user.firstName

                                    Nothing ->
                                        model.firstName

                             else
                                model.firstName
                            )
                         , placeholder "Enter first name"
                         ]
                            ++ (if isAdminBecomingAgent model then
                                    [ disabled True ]

                                else
                                    [ onInput UpdateFirstName ]
                               )
                        )
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "Last Name" ]
                    , input
                        ([ type_ "text"
                         , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
                         , value
                            (if isAdminBecomingAgent model then
                                case model.currentUser of
                                    Just user ->
                                        user.lastName

                                    Nothing ->
                                        model.lastName

                             else
                                model.lastName
                            )
                         , placeholder "Enter last name"
                         ]
                            ++ (if isAdminBecomingAgent model then
                                    [ disabled True ]

                                else
                                    [ onInput UpdateLastName ]
                               )
                        )
                        []
                    ]
                ]
            , div [ class "grid grid-cols-2 gap-4" ]
                [ div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "Email" ]
                    , input
                        ([ type_ "email"
                         , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
                         , value
                            (if isAdminBecomingAgent model then
                                Maybe.map .email model.currentUser |> Maybe.withDefault ""

                             else
                                model.email
                            )
                         , placeholder "name@example.com"
                         ]
                            ++ (if isAdminBecomingAgent model then
                                    [ disabled True ]

                                else
                                    [ onInput UpdateEmail ]
                               )
                        )
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "Phone" ]
                    , input
                        [ type_ "tel"
                        , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
                        , value model.displayPhone
                        , onInput UpdatePhone
                        , placeholder "(555) 555-5555"
                        , disabled (not (isAdminBecomingAgent model))
                        ]
                        []
                    ]
                ]
            , div [ class "mt-4" ]
                [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                    [ text "Role" ]
                , div [ class "flex items-center space-x-6" ]
                    [ label [ class "inline-flex items-center" ]
                        [ input
                            [ type_ "checkbox"
                            , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                            , checked model.isAdmin
                            , onClick (ToggleAdmin "")
                            , disabled (not (isAdminBecomingAgent model))
                            ]
                            []
                        , span [ class "ml-2 text-sm text-gray-700" ]
                            [ text "Admin" ]
                        ]
                    , label [ class "inline-flex items-center" ]
                        [ input
                            [ type_ "checkbox"
                            , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                            , checked
                                (if model.showAddForm then
                                    True

                                 else
                                    model.isAgent
                                )
                            , onClick (ToggleAgent "")
                            , disabled (not (isAdminBecomingAgent model))
                            ]
                            []
                        , span [ class "ml-2 text-sm text-gray-700" ]
                            [ text "Agent" ]
                        ]
                    ]
                ]
            ]
        ]


viewWritingNumbers : Model -> Html Msg
viewWritingNumbers model =
    let
        orgCarriers =
            model.orgSettings
                |> Maybe.map .carrierContracts
                |> Maybe.withDefault []
    in
    div [ class "space-y-4" ]
        [ h3 [ class "text-lg font-medium text-gray-900" ]
            [ text "Carriers" ]
        , div [ class "mb-4 space-y-2" ]
            [ div [ class "flex gap-4" ]
                [ div []
                    [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                        [ text "Batch Select" ]
                    , div [ class "flex gap-2" ]
                        [ button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px] disabled:opacity-50 disabled:cursor-not-allowed"
                            , onClick (SelectAllCarriers True)
                            , disabled (not (isAdminBecomingAgent model))
                            ]
                            [ text "Select All" ]
                        , button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px] disabled:opacity-50 disabled:cursor-not-allowed"
                            , onClick (SelectAllCarriers False)
                            , disabled (not (isAdminBecomingAgent model))
                            ]
                            [ text "Clear All" ]
                        ]
                    ]
                ]
            ]
        ]


viewStateLicenses : Model -> Html Msg
viewStateLicenses model =
    let
        orgStates =
            model.orgSettings
                |> Maybe.map .stateLicenses
                |> Maybe.withDefault []
    in
    div [ class "space-y-4" ]
        [ h3 [ class "text-lg font-medium text-gray-900" ]
            [ text "State Licenses" ]
        , div [ class "mb-4 space-y-2" ]
            [ div [ class "space-y-4" ]
                [ div []
                    [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                        [ text "Batch Select" ]
                    , div [ class "flex gap-2" ]
                        [ button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px] disabled:opacity-50 disabled:cursor-not-allowed"
                            , onClick (SelectAllStates True)
                            , disabled (not (isAdminBecomingAgent model))
                            ]
                            [ text "Select All" ]
                        , button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px] disabled:opacity-50 disabled:cursor-not-allowed"
                            , onClick (SelectAllStates False)
                            , disabled (not (isAdminBecomingAgent model))
                            ]
                            [ text "Clear All" ]
                        ]
                    ]
                , div []
                    [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                        [ text "By Region:" ]
                    , div [ class "flex gap-2" ]
                        (List.map
                            (\region ->
                                button
                                    [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                                    , onClick (SelectCommonStates region)
                                    , disabled (not (isAdminBecomingAgent model))
                                    ]
                                    [ text (regionToString region) ]
                            )
                            StateRegions.allRegions
                        )
                    ]
                ]
            ]
        , div [ class "grid grid-cols-6 gap-4" ]
            (List.map
                (\state ->
                    let
                        isEnabled =
                            List.member state orgStates
                    in
                    label
                        [ class "inline-flex items-center"
                        , classList [ ( "opacity-50 cursor-not-allowed", not isEnabled || not (isAdminBecomingAgent model) ) ]
                        ]
                        [ input
                            [ type_ "checkbox"
                            , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                            , checked (List.member state model.stateLicenses)
                            , onCheck (\isChecked -> ToggleState state isChecked)
                            , disabled (not isEnabled || not (isAdminBecomingAgent model))
                            ]
                            []
                        , span [ class "ml-2 text-sm text-gray-700" ]
                            [ text state ]
                        ]
                )
                allStates
            )
        ]


viewAgentsList : Model -> Html Msg
viewAgentsList model =
    div [ class "space-y-6" ]
        [ div [ class "grid grid-cols-1 gap-6" ]
            (List.map
                (\agent ->
                    div [ class "bg-white shadow rounded-lg p-6" ]
                        [ div [ class "flex items-center justify-between" ]
                            [ div [ class "flex items-center" ]
                                [ div [ class "ml-4" ]
                                    [ div [ class "text-lg font-medium text-gray-900" ]
                                        [ text (agent.firstName ++ " " ++ agent.lastName) ]
                                    , div [ class "text-sm text-gray-500" ]
                                        [ text agent.email ]
                                    ]
                                ]
                            , div [ class "flex items-center space-x-4" ]
                                [ button
                                    [ class "text-gray-400 hover:text-gray-500"
                                    , onClick (EditAgent agent)
                                    ]
                                    [ text "Edit" ]
                                , button
                                    [ class "text-red-400 hover:text-red-500"
                                    , onClick (DeleteAgent agent.id)
                                    ]
                                    [ text "Delete" ]
                                ]
                            ]
                        ]
                )
                model.agents
            )
        ]


viewAgentItem : Model -> Agent -> Html Msg
viewAgentItem model agent =
    let
        isCurrentUserAgent =
            case model.currentUser of
                Just user ->
                    user.id == agent.id

                Nothing ->
                    False

        isValid =
            not (String.isEmpty agent.phone)
                && not (String.isEmpty agent.firstName)
                && not (String.isEmpty agent.lastName)
                && not (String.isEmpty agent.email)
                && (agent.isAdmin || agent.isAgent)

        validationIndicator =
            if not isValid then
                span [ class "text-red-500 ml-2" ] [ text "*" ]

            else
                text ""

        formattedPhone =
            formatPhoneNumber (String.filter Char.isDigit agent.phone)
    in
    div [ class "mb-4" ]
        [ div [ class "bg-white border border-gray-200 rounded-lg shadow-sm hover:shadow-md transition-shadow duration-200" ]
            [ div
                [ class "p-4"
                ]
                [ div
                    [ class "flex items-center justify-between cursor-pointer"
                    , onClick (ToggleAgentExpanded agent.id)
                    ]
                    [ div [ class "min-w-0" ]
                        [ div [ class "flex items-center" ]
                            [ p [ class "text-sm font-medium text-gray-900 truncate" ]
                                [ text (agent.firstName ++ " " ++ agent.lastName) ]
                            , validationIndicator
                            ]
                        , div [ class "flex flex-col space-y-1" ]
                            [ p [ class "text-sm text-gray-500 truncate" ]
                                [ text agent.email ]
                            , p [ class "text-sm text-gray-500" ]
                                [ text formattedPhone ]
                            ]
                        ]
                    ]
                ]
            , if agent.expanded then
                div [ class "border-t border-gray-200 p-4 bg-gray-50" ]
                    [ viewAgentDetails model agent ]

              else
                text ""
            ]
        ]


viewAgentDetails : Model -> Agent -> Html Msg
viewAgentDetails model agent =
    let
        orgCarriers =
            model.orgSettings
                |> Maybe.map .carrierContracts
                |> Maybe.withDefault []

        orgStates =
            model.orgSettings
                |> Maybe.map .stateLicenses
                |> Maybe.withDefault []

        fieldError field =
            case field of
                "phone" ->
                    String.isEmpty agent.phone

                "firstName" ->
                    String.isEmpty agent.firstName

                "lastName" ->
                    String.isEmpty agent.lastName

                "email" ->
                    String.isEmpty agent.email

                _ ->
                    False

        errorIndicator field =
            if fieldError field then
                span [ class "text-red-500 ml-1" ] [ text "*" ]

            else
                text ""

        isCurrentUserAgent =
            case model.currentUser of
                Just user ->
                    user.id == agent.id

                Nothing ->
                    False

        formattedPhone =
            formatPhoneNumber (String.filter Char.isDigit agent.phone)

        isLoading =
            model.isLoadingForAgent == Just agent.id

        canEdit =
            canModifySettings model agent.id

        hasChanges =
            model.pendingSave == Just agent.id

        isSaving =
            model.isLoadingForAgent == Just agent.id

        onFieldInput : String -> String -> Msg
        onFieldInput field value =
            UpdateAgentField agent.id field value

        onSelectAllCarriers : Bool -> Msg
        onSelectAllCarriers isSelected =
            SelectAllCarriersForAgent agent.id isSelected
    in
    div [ class "space-y-6" ]
        [ div [ class "space-y-4" ]
            [ div [ class "grid grid-cols-2 gap-4" ]
                [ div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "First Name"
                        , errorIndicator "firstName"
                        ]
                    , input
                        [ type_ "text"
                        , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
                        , value agent.firstName
                        , onInput (onFieldInput "firstName")
                        , disabled (not canEdit)
                        ]
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "Last Name"
                        , errorIndicator "lastName"
                        ]
                    , input
                        [ type_ "text"
                        , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
                        , value agent.lastName
                        , onInput (onFieldInput "lastName")
                        , disabled (not canEdit)
                        ]
                        []
                    ]
                ]
            , div [ class "grid grid-cols-2 gap-4" ]
                [ div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "Email"
                        , errorIndicator "email"
                        ]
                    , input
                        [ type_ "email"
                        , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
                        , value agent.email
                        , onInput (onFieldInput "email")
                        , disabled (not canEdit)
                        ]
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "Phone"
                        , errorIndicator "phone"
                        ]
                    , input
                        [ type_ "tel"
                        , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
                        , value formattedPhone
                        , onInput (onFieldInput "phone")
                        , placeholder "(555) 555-5555"
                        , disabled (not canEdit)
                        ]
                        []
                    ]
                ]
            ]
        , if agent.isAgent then
            div [ class "space-y-6" ]
                [ div [ class "mt-6" ]
                    [ button
                        [ class "w-full inline-flex justify-center items-center px-4 py-2 border border-gray-300 shadow-sm text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                        , onClick (LoadFromOrgForAgent agent.id)
                        , disabled (not canEdit)
                        ]
                        [ if isLoading then
                            div [ class "flex items-center" ]
                                [ div [ class "animate-spin -ml-1 mr-3 h-5 w-5 text-gray-700" ]
                                    [ svg
                                        [ Svg.Attributes.class "h-5 w-5"
                                        , viewBox "0 0 24 24"
                                        ]
                                        [ path
                                            [ d "M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                                            , fill "currentColor"
                                            ]
                                            []
                                        ]
                                    ]
                                , text "Loading..."
                                ]

                          else
                            text "Copy from Organization Settings"
                        ]
                    ]
                , div [ class "space-y-4" ]
                    [ h3 [ class "text-lg font-medium text-gray-900" ]
                        [ text "Carriers" ]
                    , div [ class "mb-4 space-y-2" ]
                        [ div [ class "flex gap-4" ]
                            [ div []
                                [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                                    [ text "Batch Select" ]
                                , div [ class "flex gap-2" ]
                                    [ button
                                        [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px] disabled:opacity-50 disabled:cursor-not-allowed"
                                        , onClick (onSelectAllCarriers True)
                                        , disabled (not canEdit)
                                        ]
                                        [ text "Select All" ]
                                    , button
                                        [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px] disabled:opacity-50 disabled:cursor-not-allowed"
                                        , onClick (onSelectAllCarriers False)
                                        , disabled (not canEdit)
                                        ]
                                        [ text "Clear All" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

          else
            text ""
        ]


viewAddAgentButton : Html Msg
viewAddAgentButton =
    div [ class "text-center mt-8" ]
        [ button
            [ class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700"
            , onClick AddAnotherAgent
            ]
            [ span [ class "mr-2" ] [ text "+" ]
            , text "Add Agent"
            ]
        ]


viewBottomBar : Model -> Html Msg
viewBottomBar model =
    let
        allAgentsValid =
            List.all
                (\agent ->
                    not (String.isEmpty agent.phone)
                        && not (String.isEmpty agent.firstName)
                        && not (String.isEmpty agent.lastName)
                        && not (String.isEmpty agent.email)
                        && (agent.isAdmin || agent.isAgent)
                )
                model.agents

        canAdd =
            not (String.isEmpty (String.trim model.firstName))
                && not (String.isEmpty (String.trim model.lastName))
                && isValidEmail model.email
                && isValidPhone model.displayPhone
                && (model.isAdmin || model.showAddForm)
    in
    div [ class "fixed bottom-0 left-0 right-0 bg-white border-t border-gray-200 px-4 py-4 sm:px-6 z-10" ]
        [ div [ class "max-w-3xl mx-auto" ]
            [ if model.error /= Nothing then
                div [ class "mb-4" ]
                    [ p [ class "text-red-600" ]
                        [ text (Maybe.withDefault "" model.error) ]
                    ]

              else
                text ""
            , if model.showAddForm then
                div [ class "flex items-center justify-end space-x-4" ]
                    [ button
                        [ class "px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50"
                        , onClick CancelAddAgent
                        ]
                        [ text "Cancel" ]
                    , button
                        [ class "px-4 py-2 text-sm font-medium text-white bg-blue-600 rounded-md hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
                        , onClick SaveAgent
                        , disabled (not canAdd)
                        ]
                        [ text "Add Agent" ]
                    ]

              else if model.isSetup then
                div [ class "flex justify-end" ]
                    [ button
                        [ class "px-4 py-2 text-sm font-medium text-white bg-green-600 rounded-md hover:bg-green-700 disabled:opacity-50 disabled:cursor-not-allowed"
                        , onClick FinishSetup
                        , disabled (not allAgentsValid || List.isEmpty model.agents)
                        ]
                        [ text "Continue to Dashboard" ]
                    ]

              else
                text ""
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        UpdateFirstName name ->
            ( { model | firstName = name }, Cmd.none )

        UpdateLastName name ->
            ( { model | lastName = name }, Cmd.none )

        UpdatePhone input ->
            let
                rawDigits =
                    String.filter Char.isDigit input
                        |> String.left 10

                formattedPhone =
                    formatPhoneNumber rawDigits

                _ =
                    Debug.log "Phone number updated" { raw = rawDigits, formatted = formattedPhone }
            in
            ( { model
                | rawPhone = rawDigits
                , displayPhone = formattedPhone
                , pendingSave = Just "main" -- Add pending save for main agent
              }
            , Cmd.none
            )

        ToggleAdmin agentId ->
            let
                updatedAgents =
                    List.map
                        (\agent ->
                            if agent.id == agentId then
                                { agent | isAdmin = not agent.isAdmin }

                            else
                                agent
                        )
                        model.agents
            in
            ( { model | agents = updatedAgents }
            , case List.head (List.filter (\a -> a.id == agentId) updatedAgents) of
                Just agent ->
                    saveAgentDetails agent

                Nothing ->
                    Cmd.none
            )

        ToggleAgent agentId ->
            let
                updatedAgents =
                    List.map
                        (\agent ->
                            if agent.id == agentId then
                                { agent | isAgent = not agent.isAgent }

                            else
                                agent
                        )
                        model.agents
            in
            ( { model | agents = updatedAgents }
            , case List.head (List.filter (\a -> a.id == agentId) updatedAgents) of
                Just agent ->
                    saveAgentDetails agent

                Nothing ->
                    Cmd.none
            )

        UpdateField field value ->
            case model.currentUser of
                Just user ->
                    let
                        updatedUser =
                            case field of
                                "firstName" ->
                                    { user | firstName = value }

                                "lastName" ->
                                    { user | lastName = value }

                                "phone" ->
                                    { user | phone = String.filter Char.isDigit value }

                                _ ->
                                    user
                    in
                    ( { model | currentUser = Just updatedUser }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        SaveAgent ->
            ( model
            , case model.currentUser of
                Just user ->
                    let
                        agent =
                            { id = user.id
                            , firstName = user.firstName
                            , lastName = user.lastName
                            , email = user.email
                            , phone = user.phone
                            , isAdmin = user.isAdmin
                            , isAgent = user.isAgent
                            , carriers = []
                            , stateLicenses = []
                            , expanded = False
                            }
                    in
                    saveAgentDetails agent

                Nothing ->
                    Cmd.none
            )

        AgentSaved (Ok ()) ->
            if model.isSetup then
                ( { model
                    | showAddForm = False
                    , isAdmin = False
                    , firstName = ""
                    , lastName = ""
                    , email = ""
                    , rawPhone = ""
                    , displayPhone = ""
                    , carriers = []
                    , stateLicenses = []
                  }
                , Cmd.batch
                    [ fetchAgents -- Refresh the agents list
                    , fetchCurrentUser -- Refresh current user to get updated role
                    ]
                )

            else
                ( { model | error = Nothing }
                , Nav.pushUrl model.key "/agents"
                )

        AgentSaved (Err _) ->
            ( { model | error = Just "Failed to save agent" }
            , Cmd.none
            )

        FinishSetup ->
            ( model
            , Nav.pushUrl model.key "/dashboard"
            )

        LoadFromOrg ->
            ( { model | isLoading = True }
            , Http.get
                { url = "/api/settings"
                , expect = Http.expectJson GotOrgSettings (Decode.field "orgSettings" settingsObjectDecoder)
                }
            )

        GotOrgSettings (Ok settings) ->
            ( { model
                | carriers = settings.carrierContracts
                , stateLicenses = settings.stateLicenses
                , isLoading = False
                , orgSettings = Just settings
              }
            , Cmd.none
            )

        GotOrgSettings (Err _) ->
            ( { model
                | error = Just "Failed to load organization settings"
                , isLoading = False
              }
            , Cmd.none
            )

        AddAnotherAgent ->
            ( { model
                | showAddForm = True
                , firstName = ""
                , lastName = ""
                , email = ""
                , rawPhone = ""
                , displayPhone = ""
                , carriers = []
                , stateLicenses = []
                , isAdmin = False
                , isAgent = True
              }
            , Cmd.none
            )

        CancelAddAgent ->
            ( { model | showAddForm = False }, Cmd.none )

        RemoveAgent id ->
            ( { model | agents = List.filter (\agent -> agent.id /= id) model.agents }, Cmd.none )

        FetchAgents ->
            ( model, fetchAgents )

        GotAgents result ->
            case result of
                Ok agents ->
                    let
                        _ =
                            Debug.log "Successfully decoded agents" agents
                    in
                    ( { model | agents = agents }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadStatus 403 ->
                            -- For 403, keep the current user in the agents list
                            -- Don't show an error since this is expected for non-admin users
                            ( model, Cmd.none )

                        _ ->
                            let
                                errorMessage =
                                    case error of
                                        Http.BadUrl url ->
                                            "Invalid URL: " ++ url

                                        Http.Timeout ->
                                            "Request timed out"

                                        Http.NetworkError ->
                                            "Network error occurred"

                                        Http.BadStatus status ->
                                            "Server error: " ++ String.fromInt status

                                        Http.BadBody message ->
                                            "Data error: " ++ message
                            in
                            ( { model | error = Just errorMessage }, Cmd.none )

        GotCurrentUser result ->
            case result of
                Ok response ->
                    case response.user of
                        Just user ->
                            let
                                isAdminAgent =
                                    user.isAdmin && user.isAgent

                                -- Always create an initial agent for the current user in non-setup mode
                                initialAgent =
                                    { id = user.id
                                    , firstName = user.firstName
                                    , lastName = user.lastName
                                    , email = user.email
                                    , phone = user.phone
                                    , isAdmin = isAdminAgent || user.isAdmin
                                    , isAgent = isAdminAgent || user.isAgent
                                    , carriers = []
                                    , stateLicenses = []
                                    , expanded = False
                                    }
                            in
                            ( { model
                                | currentUser = Just user
                                , agents =
                                    if model.isSetup then
                                        [ initialAgent ]

                                    else
                                        initialAgent :: model.agents
                                , error = Nothing -- Clear any previous errors
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | currentUser = Nothing }, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "GotCurrentUser error" error
                    in
                    ( { model | error = Just "Failed to load current user" }
                    , Cmd.none
                    )

        ToggleAgentRole agentId role ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        case role of
                            "admin" ->
                                { agent | isAdmin = not agent.isAdmin }

                            "agent" ->
                                { agent | isAgent = not agent.isAgent }

                            _ ->
                                agent

                    else
                        agent
            in
            ( { model
                | agents = List.map updateAgent model.agents
                , pendingSave = Just agentId
              }
            , Cmd.none
            )

        UpdateAgentField agentId field value ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        case field of
                            "firstName" ->
                                { agent | firstName = value }

                            "lastName" ->
                                { agent | lastName = value }

                            "phone" ->
                                { agent | phone = formatPhoneNumber (String.filter Char.isDigit value) }

                            _ ->
                                agent

                    else
                        agent
            in
            ( { model
                | agents = List.map updateAgent model.agents
                , pendingSave = Just agentId
              }
            , Cmd.none
            )

        ToggleAgentExpanded agentId ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent | expanded = not agent.expanded }

                    else
                        agent
            in
            ( { model | agents = List.map updateAgent model.agents }, Cmd.none )

        UpdateAgentCarrier agentId carrier isSelected ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent
                            | carriers =
                                if isSelected then
                                    agent.carriers ++ [ carrier ]

                                else
                                    List.filter ((/=) carrier) agent.carriers
                        }

                    else
                        agent
            in
            ( { model
                | agents = List.map updateAgent model.agents
                , pendingSave = Just agentId
              }
            , Cmd.none
            )

        UpdateAgentState agentId state isSelected ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent
                            | stateLicenses =
                                if isSelected then
                                    agent.stateLicenses ++ [ state ]

                                else
                                    List.filter ((/=) state) agent.stateLicenses
                        }

                    else
                        agent
            in
            ( { model
                | agents = List.map updateAgent model.agents
                , pendingSave = Just agentId
              }
            , Cmd.none
            )

        SelectAllStatesForAgent agentId isSelected ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent
                            | stateLicenses =
                                if isSelected then
                                    allStates

                                else
                                    []
                        }

                    else
                        agent
            in
            ( { model
                | agents = List.map updateAgent model.agents
                , pendingSave = Just agentId
              }
            , Cmd.none
            )

        SelectCommonStatesForAgent agentId region ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent
                            | stateLicenses = agent.stateLicenses ++ getRegionStates region
                        }

                    else
                        agent
            in
            ( { model
                | agents = List.map updateAgent model.agents
                , pendingSave = Just agentId
              }
            , Cmd.none
            )

        LoadFromOrgForAgent agentId ->
            ( { model | isLoadingForAgent = Just agentId }
            , Http.get
                { url = "/api/settings"
                , expect = Http.expectJson (GotOrgSettingsForAgent agentId) (Decode.field "orgSettings" settingsObjectDecoder)
                }
            )

        GotOrgSettingsForAgent agentId (Ok settings) ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent
                            | carriers = settings.carrierContracts
                            , stateLicenses = settings.stateLicenses
                        }

                    else
                        agent
            in
            ( { model
                | agents = List.map updateAgent model.agents
                , isLoadingForAgent = Nothing -- Clear the Loading state
              }
            , Cmd.none
            )

        GotOrgSettingsForAgent agentId (Err _) ->
            ( { model
                | error = Just "Failed to load organization settings"
                , isLoadingForAgent = Nothing -- Clear the Loading state
              }
            , Cmd.none
            )

        SelectAllCarriers isSelected ->
            ( { model
                | carriers =
                    if isSelected then
                        allCarriers

                    else
                        []
              }
            , Cmd.none
            )

        SelectAllStates isSelected ->
            ( { model
                | stateLicenses =
                    if isSelected then
                        allStates

                    else
                        []
              }
            , Cmd.none
            )

        DeleteAgent agentId ->
            ( { model | agents = List.filter (\a -> a.id /= agentId) model.agents }
            , Cmd.none
            )

        AgentDeleted result ->
            case result of
                Ok _ ->
                    ( model, fetchAgents )

                Err _ ->
                    ( { model | error = Just "Failed to delete agent" }, Cmd.none )

        SelectCommonStates region ->
            ( { model | stateLicenses = model.stateLicenses ++ getRegionStates region }
            , Cmd.none
            )

        NavigateTo path ->
            ( model, Nav.pushUrl model.key path )

        ShowModal ->
            ( { model | showAddForm = True }, Cmd.none )

        CloseModal ->
            ( { model | showAddForm = False }, Cmd.none )

        ToggleCarrier agentId isSelected ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent
                            | carriers =
                                if isSelected then
                                    agent.carriers ++ [ agentId ]

                                else
                                    List.filter ((/=) agentId) agent.carriers
                        }

                    else
                        agent
            in
            ( { model | agents = List.map updateAgent model.agents }
            , Cmd.none
            )

        ToggleState agentId isSelected ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent
                            | stateLicenses =
                                if isSelected then
                                    agent.stateLicenses ++ [ agentId ]

                                else
                                    List.filter ((/=) agentId) agent.stateLicenses
                        }

                    else
                        agent
            in
            ( { model | agents = List.map updateAgent model.agents }
            , Cmd.none
            )

        SelectAllCarriersForAgent agentId isSelected ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent
                            | carriers =
                                if isSelected then
                                    allCarriers

                                else
                                    []
                        }

                    else
                        agent
            in
            ( { model
                | agents = List.map updateAgent model.agents
                , pendingSave = Just agentId
              }
            , Cmd.none
            )

        SaveAgentDetails agentId ->
            ( { model
                | pendingSave = Nothing
                , isLoadingForAgent = Just agentId
              }
            , case List.filter (\a -> a.id == agentId) model.agents of
                agent :: _ ->
                    saveAgentDetails agent

                [] ->
                    Cmd.none
            )

        AgentDetailsSaved agentId result ->
            case result of
                Ok _ ->
                    let
                        updateAgent agent =
                            if agent.id == agentId then
                                { agent | expanded = False }

                            else
                                agent
                    in
                    ( { model
                        | agents = List.map updateAgent model.agents
                        , isLoadingForAgent = Nothing
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to save agent details"
                        , isLoadingForAgent = Nothing
                      }
                    , Cmd.none
                    )

        DebounceSaveAgent agentId ->
            if agentId == "main" then
                -- Handle main agent save
                case model.currentUser of
                    Just user ->
                        let
                            agent =
                                { id = user.id
                                , firstName = user.firstName
                                , lastName = user.lastName
                                , email = user.email
                                , phone = user.phone
                                , isAdmin = user.isAdmin
                                , isAgent = user.isAgent
                                , carriers = []
                                , stateLicenses = []
                                , expanded = False
                                }
                        in
                        ( { model | pendingSave = Nothing }
                        , saveAgentDetails agent
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                -- Handle sub-agent save
                ( { model | pendingSave = Nothing }
                , case List.filter (\a -> a.id == agentId) model.agents of
                    agent :: _ ->
                        saveAgentDetails agent

                    [] ->
                        Cmd.none
                )

        EditAgent agent ->
            ( model, Cmd.none )



-- Helper functions


formatPhoneNumber : String -> String
formatPhoneNumber rawPhone =
    let
        digits =
            String.filter Char.isDigit rawPhone
                |> String.left 10

        len =
            String.length digits
    in
    if len == 0 then
        ""

    else if len <= 3 then
        "(" ++ digits

    else if len <= 6 then
        "(" ++ String.left 3 digits ++ ") " ++ String.dropLeft 3 digits

    else
        "("
            ++ String.left 3 digits
            ++ ") "
            ++ String.slice 3 6 digits
            ++ "-"
            ++ String.dropLeft 6 digits


saveAgent : User -> Cmd Msg
saveAgent user =
    let
        agent =
            { id = user.id
            , firstName = user.firstName
            , lastName = user.lastName
            , email = user.email
            , phone = user.phone
            , isAdmin = user.isAdmin
            , isAgent = user.isAgent
            , carriers = []
            , stateLicenses = []
            , expanded = False
            }
    in
    Http.post
        { url = "/api/agents"
        , body = Http.jsonBody (encodeAgent agent)
        , expect = Http.expectWhatever AgentSaved
        }


settingsDecoder : Decoder SettingsResponse
settingsDecoder =
    Decode.map2 SettingsResponse
        (Decode.field "orgSettings" settingsObjectDecoder)
        (Decode.field "canEditOrgSettings" Decode.bool)


type alias SettingsResponse =
    { orgSettings : Settings
    , canEditOrgSettings : Bool
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


type alias StateCarrierSetting =
    { state : String
    , carrier : String
    , active : Bool
    , targetGI : Bool
    }


settingsObjectDecoder : Decoder Settings
settingsObjectDecoder =
    Decode.map8 Settings
        (Decode.field "stateLicenses" (Decode.list Decode.string))
        (Decode.field "carrierContracts" (Decode.list Decode.string))
        (Decode.field "stateCarrierSettings" (Decode.list stateCarrierSettingDecoder))
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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pendingSave of
        Just agentId ->
            Time.every 2000 (\_ -> DebounceSaveAgent agentId)

        Nothing ->
            Sub.none


isValidEmail : String -> Bool
isValidEmail email =
    case Parser.run emailParser email of
        Ok _ ->
            True

        Err _ ->
            False


emailParser : Parser ()
emailParser =
    let
        usernameChar c =
            Char.isAlphaNum c || List.member c [ '.', '_', '-', '+', '%' ]

        domainChar c =
            Char.isAlphaNum c || c == '-'
    in
    succeed ()
        |. chompIf usernameChar
        |. chompWhile usernameChar
        |. symbol "@"
        |. chompIf domainChar
        |. chompWhile domainChar
        |. symbol "."
        |. chompIf Char.isAlpha
        |. chompWhile Char.isAlpha
        |. end


isValidPhone : String -> Bool
isValidPhone phone =
    String.length (String.filter Char.isDigit phone) == 10


canSave : Model -> Bool
canSave model =
    let
        hasValidName =
            not (String.isEmpty (String.trim model.firstName))
                && not (String.isEmpty (String.trim model.lastName))

        hasValidEmail =
            isValidEmail model.email

        hasValidPhone =
            isValidPhone model.displayPhone

        hasValidRole =
            model.isAdmin || model.isAgent

        allAgentsValid =
            List.all
                (\agent ->
                    not (String.isEmpty agent.phone)
                        && not (String.isEmpty agent.firstName)
                        && not (String.isEmpty agent.lastName)
                        && not (String.isEmpty agent.email)
                        && (agent.isAdmin || agent.isAgent)
                )
                model.agents
    in
    if model.showAddForm then
        hasValidName && hasValidEmail && hasValidPhone && hasValidRole

    else
        allAgentsValid


fetchAgents : Cmd Msg
fetchAgents =
    Http.get
        { url = "/api/agents"
        , expect =
            Http.expectStringResponse GotAgents
                (\response ->
                    case response of
                        Http.BadUrl_ url ->
                            Err (Http.BadUrl url)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata body ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ metadata body ->
                            let
                                _ =
                                    Debug.log "Raw response body" body
                            in
                            case Decode.decodeString (Decode.list agentDecoder) body of
                                Ok value ->
                                    Ok value

                                Err err ->
                                    Err (Http.BadBody (Decode.errorToString err))
                )
        }


agentDecoder : Decoder Agent
agentDecoder =
    Decode.succeed Agent
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "firstName" Decode.string
        |> Pipeline.required "lastName" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "phone" Decode.string
        |> Pipeline.required "is_admin" Decode.bool
        |> Pipeline.required "is_agent" Decode.bool
        |> Pipeline.optional "carriers" (Decode.list Decode.string) []
        |> Pipeline.optional "stateLicenses" (Decode.list Decode.string) []
        |> Pipeline.hardcoded False



-- expanded field


encodeAgent : Agent -> Encode.Value
encodeAgent agent =
    Encode.object
        [ ( "firstName", Encode.string agent.firstName )
        , ( "lastName", Encode.string agent.lastName )
        , ( "email", Encode.string agent.email )
        , ( "phone", Encode.string agent.phone )
        , ( "is_admin", Encode.bool agent.isAdmin )
        , ( "is_agent", Encode.bool agent.isAgent )
        , ( "carriers", Encode.list Encode.string agent.carriers )
        , ( "stateLicenses", Encode.list Encode.string agent.stateLicenses )
        ]



-- isAgent


isAdminBecomingAgent : Model -> Bool
isAdminBecomingAgent model =
    case model.currentUser of
        Just user ->
            model.isAdmin && user.isAdmin

        Nothing ->
            False


fetchCurrentUser : Cmd Msg
fetchCurrentUser =
    Http.get
        { url = "/api/me"
        , expect = Http.expectJson GotCurrentUser currentUserResponseDecoder
        }


currentUserResponseDecoder : Decoder CurrentUserResponse
currentUserResponseDecoder =
    let
        _ =
            Debug.log "Running currentUserResponseDecoder" ()

        debugField fieldName decoder =
            Decode.field fieldName decoder
                |> Decode.map
                    (\value ->
                        let
                            _ =
                                Debug.log ("Decoded " ++ fieldName) value
                        in
                        value
                    )
    in
    Decode.map2 CurrentUserResponse
        (debugField "success" Decode.bool)
        (Decode.maybe (debugField "user" userDecoder))


userDecoder : Decoder User
userDecoder =
    let
        idDecoder =
            Decode.oneOf
                [ Decode.field "id" Decode.string
                , Decode.field "id" (Decode.map String.fromInt Decode.int)
                ]
    in
    Decode.map7 User
        idDecoder
        (Decode.field "email" (Debug.log "email field" Decode.string))
        (Decode.field "firstName" (Debug.log "firstName field" Decode.string))
        (Decode.field "lastName" (Debug.log "lastName field" Decode.string))
        (Decode.field "is_admin" (Debug.log "is_admin field" Decode.bool))
        (Decode.field "is_agent" (Debug.log "is_agent field" Decode.bool))
        (Decode.oneOf
            [ Decode.field "phone" Decode.string
            , Decode.succeed ""
            ]
        )


saveAgentDetails : Agent -> Cmd Msg
saveAgentDetails agent =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/agents/" ++ agent.id
        , body = Http.jsonBody (encodeAgent agent)
        , expect = Http.expectWhatever (AgentDetailsSaved agent.id)
        , timeout = Nothing
        , tracker = Nothing
        }


isCurrentUser : Agent -> Model -> Bool
isCurrentUser agent model =
    case model.currentUser of
        Just user ->
            user.id == agent.id

        Nothing ->
            False


canModifySettings : Model -> String -> Bool
canModifySettings model agentId =
    case ( model.currentUser, model.orgSettings ) of
        ( Just user, Just settings ) ->
            -- Admin and admin_agent can always modify settings
            user.isAdmin
                || user.isAgent
                || -- Regular agents can modify if allowed and it's their own settings
                   (settings.allowAgentSettings && user.id == agentId)

        _ ->
            False
