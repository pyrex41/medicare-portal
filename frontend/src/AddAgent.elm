module AddAgent exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Components.ProgressIndicator
import Components.SetupLayout as SetupLayout
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
import Svg.Attributes exposing (d, fill, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)
import Time
import Url



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
    , showDeleteConfirm : Maybe String
    , reassignAgentId : Maybe String
    , contacts : List ContactSummary
    , emailStatus : EmailStatus
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


type alias ContactSummary =
    { id : Int
    , agentId : Maybe String
    }


type EmailStatus
    = NotChecked
    | Checking
    | Valid
    | Invalid String


type Msg
    = NoOp
    | UpdateEmail String
    | UpdateFirstName String
    | UpdateLastName String
    | UpdatePhone String
    | ToggleAdmin String
    | ToggleAgent String
    | UpdateField String String
    | UpdateAdminCheckbox Bool
    | UpdateAgentCheckbox Bool
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
    | ConfirmDeleteAgent String (Maybe String)
    | CloseDeleteConfirmModal
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
    | CheckAgentEmail
    | GotEmailResponse (Result Http.Error EmailResponse)


type alias CurrentUserResponse =
    { success : Bool
    , user : Maybe User
    }


type alias EmailResponse =
    { available : Bool
    , message : String
    }


init : Bool -> Nav.Key -> Maybe CurrentUser -> String -> ( Model, Cmd Msg )
init isSetup key currentUser planType =
    let
        initialAgents =
            case currentUser of
                Just user ->
                    -- Create an initial agent from the current user for setup mode
                    let
                        initialAgent =
                            { id = user.id
                            , firstName = user.firstName
                            , lastName = user.lastName
                            , email = user.email
                            , phone = user.phone
                            , isAdmin = user.isAdmin
                            , isAgent = user.isAgent -- Use actual agent status
                            , carriers = []
                            , stateLicenses = []
                            , expanded = False
                            }
                    in
                    if isSetup then
                        [ initialAgent ]

                    else
                        []

                Nothing ->
                    []
    in
    ( { email = ""
      , firstName = ""
      , lastName = ""
      , rawPhone = ""
      , displayPhone = ""
      , isAdmin = False
      , isAgent = True -- Default to agent role being checked
      , carriers = []
      , stateLicenses = []
      , error = Nothing
      , isSetup = isSetup
      , key = key
      , isLoading = True
      , agents = initialAgents
      , showAddForm = False
      , currentUser = currentUser
      , isLoadingForAgent = Nothing
      , orgSettings = Nothing
      , pendingSave = Nothing
      , planType = planType
      , showDeleteConfirm = Nothing
      , reassignAgentId = Nothing
      , contacts = []
      , emailStatus = NotChecked
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
            -- Show setup UI with sidebar
            SetupLayout.view SetupLayout.AgentSetup
                (model.planType == "basic")
                4
                -- Using 4 for AddAgent as it's typically the 5th step (0-indexed)
                [ div [ class "max-w-3xl mx-auto pb-24" ]
                    [ viewSetupHeader model
                    , viewAgentsList model
                    ]
                , viewBottomBar model
                ]

          else
            -- Show regular UI without sidebar
            div [ class "min-h-screen bg-gray-50 pb-24" ]
                [ div [ class "max-w-5xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
                    [ viewNormalHeader
                    , div [ class "bg-white shadow rounded-lg p-6" ]
                        [ viewAgentsList model
                        ]
                    ]
                , text "" -- No bottom bar in regular mode
                ]
        , viewDeleteConfirmationModal model
        ]
    }


viewSetupHeader : Model -> Html Msg
viewSetupHeader model =
    div [ class "mb-8 flex justify-between items-center" ]
        [ div []
            [ h1 [ class "text-3xl font-bold text-gray-900" ]
                [ text "Add Your First Agent" ]
            , p [ class "mt-2 text-gray-600" ]
                [ text "Set up your first agent to get started" ]
            ]
        , if not model.showAddForm then
            button
                [ class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700"
                , onClick AddAnotherAgent
                ]
                [ span [ class "mr-2" ] [ text "+" ]
                , text "Add Agent"
                ]

          else
            text ""
        ]


viewNormalHeader : Html Msg
viewNormalHeader =
    div [ class "mb-8 flex justify-center items-center" ]
        [ h1 [ class "text-2xl font-semibold text-gray-900" ]
            [ text "Manage Agents" ]
        ]


viewBasicInfo : Model -> Html Msg
viewBasicInfo model =
    div [ class "space-y-4" ]
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
                                [ onInput UpdateEmail, onBlur CheckAgentEmail ]
                           )
                    )
                    []
                , viewEmailStatus model.emailStatus
                ]
            , div []
                [ label [ class "block text-sm font-medium text-gray-700" ]
                    [ text "Phone" ]
                , input
                    [ type_ "tel"
                    , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                    , value model.displayPhone
                    , onInput UpdatePhone
                    , placeholder "(555) 555-5555"
                    ]
                    []
                ]
            ]
        , div [ class "mt-4" ]
            [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                [ text "Role (at least one required)" ]
            , div [ class "flex items-center space-x-6" ]
                [ label
                    [ class "inline-flex items-center" ]
                    [ input
                        [ type_ "checkbox"
                        , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                        , checked model.isAdmin
                        , onClick (UpdateAdminCheckbox (not model.isAdmin))
                        ]
                        []
                    , span [ class "ml-2 text-sm text-gray-700" ]
                        [ text "Admin" ]
                    ]
                , label [ class "inline-flex items-center" ]
                    [ input
                        [ type_ "checkbox"
                        , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                        , checked model.isAgent
                        , onClick (UpdateAgentCheckbox (not model.isAgent))
                        ]
                        []
                    , span [ class "ml-2 text-sm text-gray-700" ]
                        [ text "Agent" ]
                    ]
                ]
            ]
        ]


viewAgentsList : Model -> Html Msg
viewAgentsList model =
    div [ class "space-y-6" ]
        [ if model.error /= Nothing then
            div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded mb-6" ]
                [ text (Maybe.withDefault "" model.error) ]

          else
            text ""
        , div [ class "grid grid-cols-1 gap-6" ]
            (List.map
                (\agent ->
                    let
                        isSelfUser =
                            case model.currentUser of
                                Just user ->
                                    user.id == agent.id

                                Nothing ->
                                    False
                    in
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
                                    [ class "text-blue-600 hover:text-blue-800 font-medium"
                                    , onClick (ToggleAgentExpanded agent.id)
                                    ]
                                    [ text "Edit" ]
                                , button
                                    [ class
                                        ("text-red-400 "
                                            ++ (if isSelfUser then
                                                    "opacity-50 cursor-not-allowed"

                                                else
                                                    "hover:text-red-500"
                                               )
                                        )
                                    , onClick (DeleteAgent agent.id)
                                    , disabled isSelfUser
                                    , title
                                        (if isSelfUser then
                                            "You cannot delete your own account"

                                         else
                                            "Delete"
                                        )
                                    ]
                                    [ text "Delete" ]
                                ]
                            ]
                        , if agent.expanded then
                            div [ class "border-t border-gray-200 mt-4 pt-4" ]
                                [ viewAgentDetails model agent ]

                          else
                            text ""
                        ]
                )
                model.agents
            )
        , div [ class "mt-8 bg-white shadow rounded-lg p-6" ]
            [ if model.showAddForm then
                div [ class "space-y-6" ]
                    [ div [ class "border-b border-gray-200 pb-4" ]
                        [ h2 [ class "text-lg font-medium text-gray-900" ]
                            [ text "Add New Agent" ]
                        , p [ class "mt-1 text-sm text-gray-500" ]
                            [ text "Fill in the agent's information below" ]
                        ]
                    , viewBasicInfo model
                    , div [ class "flex justify-end space-x-4 mt-6" ]
                        [ button
                            [ class "px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50"
                            , onClick CancelAddAgent
                            ]
                            [ text "Cancel" ]
                        , button
                            [ class "px-4 py-2 text-sm font-medium text-white bg-blue-600 rounded-md hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
                            , onClick SaveAgent
                            , disabled (not (isFormValid model))
                            ]
                            [ text "Add Agent" ]
                        ]
                    ]

              else
                div [ class "flex justify-between items-center" ]
                    [ div []
                        [ h3 [ class "text-lg font-medium text-gray-900" ]
                            [ text "Add New Agent" ]
                        , p [ class "text-sm text-gray-500" ]
                            [ text "Add a team member to your organization" ]
                        ]
                    , button
                        [ class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700"
                        , onClick AddAnotherAgent
                        ]
                        [ span [ class "mr-2" ] [ text "+" ]
                        , text "Add Agent"
                        ]
                    ]
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

        -- Allow current user to edit their own details
        canEditField =
            isCurrentUserAgent || canEdit

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
                        , disabled (not canEditField)
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
                        , disabled (not canEditField)
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
                        , disabled (not canEditField)
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
                        , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                        , value formattedPhone
                        , onInput (onFieldInput "phone")
                        , placeholder "(555) 555-5555"
                        , disabled (not canEditField)
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "mt-4" ]
            [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                [ text "Role (at least one required)" ]
            , div [ class "flex items-center space-x-6" ]
                [ label
                    [ class "inline-flex items-center"
                    , classList [ ( "opacity-60", isCurrentUserAgent && agent.isAdmin ) ]
                    ]
                    [ input
                        [ type_ "checkbox"
                        , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                        , checked agent.isAdmin
                        , onClick (ToggleAgentRole agent.id "admin")
                        , disabled ((isCurrentUserAgent && agent.isAdmin) || not canEdit)
                        , title
                            (if isCurrentUserAgent && agent.isAdmin then
                                "You cannot remove your admin role"

                             else
                                ""
                            )
                        ]
                        []
                    , span [ class "ml-2 text-sm text-gray-700" ]
                        [ text "Admin" ]
                    ]
                , label [ class "inline-flex items-center" ]
                    [ input
                        [ type_ "checkbox"
                        , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                        , checked agent.isAgent
                        , onClick (ToggleAgentRole agent.id "agent")
                        , disabled (not canEditField)
                        ]
                        []
                    , span [ class "ml-2 text-sm text-gray-700" ]
                        [ text "Agent" ]
                    ]
                ]
            ]
        , div [ class "mt-6" ]
            [ p [ class "text-sm text-gray-500" ]
                [ text "This agent will automatically use the carriers and state licenses from your organization settings." ]
            ]
        , if hasChanges then
            div [ class "mt-4 flex justify-end" ]
                [ button
                    [ class "px-4 py-2 text-sm font-medium text-white bg-blue-600 rounded-md hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
                    , onClick (SaveAgentDetails agent.id)
                    , disabled isSaving
                    ]
                    [ if isSaving then
                        text "Saving..."

                      else
                        text "Save Changes"
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
                && model.emailStatus
                == Valid
                && isValidPhone model.displayPhone
                && (model.isAdmin || model.isAgent)
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
            , if model.isSetup then
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
            ( { model | email = email, emailStatus = NotChecked }, Cmd.none )

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
            if isFormValid model then
                ( { model | isLoading = True }
                , submitNewAgent model
                )

            else
                ( { model | error = Just "Please fill out all fields, ensure email is valid, and select at least one role (admin or agent)" }
                , Cmd.none
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

        ToggleAgentRole agentId role ->
            let
                isSelfUser agId =
                    case model.currentUser of
                        Just user ->
                            user.id == agId

                        Nothing ->
                            False

                updateAgent agent =
                    if agent.id == agentId then
                        -- Special handling for current user to prevent unchecking admin
                        if isSelfUser agent.id && role == "admin" && agent.isAdmin then
                            -- Don't allow removing admin role for self if already admin
                            agent

                        else
                            case role of
                                "admin" ->
                                    let
                                        -- Ensure at least one role is selected
                                        newIsAgent =
                                            if not (not agent.isAdmin) then
                                                agent.isAgent

                                            else
                                                True

                                        -- If admin is being unchecked, ensure agent is checked
                                    in
                                    { agent | isAdmin = not agent.isAdmin, isAgent = newIsAgent }

                                "agent" ->
                                    let
                                        -- Ensure at least one role is selected
                                        newIsAdmin =
                                            if not (not agent.isAgent) then
                                                agent.isAdmin

                                            else
                                                True

                                        -- If agent is being unchecked, ensure admin is checked
                                    in
                                    { agent | isAgent = not agent.isAgent, isAdmin = newIsAdmin }

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

        CheckAgentEmail ->
            if String.isEmpty (String.trim model.email) then
                ( { model | emailStatus = NotChecked }
                , Cmd.none
                )

            else if model.emailStatus == Checking then
                ( model, Cmd.none )

            else
                ( { model | emailStatus = Checking }
                , checkAgentEmail model.email
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
                        finalAgents =
                            if model.isSetup then
                                -- In setup mode, make sure we have at least the current user as an agent
                                case model.currentUser of
                                    Just user ->
                                        -- Check if the current user is already in the agents list
                                        if List.any (\a -> a.id == user.id) agents then
                                            -- Current user is already in the list, use the API result
                                            agents

                                        else
                                            -- Add the current user to the agents list
                                            { id = user.id
                                            , firstName = user.firstName
                                            , lastName = user.lastName
                                            , email = user.email
                                            , phone = user.phone
                                            , isAdmin = user.isAdmin
                                            , isAgent = user.isAgent -- Use actual agent status from user
                                            , carriers = []
                                            , stateLicenses = []
                                            , expanded = False
                                            }
                                                :: agents

                                    Nothing ->
                                        agents

                            else
                                -- In normal mode, use the API result
                                agents
                    in
                    ( { model | agents = finalAgents }, Cmd.none )

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
                                -- Create agent from current user
                                initialAgent =
                                    { id = user.id
                                    , firstName = user.firstName
                                    , lastName = user.lastName
                                    , email = user.email
                                    , phone = user.phone
                                    , isAdmin = user.isAdmin
                                    , isAgent = user.isAgent -- Use actual agent status
                                    , carriers = []
                                    , stateLicenses = []
                                    , expanded = False
                                    }

                                -- Include the current user in agents list for setup mode
                                updatedAgents =
                                    if model.isSetup then
                                        -- In setup mode, always have current user as the first agent
                                        if List.any (\a -> a.id == user.id) model.agents then
                                            -- If current user is already in the list, keep existing agents
                                            model.agents

                                        else
                                            -- Add current user to the list
                                            initialAgent :: model.agents

                                    else
                                        -- In normal mode, keep the existing agents
                                        model.agents
                            in
                            ( { model
                                | currentUser = Just user
                                , agents = updatedAgents
                                , error = Nothing -- Clear any previous errors
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | currentUser = Nothing }, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Failed to load current user" }
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
            ( { model | showDeleteConfirm = Just agentId, reassignAgentId = Nothing }, Cmd.none )

        ConfirmDeleteAgent agentId reassignToAgentId ->
            ( { model | showDeleteConfirm = Nothing, reassignAgentId = reassignToAgentId, isLoading = True }
            , deleteAgent agentId reassignToAgentId
            )

        CloseDeleteConfirmModal ->
            ( { model | showDeleteConfirm = Nothing, error = Nothing }, Cmd.none )

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

        AgentDeleted result ->
            case result of
                Ok _ ->
                    ( { model | isLoading = False, error = Nothing }
                    , fetchAgents
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to delete agent"
                        , isLoading = False
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
            ( { model
                | agents =
                    List.map
                        (\a ->
                            if a.id == agent.id then
                                { a | expanded = not a.expanded }

                            else
                                a
                        )
                        model.agents
              }
            , Cmd.none
            )

        UpdateAdminCheckbox value ->
            let
                -- Ensure at least one role is selected
                newIsAgent =
                    if not value then
                        True
                        -- If admin is being unchecked, ensure agent is checked

                    else
                        model.isAgent
            in
            ( { model | isAdmin = value, isAgent = newIsAgent }, Cmd.none )

        UpdateAgentCheckbox value ->
            let
                -- Ensure at least one role is selected
                newIsAdmin =
                    if not value then
                        True
                        -- If agent is being unchecked, ensure admin is checked

                    else
                        model.isAdmin
            in
            ( { model | isAgent = value, isAdmin = newIsAdmin }, Cmd.none )

        GotEmailResponse result ->
            case result of
                Ok response ->
                    ( { model
                        | emailStatus =
                            if response.available then
                                Valid

                            else
                                Invalid response.message
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | emailStatus = Invalid "Failed to check email availability"
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


saveAgent : User -> Model -> Cmd Msg
saveAgent user model =
    let
        carriers =
            case model.orgSettings of
                Just settings ->
                    settings.carrierContracts

                Nothing ->
                    []

        stateLicenses =
            case model.orgSettings of
                Just settings ->
                    settings.stateLicenses

                Nothing ->
                    []

        agent =
            { id = user.id
            , firstName = user.firstName
            , lastName = user.lastName
            , email = user.email
            , phone = user.phone
            , isAdmin = user.isAdmin
            , isAgent = user.isAgent
            , carriers = carriers
            , stateLicenses = stateLicenses
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
    let
        containsAtSign =
            String.contains "@" email

        containsDot =
            String.contains "." email

        hasValidLength =
            String.length email >= 5
    in
    containsAtSign && containsDot && hasValidLength


isValidPhone : String -> Bool
isValidPhone phone =
    let
        -- Remove all non-digit characters
        digits =
            String.filter Char.isDigit phone
    in
    String.length digits == 10


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
        |> Pipeline.required "isAdmin" Decode.bool
        |> Pipeline.required "isAgent" Decode.bool
        |> Pipeline.optional "carriers" (Decode.list Decode.string) []
        |> Pipeline.optional "stateLicenses" (Decode.list Decode.string) []
        |> Pipeline.hardcoded False


encodeAgent : Agent -> Encode.Value
encodeAgent agent =
    Encode.object
        [ ( "firstName", Encode.string agent.firstName )
        , ( "lastName", Encode.string agent.lastName )
        , ( "email", Encode.string agent.email )
        , ( "phone", Encode.string agent.phone )
        , ( "isAdmin", Encode.bool agent.isAdmin )
        , ( "isAgent", Encode.bool agent.isAgent )
        , ( "carriers", Encode.list Encode.string agent.carriers )
        , ( "stateLicenses", Encode.list Encode.string agent.stateLicenses )
        ]


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
    Decode.map2 CurrentUserResponse
        (Decode.field "success" Decode.bool)
        (Decode.maybe (Decode.field "user" userDecoder))


userDecoder : Decoder User
userDecoder =
    let
        idDecoder =
            Decode.oneOf
                [ Decode.field "id" Decode.string
                , Decode.field "id" (Decode.map String.fromInt Decode.int)
                ]

        -- Add decoders that handle different boolean formats
        boolDecoder =
            Decode.oneOf
                [ Decode.bool
                , Decode.map (\n -> n == 1) Decode.int
                , Decode.map (\s -> s == "1" || s == "true") Decode.string
                ]
    in
    Decode.map7 User
        idDecoder
        (Decode.field "email" Decode.string)
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.field "is_admin" boolDecoder)
        (Decode.field "is_agent" boolDecoder)
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


deleteAgent : String -> Maybe String -> Cmd Msg
deleteAgent agentId maybeReassignToAgentId =
    let
        url =
            case maybeReassignToAgentId of
                Just reassignToAgentId ->
                    "/api/agents/" ++ agentId ++ "?reassignTo=" ++ reassignToAgentId

                Nothing ->
                    "/api/agents/" ++ agentId
    in
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever AgentDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


viewDeleteConfirmationModal : Model -> Html Msg
viewDeleteConfirmationModal model =
    case model.showDeleteConfirm of
        Just agentId ->
            let
                targetAgent =
                    List.filter (\a -> a.id == agentId) model.agents
                        |> List.head

                otherAgents =
                    List.filter (\a -> a.id /= agentId) model.agents

                agentName =
                    case targetAgent of
                        Just agent ->
                            agent.firstName ++ " " ++ agent.lastName

                        Nothing ->
                            "this agent"

                errorMessageBlock =
                    if model.error /= Nothing && model.showDeleteConfirm /= Nothing then
                        div [ class "mb-4 bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                            [ text (Maybe.withDefault "" model.error) ]

                    else
                        text ""
            in
            div [ class "fixed inset-0 bg-gray-500 bg-opacity-75 flex items-center justify-center p-4 z-50" ]
                [ div [ class "bg-white rounded-lg max-w-lg w-full p-6" ]
                    [ h3 [ class "text-lg font-medium text-gray-900 mb-4" ]
                        [ text ("Delete " ++ agentName ++ "?") ]
                    , p [ class "text-sm text-gray-500 mb-4" ]
                        [ text "This will permanently remove this agent from your organization and cannot be undone." ]
                    , errorMessageBlock
                    , if not (List.isEmpty model.contacts) then
                        div [ class "mb-6" ]
                            [ p [ class "text-sm text-gray-500 mb-2" ]
                                [ text "This agent has contacts assigned to them. What would you like to do with these contacts?" ]
                            , div [ class "mt-4" ]
                                [ select
                                    [ class "mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
                                    , onInput
                                        (\val ->
                                            if val == "" then
                                                NoOp

                                            else
                                                ConfirmDeleteAgent agentId (Just val)
                                        )
                                    ]
                                    (option [ value "" ] [ text "Select an agent to reassign contacts" ]
                                        :: List.map
                                            (\agent ->
                                                option [ value agent.id ]
                                                    [ text (agent.firstName ++ " " ++ agent.lastName) ]
                                            )
                                            otherAgents
                                    )
                                ]
                            ]

                      else
                        text ""
                    , div [ class "flex justify-end space-x-3" ]
                        [ button
                            [ class "px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50"
                            , onClick (ConfirmDeleteAgent agentId Nothing)
                            ]
                            [ text "Delete Without Reassigning" ]
                        , button
                            [ class "px-4 py-2 text-sm font-medium text-white bg-red-600 rounded-md hover:bg-red-700"
                            , onClick (ConfirmDeleteAgent agentId model.reassignAgentId)
                            ]
                            [ text "Delete" ]
                        , button
                            [ class "px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50"
                            , onClick CloseDeleteConfirmModal
                            ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]

        Nothing ->
            text ""


viewEmailStatus : EmailStatus -> Html Msg
viewEmailStatus status =
    div [ class "mt-1 transition-all duration-200" ]
        [ case status of
            NotChecked ->
                text ""

            Checking ->
                div [ class "text-blue-600 text-sm flex items-center" ]
                    [ div [ class "animate-spin h-4 w-4 mr-2 border-2 border-blue-600 border-t-transparent rounded-full" ] []
                    , text "Checking availability..."
                    ]

            Valid ->
                div [ class "text-green-600 text-sm flex items-center" ]
                    [ -- Checkmark icon
                      svg
                        [ Svg.Attributes.class "h-4 w-4 mr-1"
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.viewBox "0 0 24 24"
                        , Svg.Attributes.stroke "currentColor"
                        ]
                        [ path
                            [ Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.d "M5 13l4 4L19 7"
                            ]
                            []
                        ]
                    , text "Email is available"
                    ]

            Invalid message ->
                div [ class "text-red-600 text-sm flex items-center" ]
                    [ -- X icon
                      svg
                        [ Svg.Attributes.class "h-4 w-4 mr-1"
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.viewBox "0 0 24 24"
                        , Svg.Attributes.stroke "currentColor"
                        ]
                        [ path
                            [ Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.d "M6 18L18 6M6 6l12 12"
                            ]
                            []
                        ]
                    , text message
                    ]
        ]


checkAgentEmail : String -> Cmd Msg
checkAgentEmail email =
    Http.get
        { url = "/api/organizations/check-email/" ++ Url.percentEncode email
        , expect = Http.expectJson GotEmailResponse emailResponseDecoder
        }


emailResponseDecoder : Decode.Decoder EmailResponse
emailResponseDecoder =
    Decode.map2 EmailResponse
        (Decode.field "available" Decode.bool)
        (Decode.field "message" Decode.string)


isFormValid : Model -> Bool
isFormValid model =
    let
        isEmailValid =
            model.emailStatus == Valid

        areNamesValid =
            not (String.isEmpty (String.trim model.firstName))
                && not (String.isEmpty (String.trim model.lastName))

        isPhoneValid =
            not (String.isEmpty (String.trim model.displayPhone))

        hasValidRole =
            model.isAdmin || model.isAgent
    in
    isEmailValid && areNamesValid && isPhoneValid && hasValidRole


submitNewAgent : Model -> Cmd Msg
submitNewAgent model =
    -- For new agents, we need to send a POST to /api/agents/create
    -- This endpoint should be more permissive than updating an existing agent
    Http.post
        { url = "/api/agents/create"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "firstName", Encode.string model.firstName )
                    , ( "lastName", Encode.string model.lastName )
                    , ( "email", Encode.string model.email )
                    , ( "phone", Encode.string model.rawPhone )
                    , ( "isAdmin", Encode.bool model.isAdmin )
                    , ( "isAgent", Encode.bool model.isAgent )
                    , ( "carriers", Encode.list Encode.string model.carriers )
                    , ( "stateLicenses", Encode.list Encode.string model.stateLicenses )
                    ]
                )
        , expect = Http.expectWhatever AgentSaved
        }
