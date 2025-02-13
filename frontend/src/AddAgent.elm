module AddAgent exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Components.ProgressIndicator
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser, chompIf, chompWhile, end, succeed, symbol)
import StateRegions exposing (Region(..), getRegionStates, regionToString)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, viewBox)



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
    , currentUser : Maybe User
    , isLoadingForAgent : Maybe String
    }


type alias User =
    { id : String
    , email : String
    , firstName : String
    , lastName : String
    , role : String
    }


type alias Agent =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    , role : String
    , carriers : List String
    , stateLicenses : List String
    , isExpanded : Bool
    , isAdmin : Bool
    , isAgent : Bool
    }


type Msg
    = UpdateEmail String
    | UpdateFirstName String
    | UpdateLastName String
    | UpdatePhone String
    | ToggleAdmin
    | ToggleAdminAgent
    | SelectCarrier String Bool
    | SelectState String Bool
    | SelectAllStates Bool
    | SaveAgent
    | AgentSaved (Result Http.Error ())
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


type alias CurrentUserResponse =
    { success : Bool
    , user : Maybe User
    }


init : { isSetup : Bool, key : Nav.Key } -> ( Model, Cmd Msg )
init flags =
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
      , isSetup = flags.isSetup
      , key = flags.key
      , isLoading = False
      , agents = []
      , showAddForm = False
      , currentUser = Nothing
      , isLoadingForAgent = Nothing
      }
    , Cmd.batch
        [ fetchAgents
        , fetchCurrentUser
        ]
    )


view : Model -> Browser.Document Msg
view model =
    { title =
        if model.isSetup then
            "Add Team Members"

        else
            "Manage Agents"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex" ]
            [ if model.isSetup then
                div [ class "flex w-full" ]
                    [ Components.ProgressIndicator.view
                        [ { icon = "1"
                          , title = "Choose Plan"
                          , description = "Select your subscription"
                          , isCompleted = True
                          , isActive = False
                          }
                        , { icon = "2"
                          , title = "Organization Settings"
                          , description = "Configure your organization"
                          , isCompleted = True
                          , isActive = False
                          }
                        , { icon = "3"
                          , title = "Add Team Members"
                          , description = "Invite your team"
                          , isCompleted = False
                          , isActive = True
                          }
                        ]
                    , div [ class "flex-1 ml-[280px] pb-24" ]
                        [ div [ class "max-w-3xl mx-auto py-6 px-4 sm:px-6 lg:px-8" ]
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
                        ]
                    , viewBottomBar model
                    ]

              else
                div [ class "max-w-3xl mx-auto py-12 pb-24" ]
                    [ viewNormalHeader
                    , div [ class "bg-white shadow rounded-lg p-6 space-y-6" ]
                        [ viewBasicInfo model
                        , viewWritingNumbers model
                        , viewStateLicenses model
                        ]
                    , viewBottomBar model
                    ]
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
                    [ text "Role" ]
                , div [ class "flex items-center space-x-6" ]
                    [ label [ class "inline-flex items-center" ]
                        [ input
                            [ type_ "checkbox"
                            , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                            , checked model.isAdmin
                            , onClick ToggleAdmin
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
                            , onClick ToggleAdminAgent
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
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                            , onClick (SelectCarrier "" True)
                            ]
                            [ text "Select All" ]
                        , button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                            , onClick (SelectCarrier "" False)
                            ]
                            [ text "Clear All" ]
                        ]
                    ]
                ]
            ]
        , div [ class "grid grid-cols-3 gap-4" ]
            (List.map
                (\carrier ->
                    label [ class "inline-flex items-center" ]
                        [ input
                            [ type_ "checkbox"
                            , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                            , checked (List.member carrier model.carriers)
                            , onCheck (\isChecked -> SelectCarrier carrier isChecked)
                            ]
                            []
                        , span [ class "ml-2 text-sm text-gray-700" ]
                            [ text carrier ]
                        ]
                )
                allCarriers
            )
        ]


viewStateLicenses : Model -> Html Msg
viewStateLicenses model =
    div [ class "space-y-4" ]
        [ h3 [ class "text-lg font-medium text-gray-900" ]
            [ text "State Licenses" ]
        , div [ class "mb-4 space-y-2" ]
            [ div [ class "flex gap-4" ]
                [ div []
                    [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                        [ text "Batch Select" ]
                    , div [ class "flex gap-2" ]
                        [ button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                            , onClick (SelectAllStates True)
                            ]
                            [ text "Select All" ]
                        , button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                            , onClick (SelectAllStates False)
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
        , div [ class "grid grid-cols-6 gap-4" ]
            (List.map (viewStateCheckbox model) allStates)
        ]


viewStateCheckbox : Model -> String -> Html Msg
viewStateCheckbox model state =
    label [ class "inline-flex items-center" ]
        [ input
            [ type_ "checkbox"
            , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
            , checked (List.member state model.stateLicenses)
            , onCheck (\isChecked -> SelectState state isChecked)
            ]
            []
        , span [ class "ml-2 text-sm text-gray-700" ]
            [ text state ]
        ]


viewAgentsList : Model -> Html Msg
viewAgentsList model =
    div [ class "space-y-8" ]
        [ div [ class "flow-root" ]
            [ ul [ class "-my-5 divide-y divide-gray-200" ]
                (let
                    -- Show admin/admin_agent first, but only once
                    adminAgents =
                        List.filter
                            (\agent ->
                                (agent.role == "admin" || agent.role == "admin_agent")
                                    && not (List.any (\a -> a.id == agent.id && a /= agent) model.agents)
                            )
                            model.agents

                    -- Then show regular agents
                    regularAgents =
                        List.filter (\agent -> agent.role == "agent") model.agents
                 in
                 List.map (viewAgentItem model) adminAgents ++ List.map (viewAgentItem model) regularAgents
                )
            ]
        ]


viewAgentItem : Model -> Agent -> Html Msg
viewAgentItem model agent =
    let
        isCurrentUser =
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

        roleCheckboxes =
            div [ class "flex items-center space-x-6" ]
                [ label [ class "inline-flex items-center" ]
                    [ input
                        [ type_ "checkbox"
                        , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500 disabled:opacity-50 disabled:cursor-not-allowed"
                        , checked agent.isAdmin
                        , disabled isCurrentUser
                        , onClick (ToggleAgentRole agent.id "admin")
                        , stopPropagationOn "click" (Decode.succeed ( ToggleAgentRole agent.id "admin", True ))
                        ]
                        []
                    , span
                        [ class
                            (if isCurrentUser then
                                "ml-2 text-sm text-gray-400"

                             else
                                "ml-2 text-sm text-gray-700"
                            )
                        ]
                        [ text "Admin" ]
                    ]
                , div
                    [ class "inline-flex items-center cursor-pointer select-none"
                    , onClick (ToggleAgentRole agent.id "agent")
                    , stopPropagationOn "click" (Decode.succeed ( ToggleAgentRole agent.id "agent", True ))
                    ]
                    [ div
                        [ class "w-4 h-4 border rounded flex items-center justify-center mr-2"
                        , classList
                            [ ( "bg-blue-600 border-blue-600", agent.isAgent )
                            , ( "border-gray-300", not agent.isAgent )
                            ]
                        ]
                        [ if agent.isAgent then
                            span [ class "text-white text-xs" ] [ text "✓" ]

                          else
                            text ""
                        ]
                    , span [ class "text-sm text-gray-700" ]
                        [ text "Agent" ]
                    ]
                ]

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
                    , div [ class "flex items-center space-x-4" ]
                        [ roleCheckboxes
                        , if not isCurrentUser then
                            button
                                [ class "text-sm text-red-600 hover:text-red-900"
                                , stopPropagationOn "click" (Decode.succeed ( RemoveAgent agent.id, True ))
                                ]
                                [ text "Remove" ]

                          else
                            text ""
                        ]
                    ]
                ]
            , if agent.isExpanded then
                div [ class "border-t border-gray-200 p-4 bg-gray-50" ]
                    [ viewAgentDetails model agent ]

              else
                text ""
            ]
        ]


viewAgentDetails : Model -> Agent -> Html Msg
viewAgentDetails model agent =
    let
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

                "role" ->
                    not (agent.isAdmin || agent.isAgent)

                _ ->
                    False

        errorIndicator field =
            if fieldError field then
                span [ class "text-red-500 ml-1" ] [ text "*" ]

            else
                text ""

        isCurrentUser =
            case model.currentUser of
                Just user ->
                    user.id == agent.id

                Nothing ->
                    False

        formattedPhone =
            formatPhoneNumber (String.filter Char.isDigit agent.phone)

        isLoading =
            model.isLoadingForAgent == Just agent.id
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
                        , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                        , value agent.firstName
                        , onInput (UpdateAgentField agent.id "firstName")
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
                        , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                        , value agent.lastName
                        , onInput (UpdateAgentField agent.id "lastName")
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
                        , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                        , value agent.email
                        , onInput (UpdateAgentField agent.id "email")
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
                        , onInput (UpdateAgentField agent.id "phone")
                        , placeholder "(555) 555-5555"
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
                                        [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                                        , onClick (SelectAllCarriersForAgent agent.id True)
                                        ]
                                        [ text "Select All" ]
                                    , button
                                        [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                                        , onClick (SelectAllCarriersForAgent agent.id False)
                                        ]
                                        [ text "Clear All" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grid grid-cols-3 gap-4" ]
                        (List.map
                            (\carrier ->
                                label [ class "inline-flex items-center" ]
                                    [ input
                                        [ type_ "checkbox"
                                        , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                                        , checked (List.member carrier agent.carriers)
                                        , onCheck (\isChecked -> UpdateAgentCarrier agent.id carrier isChecked)
                                        ]
                                        []
                                    , span [ class "ml-2 text-sm text-gray-700" ]
                                        [ text carrier ]
                                    ]
                            )
                            allCarriers
                        )
                    ]
                , div [ class "space-y-4" ]
                    [ h3 [ class "text-lg font-medium text-gray-900" ]
                        [ text "State Licenses" ]
                    , div [ class "mb-4 space-y-2" ]
                        [ div [ class "flex gap-4" ]
                            [ div []
                                [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                                    [ text "Batch Select" ]
                                , div [ class "flex gap-2" ]
                                    [ button
                                        [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                                        , onClick (SelectAllStatesForAgent agent.id True)
                                        ]
                                        [ text "Select All" ]
                                    , button
                                        [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                                        , onClick (SelectAllStatesForAgent agent.id False)
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
                                                [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50"
                                                , onClick (SelectCommonStatesForAgent agent.id region)
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
                                label [ class "inline-flex items-center" ]
                                    [ input
                                        [ type_ "checkbox"
                                        , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                                        , checked (List.member state agent.stateLicenses)
                                        , onCheck (\isChecked -> UpdateAgentState agent.id state isChecked)
                                        ]
                                        []
                                    , span [ class "ml-2 text-sm text-gray-700" ]
                                        [ text state ]
                                    ]
                            )
                            allStates
                        )
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

        -- Allow adding if admin or if in add form (which defaults to agent)
    in
    div [ class "fixed bottom-0 left-0 right-0 bg-white border-t border-gray-200 px-4 py-4 sm:px-6" ]
        [ div
            [ class
                (if model.isSetup then
                    "ml-[280px]"

                 else
                    ""
                )
            ]
            [ div [ class "max-w-3xl mx-auto" ]
                [ if model.error /= Nothing then
                    div [ class "mb-4" ]
                        [ p [ class "text-red-600" ]
                            [ text (Maybe.withDefault "" model.error) ]
                        ]

                  else
                    text ""
                , if model.showAddForm then
                    div [ class "flex flex-col space-y-4" ]
                        [ div [ class "flex items-center justify-between" ]
                            [ button
                                [ class "w-full inline-flex justify-center items-center px-4 py-2 border border-gray-300 shadow-sm text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                                , onClick LoadFromOrg
                                , disabled model.isLoading
                                ]
                                [ if model.isLoading then
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
                                    text "Load from Organization Settings"
                                ]
                            ]
                        , div [ class "flex items-center justify-end space-x-4" ]
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
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            in
            ( { model
                | rawPhone = rawDigits
                , displayPhone = formattedPhone
              }
            , Cmd.none
            )

        ToggleAdminAgent ->
            ( { model | isAdmin = not model.isAdmin }, Cmd.none )

        ToggleAdmin ->
            case model.currentUser of
                Just user ->
                    let
                        existingAgent =
                            List.filter (\agent -> agent.email == user.email) model.agents
                                |> List.head
                    in
                    ( { model | isAdmin = not model.isAdmin }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | isAdmin = not model.isAdmin }, Cmd.none )

        SelectCarrier carrier isSelected ->
            ( { model
                | carriers =
                    if isSelected then
                        carrier :: model.carriers

                    else
                        List.filter ((/=) carrier) model.carriers
              }
            , Cmd.none
            )

        SelectState state isSelected ->
            ( { model
                | stateLicenses =
                    if isSelected then
                        state :: model.stateLicenses

                    else
                        List.filter ((/=) state) model.stateLicenses
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

        SelectCommonStates region ->
            ( { model | stateLicenses = model.stateLicenses ++ getRegionStates region }
            , Cmd.none
            )

        SaveAgent ->
            ( model
            , saveAgent model
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

        GotAgents (Ok agents) ->
            let
                _ =
                    Debug.log "Successfully decoded agents" agents
            in
            ( { model | agents = agents }, Cmd.none )

        GotAgents (Err error) ->
            let
                errorMessage =
                    case error of
                        Http.BadBody message ->
                            Debug.log "Decoder error" message

                        Http.BadUrl url ->
                            Debug.log "Bad URL" url

                        Http.Timeout ->
                            Debug.log "Request timeout" "Request timed out"

                        Http.NetworkError ->
                            Debug.log "Network error" "Network error occurred"

                        Http.BadStatus status ->
                            Debug.log "Bad status" ("Bad status: " ++ String.fromInt status)
            in
            ( { model | error = Just ("Failed to fetch agents: " ++ errorMessage) }, Cmd.none )

        GotCurrentUser result ->
            case result of
                Ok response ->
                    let
                        _ =
                            Debug.log "GotCurrentUser success response" response
                    in
                    ( { model
                        | currentUser = response.user
                        , isAdmin =
                            case response.user of
                                Just user ->
                                    user.role == "admin" || user.role == "admin_agent"

                                Nothing ->
                                    False
                      }
                    , Cmd.none
                    )

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
            ( { model | agents = List.map updateAgent model.agents }, Cmd.none )

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
            ( { model | agents = List.map updateAgent model.agents }, Cmd.none )

        ToggleAgentExpanded agentId ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent | isExpanded = not agent.isExpanded }

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
                                    carrier :: agent.carriers

                                else
                                    List.filter ((/=) carrier) agent.carriers
                        }

                    else
                        agent
            in
            ( { model | agents = List.map updateAgent model.agents }, Cmd.none )

        UpdateAgentState agentId state isSelected ->
            let
                updateAgent agent =
                    if agent.id == agentId then
                        { agent
                            | stateLicenses =
                                if isSelected then
                                    state :: agent.stateLicenses

                                else
                                    List.filter ((/=) state) agent.stateLicenses
                        }

                    else
                        agent
            in
            ( { model | agents = List.map updateAgent model.agents }, Cmd.none )

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
            ( { model | agents = List.map updateAgent model.agents }, Cmd.none )

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
            ( { model | agents = List.map updateAgent model.agents }, Cmd.none )

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
            ( { model | agents = List.map updateAgent model.agents }, Cmd.none )



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


saveAgent : Model -> Cmd Msg
saveAgent model =
    case model.currentUser of
        Just user ->
            let
                role =
                    if model.isAdmin && model.isAgent then
                        "admin_agent"

                    else if model.isAdmin then
                        "admin"

                    else
                        "agent"

                _ =
                    Debug.log "Saving agent for user" { id = user.id, role = user.role }
            in
            if user.role == "admin" && model.isAdmin then
                -- Admin becoming agent - use PUT endpoint
                Http.request
                    { method = "PUT"
                    , headers = []
                    , url = "/api/agents/" ++ user.id ++ "/role"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "role", Encode.string role )
                                , ( "carriers", Encode.list Encode.string model.carriers )
                                , ( "stateLicenses", Encode.list Encode.string model.stateLicenses )
                                , ( "phone", Encode.string model.rawPhone )
                                ]
                            )
                    , expect = Http.expectWhatever AgentSaved
                    , timeout = Nothing
                    , tracker = Nothing
                    }

            else
                -- Creating new agent - use POST endpoint
                Http.post
                    { url = "/api/agents"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "firstName", Encode.string model.firstName )
                                , ( "lastName", Encode.string model.lastName )
                                , ( "email", Encode.string model.email )
                                , ( "phone", Encode.string model.rawPhone )
                                , ( "role", Encode.string role )
                                , ( "carriers", Encode.list Encode.string model.carriers )
                                , ( "stateLicenses", Encode.list Encode.string model.stateLicenses )
                                ]
                            )
                    , expect = Http.expectWhatever AgentSaved
                    }

        Nothing ->
            -- Creating new agent without being logged in (shouldn't happen)
            Http.post
                { url = "/api/agents"
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "firstName", Encode.string model.firstName )
                            , ( "lastName", Encode.string model.lastName )
                            , ( "email", Encode.string model.email )
                            , ( "phone", Encode.string model.rawPhone )
                            , ( "role", Encode.string "agent" )
                            , ( "carriers", Encode.list Encode.string model.carriers )
                            , ( "stateLicenses", Encode.list Encode.string model.stateLicenses )
                            ]
                        )
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
subscriptions _ =
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
    let
        buildAgent id firstName lastName email phone role carriers stateLicenses =
            { id = id
            , firstName = firstName
            , lastName = lastName
            , email = email
            , phone = phone
            , role = role
            , carriers = carriers
            , stateLicenses = stateLicenses
            , isExpanded = False
            , isAdmin = role == "admin" || role == "admin_agent"
            , isAgent = role == "agent" || role == "admin_agent"
            }
    in
    Decode.succeed buildAgent
        |> Decode.andThen
            (\build ->
                Decode.map build
                    (Decode.oneOf
                        [ Decode.field "id" Decode.string
                        , Decode.field "id" (Decode.map String.fromInt Decode.int)
                        ]
                    )
            )
        |> Decode.andThen
            (\build ->
                Decode.map build (Decode.field "firstName" Decode.string)
            )
        |> Decode.andThen
            (\build ->
                Decode.map build (Decode.field "lastName" Decode.string)
            )
        |> Decode.andThen
            (\build ->
                Decode.map build (Decode.field "email" Decode.string)
            )
        |> Decode.andThen
            (\build ->
                Decode.map build (Decode.field "phone" Decode.string)
            )
        |> Decode.andThen
            (\build ->
                Decode.map build (Decode.field "role" Decode.string)
            )
        |> Decode.andThen
            (\build ->
                Decode.map build (Decode.field "carriers" (Decode.list Decode.string))
            )
        |> Decode.andThen
            (\build ->
                Decode.map build (Decode.field "stateLicenses" (Decode.list Decode.string))
            )



-- isAgent


isAdminBecomingAgent : Model -> Bool
isAdminBecomingAgent model =
    case model.currentUser of
        Just user ->
            model.isAdmin && user.role == "admin"

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
        _ =
            Debug.log "Running userDecoder" ()

        idDecoder =
            Decode.oneOf
                [ Decode.field "id" Decode.string
                , Decode.field "id" (Decode.map String.fromInt Decode.int)
                ]
    in
    Decode.map5 User
        idDecoder
        (Decode.field "email" (Debug.log "email field" Decode.string))
        (Decode.field "firstName" (Debug.log "firstName field" Decode.string))
        (Decode.field "lastName" (Debug.log "lastName field" Decode.string))
        (Decode.field "role" (Debug.log "role field" Decode.string))
