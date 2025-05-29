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
import Ports
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


type alias Settings =
    { carrierContracts : List String
    , stateLicenses : List String
    , forceOrgSenderDetails : Bool
    }


settingsObjectDecoder : Decoder Settings
settingsObjectDecoder =
    Decode.map3 Settings
        (Decode.field "carrierContracts" (Decode.list Decode.string))
        (Decode.field "stateLicenses" (Decode.list Decode.string))
        (Decode.oneOf
            [ Decode.field "forceOrgSenderDetails" Decode.bool
            , Decode.succeed False
            ]
        )


type alias Model =
    { email : String
    , firstName : String
    , lastName : String
    , rawPhone : String
    , displayPhone : String
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
    , defaultAgentId : Maybe String
    , forceOrgSenderDetails : Bool
    , copiedAgentLinks : List String -- Track which agent IDs have had their links copied
    , currentlyCopyingAgent : Maybe String -- Track which agent's link is currently being copied
    , orgSlug : String -- ADDED
    }


type alias User =
    { id : String
    , email : String
    , firstName : String
    , lastName : String
    , phone : String
    , orgSlug : String -- ADDED
    }


type alias Agent =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    , carriers : List String
    , stateLicenses : List String
    , expanded : Bool
    , calendarLink : String
    , signature : String
    }


type alias CurrentUser =
    { id : String
    , email : String
    , firstName : String
    , lastName : String
    , phone : String
    , orgSlug : String -- ADDED
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
    | GotAgents (Result Http.Error AgentsResponse)
    | GotCurrentUser (Result Http.Error CurrentUserResponse)
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
    | SetDefaultAgent String
    | SetDefaultAgentResult (Result Http.Error ())
    | CopyAgentSelfOnboardingLink String
    | LinkCopied Bool


type alias CurrentUserResponse =
    { success : Bool
    , user : Maybe User
    }


type alias EmailResponse =
    { available : Bool
    , message : String
    }


type alias AgentsResponse =
    { agents : List Agent
    , defaultAgentId : Maybe String
    }


init : Bool -> Nav.Key -> Maybe { id : String, email : String, firstName : String, lastName : String, phone : String, isAdmin : Bool, isAgent : Bool, orgSlug : String } -> String -> ( Model, Cmd Msg )
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
                            , carriers = []
                            , stateLicenses = []
                            , expanded = False
                            , calendarLink = ""
                            , signature = ""
                            }

                        -- Convert old user type to new CurrentUser type
                        convertedUser =
                            { id = user.id
                            , email = user.email
                            , firstName = user.firstName
                            , lastName = user.lastName
                            , phone = user.phone
                            , orgSlug = user.orgSlug
                            }
                    in
                    if isSetup then
                        [ initialAgent ]

                    else
                        []

                Nothing ->
                    []

        -- Convert old user type to new CurrentUser type
        convertedCurrentUser =
            currentUser
                |> Maybe.map
                    (\user ->
                        { id = user.id
                        , email = user.email
                        , firstName = user.firstName
                        , lastName = user.lastName
                        , phone = user.phone
                        , orgSlug = user.orgSlug
                        }
                    )
    in
    ( { email = ""
      , firstName = ""
      , lastName = ""
      , rawPhone = ""
      , displayPhone = ""
      , carriers = []
      , stateLicenses = []
      , error = Nothing
      , isSetup = isSetup
      , key = key
      , isLoading = True
      , agents = initialAgents
      , showAddForm = False
      , currentUser = convertedCurrentUser
      , isLoadingForAgent = Nothing
      , orgSettings = Nothing
      , pendingSave = Nothing
      , planType = planType
      , showDeleteConfirm = Nothing
      , reassignAgentId = Nothing
      , contacts = []
      , emailStatus = NotChecked
      , defaultAgentId = Nothing
      , forceOrgSenderDetails = False
      , copiedAgentLinks = []
      , currentlyCopyingAgent = Nothing
      , orgSlug = Maybe.map .orgSlug convertedCurrentUser |> Maybe.withDefault ""
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
    div [ class "mb-8 flex justify-left items-center" ]
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
                     , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500 text-base"
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
                     , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500 text-base"
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
                     , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500 text-base"
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
                    , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base"
                    , value model.displayPhone
                    , onInput UpdatePhone
                    , placeholder "(555) 555-5555"
                    ]
                    []
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
        , div [ class "bg-blue-50 border border-blue-200 text-blue-700 px-4 py-3 rounded mb-6" ]
            [ div [ class "font-medium" ] [ text "Default Agent" ]
            , p [ class "text-sm mt-1" ]
                [ text "The default agent is automatically assigned to contacts who don't have an agent assigned. They also become the fallback when an agent is deleted, if another agent is not selected." ]
            ]
        , if model.isLoading then
            div [ class "flex justify-center items-center py-12" ]
                [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-blue-500 border-t-transparent" ] []
                , p [ class "ml-4 text-gray-500" ] [ text "Loading agents..." ]
                ]

          else
            div [ class "grid grid-cols-1 gap-6" ]
                (List.map
                    (\agent ->
                        let
                            isSelfUser =
                                case model.currentUser of
                                    Just user ->
                                        user.id == agent.id

                                    Nothing ->
                                        False

                            isDefault =
                                model.defaultAgentId == Just agent.id

                            cardBackgroundClass =
                                if isDefault then
                                    "bg-blue-50"

                                else
                                    "bg-white"
                        in
                        div [ class (cardBackgroundClass ++ " shadow rounded-lg p-6") ]
                            [ div
                                [ class "flex items-center justify-between cursor-pointer hover:bg-gray-50 -m-2 p-2 rounded-md"
                                , onClick (ToggleAgentExpanded agent.id)
                                ]
                                [ div [ class "flex items-center" ]
                                    [ -- Expand/Collapse Icon
                                      div [ class "mr-2" ]
                                        [ svg
                                            [ Svg.Attributes.class "h-5 w-5 text-gray-400 transform transition-transform duration-200"
                                            , Svg.Attributes.class
                                                (if agent.expanded then
                                                    "rotate-90"

                                                 else
                                                    ""
                                                )
                                            , Svg.Attributes.viewBox "0 0 20 20"
                                            , Svg.Attributes.fill "currentColor"
                                            ]
                                            [ path
                                                [ Svg.Attributes.fillRule "evenodd"
                                                , Svg.Attributes.d "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                                                , Svg.Attributes.clipRule "evenodd"
                                                ]
                                                []
                                            ]
                                        ]
                                    , div [ class "ml-2" ]
                                        [ div [ class "text-lg font-medium text-gray-900" ]
                                            [ text (agent.firstName ++ " " ++ agent.lastName) ]
                                        , div [ class "text-sm text-gray-500" ]
                                            [ text agent.email ]
                                        ]
                                    ]
                                , div [ class "flex items-center space-x-4" ]
                                    [ if not isDefault then
                                        button
                                            [ class "px-3 py-1 text-sm text-blue-600 hover:text-blue-800 font-medium border border-blue-600 rounded-md hover:bg-blue-50"
                                            , stopPropagationOn "click" (Decode.succeed ( SetDefaultAgent agent.id, True ))
                                            ]
                                            [ text "Set as Default" ]

                                      else
                                        div [ class "px-3 py-1 text-sm text-blue-600 font-medium" ]
                                            [ text "Default Agent" ]
                                    , button
                                        [ class "text-blue-600 hover:text-blue-800 font-medium"
                                        , stopPropagationOn "click" (Decode.succeed ( ToggleAgentExpanded agent.id, True ))
                                        ]
                                        [ text "Edit" ]
                                    , button
                                        [ class
                                            ("text-red-400 "
                                                ++ (if isSelfUser || isDefault then
                                                        "opacity-50 cursor-not-allowed"

                                                    else
                                                        "hover:text-red-500"
                                                   )
                                            )
                                        , stopPropagationOn "click" (Decode.succeed ( DeleteAgent agent.id, True ))
                                        , disabled (isSelfUser || isDefault)
                                        , title
                                            (if isSelfUser then
                                                "You cannot delete your own account"

                                             else if isDefault then
                                                "You cannot delete the default agent"

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

        -- Check if organization forces org sender details
        fieldsDisabled =
            model.forceOrgSenderDetails

        agentSelfOnboardingUrl =
            "https://" ++ "medicaremax.ai/self-onboarding/" ++ model.orgSlug ++ "?agentId=" ++ agent.id
    in
    div [ class "space-y-6" ]
        [ -- Basic Information Section
          div [ class "space-y-4" ]
            [ div [ class "grid grid-cols-2 gap-4" ]
                [ div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "First Name"
                        , errorIndicator "firstName"
                        ]
                    , input
                        [ type_ "text"
                        , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500 text-base"
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
                        , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500 text-base"
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
                        [ text "Email Address"
                        , errorIndicator "email"
                        ]
                    , input
                        [ type_ "email"
                        , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500 text-base"
                        , value agent.email
                        , onInput (onFieldInput "email")
                        , disabled (not canEditField)
                        ]
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "Phone number"
                        , errorIndicator "phone"
                        ]
                    , div [ class "flex" ]
                        [ span [ class "inline-flex items-center px-3 py-2 rounded-l-md border border-r-0 border-gray-300 bg-gray-50 text-gray-500 shadow-sm" ]
                            [ text "US" ]
                        , input
                            [ type_ "tel"
                            , class "flex-1 px-3 py-2 rounded-r-md border border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base focus:z-10"
                            , value formattedPhone
                            , onInput (onFieldInput "phone")
                            , placeholder "+1 (555) 000-0000"
                            , disabled (not canEditField)
                            ]
                            []
                        ]
                    ]
                ]
            , div []
                [ label [ class "block text-sm font-medium text-gray-700" ]
                    [ text "Calendar Booking Link (optional)" ]
                , input
                    [ type_ "text"
                    , class
                        ("mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base "
                            ++ (if fieldsDisabled then
                                    "disabled:bg-gray-100 disabled:text-gray-500"

                                else
                                    ""
                               )
                        )
                    , value agent.calendarLink
                    , onInput (onFieldInput "calendarLink")
                    , placeholder "example.biz"
                    , disabled (fieldsDisabled || not canEditField)
                    ]
                    []
                , p [ class "text-gray-500 text-xs mt-1" ]
                    [ text "If provided, this link will be used as one of the options a client may select to connect with your agency. Traditionally this would be a Calendly link, Acuity link, etc." ]
                ]
            ]
        , -- Agent Self-Onboarding Link Section
          div []
            [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                [ text "Agent Self-Onboarding Link" ]
            , div [ class "flex items-center space-x-2" ]
                [ svg [ Svg.Attributes.class "h-5 w-5 text-gray-400", Svg.Attributes.viewBox "0 0 20 20", Svg.Attributes.fill "currentColor" ]
                    [ path [ Svg.Attributes.fillRule "evenodd", Svg.Attributes.d "M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z", Svg.Attributes.clipRule "evenodd" ] []
                    ]
                , input
                    [ type_ "text"
                    , class "flex-1 px-3 py-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500 bg-gray-50 text-gray-500"
                    , value agentSelfOnboardingUrl
                    , readonly True
                    ]
                    []
                , button
                    [ class "px-4 py-2 text-sm bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                    , onClick (CopyAgentSelfOnboardingLink agent.id)
                    ]
                    [ text
                        (if List.member agent.id model.copiedAgentLinks then
                            "Copied!"

                         else
                            "Copy Link"
                        )
                    ]
                ]
            , p [ class "text-gray-500 text-xs mt-1" ]
                [ text "Share this link with clients or non-clients to gather missing information or capture new leads to your book of business. New leads created in this way will be assigned to the agent associated with this link." ]
            ]
        , -- Sender Settings Section
          div []
            [ h3 [ class "text-sm font-medium text-gray-700 mb-4" ]
                [ text "Sender Settings" ]
            , p [ class "text-sm text-gray-500 mb-4" ]
                [ text "The sender settings below are controlled by your organization's configuration. "
                , a [ class "text-blue-600 hover:text-blue-800 underline", href "/settings" ]
                    [ text "Visit Organization Settings" ]
                , text " to change how agent details are used across your organization. When Organization Details is selected, agents will use the organization's contact information. When Agent Details is selected, agents can use their own contact information set above."
                ]
            , div [ class "grid grid-cols-1 md:grid-cols-2 gap-4" ]
                [ -- Organization Details Card
                  div
                    [ class
                        ("relative rounded-lg border-2 p-6 transition-all "
                            ++ (if model.forceOrgSenderDetails then
                                    "border-blue-500 bg-blue-50"

                                else
                                    "border-gray-200 bg-white"
                               )
                        )
                    ]
                    [ div [ class "flex items-start" ]
                        [ div [ class "flex items-center h-5" ]
                            [ input
                                [ type_ "radio"
                                , name ("senderSettings-" ++ agent.id)
                                , checked model.forceOrgSenderDetails
                                , class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300"
                                , disabled True
                                ]
                                []
                            ]
                        , div [ class "ml-3" ]
                            [ label [ class "font-medium text-gray-900" ] [ text "Organization Details" ]
                            , p [ class "text-sm text-gray-500 mt-1" ]
                                [ if model.forceOrgSenderDetails then
                                    span []
                                        [ text "These details are set in the organization settings. "
                                        , a [ class "text-blue-600 hover:text-blue-800", href "/settings" ] [ text "Change organization defaults" ]
                                        , text "."
                                        ]

                                  else
                                    text "When this option is selected the Organization Details will be used for the signature, phone number, and scheduling link if applicable."
                                ]
                            ]
                        ]
                    ]
                , -- Agent Details Card
                  div
                    [ class
                        ("relative rounded-lg border-2 p-6 transition-all "
                            ++ (if not model.forceOrgSenderDetails then
                                    "border-blue-500 bg-blue-50"

                                else
                                    "border-gray-200 bg-white"
                               )
                        )
                    ]
                    [ div [ class "flex items-start" ]
                        [ div [ class "flex items-center h-5" ]
                            [ input
                                [ type_ "radio"
                                , name ("senderSettings-" ++ agent.id)
                                , checked (not model.forceOrgSenderDetails)
                                , class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300"
                                , disabled True
                                ]
                                []
                            ]
                        , div [ class "ml-3" ]
                            [ label [ class "font-medium text-gray-900" ] [ text "Agent Details" ]
                            , p [ class "text-sm text-gray-500 mt-1" ]
                                [ if not model.forceOrgSenderDetails then
                                    text "These details are set above in this agent's settings."

                                  else
                                    text "When this option is selected the Agent's personal information from their agent settings will be used for the signature, phone number, and scheduling link if applicable."
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , -- Signature Section (only if agent details are enabled)
          if not model.forceOrgSenderDetails then
            div []
                [ label [ class "block text-sm font-medium text-gray-700" ]
                    [ text "Email & SMS Signature or Sign Off" ]
                , textarea
                    [ class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base"
                    , value agent.signature
                    , onInput (onFieldInput "signature")
                    , rows 3
                    , placeholder ("Thanks,\n" ++ agent.firstName ++ " " ++ agent.lastName)
                    , disabled (not canEditField)
                    ]
                    []
                ]

          else
            text ""
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
                )
                model.agents

        canAdd =
            not (String.isEmpty (String.trim model.firstName))
                && not (String.isEmpty (String.trim model.lastName))
                && model.emailStatus
                == Valid
                && isValidPhone model.displayPhone
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
                div [ class "flex justify-center" ]
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
                , Cmd.batch
                    [ Nav.pushUrl model.key "/add-agents"
                    , fetchAgents -- Also refresh agents list when not in setup mode
                    ]
                )

        AgentSaved (Err _) ->
            ( { model | error = Just "Failed to save agent" }
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
                , expect = Http.expectJson GotOrgSettings settingsObjectDecoder
                }
            )

        GotOrgSettings (Ok settings) ->
            ( { model
                | carriers = settings.carrierContracts
                , stateLicenses = settings.stateLicenses
                , isLoading = False
                , orgSettings = Just settings
                , forceOrgSenderDetails = settings.forceOrgSenderDetails
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
                Ok response ->
                    let
                        agents =
                            response.agents

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
                                            , carriers = []
                                            , stateLicenses = []
                                            , expanded = False
                                            , calendarLink = ""
                                            , signature = ""
                                            }
                                                :: agents

                                    Nothing ->
                                        agents

                            else
                                -- In normal mode, use the API result
                                agents
                    in
                    ( { model
                        | agents = finalAgents
                        , defaultAgentId = response.defaultAgentId
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err error ->
                    case error of
                        Http.BadStatus 403 ->
                            -- For 403, keep the current user in the agents list
                            -- Don't show an error since this is expected for non-admin users
                            ( { model | isLoading = False }, Cmd.none )

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
                            ( { model | error = Just errorMessage, isLoading = False }, Cmd.none )

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
                                    , carriers = []
                                    , stateLicenses = []
                                    , expanded = False
                                    , calendarLink = ""
                                    , signature = ""
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
                , expect = Http.expectJson (GotOrgSettingsForAgent agentId) settingsObjectDecoder
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
                        | error = Just "Failed to delete agent -- cannot delete default agent or current user"
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
                                , carriers = []
                                , stateLicenses = []
                                , expanded = False
                                , calendarLink = ""
                                , signature = ""
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

                            "calendarLink" ->
                                { agent | calendarLink = value }

                            "signature" ->
                                { agent | signature = value }

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

        SetDefaultAgent agentId ->
            ( { model
                | defaultAgentId = Just agentId
                , isLoading = True
              }
            , Http.request
                { method = "POST"
                , url = "/api/agents/set_default_agent"
                , body = Http.jsonBody (Encode.object [ ( "agentId", Encode.string agentId ) ])
                , expect = Http.expectWhatever SetDefaultAgentResult
                , timeout = Nothing
                , tracker = Nothing
                , headers = []
                }
            )

        SetDefaultAgentResult result ->
            case result of
                Ok _ ->
                    ( { model | isLoading = False }, fetchAgents )

                Err _ ->
                    ( { model | isLoading = False, error = Just "Failed to set default agent" }, Cmd.none )

        CopyAgentSelfOnboardingLink agentId ->
            let
                agentSelfOnboardingUrl =
                    "https://" ++ "medicaremax.ai/self-onboarding/" ++ model.orgSlug ++ "?agentId=" ++ agentId
            in
            ( { model
                | copiedAgentLinks = List.filter ((/=) agentId) model.copiedAgentLinks
                , currentlyCopyingAgent = Just agentId
              }
            , Ports.copyToClipboard agentSelfOnboardingUrl
            )

        LinkCopied success ->
            case model.currentlyCopyingAgent of
                Just agentId ->
                    if success then
                        ( { model
                            | copiedAgentLinks = agentId :: model.copiedAgentLinks
                            , currentlyCopyingAgent = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | error = Just "Failed to copy link to clipboard"
                            , currentlyCopyingAgent = Nothing
                          }
                        , Cmd.none
                        )

                Nothing ->
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


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        timeSub =
            case model.pendingSave of
                Just agentId ->
                    Time.every 2000 (\_ -> DebounceSaveAgent agentId)

                Nothing ->
                    Sub.none

        copyResultSub =
            Ports.onCopyResult LinkCopied
    in
    Sub.batch [ timeSub, copyResultSub ]


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
                            case Decode.decodeString agentsResponseDecoder body of
                                Ok value ->
                                    Ok value

                                Err err ->
                                    Err (Http.BadBody (Decode.errorToString err))
                )
        }


agentsResponseDecoder : Decoder AgentsResponse
agentsResponseDecoder =
    Decode.map2 AgentsResponse
        (Decode.field "agents" (Decode.list agentDecoder))
        (Decode.field "defaultAgentId" (Decode.nullable Decode.string))


agentDecoder : Decoder Agent
agentDecoder =
    Decode.succeed Agent
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "firstName" Decode.string
        |> Pipeline.required "lastName" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "phone" Decode.string
        |> Pipeline.required "carriers" (Decode.list Decode.string)
        |> Pipeline.required "stateLicenses" (Decode.list Decode.string)
        |> Pipeline.hardcoded False
        |> Pipeline.optional "bookingLink" Decode.string ""
        |> Pipeline.optional "signature" Decode.string ""


encodeAgent : Agent -> Encode.Value
encodeAgent agent =
    Encode.object
        [ ( "firstName", Encode.string agent.firstName )
        , ( "lastName", Encode.string agent.lastName )
        , ( "email", Encode.string agent.email )
        , ( "phone", Encode.string agent.phone )
        , ( "carriers", Encode.list Encode.string agent.carriers )
        , ( "stateLicenses", Encode.list Encode.string agent.stateLicenses )
        , ( "bookingLink", Encode.string agent.calendarLink )
        , ( "signature", Encode.string agent.signature )
        ]


isAdminBecomingAgent : Model -> Bool
isAdminBecomingAgent model =
    False



-- This check is no longer needed since all users are both admin and agent


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
    in
    Decode.map6 User
        idDecoder
        (Decode.field "email" Decode.string)
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.oneOf
            [ Decode.field "phone" Decode.string
            , Decode.succeed ""
            ]
        )
        (Decode.field "org_slug" Decode.string)


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
    case model.currentUser of
        Just user ->
            True

        -- All users can modify settings now
        Nothing ->
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
    in
    isEmailValid && areNamesValid && isPhoneValid


submitNewAgent : Model -> Cmd Msg
submitNewAgent model =
    Http.post
        { url = "/api/agents/create"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "firstName", Encode.string model.firstName )
                    , ( "lastName", Encode.string model.lastName )
                    , ( "email", Encode.string model.email )
                    , ( "phone", Encode.string model.rawPhone )
                    , ( "carriers", Encode.list Encode.string model.carriers )
                    , ( "stateLicenses", Encode.list Encode.string model.stateLicenses )
                    ]
                )
        , expect = Http.expectWhatever AgentSaved
        }
