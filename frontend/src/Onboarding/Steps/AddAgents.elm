module Onboarding.Steps.AddAgents exposing
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
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, viewBox)
import Time
import Url



-- MODEL


type alias Model =
    { agents : List Agent
    , newAgent : NewAgentForm
    , isLoading : Bool
    , error : Maybe String
    , key : Nav.Key
    , orgSlug : String
    , showAddForm : Bool
    , pendingSave : Maybe String
    , emailStatus : EmailStatus
    }


type alias Agent =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    , isAdmin : Bool
    , isAgent : Bool
    }


type alias NewAgentForm =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , isAdmin : Bool
    , isAgent : Bool
    }


type EmailStatus
    = NotChecked
    | Checking
    | Valid
    | Invalid String


init : Nav.Key -> String -> ( Model, Cmd Msg )
init key orgSlug =
    ( { agents = []
      , newAgent =
            { firstName = ""
            , lastName = ""
            , email = ""
            , phone = ""
            , isAdmin = False
            , isAgent = True
            }
      , isLoading = True
      , error = Nothing
      , key = key
      , orgSlug = orgSlug
      , showAddForm = False
      , pendingSave = Nothing
      , emailStatus = NotChecked
      }
    , fetchAgents orgSlug
    )



-- UPDATE


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateEmail String
    | UpdatePhone String
    | UpdateAdminCheckbox Bool
    | UpdateAgentCheckbox Bool
    | AddAgent
    | SaveAgent
    | CancelAddAgent
    | NextStepClicked
    | GotAgents (Result Http.Error (List Agent))
    | AgentSaved (Result Http.Error ())
    | CheckAgentEmail
    | GotEmailResponse (Result Http.Error EmailResponse)
    | NoOp


type OutMsg
    = NoOutMsg
    | NextStep
    | ShowError String


type alias EmailResponse =
    { available : Bool
    , message : String
    }


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        UpdateFirstName value ->
            ( { model | newAgent = updateNewAgentField model.newAgent "firstName" value }
            , Cmd.none
            , NoOutMsg
            )

        UpdateLastName value ->
            ( { model | newAgent = updateNewAgentField model.newAgent "lastName" value }
            , Cmd.none
            , NoOutMsg
            )

        UpdateEmail value ->
            ( { model | newAgent = updateNewAgentField model.newAgent "email" value, emailStatus = NotChecked }
            , Cmd.none
            , NoOutMsg
            )

        UpdatePhone value ->
            ( { model | newAgent = updateNewAgentField model.newAgent "phone" value }
            , Cmd.none
            , NoOutMsg
            )

        UpdateAdminCheckbox value ->
            let
                -- Ensure at least one role is selected
                newIsAgent =
                    if not value then
                        True
                        -- If admin is being unchecked, ensure agent is checked

                    else
                        model.newAgent.isAgent

                updatedNewAgent =
                    { firstName = model.newAgent.firstName
                    , lastName = model.newAgent.lastName
                    , email = model.newAgent.email
                    , phone = model.newAgent.phone
                    , isAdmin = value
                    , isAgent = newIsAgent
                    }
            in
            ( { model | newAgent = updatedNewAgent }
            , Cmd.none
            , NoOutMsg
            )

        UpdateAgentCheckbox value ->
            let
                -- Ensure at least one role is selected
                newIsAdmin =
                    if not value then
                        True
                        -- If agent is being unchecked, ensure admin is checked

                    else
                        model.newAgent.isAdmin

                updatedNewAgent =
                    { firstName = model.newAgent.firstName
                    , lastName = model.newAgent.lastName
                    , email = model.newAgent.email
                    , phone = model.newAgent.phone
                    , isAgent = value
                    , isAdmin = newIsAdmin
                    }
            in
            ( { model | newAgent = updatedNewAgent }
            , Cmd.none
            , NoOutMsg
            )

        AddAgent ->
            ( { model | showAddForm = True }
            , Cmd.none
            , NoOutMsg
            )

        SaveAgent ->
            if isFormValid model then
                ( { model | isLoading = True }
                , saveAgent model.orgSlug model.newAgent
                , NoOutMsg
                )

            else
                ( { model | error = Just "Please fill out all required fields and ensure email is valid" }
                , Cmd.none
                , ShowError "Please fill out all required fields and ensure email is valid"
                )

        CancelAddAgent ->
            ( { model | showAddForm = False }
            , Cmd.none
            , NoOutMsg
            )

        NextStepClicked ->
            ( model
            , Cmd.none
            , NextStep
            )

        GotAgents result ->
            case result of
                Ok agents ->
                    ( { model | agents = agents, isLoading = False }
                    , Cmd.none
                    , NoOutMsg
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to load agents"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to load agents"
                    )

        AgentSaved result ->
            case result of
                Ok _ ->
                    ( { model
                        | isLoading = False
                        , showAddForm = False
                        , newAgent =
                            { firstName = ""
                            , lastName = ""
                            , email = ""
                            , phone = ""
                            , isAdmin = False
                            , isAgent = True
                            }
                        , emailStatus = NotChecked
                      }
                    , fetchAgents model.orgSlug
                    , NoOutMsg
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to save agent"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to save agent"
                    )

        CheckAgentEmail ->
            if String.isEmpty (String.trim model.newAgent.email) then
                ( { model | emailStatus = NotChecked }
                , Cmd.none
                , NoOutMsg
                )

            else if model.emailStatus == Checking then
                ( model, Cmd.none, NoOutMsg )

            else
                ( { model | emailStatus = Checking }
                , checkAgentEmail model.orgSlug model.newAgent.email
                , NoOutMsg
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
                    , NoOutMsg
                    )

                Err _ ->
                    ( { model
                        | emailStatus = Invalid "Failed to check email availability"
                      }
                    , Cmd.none
                    , NoOutMsg
                    )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Add Team Members" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Invite agents to join your organization" ]
            ]
        , if model.isLoading then
            viewLoading

          else
            div [ class "space-y-6" ]
                [ if model.error /= Nothing then
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded mb-4" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , viewAgentsList model
                , viewAddAgentSection model
                , div [ class "flex justify-end space-x-4 mt-8" ]
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


viewAgentsList : Model -> Html Msg
viewAgentsList model =
    if List.isEmpty model.agents then
        div [ class "bg-gray-50 p-6 rounded-lg text-center" ]
            [ p [ class "text-gray-500" ]
                [ text "No agents added yet. Add your first agent below." ]
            ]

    else
        div [ class "bg-white shadow rounded-lg overflow-hidden" ]
            [ div [ class "px-6 py-4 border-b border-gray-200" ]
                [ h2 [ class "text-lg font-medium text-gray-900" ]
                    [ text "Team Members" ]
                ]
            , div [ class "divide-y divide-gray-200" ]
                (List.map viewAgentItem model.agents)
            ]


viewAgentItem : Agent -> Html Msg
viewAgentItem agent =
    div [ class "px-6 py-4" ]
        [ div [ class "flex items-center justify-between" ]
            [ div [ class "flex items-center" ]
                [ div [ class "ml-4" ]
                    [ div [ class "text-sm font-medium text-gray-900" ]
                        [ text (agent.firstName ++ " " ++ agent.lastName) ]
                    , div [ class "text-sm text-gray-500" ]
                        [ text agent.email ]
                    ]
                ]
            , div [ class "flex space-x-2" ]
                [ if agent.isAdmin then
                    span [ class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800" ]
                        [ text "Admin" ]

                  else
                    text ""
                , if agent.isAgent then
                    span [ class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800" ]
                        [ text "Agent" ]

                  else
                    text ""
                ]
            ]
        ]


viewAddAgentSection : Model -> Html Msg
viewAddAgentSection model =
    div [ class "bg-white shadow rounded-lg overflow-hidden" ]
        [ div [ class "px-6 py-4 border-b border-gray-200" ]
            [ h2 [ class "text-lg font-medium text-gray-900" ]
                [ text "Add New Agent" ]
            ]
        , if model.showAddForm then
            div [ class "px-6 py-4" ]
                [ div [ class "space-y-6" ]
                    [ div [ class "grid grid-cols-2 gap-6" ]
                        [ div []
                            [ label [ class "block text-sm font-medium text-gray-700" ]
                                [ text "First Name" ]
                            , input
                                [ type_ "text"
                                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                , value model.newAgent.firstName
                                , onInput UpdateFirstName
                                , placeholder "Enter first name"
                                ]
                                []
                            ]
                        , div []
                            [ label [ class "block text-sm font-medium text-gray-700" ]
                                [ text "Last Name" ]
                            , input
                                [ type_ "text"
                                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                , value model.newAgent.lastName
                                , onInput UpdateLastName
                                , placeholder "Enter last name"
                                ]
                                []
                            ]
                        ]
                    , div [ class "grid grid-cols-2 gap-6" ]
                        [ div []
                            [ label [ class "block text-sm font-medium text-gray-700" ]
                                [ text "Email" ]
                            , input
                                [ type_ "email"
                                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                , value model.newAgent.email
                                , onInput UpdateEmail
                                , onBlur CheckAgentEmail
                                , placeholder "name@example.com"
                                ]
                                []
                            , viewEmailStatus model.emailStatus
                            ]
                        , div []
                            [ label [ class "block text-sm font-medium text-gray-700" ]
                                [ text "Phone" ]
                            , input
                                [ type_ "tel"
                                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                , value model.newAgent.phone
                                , onInput UpdatePhone
                                , placeholder "(555) 555-5555"
                                ]
                                []
                            ]
                        ]
                    , div []
                        [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                            [ text "Role (at least one required)" ]
                        , div [ class "flex items-center space-x-6" ]
                            [ label [ class "inline-flex items-center" ]
                                [ input
                                    [ type_ "checkbox"
                                    , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                                    , checked model.newAgent.isAdmin
                                    , onClick (UpdateAdminCheckbox (not model.newAgent.isAdmin))
                                    ]
                                    []
                                , span [ class "ml-2 text-sm text-gray-700" ]
                                    [ text "Admin" ]
                                ]
                            , label [ class "inline-flex items-center" ]
                                [ input
                                    [ type_ "checkbox"
                                    , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                                    , checked model.newAgent.isAgent
                                    , onClick (UpdateAgentCheckbox (not model.newAgent.isAgent))
                                    ]
                                    []
                                , span [ class "ml-2 text-sm text-gray-700" ]
                                    [ text "Agent" ]
                                ]
                            ]
                        ]
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
                ]

          else
            div [ class "px-6 py-4 flex justify-center" ]
                [ button
                    [ class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700"
                    , onClick AddAgent
                    ]
                    [ span [ class "mr-2" ] [ text "+" ]
                    , text "Add Agent"
                    ]
                ]
        ]


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
                        [ class "h-4 w-4 mr-1"
                        , viewBox "0 0 20 20"
                        , fill "currentColor"
                        ]
                        [ path
                            [ fillRule "evenodd"
                            , d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
                            , clipRule "evenodd"
                            ]
                            []
                        ]
                    , text "Email is available"
                    ]

            Invalid message ->
                div [ class "text-red-600 text-sm flex items-center" ]
                    [ -- X icon
                      svg
                        [ class "h-4 w-4 mr-1"
                        , viewBox "0 0 20 20"
                        , fill "currentColor"
                        ]
                        [ path
                            [ fillRule "evenodd"
                            , d "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                            , clipRule "evenodd"
                            ]
                            []
                        ]
                    , text message
                    ]
        ]



-- HELPERS


updateNewAgentField : NewAgentForm -> String -> String -> NewAgentForm
updateNewAgentField form field value =
    case field of
        "firstName" ->
            { form | firstName = value }

        "lastName" ->
            { form | lastName = value }

        "email" ->
            { form | email = value }

        "phone" ->
            { form | phone = value }

        _ ->
            form


isFormValid : Model -> Bool
isFormValid model =
    let
        isEmailValid =
            model.emailStatus == Valid

        areNamesValid =
            not (String.isEmpty (String.trim model.newAgent.firstName))
                && not (String.isEmpty (String.trim model.newAgent.lastName))

        isPhoneValid =
            not (String.isEmpty (String.trim model.newAgent.phone))

        hasValidRole =
            model.newAgent.isAdmin || model.newAgent.isAgent
    in
    isEmailValid && areNamesValid && isPhoneValid && hasValidRole



-- API CALLS


fetchAgents : String -> Cmd Msg
fetchAgents orgSlug =
    Http.get
        { url = "/api/organizations/" ++ orgSlug ++ "/agents"
        , expect = Http.expectJson GotAgents (Decode.list agentDecoder)
        }


saveAgent : String -> NewAgentForm -> Cmd Msg
saveAgent orgSlug agent =
    Http.post
        { url = "/api/organizations/" ++ orgSlug ++ "/agents"
        , body = Http.jsonBody (encodeNewAgent agent)
        , expect = Http.expectWhatever AgentSaved
        }


checkAgentEmail : String -> String -> Cmd Msg
checkAgentEmail orgSlug email =
    Http.get
        { url = "/api/organizations/" ++ orgSlug ++ "/check-email/" ++ Url.percentEncode email
        , expect = Http.expectJson GotEmailResponse emailResponseDecoder
        }



-- DECODERS & ENCODERS


agentDecoder : Decode.Decoder Agent
agentDecoder =
    Decode.succeed Agent
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "firstName" Decode.string
        |> Pipeline.required "lastName" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "phone" Decode.string
        |> Pipeline.required "isAdmin" Decode.bool
        |> Pipeline.required "isAgent" Decode.bool


encodeNewAgent : NewAgentForm -> Encode.Value
encodeNewAgent agent =
    Encode.object
        [ ( "firstName", Encode.string agent.firstName )
        , ( "lastName", Encode.string agent.lastName )
        , ( "email", Encode.string agent.email )
        , ( "phone", Encode.string agent.phone )
        , ( "isAdmin", Encode.bool agent.isAdmin )
        , ( "isAgent", Encode.bool agent.isAgent )
        ]


emailResponseDecoder : Decode.Decoder EmailResponse
emailResponseDecoder =
    Decode.map2 EmailResponse
        (Decode.field "available" Decode.bool)
        (Decode.field "message" Decode.string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
