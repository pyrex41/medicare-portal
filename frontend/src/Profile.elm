module Profile exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Ports
import Process
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Task
import Time
import TutorialModal exposing (viewTutorialModal)



-- MODEL


type alias Settings =
    { carrierContracts : List String
    , stateLicenses : List String
    , forceOrgSenderDetails : Bool
    }


type alias Model =
    { currentUser : Maybe User
    , originalUser : Maybe User -- Store original user data for comparison
    , isLoading : Bool
    , error : Maybe String
    , pendingSave : Bool
    , agentProfileLinkCopied : Bool
    , showTutorialModal : Bool -- ADDED for tutorial modal
    , orgSettings : Maybe Settings
    , forceOrgSenderDetails : Bool
    , selfOnboardingUrl : Maybe String
    }


type alias User =
    { id : Int
    , email : String
    , firstName : String
    , lastName : String
    , phone : String
    , isAdmin : Bool
    , isAgent : Bool
    , orgSlug : String
    , signature : String
    , useOrgSenderDetails : Bool -- True = use org details, False = use agent details
    , bookingLink : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentUser = Nothing
      , originalUser = Nothing
      , isLoading = True
      , error = Nothing
      , pendingSave = False
      , agentProfileLinkCopied = False
      , showTutorialModal = False -- ADDED: Initialize tutorial modal state
      , orgSettings = Nothing
      , forceOrgSenderDetails = False
      , selfOnboardingUrl = Nothing
      }
    , Cmd.batch
        [ fetchCurrentUser
        , fetchOrgSettings
        , fetchSelfOnboardingUrl
        ]
    )



-- UPDATE


type Msg
    = GotCurrentUser (Result Http.Error CurrentUserResponse)
    | UpdateField String String
    | UpdateSenderPreference Bool
    | SaveProfile
    | ProfileSaved (Result Http.Error ())
    | NavigateTo String
    | WatchTutorial -- This will now open the tutorial modal
    | CopyAgentProfileLink
    | AgentProfileLinkCopied Bool
    | ResetAgentProfileLinkCopiedStatus
    | OpenTutorialModal -- ADDED
    | CloseTutorialModal -- ADDED
    | GotOrgSettings (Result Http.Error Settings)
    | GotSelfOnboardingUrl (Result Http.Error SelfOnboardingUrlResponse)


type alias CurrentUserResponse =
    { success : Bool
    , user : Maybe User
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCurrentUser (Ok response) ->
            ( { model
                | currentUser = response.user
                , originalUser = response.user
                , isLoading = False
              }
            , Cmd.none
            )

        GotCurrentUser (Err error) ->
            ( { model
                | error = Just "Failed to load profile"
                , isLoading = False
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

                                "signature" ->
                                    { user | signature = value }

                                "bookingLink" ->
                                    { user | bookingLink = value }

                                _ ->
                                    user
                    in
                    ( { model | currentUser = Just updatedUser }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UpdateSenderPreference value ->
            case model.currentUser of
                Just user ->
                    ( { model | currentUser = Just { user | useOrgSenderDetails = value } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        SaveProfile ->
            ( { model | pendingSave = True }
            , case model.currentUser of
                Just user ->
                    saveProfile user

                Nothing ->
                    Cmd.none
            )

        ProfileSaved (Ok _) ->
            ( { model
                | pendingSave = False
                , originalUser = model.currentUser
              }
            , Cmd.none
            )

        ProfileSaved (Err _) ->
            ( { model
                | pendingSave = False
                , error = Just "Failed to save profile changes"
              }
            , Cmd.none
            )

        NavigateTo path ->
            ( model, Cmd.none )

        -- Main.elm handles navigation
        WatchTutorial ->
            -- Changed to open tutorial modal
            ( { model | showTutorialModal = True }
            , Cmd.none
            )

        OpenTutorialModal ->
            -- ADDED
            ( { model | showTutorialModal = True }
            , Cmd.none
            )

        CloseTutorialModal ->
            -- ADDED
            ( { model | showTutorialModal = False }
            , Cmd.none
            )

        CopyAgentProfileLink ->
            case model.currentUser of
                Just user ->
                    let
                        agentLink =
                            "https://" ++ "medicaremax.ai/self-onboarding/" ++ user.orgSlug ++ "?agentId=" ++ String.fromInt user.id
                    in
                    ( model, Ports.copyToClipboard agentLink )

                Nothing ->
                    ( model, Cmd.none )

        AgentProfileLinkCopied success ->
            if success then
                ( { model | agentProfileLinkCopied = True }
                , Task.perform (\_ -> ResetAgentProfileLinkCopiedStatus) (Process.sleep 2000)
                )

            else
                ( { model | error = Just "Failed to copy link to clipboard." }
                , Cmd.none
                )

        ResetAgentProfileLinkCopiedStatus ->
            ( { model | agentProfileLinkCopied = False }, Cmd.none )

        GotOrgSettings (Ok settings) ->
            ( { model
                | orgSettings = Just settings
                , forceOrgSenderDetails = settings.forceOrgSenderDetails
              }
            , Cmd.none
            )

        GotOrgSettings (Err _) ->
            ( { model
                | error = Just "Failed to load organization settings"
              }
            , Cmd.none
            )

        GotSelfOnboardingUrl (Ok response) ->
            ( { model | selfOnboardingUrl = Just response.selfOnboardingUrl }
            , Cmd.none
            )

        GotSelfOnboardingUrl (Err _) ->
            ( { model | error = Just "Failed to load self-onboarding URL" }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Profile"
    , body =
        [ div [ class "min-h-screen bg-gray-50" ]
            [ div [ class "max-w-3xl mx-auto py-6 sm:py-12 px-4 sm:px-6 lg:px-8" ]
                [ h1 [ class "text-3xl font-bold text-gray-900 mb-8" ]
                    [ text "Profile" ]
                , viewContent model
                ]
            ]

        -- Tutorial modal
        , if model.showTutorialModal then
            viewTutorialModal CloseTutorialModal

          else
            text ""
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    if model.isLoading then
        div [ class "flex justify-center items-center h-64" ]
            [ viewSpinner ]

    else
        case model.currentUser of
            Just user ->
                div [ class "bg-white shadow rounded-lg p-6 space-y-6" ]
                    [ div [ class "mb-4 flex justify-end space-x-4" ]
                        [ button
                            [ class "flex items-center text-sm text-blue-600 hover:text-blue-800"
                            , onClick OpenTutorialModal -- CHANGED from OpenWalkthroughModal
                            ]
                            [ div [ class "mr-2" ]
                                [ svg
                                    [ SvgAttr.class "h-5 w-5"
                                    , SvgAttr.viewBox "0 0 20 20"
                                    , SvgAttr.fill "currentColor"
                                    ]
                                    [ path
                                        [ SvgAttr.d "M10 12a2 2 0 100-4 2 2 0 000 4z" ]
                                        []
                                    , path
                                        [ SvgAttr.fillRule "evenodd"
                                        , SvgAttr.d "M.458 10C1.732 5.943 5.522 3 10 3s8.268 2.943 9.542 7c-1.274 4.057-5.064 7-9.542 7S1.732 14.057.458 10zM14 10a4 4 0 11-8 0 4 4 0 018 0z"
                                        , SvgAttr.clipRule "evenodd"
                                        ]
                                        []
                                    ]
                                ]
                            , text "Watch Setup Tutorial"
                            ]
                        , if user.isAdmin then
                            button
                                [ class "flex items-center text-sm text-purple-600 hover:text-purple-800"
                                , onClick (NavigateTo "/stripe")
                                ]
                                [ div [ class "mr-2" ]
                                    [ svg
                                        [ SvgAttr.class "h-5 w-5"
                                        , SvgAttr.viewBox "0 0 20 20"
                                        , SvgAttr.fill "currentColor"
                                        ]
                                        [ path
                                            [ SvgAttr.d "M4 4a2 2 0 00-2 2v1h16V6a2 2 0 00-2-2H4z" ]
                                            []
                                        , path
                                            [ SvgAttr.fillRule "evenodd"
                                            , SvgAttr.d "M18 9H2v5a2 2 0 002 2h12a2 2 0 002-2V9zM4 13a1 1 0 011-1h1a1 1 0 110 2H5a1 1 0 01-1-1zm5-1a1 1 0 100 2h1a1 1 0 100-2H9z"
                                            , SvgAttr.clipRule "evenodd"
                                            ]
                                            []
                                        ]
                                    ]
                                , text "Billing & Payments"
                                ]

                          else
                            text ""
                        ]
                    , viewBasicInfo model user
                    , viewAgentProfileLinkSection model user
                    , viewSenderSettingsSection model user
                    , viewSaveButton model
                    ]

            Nothing ->
                div [ class "text-center text-gray-600" ]
                    [ text "Failed to load profile" ]


viewBasicInfo : Model -> User -> Html Msg
viewBasicInfo model user =
    div [ class "space-y-6" ]
        [ div [ class "border-b border-gray-200 pb-4" ]
            [ h2 [ class "text-lg font-medium text-gray-900" ]
                [ text "Basic Information" ]
            ]
        , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-4" ]
            [ viewField "First Name" "text" user.firstName "firstName"
            , viewField "Last Name" "text" user.lastName "lastName"
            , viewField "Email" "email" user.email "email"
            , viewField "Phone" "tel" user.phone "phone"
            ]
        ]


viewField : String -> String -> String -> String -> Html Msg
viewField label inputType value field =
    div []
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text label ]
        , input
            [ type_ inputType
            , class "mt-1 px-3.5 py-2.5 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
            , Html.Attributes.value
                (if field == "phone" then
                    formatPhoneNumber value

                 else
                    value
                )
            , onInput (UpdateField field)
            , disabled (field == "email") -- Email cannot be changed
            ]
            []
        ]


viewSaveButton : Model -> Html Msg
viewSaveButton model =
    div [ class "mt-8 flex justify-center" ]
        [ if model.pendingSave then
            div [ class "px-6 py-3 flex items-center space-x-2" ]
                [ viewSpinner ]

          else
            button
                [ class "px-4 py-2 sm:px-6 sm:py-3 bg-blue-600 text-white text-sm font-medium rounded-lg hover:bg-blue-700 transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-blue-600"
                , onClick SaveProfile
                , disabled (not (hasChanges model))
                ]
                [ text "Save Changes" ]
        ]


viewSpinner : Html Msg
viewSpinner =
    div [ class "animate-spin rounded-full h-5 w-5 border-2 border-blue-500 border-t-transparent" ] []


viewAgentProfileLinkSection : Model -> User -> Html Msg
viewAgentProfileLinkSection model user =
    let
        maybeAgentProfileLink : Maybe String
        maybeAgentProfileLink =
            Maybe.map
                (\url -> url ++ "?agentId=" ++ String.fromInt user.id)
                model.selfOnboardingUrl
    in
    div [ class "mt-6" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text "Agent Self-Onboarding Link" ]
        , div [ class "flex items-center space-x-2" ]
            [ Svg.svg [ SvgAttr.class "h-5 w-5 text-gray-400", SvgAttr.viewBox "0 0 20 20", SvgAttr.fill "currentColor" ]
                [ Svg.path [ SvgAttr.fillRule "evenodd", SvgAttr.d "M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z", SvgAttr.clipRule "evenodd" ] []
                ]
            , case maybeAgentProfileLink of
                Just agentProfileLink ->
                    input
                        [ type_ "text"
                        , class "flex-1 px-3 py-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500 bg-gray-50 text-gray-500"
                        , Html.Attributes.value agentProfileLink
                        , readonly True
                        ]
                        []

                Nothing ->
                    text ""
            , button
                [ class "px-4 py-2 text-sm bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                , onClick CopyAgentProfileLink
                ]
                [ text
                    (if model.agentProfileLinkCopied then
                        "Copied!"

                     else
                        "Copy Link"
                    )
                ]
            ]
        , p [ class "text-gray-500 text-xs mt-1" ]
            [ text "Share this link with clients or non-clients to gather missing information or capture new leads to your book of business. New leads created in this way will be assigned to you."
            ]
        ]


viewSenderSettingsSection : Model -> User -> Html Msg
viewSenderSettingsSection model user =
    let
        -- If org forces org sender details, always use org details
        -- If org allows choice, use user's preference
        effectiveUseOrgDetails =
            if model.forceOrgSenderDetails then
                True

            else
                user.useOrgSenderDetails

        canChoose =
            not model.forceOrgSenderDetails
    in
    div [ class "space-y-6" ]
        [ div [ class "border-b border-gray-200 pb-4" ]
            [ h2 [ class "text-lg font-medium text-gray-900" ]
                [ text "Sender Settings" ]
            ]
        , div []
            [ p [ class "text-sm text-gray-500 mb-4" ]
                [ text "The sender settings below are controlled by your organization's configuration. "
                , a [ class "text-blue-600 hover:text-blue-800 underline", href "/settings" ]
                    [ text "Visit Organization Settings" ]
                , text " to change how agent details are used across your organization. When Organization Details is selected, agents will use the organization's contact information. When Agent Details is selected, agents can use their own contact information set above."
                ]
            , div [ class "grid grid-cols-1 md:grid-cols-2 gap-4" ]
                [ -- Organization Details Card
                  div
                    ([ class
                        ("relative rounded-lg border-2 p-6 transition-all "
                            ++ (if effectiveUseOrgDetails then
                                    "border-blue-500 bg-blue-50"

                                else
                                    "border-gray-200 bg-white"
                               )
                        )
                     , classList [ ( "cursor-pointer", canChoose ) ]
                     ]
                        ++ (if canChoose then
                                [ onClick (UpdateSenderPreference True) ]

                            else
                                []
                           )
                    )
                    [ div [ class "flex items-start" ]
                        [ div [ class "flex items-center h-5" ]
                            [ input
                                [ type_ "radio"
                                , name ("senderSettings-" ++ String.fromInt user.id)
                                , checked effectiveUseOrgDetails
                                , class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300"
                                , disabled (not canChoose)
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

                                  else if effectiveUseOrgDetails then
                                    text "You have chosen to use the organization's contact information for communications."

                                  else
                                    text "When this option is selected the Organization Details will be used for the signature, phone number, and scheduling link if applicable."
                                ]
                            ]
                        ]
                    ]
                , -- Agent Details Card
                  div
                    ([ class
                        ("relative rounded-lg border-2 p-6 transition-all "
                            ++ (if not effectiveUseOrgDetails then
                                    "border-blue-500 bg-blue-50"

                                else
                                    "border-gray-200 bg-white"
                               )
                        )
                     , classList [ ( "cursor-pointer", canChoose ) ]
                     ]
                        ++ (if canChoose then
                                [ onClick (UpdateSenderPreference False) ]

                            else
                                []
                           )
                    )
                    [ div [ class "flex items-start" ]
                        [ div [ class "flex items-center h-5" ]
                            [ input
                                [ type_ "radio"
                                , name ("senderSettings-" ++ String.fromInt user.id)
                                , checked (not effectiveUseOrgDetails)
                                , class "h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300"
                                , disabled (not canChoose)
                                ]
                                []
                            ]
                        , div [ class "ml-3" ]
                            [ label [ class "font-medium text-gray-900" ] [ text "Agent Details" ]
                            , p [ class "text-sm text-gray-500 mt-1" ]
                                [ if not effectiveUseOrgDetails then
                                    text "You have chosen to use your personal information for communications."

                                  else
                                    text "When this option is selected your personal information from your profile will be used for the signature, phone number, and scheduling link if applicable."
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , -- Email & SMS Signature Section (only if agent details are enabled)
          if not effectiveUseOrgDetails then
            div [ class "space-y-4" ]
                [ div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "Calendar Booking Link (optional)" ]
                    , input
                        [ type_ "text"
                        , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base"
                        , Html.Attributes.value user.bookingLink
                        , onInput (UpdateField "bookingLink")
                        , placeholder "https://calendly.com/yourname"
                        ]
                        []
                    , p [ class "text-gray-500 text-xs mt-1" ]
                        [ text "If provided, this link will be used as one of the options a client may select to connect with your agency. Traditionally this would be a Calendly link, Acuity link, etc." ]
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700" ]
                        [ text "Email & SMS Signature or Sign Off" ]
                    , input
                        [ type_ "text"
                        , class "mt-1 px-3 py-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 text-base"
                        , Html.Attributes.value user.signature
                        , onInput (UpdateField "signature")
                        , placeholder ("Thanks, " ++ user.firstName ++ " " ++ user.lastName)
                        ]
                        []
                    ]
                ]

          else
            text ""
        ]



-- ADDED: Tutorial Modal View (adapted from Dashboard.elm)
-- HTTP


type alias SelfOnboardingUrlResponse =
    { selfOnboardingUrl : String
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


fetchCurrentUser : Cmd Msg
fetchCurrentUser =
    Http.get
        { url = "/api/me"
        , expect = Http.expectJson GotCurrentUser currentUserResponseDecoder
        }


fetchOrgSettings : Cmd Msg
fetchOrgSettings =
    Http.get
        { url = "/api/settings"
        , expect = Http.expectJson GotOrgSettings orgSettingsDecoder
        }


saveProfile : User -> Cmd Msg
saveProfile user =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/profile"
        , body = Http.jsonBody (encodeUser user)
        , expect = Http.expectWhatever ProfileSaved
        , timeout = Nothing
        , tracker = Nothing
        }



-- DECODERS


currentUserResponseDecoder : Decoder CurrentUserResponse
currentUserResponseDecoder =
    Decode.map2 CurrentUserResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "user" (Decode.nullable userDecoder))


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "firstName" Decode.string
        |> Pipeline.required "lastName" Decode.string
        |> Pipeline.required "phone" Decode.string
        |> Pipeline.required "is_admin" Decode.bool
        |> Pipeline.required "is_agent" Decode.bool
        |> Pipeline.required "organization_slug" Decode.string
        |> Pipeline.optional "signature" Decode.string ""
        |> Pipeline.optional "useOrgSenderDetails" Decode.bool True
        |> Pipeline.optional "booking_link" Decode.string ""


orgSettingsDecoder : Decoder Settings
orgSettingsDecoder =
    Decode.succeed Settings
        |> Pipeline.required "carrierContracts" (Decode.list Decode.string)
        |> Pipeline.required "stateLicenses" (Decode.list Decode.string)
        |> Pipeline.required "forceOrgSenderDetails" Decode.bool



-- ADDED
-- ENCODERS


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "firstName", Encode.string user.firstName )
        , ( "lastName", Encode.string user.lastName )
        , ( "email", Encode.string user.email )
        , ( "phone", Encode.string user.phone )
        , ( "signature", Encode.string user.signature )
        , ( "useOrgSenderDetails", Encode.bool user.useOrgSenderDetails )
        , ( "booking_link", Encode.string user.bookingLink )
        ]



-- HELPERS


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    let
        digits =
            String.filter Char.isDigit phone
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



-- Add this helper function to check for changes


hasChanges : Model -> Bool
hasChanges model =
    case ( model.currentUser, model.originalUser ) of
        ( Just current, Just original ) ->
            current.firstName
                /= original.firstName
                || current.lastName
                /= original.lastName
                || current.phone
                /= original.phone
                || current.signature
                /= original.signature
                || current.useOrgSenderDetails
                /= original.useOrgSenderDetails
                || current.bookingLink
                /= original.bookingLink

        _ ->
            False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.onCopyResult AgentProfileLinkCopied
