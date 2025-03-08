module Profile exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Time



-- MODEL


type alias Model =
    { currentUser : Maybe User
    , originalUser : Maybe User -- Store original user data for comparison
    , isLoading : Bool
    , error : Maybe String
    , pendingSave : Bool
    }


type alias User =
    { id : Int
    , email : String
    , firstName : String
    , lastName : String
    , phone : String
    , isAdmin : Bool
    , isAgent : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentUser = Nothing
      , originalUser = Nothing
      , isLoading = True
      , error = Nothing
      , pendingSave = False
      }
    , fetchCurrentUser
    )



-- UPDATE


type Msg
    = GotCurrentUser (Result Http.Error CurrentUserResponse)
    | UpdateField String String
    | SaveProfile
    | ProfileSaved (Result Http.Error ())
    | NavigateTo String
    | WatchTutorial


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
                , originalUser = response.user -- Store original user data
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

                                _ ->
                                    user
                    in
                    ( { model | currentUser = Just updatedUser }
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
                , originalUser = model.currentUser -- Update original user after successful save
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

        WatchTutorial ->
            ( model, Cmd.none )



-- Main.elm will handle the actual navigation
-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Profile"
    , body =
        [ div [ class "min-h-screen bg-gray-50" ]
            [ div [ class "max-w-3xl mx-auto py-12 px-4 sm:px-6 lg:px-8" ]
                [ h1 [ class "text-3xl font-bold text-gray-900 mb-8" ]
                    [ text "Profile" ]
                , viewContent model
                ]
            ]
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
                    [ viewTutorialLink
                    , viewBasicInfo user
                    , viewSaveButton model
                    ]

            Nothing ->
                div [ class "text-center text-gray-600" ]
                    [ text "Failed to load profile" ]


viewTutorialLink : Html Msg
viewTutorialLink =
    div [ class "mb-4 flex justify-end" ]
        [ button
            [ class "flex items-center text-sm text-blue-600 hover:text-blue-800"
            , onClick (NavigateTo "/dashboard?payment_success=true")
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
        ]


viewBasicInfo : User -> Html Msg
viewBasicInfo user =
    div [ class "space-y-6" ]
        [ div [ class "border-b border-gray-200 pb-4" ]
            [ h2 [ class "text-lg font-medium text-gray-900" ]
                [ text "Basic Information" ]
            ]
        , div [ class "grid grid-cols-2 gap-4" ]
            [ viewField "First Name" "text" user.firstName "firstName"
            , viewField "Last Name" "text" user.lastName "lastName"
            , viewField "Email" "email" user.email "email"
            , viewField "Phone" "tel" user.phone "phone"
            ]
        , viewRoleInfo user
        ]


viewField : String -> String -> String -> String -> Html Msg
viewField label inputType value field =
    div []
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text label ]
        , input
            [ type_ inputType
            , class "mt-1 px-2 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-500"
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


viewRoleInfo : User -> Html Msg
viewRoleInfo user =
    div [ class "mb-6" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text "Role" ]
        , div [ class "text-gray-900" ]
            [ text (formatRole user) ]
        , if user.isAdmin then
            div [ class "mt-2 text-sm text-gray-500" ]
                [ text "You have administrator privileges" ]

          else
            text ""
        ]


viewSaveButton : Model -> Html Msg
viewSaveButton model =
    div [ class "mt-8 flex justify-center" ]
        [ if model.pendingSave then
            div [ class "px-6 py-3 flex items-center space-x-2" ]
                [ viewSpinner ]

          else
            button
                [ class "px-6 py-3 bg-blue-600 text-white text-sm font-medium rounded-lg hover:bg-blue-700 transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-blue-600"
                , onClick SaveProfile
                , disabled (not (hasChanges model))
                ]
                [ text "Save Changes" ]
        ]


viewSpinner : Html Msg
viewSpinner =
    div [ class "animate-spin rounded-full h-5 w-5 border-2 border-blue-500 border-t-transparent" ] []



-- HTTP


fetchCurrentUser : Cmd Msg
fetchCurrentUser =
    Http.get
        { url = "/api/me"
        , expect = Http.expectJson GotCurrentUser currentUserResponseDecoder
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



-- ENCODERS


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "firstName", Encode.string user.firstName )
        , ( "lastName", Encode.string user.lastName )
        , ( "email", Encode.string user.email )
        , ( "phone", Encode.string user.phone )
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


formatRole : User -> String
formatRole user =
    if user.isAdmin && user.isAgent then
        "Admin Agent"

    else if user.isAdmin then
        "Administrator"

    else if user.isAgent then
        "Agent"

    else
        "User"


isAgent : User -> Bool
isAgent user =
    user.isAgent



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

        _ ->
            False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
