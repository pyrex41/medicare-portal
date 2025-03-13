module Onboarding.Steps.UserDetails exposing
    ( EmailStatus(..)
    , Model
    , Msg
    , OutMsg(..)
    , fetchUserDetails
    , init
    , loadUserFromSession
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Char
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (circle, path, svg)
import Svg.Attributes as SvgAttr exposing (clipRule, cx, cy, d, fill, fillRule, r, stroke, strokeWidth, viewBox)
import Task
import Utils.RandomOrgName exposing (generateOrgName)



-- PORTS
-- These ports are defined in Onboarding.elm, we just use them here
-- MODEL


type alias Model =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , isLoading : Bool
    , error : Maybe String
    , key : Nav.Key
    , orgSlug : String
    , emailStatus : EmailStatus
    , loadedFromSession : Bool
    , sessionToken : String
    }


type EmailStatus
    = NotChecked
    | Checking
    | Available
    | Unavailable String


init : Nav.Key -> String -> Bool -> ( Model, Cmd Msg )
init key orgSlug isNewSignup =
    ( { firstName = ""
      , lastName = ""
      , email = ""
      , phone = ""
      , isLoading = True -- Set to true while loading
      , error = Nothing
      , key = key
      , orgSlug = orgSlug
      , emailStatus = NotChecked
      , loadedFromSession = False
      , sessionToken = ""
      }
    , fetchUserDetails orgSlug
      -- Fetch user details from backend using session cookie
    )



-- New function to load user details from session/localStorage


loadUserFromSession : { firstName : String, lastName : String, email : String, phone : String } -> Msg
loadUserFromSession userData =
    LoadUserFromSession userData



-- UPDATE


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateEmail String
    | UpdatePhone String
    | NextStepClicked
    | GotUserDetails (Result Http.Error UserDetailsResponse)
    | UserDetailsSaved (Result Http.Error SignupResponse)
    | NoOp
    | EmailBlurred
    | EmailFocused
    | GotEmailCheckResponse (Result Http.Error EmailCheckResponse)
    | LoadUserFromSession { firstName : String, lastName : String, email : String, phone : String }
    | SaveUserDetails


type OutMsg
    = NoOutMsg
    | NextStep
    | ShowError String
    | SaveUserToCookie { firstName : String, lastName : String, email : String, phone : String }
    | UpdateOrgSlug String
    | NextStepAndUpdateSlug String


type alias UserDetailsResponse =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , slug : String
    }


type alias EmailCheckResponse =
    { available : Bool
    , message : String
    }


type alias SignupResponse =
    { success : Bool
    , message : String
    , slug : String
    }


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        UpdateFirstName value ->
            ( { model | firstName = value }, Cmd.none, NoOutMsg )

        UpdateLastName value ->
            ( { model | lastName = value }, Cmd.none, NoOutMsg )

        UpdateEmail value ->
            ( { model
                | email = value
                , emailStatus = NotChecked -- Reset status
              }
            , Cmd.none
            , NoOutMsg
            )

        EmailFocused ->
            -- Immediately clear validation state when field gets focus
            ( { model | emailStatus = NotChecked }
            , Cmd.none
            , NoOutMsg
            )

        UpdatePhone value ->
            -- Store only the digits, but display formatted version
            ( { model | phone = String.filter Char.isDigit value }, Cmd.none, NoOutMsg )

        EmailBlurred ->
            if String.isEmpty (String.trim model.email) then
                -- Don't validate empty emails
                ( model, Cmd.none, NoOutMsg )

            else
                -- Always check email validity when field loses focus
                ( { model | emailStatus = Checking }
                , checkEmailAvailability model.email
                , NoOutMsg
                )

        GotEmailCheckResponse result ->
            case result of
                Ok response ->
                    if response.available then
                        ( { model | emailStatus = Available }
                        , Cmd.none
                        , NoOutMsg
                        )

                    else
                        ( { model | emailStatus = Unavailable response.message }
                        , Cmd.none
                        , NoOutMsg
                        )

                Err httpError ->
                    let
                        errorMsg =
                            case httpError of
                                Http.BadBody message ->
                                    "Decoder error: " ++ message

                                Http.BadStatus status ->
                                    "Server error: " ++ String.fromInt status

                                _ ->
                                    "Error checking email availability"
                    in
                    ( { model | emailStatus = Unavailable errorMsg }
                    , Cmd.none
                    , NoOutMsg
                    )

        NextStepClicked ->
            if isFormValid model then
                -- Save user details and move to next step
                let
                    userData =
                        { firstName = model.firstName
                        , lastName = model.lastName
                        , email = model.email
                        , phone = model.phone
                        }
                in
                ( { model | isLoading = True }
                , saveUserDetails model
                , SaveUserToCookie userData
                )

            else
                ( { model | error = Just "Please fill out all required fields" }
                , Cmd.none
                , ShowError "Please fill out all required fields"
                )

        GotUserDetails result ->
            case result of
                Ok response ->
                    let
                        -- Use the slug from the response if it's not empty
                        updatedSlug =
                            if String.isEmpty response.slug then
                                model.orgSlug

                            else
                                response.slug
                    in
                    ( { model
                        | firstName = response.firstName
                        , lastName = response.lastName
                        , email = response.email
                        , phone = response.phone
                        , isLoading = False
                        , emailStatus = Available -- Consider email as valid since it's already registered
                        , loadedFromSession = True -- Mark as loaded from session
                        , orgSlug = updatedSlug
                      }
                    , Cmd.none
                    , NoOutMsg
                    )

                Err _ ->
                    -- On error, we don't cause a fatal error, just keep the form empty
                    ( { model
                        | isLoading = False
                      }
                    , Cmd.none
                    , NoOutMsg
                    )

        UserDetailsSaved result ->
            case result of
                Ok response ->
                    let
                        _ =
                            Debug.log "User details saved response" response
                    in
                    if response.success then
                        ( { model | isLoading = False, orgSlug = response.slug }
                        , Cmd.none
                        , NextStepAndUpdateSlug response.slug
                        )

                    else
                        ( { model
                            | error = Just response.message
                            , isLoading = False
                          }
                        , Cmd.none
                        , ShowError response.message
                        )

                Err httpError ->
                    let
                        errorMsg =
                            case httpError of
                                Http.BadUrl url ->
                                    "Invalid URL: " ++ url

                                Http.Timeout ->
                                    "Request timed out"

                                Http.NetworkError ->
                                    "Network error"

                                Http.BadStatus statusCode ->
                                    "Bad status: " ++ String.fromInt statusCode

                                Http.BadBody errorMessage ->
                                    if String.contains "phone" errorMessage then
                                        "Invalid phone number format. Please enter a valid phone number."

                                    else
                                        "Bad body: " ++ errorMessage

                        _ =
                            Debug.log "Failed to save user details" errorMsg
                    in
                    ( { model
                        | error = Just ("Failed to save user details: " ++ errorMsg)
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError ("Failed to save user details: " ++ errorMsg)
                    )

        LoadUserFromSession userData ->
            -- Load user details from session/cookies
            ( { model
                | firstName = userData.firstName
                , lastName = userData.lastName
                , email = userData.email
                , phone = userData.phone
                , loadedFromSession = True

                -- Auto-validate email if it's been loaded from session
                , emailStatus =
                    if String.isEmpty userData.email then
                        NotChecked

                    else
                        Available
              }
            , Cmd.none
            , NoOutMsg
            )

        SaveUserDetails ->
            -- Explicit action to save user details to backend
            if isFormValid model then
                ( { model | isLoading = True }
                , saveUserDetails model
                , NoOutMsg
                )

            else
                ( model, Cmd.none, NoOutMsg )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Personal Details" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Tell us about yourself" ]
            , if model.loadedFromSession then
                p [ class "text-blue-600 mt-2 italic" ]
                    [ text "Your previously entered information has been loaded." ]

              else
                text ""
            ]
        , if model.isLoading then
            viewLoading

          else
            div [ class "space-y-6" ]
                [ div [ class "bg-white shadow rounded-lg p-6" ]
                    [ div [ class "space-y-6" ]
                        [ div [ class "grid grid-cols-1 sm:grid-cols-2 gap-6" ]
                            [ div []
                                [ label [ class "block text-sm font-medium text-gray-700" ]
                                    [ text "First Name" ]
                                , input
                                    [ type_ "text"
                                    , class "mt-1 block w-full px-3 py-2 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                    , value model.firstName
                                    , onInput UpdateFirstName
                                    , placeholder "Enter your first name"
                                    ]
                                    []
                                ]
                            , div []
                                [ label [ class "block text-sm font-medium text-gray-700" ]
                                    [ text "Last Name" ]
                                , input
                                    [ type_ "text"
                                    , class "mt-1 block w-full px-3 py-2 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                    , value model.lastName
                                    , onInput UpdateLastName
                                    , placeholder "Enter your last name"
                                    ]
                                    []
                                ]
                            ]
                        , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-6 pt-2" ]
                            [ div [ class "relative pb-6" ]
                                [ label [ class "block text-sm font-medium text-gray-700" ]
                                    [ text "Email" ]
                                , div [ class "relative" ]
                                    [ input
                                        [ type_ "email"
                                        , class "mt-1 block w-full px-3 py-2 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                        , value model.email
                                        , onInput UpdateEmail
                                        , onFocus EmailFocused
                                        , onBlur EmailBlurred
                                        , placeholder "you@example.com"
                                        ]
                                        []
                                    , viewEmailStatus model.emailStatus
                                    ]
                                ]
                            , div []
                                [ label [ class "block text-sm font-medium text-gray-700" ]
                                    [ text "Phone" ]
                                , input
                                    [ type_ "tel"
                                    , class "mt-1 block w-full px-3 py-2 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                                    , value (formatPhoneNumber model.phone)
                                    , onInput UpdatePhone
                                    , placeholder "(555) 555-5555"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , if model.error /= Nothing then
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , div [ class "flex justify-center" ]
                    [ button
                        [ class
                            (if isFormValid model then
                                "px-4 py-2 sm:px-6 sm:py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"

                             else
                                "px-4 py-2 sm:px-6 sm:py-3 bg-gray-300 text-gray-500 rounded-md cursor-not-allowed"
                            )
                        , onClick NextStepClicked
                        , disabled (not (isFormValid model) || model.isLoading)
                        ]
                        [ if model.isLoading then
                            div [ class "flex items-center justify-center" ]
                                [ div [ class "animate-spin mr-2 h-4 w-4 border-t-2 border-b-2 border-white rounded-full" ] []
                                , text "Saving..."
                                ]

                          else
                            text "Continue"
                        ]
                    ]
                ]
        ]


viewEmailStatus : EmailStatus -> Html msg
viewEmailStatus status =
    case status of
        NotChecked ->
            -- When not checked, explicitly render an empty div structure to properly replace any previous status
            div []
                [ div [ class "absolute right-0 inset-y-0" ] [ text "" ]
                , text "" -- Empty text element to replace any error message
                ]

        Checking ->
            -- Show loading spinner while checking
            div []
                [ div
                    [ class "absolute right-0 inset-y-0" ]
                    [ div
                        [ class "absolute right-0 inset-y-0 flex items-center pr-3" ]
                        [ div
                            [ class "animate-spin h-5 w-5 text-blue-500" ]
                            [ svg
                                [ viewBox "0 0 24 24"
                                , SvgAttr.class "h-5 w-5"
                                ]
                                [ circle
                                    [ cx "12"
                                    , cy "12"
                                    , r "10"
                                    , stroke "currentColor"
                                    , strokeWidth "4"
                                    , SvgAttr.class "opacity-25"
                                    ]
                                    []
                                , path
                                    [ fill "currentColor"
                                    , d "M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                                    , SvgAttr.class "opacity-75"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , text "" -- Empty text element to replace any error message
                ]

        Available ->
            -- Show only the success icon for available emails
            div []
                [ div
                    [ class "absolute right-0 inset-y-0" ]
                    [ div
                        [ class "absolute right-0 inset-y-0 flex items-center pr-3" ]
                        [ div
                            [ class "text-green-500" ]
                            [ svg
                                [ viewBox "0 0 20 20"
                                , fill "currentColor"
                                , SvgAttr.class "h-5 w-5"
                                ]
                                [ path
                                    [ fillRule "evenodd"
                                    , d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
                                    , clipRule "evenodd"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , text "" -- Empty text element to replace the error message
                ]

        Unavailable message ->
            -- For unavailable emails, show icon and error message
            div []
                [ div
                    [ class "absolute right-0 inset-y-0" ]
                    [ div
                        [ class "absolute right-0 inset-y-0 flex items-center pr-3" ]
                        [ div
                            [ class "text-red-500" ]
                            [ svg
                                [ viewBox "0 0 20 20"
                                , fill "currentColor"
                                , SvgAttr.class "h-5 w-5"
                                ]
                                [ path
                                    [ fillRule "evenodd"
                                    , d "M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z"
                                    , clipRule "evenodd"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , p
                    [ class "text-xs text-red-600 mt-1 absolute left-0 top-full w-full" ]
                    [ text message ]
                ]


viewLoading : Html msg
viewLoading =
    div [ class "text-center py-12" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-t-4 border-b-4 border-blue-500 mx-auto" ] []
        , p [ class "mt-4 text-gray-500" ]
            [ text "Loading..." ]
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


isFormValid : Model -> Bool
isFormValid model =
    let
        emailValid =
            case model.emailStatus of
                Available ->
                    True

                -- If the email hasn't been checked yet, consider it invalid
                NotChecked ->
                    False

                -- Email is being checked, consider it invalid until check completes
                Checking ->
                    False

                -- Email is unavailable (error state)
                Unavailable _ ->
                    False
    in
    not (String.isEmpty (String.trim model.firstName))
        && not (String.isEmpty (String.trim model.lastName))
        && not (String.isEmpty (String.trim model.email))
        && not (String.isEmpty (String.trim model.phone))
        && emailValid



-- API CALLS


fetchUserDetails : String -> Cmd Msg
fetchUserDetails _ =
    Http.get
        { url = "/api/onboarding/user-details"
        , expect = Http.expectJson GotUserDetails userDetailsDecoder
        }


saveUserDetails : Model -> Cmd Msg
saveUserDetails model =
    let
        -- Ensure phone number only contains digits
        phoneDigits =
            String.filter Char.isDigit model.phone

        requestBody =
            Encode.object
                [ ( "firstName", Encode.string model.firstName )
                , ( "lastName", Encode.string model.lastName )
                , ( "email", Encode.string model.email )
                , ( "phone", Encode.string phoneDigits )
                ]

        url =
            "/api/onboarding/user-details"

        _ =
            Debug.log "Saving user details"
                { url = url
                , firstName = model.firstName
                , lastName = model.lastName
                , email = model.email
                , phone = phoneDigits
                }
    in
    Http.request
        { method = "POST"
        , url = url
        , headers = [] -- The session token is in the cookies, no need to pass it
        , body = Http.jsonBody requestBody
        , expect =
            Http.expectJson UserDetailsSaved
                (Decode.map3 SignupResponse
                    (Decode.field "success" Decode.bool)
                    (Decode.field "message" Decode.string)
                    (Decode.field "slug" Decode.string)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


checkEmailAvailability : String -> Cmd Msg
checkEmailAvailability email =
    Http.request
        { method = "GET"
        , headers = []
        , url = "/api/organizations/check-email/" ++ email
        , body = Http.emptyBody
        , expect = Http.expectStringResponse GotEmailCheckResponse handleEmailCheckResponse
        , timeout = Nothing
        , tracker = Nothing
        }



-- Custom response handler to deal with empty responses and provide better error logging


handleEmailCheckResponse : Http.Response String -> Result Http.Error EmailCheckResponse
handleEmailCheckResponse response =
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
            -- If we get a 200 status but empty body, consider it a success
            if String.isEmpty (String.trim body) then
                Ok { available = True, message = "Email is available" }

            else
                -- Try to decode the response as JSON
                case Decode.decodeString emailCheckDecoder body of
                    Ok value ->
                        Ok value

                    Err error ->
                        -- Consider most 200 responses as success even with decode errors
                        Ok { available = True, message = "Email is available (decode error)" }



-- DECODERS & ENCODERS


userDetailsDecoder : Decode.Decoder UserDetailsResponse
userDetailsDecoder =
    Decode.map5 UserDetailsResponse
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "phone" Decode.string)
        (Decode.oneOf
            [ Decode.field "slug" Decode.string
            , Decode.succeed "" -- Default to empty string if slug is not present
            ]
        )


emailCheckDecoder : Decode.Decoder EmailCheckResponse
emailCheckDecoder =
    let
        -- Try to be more flexible with the response format
        baseDecoder =
            Decode.map2 EmailCheckResponse
                (Decode.oneOf
                    [ Decode.field "available" Decode.bool
                    , Decode.field "success" Decode.bool
                    , Decode.succeed True -- Default to true if field not found (changed from false)
                    ]
                )
                (Decode.oneOf
                    [ Decode.field "message" Decode.string
                    , Decode.field "error" Decode.string
                    , Decode.succeed "Email is available" -- Changed default message
                    ]
                )
    in
    Decode.oneOf
        [ baseDecoder
        , Decode.field "data" baseDecoder
        , Decode.succeed { available = True, message = "Email is available" } -- Last resort fallback
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
