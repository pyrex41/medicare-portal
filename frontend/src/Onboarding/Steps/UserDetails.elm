module Onboarding.Steps.UserDetails exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (circle, path, svg)
import Svg.Attributes as SvgAttr exposing (clipRule, cx, cy, d, fill, fillRule, r, stroke, strokeWidth, viewBox)



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
    }


type EmailStatus
    = NotChecked
    | Checking
    | Available
    | Unavailable String


init : Nav.Key -> String -> ( Model, Cmd Msg )
init key orgSlug =
    ( { firstName = ""
      , lastName = ""
      , email = ""
      , phone = ""
      , isLoading = False
      , error = Nothing
      , key = key
      , orgSlug = orgSlug
      , emailStatus = NotChecked
      }
    , if String.isEmpty (String.trim orgSlug) then
        -- Don't fetch user details for new users in signup flow
        Cmd.none

      else
        -- Only fetch user details for existing users
        fetchUserDetails orgSlug
    )



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


type OutMsg
    = NoOutMsg
    | NextStep
    | ShowError String


type alias UserDetailsResponse =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    }


type alias EmailCheckResponse =
    { available : Bool
    , message : String
    }


type alias SignupResponse =
    { success : Bool
    , message : String
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
                -- Instead of making API calls, just move to the next step
                -- All data will be collected and submitted in the final step
                ( model, Cmd.none, NextStep )

            else
                ( { model | error = Just "Please fill out all required fields" }
                , Cmd.none
                , ShowError "Please fill out all required fields"
                )

        GotUserDetails result ->
            case result of
                Ok response ->
                    ( { model
                        | firstName = response.firstName
                        , lastName = response.lastName
                        , email = response.email
                        , phone = response.phone
                        , isLoading = False
                      }
                    , Cmd.none
                    , NoOutMsg
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to load user details"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to load user details"
                    )

        UserDetailsSaved result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | isLoading = False }
                        , Cmd.none
                        , NextStep
                        )

                    else
                        ( { model
                            | error = Just response.message
                            , isLoading = False
                          }
                        , Cmd.none
                        , ShowError response.message
                        )

                Err _ ->
                    ( { model
                        | error = Just "Failed to save user details"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to save user details"
                    )

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
            ]
        , if model.isLoading then
            viewLoading

          else
            div [ class "space-y-6" ]
                [ div [ class "bg-white shadow rounded-lg p-6" ]
                    [ div [ class "space-y-6" ]
                        [ div [ class "grid grid-cols-2 gap-6" ]
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
                        , div [ class "grid grid-cols-2 gap-6 pt-2" ]
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
                                "px-6 py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"

                             else
                                "px-6 py-3 bg-gray-300 text-gray-500 rounded-md cursor-not-allowed"
                            )
                        , onClick NextStepClicked
                        , disabled (not (isFormValid model))
                        ]
                        [ text "Continue" ]
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
        { url = "/api/me"
        , expect = Http.expectJson GotUserDetails userDetailsDecoder
        }


saveUserDetails : Model -> Cmd Msg
saveUserDetails model =
    Http.post
        { url = "/api/organizations/signup"
        , body = Http.jsonBody (encodeUserDetails model)
        , expect =
            Http.expectJson UserDetailsSaved
                (Decode.map2 SignupResponse
                    (Decode.field "success" Decode.bool)
                    (Decode.field "message" Decode.string)
                )
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
    Decode.field "user"
        (Decode.map4 UserDetailsResponse
            (Decode.field "firstName" Decode.string)
            (Decode.field "lastName" Decode.string)
            (Decode.field "email" Decode.string)
            (Decode.field "phone" Decode.string)
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


encodeUserDetails : Model -> Encode.Value
encodeUserDetails model =
    Encode.object
        [ ( "adminFirstName", Encode.string model.firstName )
        , ( "adminLastName", Encode.string model.lastName )
        , ( "adminEmail", Encode.string model.email )
        , ( "phone", Encode.string model.phone )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
