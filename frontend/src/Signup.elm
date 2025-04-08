module Signup exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Process
import Task
import Url
import Url.Builder exposing (absolute, string)



-- MODEL


type alias Model =
    { firstName : String
    , lastName : String
    , organizationName : String
    , email : String
    , formState : FormState
    , error : Maybe String
    , key : Nav.Key
    , emailStatus : EmailStatus
    , debounceCounter : Int
    }


type FormState
    = Editing
    | Submitting
    | LinkSent
    | Success
    | Error


type EmailStatus
    = NotChecked
    | Checking
    | Available
    | AlreadyRegistered String
    | InvalidFormat


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { firstName = ""
      , lastName = ""
      , organizationName = ""
      , email = ""
      , formState = Editing
      , error = Nothing
      , key = key
      , emailStatus = NotChecked
      , debounceCounter = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateOrganizationName String
    | UpdateEmail String
    | CheckEmail Int
    | DebounceCheckEmail Int
    | GotEmailCheckResponse Int (Result Http.Error EmailCheckResponse)
    | SubmitForm
    | GotSignupResponse (Result Http.Error SignupResponse)


type alias SignupResponse =
    { success : Bool
    , message : String
    }


type alias EmailCheckResponse =
    { available : Bool
    , message : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFirstName value ->
            ( { model | firstName = value }, Cmd.none )

        UpdateLastName value ->
            ( { model | lastName = value }, Cmd.none )

        UpdateOrganizationName value ->
            ( { model | organizationName = value }, Cmd.none )

        UpdateEmail value ->
            let
                newModel =
                    { model
                        | email = value
                        , emailStatus =
                            if String.isEmpty value then
                                NotChecked

                            else if not (isValidEmailFormat value) then
                                InvalidFormat

                            else
                                Checking
                        , debounceCounter = model.debounceCounter + 1
                    }

                counter =
                    newModel.debounceCounter
            in
            ( newModel
            , if String.isEmpty value || not (isValidEmailFormat value) then
                Cmd.none

              else
                debounceEmailCheck counter
            )

        DebounceCheckEmail counter ->
            if counter == model.debounceCounter && model.emailStatus == Checking then
                ( model, checkEmailAvailability counter model.email )

            else
                ( model, Cmd.none )

        CheckEmail counter ->
            ( model, checkEmailAvailability counter model.email )

        GotEmailCheckResponse counter result ->
            if counter /= model.debounceCounter then
                -- Ignore outdated responses
                ( model, Cmd.none )

            else
                case result of
                    Ok response ->
                        ( { model
                            | emailStatus =
                                if response.available then
                                    Available

                                else
                                    AlreadyRegistered response.message
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        ( { model | emailStatus = NotChecked }, Cmd.none )

        SubmitForm ->
            if isValidForm model then
                ( { model | formState = Submitting }
                , Cmd.none
                  -- signup model
                )

            else
                ( { model | error = Just "Please fill out all fields correctly" }
                , Cmd.none
                )

        GotSignupResponse result ->
            case result of
                Ok response ->
                    if response.success then
                        -- Show success message
                        ( { model | formState = LinkSent }
                        , Cmd.none
                        )

                    else
                        ( { model | formState = Error, error = Just response.message }
                        , Cmd.none
                        )

                Err _ ->
                    ( { model | formState = Error, error = Just "Signup failed. Please try again." }
                    , Cmd.none
                    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Get Started - Medicare Max"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex items-center justify-center py-12 px-4 sm:px-6 lg:px-8" ]
            [ div [ class "max-w-md w-full space-y-8" ]
                [ div [ class "text-center" ]
                    [ img [ src "/images/medicare-max-logo.png", class "mx-auto h-12 w-auto", alt "Medicare Max Logo" ] []
                    , h1 [ class "mt-6 text-3xl font-bold text-gray-900" ] [ text "Get Started" ]
                    , p [ class "mt-2 text-sm text-gray-600" ] [ text "Sign up and start using Medicare Max" ]
                    ]
                , case model.formState of
                    LinkSent ->
                        div [ class "text-center bg-green-50 p-6 rounded-lg border border-green-100" ]
                            [ p [ class "text-green-800 mb-2 font-medium" ] [ text "Welcome to Medicare Max!" ]
                            , p [ class "text-green-700 mb-4" ] [ text "We've sent you an email with a magic link to continue your account setup." ]
                            , p [ class "text-green-700" ] [ text "Please check your inbox (and spam folder) to complete your registration." ]
                            ]

                    Success ->
                        div [ class "text-center" ]
                            [ p [ class "text-green-600" ] [ text "Account created successfully!" ]
                            , div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-b-2 border-blue-500 mx-auto mt-4" ] []
                            ]

                    _ ->
                        viewForm model
                ]
            ]
        ]
    }


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ onSubmit SubmitForm, class "space-y-6" ]
        [ div [ class "grid grid-cols-1 md:grid-cols-2 gap-4" ]
            [ div []
                [ label [ for "firstName", class "block text-sm font-medium text-gray-700" ] [ text "First Name" ]
                , input
                    [ type_ "text"
                    , id "firstName"
                    , value model.firstName
                    , onInput UpdateFirstName
                    , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 sm:text-sm"
                    , placeholder "John"
                    ]
                    []
                ]
            , div []
                [ label [ for "lastName", class "block text-sm font-medium text-gray-700" ] [ text "Last Name" ]
                , input
                    [ type_ "text"
                    , id "lastName"
                    , value model.lastName
                    , onInput UpdateLastName
                    , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 sm:text-sm"
                    , placeholder "Doe"
                    ]
                    []
                ]
            ]
        , div []
            [ label [ for "organizationName", class "block text-sm font-medium text-gray-700" ] [ text "Organization Name" ]
            , input
                [ type_ "text"
                , id "organizationName"
                , value model.organizationName
                , onInput UpdateOrganizationName
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 sm:text-sm"
                , placeholder "ABC Healthcare"
                ]
                []
            ]
        , div []
            [ label [ for "email", class "block text-sm font-medium text-gray-700" ] [ text "Email" ]
            , input
                [ type_ "email"
                , id "email"
                , value model.email
                , onInput UpdateEmail
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 sm:text-sm"
                , placeholder "you@example.com"
                ]
                []
            , viewEmailStatusMessage model.emailStatus
            ]
        , case model.error of
            Just err ->
                div [ class "text-red-600 text-sm" ] [ text err ]

            Nothing ->
                text ""
        , if model.formState == Submitting || not (isValidForm model) then
            button
                [ type_ "submit"
                , class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-gray-400 cursor-not-allowed opacity-70"
                , disabled True
                ]
                [ text
                    (if model.formState == Submitting then
                        "Creating Account..."

                     else
                        "Sign Up"
                    )
                ]

          else
            let
                uri =
                    absolute [ "onboarding" ]
                        [ string "firstName" model.firstName
                        , string "lastName" model.lastName
                        , string "organizationName" model.organizationName
                        , string "email" model.email
                        ]
            in
            a
                [ href uri
                , class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                ]
                [ text "Sign Up" ]
        , p [ class "mt-2 text-center text-sm text-gray-600" ]
            [ text "Already have an account? "
            , a [ href "/login", class "font-medium text-blue-600 hover:text-blue-500" ]
                [ text "Log in" ]
            ]
        ]


viewEmailStatusMessage : EmailStatus -> Html Msg
viewEmailStatusMessage status =
    case status of
        NotChecked ->
            text ""

        Checking ->
            p [ class "mt-1 text-blue-500 text-sm" ] [ text "Checking email..." ]

        Available ->
            p [ class "mt-1 text-green-500 text-sm" ] [ text "Email is available" ]

        AlreadyRegistered message ->
            p [ class "mt-1 text-red-500 text-sm" ] [ text message ]

        InvalidFormat ->
            p [ class "mt-1 text-red-500 text-sm" ] [ text "Please enter a valid email address" ]



-- HELPERS


isValidForm : Model -> Bool
isValidForm model =
    not (String.isEmpty model.firstName)
        && not (String.isEmpty model.lastName)
        && not (String.isEmpty model.organizationName)
        && (model.emailStatus == Available)


isValidEmailFormat : String -> Bool
isValidEmailFormat email =
    String.contains "@" email && String.contains "." email


debounceEmailCheck : Int -> Cmd Msg
debounceEmailCheck counter =
    Process.sleep 500
        |> Task.perform (\_ -> DebounceCheckEmail counter)



-- API CALLS


checkEmailAvailability : Int -> String -> Cmd Msg
checkEmailAvailability counter email =
    Http.get
        { url = "/api/organizations/check-email/" ++ encodeUri email
        , expect = Http.expectJson (GotEmailCheckResponse counter) emailCheckResponseDecoder
        }



-- Simple URI encoder for email parameter


encodeUri : String -> String
encodeUri string =
    string
        |> String.replace "%" "%25"
        |> String.replace "+" "%2B"
        |> String.replace " " "%20"
        |> String.replace "/" "%2F"
        |> String.replace "?" "%3F"
        |> String.replace "#" "%23"
        |> String.replace "@" "%40"
        |> String.replace ":" "%3A"
        |> String.replace "=" "%3D"
        |> String.replace "&" "%26"


emailCheckResponseDecoder : Decoder EmailCheckResponse
emailCheckResponseDecoder =
    Decode.map2 EmailCheckResponse
        (Decode.field "available" Decode.bool)
        (Decode.field "message" Decode.string)


encodeSignupData : Model -> Encode.Value
encodeSignupData model =
    Encode.object
        [ ( "firstName", Encode.string model.firstName )
        , ( "lastName", Encode.string model.lastName )
        , ( "organizationName", Encode.string model.organizationName )
        , ( "email", Encode.string model.email )
        ]


signupDecoder : Decoder SignupResponse
signupDecoder =
    Decode.map2 SignupResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "message" Decode.string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
