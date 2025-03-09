module Login exposing (Model, Msg, init, subscriptions, update, view)

import Basics
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Time
import Url
import Url.Parser as Parser exposing ((</>), (<?>), Parser, s, string)
import Url.Parser.Query as Query


type alias Model =
    { email : String
    , status : Status
    , isLoggedIn : Bool
    , key : Nav.Key
    , resendAvailableAt : Maybe Time.Posix
    , isFromOnboarding : Bool
    , prefilledEmail : Maybe String
    }


type Status
    = Idle
    | Submitting
    | Success
    | Failed String
    | LinkSent


type alias LoginResponse =
    { success : Bool
    }


type alias SessionCheckResponse =
    { valid : Bool }


type Msg
    = EmailChanged String
    | SubmitForm
    | GotLoginResponse (Result Http.Error LoginResponse)
    | GotSessionCheck (Result Http.Error SessionCheckResponse)
    | LogOut
    | NoOp
    | CheckResendAvailable Time.Posix
    | ResendLink


init : Nav.Key -> Bool -> Url.Url -> ( Model, Cmd Msg )
init key isLoggedIn url =
    let
        queryParams =
            extractQueryParams url
    in
    ( { email = Maybe.withDefault "" queryParams.email
      , status = Idle
      , isLoggedIn = isLoggedIn
      , key = key
      , resendAvailableAt = Nothing
      , isFromOnboarding = queryParams.onboarding
      , prefilledEmail = queryParams.email
      }
    , Cmd.none
    )



-- Helper to extract query parameters


extractQueryParams : Url.Url -> { onboarding : Bool, email : Maybe String }
extractQueryParams url =
    let
        parser =
            Query.map2
                (\onboarding email -> { onboarding = onboarding == Just "completed", email = email })
                (Query.string "onboarding")
                (Query.string "email")

        route =
            { url | path = "" }
                |> Parser.parse (Parser.top <?> parser)
    in
    Maybe.withDefault { onboarding = False, email = Nothing } route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = email, status = Idle }
            , Cmd.none
            )

        SubmitForm ->
            if String.isEmpty model.email then
                ( { model | status = Failed "Please enter your email address" }
                , Cmd.none
                )

            else
                ( { model | status = Submitting }
                , Http.get
                    { url = "/api/auth/session"
                    , expect = Http.expectJson GotSessionCheck sessionCheckDecoder
                    }
                )

        GotSessionCheck result ->
            case result of
                Ok response ->
                    if response.valid then
                        -- If session is valid, redirect to dashboard
                        ( model
                        , Nav.pushUrl model.key "/dashboard"
                        )

                    else
                        -- If no valid session, proceed with login
                        let
                            -- Set resend cooldown to 60 seconds from now
                            now =
                                Time.millisToPosix 0

                            -- Placeholder, would normally use actual current time
                            cooldownTime =
                                Time.millisToPosix (Time.posixToMillis now + 60000)
                        in
                        ( { model | resendAvailableAt = Just cooldownTime }
                        , Http.post
                            { url = "/api/auth/login"
                            , body = Http.jsonBody (encodeLoginBody model.email)
                            , expect = Http.expectJson GotLoginResponse loginResponseDecoder
                            }
                        )

                Err _ ->
                    -- On error checking session, proceed with normal login flow
                    let
                        -- Set resend cooldown to 60 seconds from now
                        now =
                            Time.millisToPosix 0

                        -- Placeholder, would normally use actual current time
                        cooldownTime =
                            Time.millisToPosix (Time.posixToMillis now + 60000)
                    in
                    ( { model | resendAvailableAt = Just cooldownTime }
                    , Http.post
                        { url = "/api/auth/login"
                        , body = Http.jsonBody (encodeLoginBody model.email)
                        , expect = Http.expectJson GotLoginResponse loginResponseDecoder
                        }
                    )

        NoOp ->
            ( model, Cmd.none )

        LogOut ->
            ( { model
                | isLoggedIn = False
                , status = Idle
              }
            , Http.post
                { url = "/api/auth/logout"
                , body = Http.emptyBody
                , expect = Http.expectWhatever (\_ -> NoOp)
                }
            )

        GotLoginResponse result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | status = LinkSent }
                        , Cmd.none
                        )

                    else
                        ( { model | status = Failed "Failed to send login link. Please try again." }
                        , Cmd.none
                        )

                Err _ ->
                    ( { model | status = Failed "Failed to send login link. Please try again." }
                    , Cmd.none
                    )

        CheckResendAvailable currentTime ->
            case model.resendAvailableAt of
                Just availableAt ->
                    if Time.posixToMillis currentTime >= Time.posixToMillis availableAt then
                        ( { model | resendAvailableAt = Nothing }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ResendLink ->
            update SubmitForm model


viewLoginForm : Model -> { title : String, body : List (Html Msg) }
viewLoginForm model =
    { title = "Login"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center py-12 sm:px-6 lg:px-8" ]
            [ div [ class "sm:mx-auto sm:w-full sm:max-w-md" ]
                [ h2 [ class "mt-6 text-center text-3xl font-extrabold text-gray-900" ]
                    [ if model.isFromOnboarding then
                        text "Activate Your Account"

                      else
                        text "Welcome Back To MedicareMax"
                    ]
                ]
            , div [ class "mt-8 sm:mx-auto sm:w-full sm:max-w-md" ]
                [ div [ class "bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10" ]
                    [ if model.isFromOnboarding then
                        div [ class "mb-6 bg-blue-50 p-4 rounded-md border border-blue-200" ]
                            [ p [ class "text-blue-800" ]
                                [ text "Your account has been created successfully! Please check your email for an activation link to continue to your new account." ]
                            ]

                      else
                        text ""
                    , Html.form [ onSubmit SubmitForm ]
                        [ div []
                            [ label [ for "email", class "block text-sm font-medium text-gray-700" ]
                                [ text "Email address" ]
                            , div [ class "mt-1" ]
                                [ input
                                    [ type_ "email"
                                    , name "email"
                                    , id "email"
                                    , class "appearance-none block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                                    , placeholder "you@example.com"
                                    , value model.email
                                    , onInput EmailChanged
                                    ]
                                    []
                                ]
                            ]
                        , div [ class "mt-6" ]
                            [ button
                                [ type_ "submit"
                                , class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                                ]
                                [ text "Send login link" ]
                            ]
                        ]
                    , viewStatus model.status model
                    , div [ class "mt-4 text-center text-sm" ]
                        [ text "Not Yet a MedicareMax User? "
                        , a [ href "/signup", class "font-medium text-indigo-600 hover:text-indigo-500" ]
                            [ text "Sign Up Here" ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewStatus : Status -> Model -> Html Msg
viewStatus status model =
    case status of
        LinkSent ->
            div [ class "mt-4 p-4 bg-green-50 rounded-md" ]
                [ p [ class "text-sm text-green-700 text-center" ]
                    [ p [ class "mb-2" ] [ text "If your email is registered, you'll receive a login link soon." ]
                    , p [] [ text "Check your inbox and spam folder." ]
                    ]
                , div [ class "mt-2 text-center" ]
                    [ viewResendLink model ]
                ]

        Failed error ->
            div [ class "mt-4 p-4 bg-red-50 rounded-md" ]
                [ p [ class "text-sm text-red-700" ]
                    [ text error ]
                ]

        Submitting ->
            div [ class "mt-4 text-center text-gray-600" ]
                [ text "Sending login link..." ]

        _ ->
            text ""


viewResendLink : Model -> Html Msg
viewResendLink model =
    case model.resendAvailableAt of
        Just availableAt ->
            let
                currentTime =
                    Time.millisToPosix 0

                -- Placeholder, would normally use actual current time
                diff =
                    Basics.max 0 ((Time.posixToMillis availableAt - Time.posixToMillis currentTime) // 1000)
            in
            if diff <= 0 then
                button
                    [ onClick ResendLink
                    , class "text-sm text-blue-600 hover:text-blue-800 underline"
                    ]
                    [ text "Resend link" ]

            else
                span [ class "text-sm text-gray-600" ]
                    [ text ("Resend link in " ++ String.fromInt diff ++ " seconds") ]

        Nothing ->
            button
                [ onClick ResendLink
                , class "text-sm text-blue-600 hover:text-blue-800 underline"
                ]
                [ text "Resend link" ]


encodeLoginBody : String -> Encode.Value
encodeLoginBody email =
    Encode.object
        [ ( "email", Encode.string email )
        ]


loginResponseDecoder : Decode.Decoder LoginResponse
loginResponseDecoder =
    Decode.map LoginResponse
        (Decode.field "success" Decode.bool)


sessionCheckDecoder : Decode.Decoder SessionCheckResponse
sessionCheckDecoder =
    Decode.map SessionCheckResponse
        (Decode.field "valid" Decode.bool)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.resendAvailableAt of
        Just _ ->
            Time.every 1000 CheckResendAvailable

        Nothing ->
            Sub.none


view : Model -> { title : String, body : List (Html Msg) }
view model =
    if model.isLoggedIn then
        { title = "Already Logged In"
        , body =
            [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center py-12 sm:px-6 lg:px-8" ]
                [ div [ class "sm:mx-auto sm:w-full sm:max-w-md" ]
                    [ h2 [ class "mt-6 text-center text-3xl font-extrabold text-gray-900" ]
                        [ text "Already Logged In" ]
                    , div [ class "mt-8 sm:mx-auto sm:w-full sm:max-w-md" ]
                        [ div [ class "bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10" ]
                            [ p [ class "text-center text-gray-600 mb-6" ]
                                [ text "You are already logged in." ]
                            , button
                                [ onClick LogOut
                                , class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-red-600 hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500"
                                ]
                                [ text "Log Out" ]
                            ]
                        ]
                    ]
                ]
            ]
        }

    else
        viewLoginForm model
