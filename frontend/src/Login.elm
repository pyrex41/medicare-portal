module Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model =
    { email : String
    , status : Status
    , isLoggedIn : Bool
    , key : Nav.Key
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


init : Nav.Key -> Bool -> ( Model, Cmd Msg )
init key isLoggedIn =
    ( { email = ""
      , status = Idle
      , isLoggedIn = isLoggedIn
      , key = key
      }
    , Cmd.none
    )


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
                        ( model
                        , Http.post
                            { url = "/api/auth/login"
                            , body = Http.jsonBody (encodeLoginBody model.email)
                            , expect = Http.expectJson GotLoginResponse loginResponseDecoder
                            }
                        )

                Err _ ->
                    -- On error checking session, proceed with normal login flow
                    ( model
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


viewLoginForm : Model -> { title : String, body : List (Html Msg) }
viewLoginForm model =
    { title = "Login"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center py-12 sm:px-6 lg:px-8" ]
            [ div [ class "sm:mx-auto sm:w-full sm:max-w-md" ]
                [ h2 [ class "mt-6 text-center text-3xl font-extrabold text-gray-900" ]
                    [ text "Sign in to your account" ]
                ]
            , div [ class "mt-8 sm:mx-auto sm:w-full sm:max-w-md" ]
                [ div [ class "bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10" ]
                    [ Html.form [ onSubmit SubmitForm ]
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
                    , viewStatus model.status
                    ]
                ]
            ]
        ]
    }


viewStatus : Status -> Html msg
viewStatus status =
    case status of
        LinkSent ->
            div [ class "mt-4 p-4 bg-green-50 rounded-md" ]
                [ p [ class "text-sm text-green-700 text-center space-y-2" ]
                    [ p [] [ text "You will receive an email if you are a registered agent." ]
                    , p [] [ text "Check your email for the login link!" ]
                    ]
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
subscriptions _ =
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
