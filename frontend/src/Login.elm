module Login exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Encode as Encode


type alias Model =
    { email : String
    , status : Status
    , isLoggedIn : Bool
    }


type Status
    = Idle
    | Submitting
    | Success
    | Failed String


type Msg
    = EmailChanged String
    | SubmitForm
    | GotLoginResponse (Result Http.Error ())
    | LogOut
    | NoOp


init : Bool -> ( Model, Cmd Msg )
init isLoggedIn =
    ( { email = ""
      , status = Idle
      , isLoggedIn = isLoggedIn
      }
    , if not isLoggedIn then
        -- When not logged in, try to get the most recent session
        Http.get
            { url = "/api/dev/session/login"
            , expect = Http.expectWhatever GotLoginResponse
            }

      else
        Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = email }
            , Cmd.none
            )

        SubmitForm ->
            ( { model | status = Submitting }
            , Http.post
                { url = "/api/auth/login"
                , body = Http.jsonBody (encodeLoginBody model.email)
                , expect = Http.expectWhatever GotLoginResponse
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
                Ok _ ->
                    ( { model | status = Success }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | status = Failed "Login failed. Please try again." }
                    , Cmd.none
                    )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    if model.isLoggedIn then
        viewLoggedIn

    else
        viewLoginForm model


viewLoggedIn : { title : String, body : List (Html Msg) }
viewLoggedIn =
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
                        , div [ class "mt-6 pt-6 border-t border-gray-200" ]
                            [ p [ class "text-sm text-gray-500 text-center mb-4" ]
                                [ text "Development Login Links" ]
                            , div [ class "space-y-2" ]
                                [ a
                                    [ href "/api/dev/session/settings"
                                    , class "block text-center text-sm text-indigo-600 hover:text-indigo-500"
                                    ]
                                    [ text "Login to Settings" ]
                                , a
                                    [ href "/api/dev/session/add-agent"
                                    , class "block text-center text-sm text-indigo-600 hover:text-indigo-500"
                                    ]
                                    [ text "Login to Add Agent" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


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
                    [ Html.form [ onSubmit SubmitForm, class "space-y-6" ]
                        [ div []
                            [ label [ for "email", class "block text-sm font-medium text-gray-700" ]
                                [ text "Email address" ]
                            , div [ class "mt-1" ]
                                [ input
                                    [ type_ "email"
                                    , id "email"
                                    , value model.email
                                    , onInput EmailChanged
                                    , class "appearance-none block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                                    ]
                                    []
                                ]
                            ]
                        , div []
                            [ button
                                [ type_ "submit"
                                , disabled (model.status == Submitting)
                                , class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                                ]
                                [ text
                                    (case model.status of
                                        Submitting ->
                                            "Sending..."

                                        _ ->
                                            "Send Login Link"
                                    )
                                ]
                            ]
                        ]
                    , viewStatus model.status
                    , div [ class "mt-6 text-center" ]
                        [ p [ class "text-sm text-gray-600" ]
                            [ text "Don't have an account? "
                            , a [ href "/signup", class "font-medium text-indigo-600 hover:text-indigo-500" ]
                                [ text "Sign up" ]
                            ]
                        ]
                    , if model.isLoggedIn then
                        text ""

                      else
                        div [ class "mt-6 pt-6 border-t border-gray-200" ]
                            [ p [ class "text-sm text-gray-500 text-center mb-4" ]
                                [ text "Development Login Links" ]
                            , div [ class "space-y-2" ]
                                [ a
                                    [ href "/api/dev/session/settings"
                                    , class "block text-center text-sm text-indigo-600 hover:text-indigo-500"
                                    ]
                                    [ text "Login to Settings" ]
                                , a
                                    [ href "/api/dev/session/add-agent"
                                    , class "block text-center text-sm text-indigo-600 hover:text-indigo-500"
                                    ]
                                    [ text "Login to Add Agent" ]
                                ]
                            ]
                    ]
                ]
            ]
        ]
    }


viewStatus : Status -> Html msg
viewStatus status =
    case status of
        Success ->
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

        _ ->
            text ""


encodeLoginBody : String -> Encode.Value
encodeLoginBody email =
    Encode.object
        [ ( "email", Encode.string email )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
