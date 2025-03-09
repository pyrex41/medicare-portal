module Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Ports exposing (clearSessionCookie)



-- MODEL


type alias Model =
    { key : Nav.Key
    , sessionState : SessionState
    }


type SessionState
    = Unknown
    | Checking
    | Valid
    | Invalid


type Msg
    = CheckSession
    | GotSessionResponse (Result Http.Error SessionResponse)
    | NavigateTo String
    | NavigateSignup
    | NoOp


type alias SessionResponse =
    { valid : Bool }


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key
      , sessionState = Unknown
      }
    , checkSession
    )


checkSession : Cmd Msg
checkSession =
    Http.get
        { url = "/api/auth/session"
        , expect = Http.expectJson GotSessionResponse sessionResponseDecoder
        }


sessionResponseDecoder : Decode.Decoder SessionResponse
sessionResponseDecoder =
    Decode.map SessionResponse
        (Decode.field "valid" Decode.bool)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckSession ->
            ( { model | sessionState = Checking }
            , checkSession
            )

        GotSessionResponse result ->
            case result of
                Ok response ->
                    ( { model
                        | sessionState =
                            if response.valid then
                                Valid

                            else
                                Invalid
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | sessionState = Invalid }
                    , Cmd.none
                    )

        NavigateTo path ->
            case ( path, model.sessionState ) of
                ( "/login", Valid ) ->
                    -- If trying to go to login but already logged in, go to dashboard
                    ( model, Nav.pushUrl model.key "/dashboard" )

                _ ->
                    -- Otherwise go to requested path
                    ( model, Nav.pushUrl model.key path )

        NavigateSignup ->
            -- Direct navigation to onboarding plan
            ( model
            , Nav.pushUrl model.key "/onboarding/plan"
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Medicare Max - Automate Client Retention"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ nav [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6" ]
                [ div [ class "flex justify-between items-center" ]
                    [ div [ class "flex items-center" ]
                        [ img
                            [ src "/images/medicare-max-logo.png"
                            , class "h-8 w-auto"
                            , alt "Medicare Max logo"
                            ]
                            []
                        ]
                    , div [ class "flex items-center space-x-4" ]
                        [ button
                            [ onClick (NavigateTo "/login")
                            , class "text-gray-600 hover:text-gray-900 px-4 py-2 text-sm font-medium"
                            ]
                            [ text "Log in" ]
                        , button
                            [ onClick NavigateSignup
                            , class "bg-[#0A0F4F] text-white px-4 py-2 rounded-lg text-sm font-medium hover:bg-[#1a1f5f] transition-colors duration-200"
                            ]
                            [ text "Sign up" ]
                        ]
                    ]
                ]
            , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 pt-24 pb-32" ]
                [ div [ class "lg:grid lg:grid-cols-12 lg:gap-8" ]
                    [ div [ class "sm:text-center md:max-w-2xl md:mx-auto lg:col-span-6 lg:text-left" ]
                        [ div [ class "inline-flex items-center space-x-2 bg-[#F4F3FF] rounded-full px-4 py-1.5 mb-8" ]
                            [ span [ class "text-sm font-medium text-[#0A0F4F]" ] [ text "What's new?" ]
                            , span [ class "text-sm text-gray-600" ] [ text "Instantly issue virtual cards" ]
                            ]
                        , h1
                            [ class "text-5xl tracking-tight font-bold text-gray-900 sm:text-6xl md:text-7xl" ]
                            [ text "Automate client retention." ]
                        , p
                            [ class "mt-6 text-lg text-gray-600 leading-relaxed" ]
                            [ text "Automatically engage clients in key moments of their medigap journey, and sit back as they enroll" ]
                        , div [ class "mt-10" ]
                            [ button
                                [ onClick NavigateSignup
                                , class "inline-flex items-center px-6 py-3 rounded-lg text-base font-medium text-white bg-[#0A0F4F] hover:bg-[#1a1f5f] transition-colors duration-200"
                                ]
                                [ text "Sign up" ]
                            ]
                        ]
                    , div [ class "mt-16 relative sm:max-w-lg sm:mx-auto lg:mt-0 lg:max-w-none lg:mx-0 lg:col-span-6 lg:flex lg:items-center" ]
                        [ div [ class "relative mx-auto w-full rounded-2xl shadow-xl overflow-hidden bg-gray-100" ]
                            [ img
                                [ src "/images/dashboard.png"
                                , class "w-full"
                                , alt "Dashboard preview"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-16" ]
                [ p [ class "text-center text-gray-600 mb-12" ]
                    [ text "Join 200+ companies retaining their clients" ]
                , div [ class "grid grid-cols-2 gap-8 md:grid-cols-5 items-center justify-items-center" ]
                    [ img [ src "/company-logos/bitshift.svg", class "h-8", alt "Bitshift logo" ] []
                    , img [ src "/company-logos/lightbox.svg", class "h-8", alt "Lightbox logo" ] []
                    , img [ src "/company-logos/greenway.svg", class "h-8", alt "Greenway logo" ] []
                    , img [ src "/company-logos/skyline.svg", class "h-8", alt "Skyline logo" ] []
                    , img [ src "/company-logos/clockwork.svg", class "h-8", alt "Clockwork logo" ] []
                    ]
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
