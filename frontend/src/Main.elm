module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Dashboard
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder)
import Login
import TempLanding
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf)


type alias VerificationResponse =
    { success : Bool
    , session : String
    , redirectUrl : String
    }


type alias SessionResponse =
    { valid : Bool
    , session : String
    , email : String
    }


verificationDecoder : Decoder VerificationResponse
verificationDecoder =
    Decode.map3 VerificationResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "session" Decode.string)
        (Decode.field "redirectUrl" Decode.string)


sessionDecoder : Decoder SessionResponse
sessionDecoder =
    Decode.map3 SessionResponse
        (Decode.field "valid" Decode.bool)
        (Decode.field "session" Decode.string)
        (Decode.field "email" Decode.string)


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : Page
    , session : Maybe String
    }


type Page
    = NotFoundPage
    | LoginPage Login.Model
    | DashboardPage Dashboard.Model
    | TempLandingPage TempLanding.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | LoginMsg Login.Msg
    | DashboardMsg Dashboard.Msg
    | TempLandingMsg TempLanding.Msg
    | GotVerification (Result Http.Error VerificationResponse)
    | GotSession (Result Http.Error SessionResponse)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { key = key
            , url = url
            , page = NotFoundPage
            , session = Nothing
            }
    in
    ( model
    , Cmd.batch
        [ verifySession -- Check session when app starts
        , changeRouteTo url model |> Tuple.second
        ]
    )


verifySession : Cmd Msg
verifySession =
    Http.get
        { url = "/api/auth/session"
        , expect = Http.expectJson GotSession sessionDecoder
        }


type Route
    = Login
    | Dashboard
    | TempLanding
    | Verify String String -- for orgSlug and token


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Login (Parser.s "login")
        , Parser.map Dashboard (Parser.s "dashboard")
        , Parser.map TempLanding (Parser.s "templanding")
        , Parser.map Verify
            (Parser.s "auth"
                </> Parser.s "verify"
                </> Parser.string
                </> Parser.string
            )
        , Parser.map Login Parser.top
        ]


changeRouteTo : Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    case Parser.parse routeParser url of
        Nothing ->
            ( model, Nav.pushUrl model.key "/login" )

        Just route ->
            case route of
                Login ->
                    let
                        ( pageModel, pageCmd ) =
                            Login.init ()
                    in
                    ( { model | page = LoginPage pageModel }
                    , Cmd.map LoginMsg pageCmd
                    )

                Dashboard ->
                    let
                        ( pageModel, pageCmd ) =
                            Dashboard.init ()
                    in
                    ( { model | page = DashboardPage pageModel }
                    , Cmd.map DashboardMsg pageCmd
                    )

                TempLanding ->
                    case model.session of
                        Just _ ->
                            let
                                ( pageModel, pageCmd ) =
                                    TempLanding.init ()
                            in
                            ( { model | page = TempLandingPage pageModel }
                            , Cmd.map TempLandingMsg pageCmd
                            )

                        Nothing ->
                            ( model, Nav.pushUrl model.key "/login" )

                Verify orgSlug token ->
                    ( model
                    , Http.get
                        { url = "/api/auth/verify/" ++ orgSlug ++ "/" ++ token
                        , expect = Http.expectJson GotVerification verificationDecoder
                        }
                    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            changeRouteTo url { model | url = url }

        LoginMsg subMsg ->
            case model.page of
                LoginPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Login.update subMsg pageModel
                    in
                    ( { model | page = LoginPage newPageModel }
                    , Cmd.map LoginMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        DashboardMsg subMsg ->
            case model.page of
                DashboardPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            Dashboard.update subMsg pageModel
                    in
                    ( { model | page = DashboardPage newPageModel }
                    , Cmd.map DashboardMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        TempLandingMsg subMsg ->
            case model.page of
                TempLandingPage pageModel ->
                    let
                        ( newPageModel, newCmd ) =
                            TempLanding.update subMsg pageModel
                    in
                    ( { model | page = TempLandingPage newPageModel }
                    , Cmd.map TempLandingMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        GotVerification result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | session = Just response.session }
                        , Nav.pushUrl model.key response.redirectUrl
                        )

                    else
                        ( model, Nav.pushUrl model.key "/login" )

                Err _ ->
                    ( model, Nav.pushUrl model.key "/login" )

        GotSession result ->
            case result of
                Ok response ->
                    ( { model | session = Just response.session }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Nav.pushUrl model.key "/login" )


view : Model -> Document Msg
view model =
    case model.page of
        NotFoundPage ->
            { title = "Not Found"
            , body =
                [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center" ]
                    [ div [ class "text-center" ]
                        [ h1 [ class "text-4xl font-bold text-gray-900" ]
                            [ text "Page not found" ]
                        ]
                    ]
                ]
            }

        LoginPage pageModel ->
            { title = "Login"
            , body = List.map (Html.map LoginMsg) (Login.view pageModel).body
            }

        DashboardPage pageModel ->
            { title = "Dashboard"
            , body = [ Html.map DashboardMsg (Dashboard.view pageModel) ]
            }

        TempLandingPage pageModel ->
            { title = "Welcome"
            , body = List.map (Html.map TempLandingMsg) (TempLanding.view pageModel).body
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        LoginPage pageModel ->
            Sub.map LoginMsg (Login.subscriptions pageModel)

        DashboardPage pageModel ->
            Sub.map DashboardMsg (Dashboard.subscriptions pageModel)

        TempLandingPage pageModel ->
            Sub.map TempLandingMsg (TempLanding.subscriptions pageModel)

        NotFoundPage ->
            Sub.none
