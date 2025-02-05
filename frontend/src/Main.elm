module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Dashboard
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Login
import TempLanding
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf)


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : Page
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
    changeRouteTo url
        { key = key
        , url = url
        , page = NotFoundPage
        }


changeRouteTo : Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    case Parser.parse routeParser url of
        Nothing ->
            -- Default to login page if route not found
            ( model, Nav.pushUrl model.key "/login" )

        Just route ->
            case route of
                "/login" ->
                    let
                        ( pageModel, pageCmd ) =
                            Login.init ()
                    in
                    ( { model | page = LoginPage pageModel }
                    , Cmd.map LoginMsg pageCmd
                    )

                "/dashboard" ->
                    let
                        ( pageModel, pageCmd ) =
                            Dashboard.init ()
                    in
                    ( { model | page = DashboardPage pageModel }
                    , Cmd.map DashboardMsg pageCmd
                    )

                "/temp-landing" ->
                    let
                        ( pageModel, pageCmd ) =
                            TempLanding.init ()
                    in
                    ( { model | page = TempLandingPage pageModel }
                    , Cmd.map TempLandingMsg pageCmd
                    )

                _ ->
                    ( model, Nav.pushUrl model.key "/login" )


routeParser : Parser (String -> a) a
routeParser =
    oneOf
        [ Parser.map "/login" (Parser.s "login")
        , Parser.map "/dashboard" (Parser.s "dashboard")
        , Parser.map "/temp-landing" (Parser.s "temp-landing")
        , Parser.map "/login" Parser.top
        ]


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
