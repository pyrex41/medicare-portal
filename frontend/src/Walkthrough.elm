module Walkthrough exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode



-- MODEL


type alias Model =
    { key : Nav.Key
    , currentUser : Maybe CurrentUser
    }


type alias CurrentUser =
    { id : String
    , name : String
    , email : String
    , isAdmin : Bool
    , isAgent : Bool
    }


init : Nav.Key -> Maybe CurrentUser -> ( Model, Cmd Msg )
init key currentUser =
    ( { key = key
      , currentUser = currentUser
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Welcome to Medicare Max"
    , body =
        [ div [ class "min-h-screen bg-gray-50" ]
            [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-10" ]
                [ div [ class "bg-white shadow-md rounded-lg overflow-hidden" ]
                    [ div [ class "px-6 py-4 border-b border-gray-200 bg-[#03045E] text-white" ]
                        [ h1 [ class "text-2xl font-semibold" ]
                            [ text "Welcome to Medicare Max" ]
                        ]
                    , div [ class "p-6" ]
                        [ div [ class "mb-8 text-center" ]
                            [ h2 [ class "text-xl font-medium text-gray-800 mb-2" ]
                                [ text "Get Started with Our Platform" ]
                            , p [ class "text-gray-600 max-w-2xl mx-auto" ]
                                [ text "Watch this video walkthrough to learn how to use Medicare Max and make the most of its features." ]
                            ]
                        , div [ class "mx-auto max-w-4xl bg-gray-100 rounded-lg p-6 aspect-video flex items-center justify-center" ]
                            [ div [ class "text-center" ]
                                [ div [ class "text-6xl text-gray-400 mb-4" ]
                                    [ text "▶️" ]
                                , p [ class "text-gray-500 font-medium" ]
                                    [ text "Video Walkthrough Coming Soon" ]
                                , p [ class "text-gray-400 text-sm mt-2" ]
                                    [ text "This video will demonstrate how to use the Medicare Max platform" ]
                                ]
                            ]
                        , div [ class "mt-10 max-w-3xl mx-auto" ]
                            [ h3 [ class "text-lg font-medium text-gray-800 mb-4" ]
                                [ text "Quick Start Guide" ]
                            , div [ class "bg-blue-50 rounded-lg p-5 border border-blue-100" ]
                                [ ul [ class "space-y-4" ]
                                    [ li [ class "flex" ]
                                        [ div [ class "shrink-0 flex items-center justify-center w-8 h-8 bg-blue-600 text-white rounded-full mr-3" ]
                                            [ text "1" ]
                                        , div []
                                            [ h4 [ class "font-medium text-blue-800" ]
                                                [ text "Set Up Your Profile" ]
                                            , p [ class "text-blue-700 mt-1 text-sm" ]
                                                [ text "Complete your profile information to personalize your experience." ]
                                            ]
                                        ]
                                    , li [ class "flex" ]
                                        [ div [ class "shrink-0 flex items-center justify-center w-8 h-8 bg-blue-600 text-white rounded-full mr-3" ]
                                            [ text "2" ]
                                        , div []
                                            [ h4 [ class "font-medium text-blue-800" ]
                                                [ text "Add Clients" ]
                                            , p [ class "text-blue-700 mt-1 text-sm" ]
                                                [ text "Start adding your clients to manage their information and policies." ]
                                            ]
                                        ]
                                    , li [ class "flex" ]
                                        [ div [ class "shrink-0 flex items-center justify-center w-8 h-8 bg-blue-600 text-white rounded-full mr-3" ]
                                            [ text "3" ]
                                        , div []
                                            [ h4 [ class "font-medium text-blue-800" ]
                                                [ text "Generate Quotes" ]
                                            , p [ class "text-blue-700 mt-1 text-sm" ]
                                                [ text "Use our quoting tools to find the best options for your clients." ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
