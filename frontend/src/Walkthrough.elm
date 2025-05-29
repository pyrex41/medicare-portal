module Walkthrough exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Svg
import Svg.Attributes


-- MODEL


type alias Model =
    { isOpen : Bool
    , isCompleting : Bool
    , error : Maybe String
    }


init : Bool -> ( Model, Cmd Msg )
init isOpen =
    ( { isOpen = isOpen
      , isCompleting = False
      , error = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = CloseModalClicked
    | MarkWalkthroughCompleted
    | CompleteWalkthroughResponse (Result Http.Error ())
    | WalkthroughCompletedAndMarked
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseModalClicked ->
            ( { model | isOpen = False }, Cmd.none )

        MarkWalkthroughCompleted ->
            ( { model | isCompleting = True, error = Nothing }
            , markWalkthroughCompleted
            )

        CompleteWalkthroughResponse (Ok _) ->
            ( { model | isCompleting = False, isOpen = False }
            , Cmd.none
            )

        CompleteWalkthroughResponse (Err _) ->
            ( { model | isCompleting = False, error = Just "Failed to save walkthrough completion. Please try again." }
            , Cmd.none
            )

        WalkthroughCompletedAndMarked ->
            -- This message is handled by parent components
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


markWalkthroughCompleted : Cmd Msg
markWalkthroughCompleted =
    Http.post
        { url = "/api/profile/complete-walkthrough"
        , body = Http.emptyBody
        , expect = Http.expectWhatever CompleteWalkthroughResponse
        }



-- VIEW


view : Model -> Html Msg
view model =
    if model.isOpen then
        div []
            [ -- Modal backdrop
              div
                [ class "fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity z-40"
                , onClick CloseModalClicked
                ]
                []

            -- Modal content
            , div [ class "fixed inset-0 z-50 overflow-y-auto" ]
                [ div [ class "flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0" ]
                    [ div
                        [ class "relative transform overflow-hidden rounded-lg bg-white text-left shadow-xl transition-all sm:my-8 sm:w-full sm:max-w-4xl"
                        , Html.Events.stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
                        ]
                        [ -- Modal header
                          div [ class "px-6 py-4 border-b border-gray-200 bg-[#03045E] text-white" ]
                            [ div [ class "flex items-center justify-between" ]
                                [ h2 [ class "text-2xl font-semibold" ]
                                    [ text "Welcome to Medicare Max" ]
                                , button
                                    [ type_ "button"
                                    , class "text-white hover:text-gray-200"
                                    , onClick CloseModalClicked
                                    ]
                                    [ Svg.svg
                                        [ Svg.Attributes.class "h-6 w-6"
                                        , Svg.Attributes.fill "none"
                                        , Svg.Attributes.viewBox "0 0 24 24"
                                        , Svg.Attributes.stroke "currentColor"
                                        ]
                                        [ Svg.path
                                            [ Svg.Attributes.strokeLinecap "round"
                                            , Svg.Attributes.strokeLinejoin "round"
                                            , Svg.Attributes.strokeWidth "2"
                                            , Svg.Attributes.d "M6 18L18 6M6 6l12 12"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]

                        -- Modal body
                        , div [ class "p-6 max-h-[calc(100vh-200px)] overflow-y-auto" ]
                            [ -- Error message
                              case model.error of
                                Just errorMsg ->
                                    div [ class "mb-4 bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded" ]
                                        [ text errorMsg ]

                                Nothing ->
                                    text ""

                            -- Content
                            , div [ class "mb-8 text-center" ]
                                [ h3 [ class "text-xl font-medium text-gray-800 mb-2" ]
                                    [ text "Get Started with Our Platform" ]
                                , p [ class "text-gray-600 max-w-2xl mx-auto" ]
                                    [ text "Watch this video walkthrough to learn how to use Medicare Max and make the most of its features." ]
                                ]

                            -- Video placeholder
                            , div [ class "mx-auto max-w-3xl bg-gray-100 rounded-lg p-6 aspect-video flex items-center justify-center mb-8" ]
                                [ div [ class "text-center" ]
                                    [ div [ class "text-6xl text-gray-400 mb-4" ]
                                        [ text "▶️" ]
                                    , p [ class "text-gray-500 font-medium" ]
                                        [ text "Video Walkthrough Coming Soon" ]
                                    , p [ class "text-gray-400 text-sm mt-2" ]
                                        [ text "This video will demonstrate how to use the Medicare Max platform" ]
                                    ]
                                ]

                            -- Quick Start Guide
                            , div [ class "max-w-3xl mx-auto" ]
                                [ h4 [ class "text-lg font-medium text-gray-800 mb-4" ]
                                    [ text "Quick Start Guide" ]
                                , div [ class "bg-blue-50 rounded-lg p-5 border border-blue-100" ]
                                    [ ul [ class "space-y-4" ]
                                        [ li [ class "flex" ]
                                            [ div [ class "shrink-0 flex items-center justify-center w-8 h-8 bg-blue-600 text-white rounded-full mr-3" ]
                                                [ text "1" ]
                                            , div []
                                                [ h5 [ class "font-medium text-blue-800" ]
                                                    [ text "Set Up Your Profile" ]
                                                , p [ class "text-blue-700 mt-1 text-sm" ]
                                                    [ text "Complete your profile information to personalize your experience." ]
                                                ]
                                            ]
                                        , li [ class "flex" ]
                                            [ div [ class "shrink-0 flex items-center justify-center w-8 h-8 bg-blue-600 text-white rounded-full mr-3" ]
                                                [ text "2" ]
                                            , div []
                                                [ h5 [ class "font-medium text-blue-800" ]
                                                    [ text "Add Clients" ]
                                                , p [ class "text-blue-700 mt-1 text-sm" ]
                                                    [ text "Start adding your clients to manage their information and policies." ]
                                                ]
                                            ]
                                        , li [ class "flex" ]
                                            [ div [ class "shrink-0 flex items-center justify-center w-8 h-8 bg-blue-600 text-white rounded-full mr-3" ]
                                                [ text "3" ]
                                            , div []
                                                [ h5 [ class "font-medium text-blue-800" ]
                                                    [ text "Generate Quotes" ]
                                                , p [ class "text-blue-700 mt-1 text-sm" ]
                                                    [ text "Use our quoting tools to find the best options for your clients." ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]

                        -- Modal footer
                        , div [ class "bg-gray-50 px-6 py-4 sm:flex sm:flex-row-reverse" ]
                            [ button
                                [ type_ "button"
                                , class "w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-blue-600 text-base font-medium text-white hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 sm:ml-3 sm:w-auto sm:text-sm disabled:opacity-50 disabled:cursor-not-allowed"
                                , onClick MarkWalkthroughCompleted
                                , disabled model.isCompleting
                                ]
                                [ text
                                    (if model.isCompleting then
                                        "Saving..."

                                     else
                                        "Got it, Don't Show Again"
                                    )
                                ]
                            , button
                                [ type_ "button"
                                , class "mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-white text-base font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:w-auto sm:text-sm"
                                , onClick CloseModalClicked
                                ]
                                [ text "Close" ]
                            ]
                        ]
                    ]
                ]
            ]

    else
        text ""





-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none