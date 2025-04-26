module Dashboard exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Components.LimitBanner as LimitBanner exposing (LimitWarning(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


type alias Model =
    { hovering : Maybe Point
    , limitBanner : LimitBanner.Model
    , showTutorialModal : Bool
    }


type alias Point =
    { x : Float
    , y : Float
    }


type alias ChartData =
    { x : Float
    , sends : Float
    , views : Float
    , followUps : Float
    }


type Msg
    = OnHover (Maybe Point)
    | NoOp
    | LimitBannerMsg LimitBanner.Msg
    | CloseTutorialModal
    | OpenTutorialModal


type alias Flags =
    { isPostPayment : Maybe Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( limitBannerModel, limitBannerCmd ) =
            LimitBanner.init
    in
    ( { hovering = Nothing
      , limitBanner = limitBannerModel
      , showTutorialModal = Maybe.withDefault False flags.isPostPayment
      }
    , Cmd.map LimitBannerMsg limitBannerCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnHover point ->
            ( { model | hovering = point }
            , Cmd.none
            )

        LimitBannerMsg limitBannerMsg ->
            let
                ( limitBanner, cmd ) =
                    LimitBanner.update limitBannerMsg model.limitBanner
            in
            ( { model | limitBanner = limitBanner }
            , Cmd.map LimitBannerMsg cmd
            )

        CloseTutorialModal ->
            ( { model | showTutorialModal = False }
            , Cmd.none
            )

        OpenTutorialModal ->
            ( { model | showTutorialModal = True }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Dashboard"
    , body =
        [ div [ class "p-4 sm:p-6 max-w-7xl mx-auto" ]
            [ LimitBanner.view model.limitBanner
                |> Html.map LimitBannerMsg
            , if model.showTutorialModal then
                viewTutorialModal

              else
                text ""
            , div [ class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4 sm:gap-6" ]
                [ -- Stats cards
                  viewStatsCard "Quotes Sent" "0" "text-purple-600"
                , viewStatsCard "Quotes Viewed" "0" "text-purple-600"
                , viewStatsCard "Follow Ups Requested" "0" "text-purple-600"
                ]
            , div [ class "mt-6 sm:mt-8 grid grid-cols-1 lg:grid-cols-4 gap-4 sm:gap-6" ]
                [ div [ class "lg:col-span-3" ]
                    [ -- Chart section
                      div [ class "bg-white rounded-lg shadow p-4 sm:p-6" ]
                        [ h3 [ class "text-lg font-semibold mb-2 sm:mb-4" ] [ text "Quote Results" ]
                        , div [ class "h-64 overflow-x-auto overflow-y-hidden" ]
                            [ viewChart model ]
                        , div [ class "flex flex-col sm:flex-row justify-center mt-8 sm:mt-16 space-y-2 sm:space-y-0 sm:space-x-8 text-sm text-gray-600 border-t border-gray-200 pt-4 sm:pt-8" ]
                            [ div [ class "flex items-center" ]
                                [ div [ class "w-3 h-3 rounded-full bg-[#DCE2E5] mr-1.5 sm:mr-2" ] []
                                , text "Quotes Sent"
                                ]
                            , div [ class "flex items-center" ]
                                [ div [ class "w-3 h-3 rounded-full bg-[#53389E] mr-1.5 sm:mr-2" ] []
                                , text "Quotes Viewed"
                                ]
                            , div [ class "flex items-center" ]
                                [ div [ class "w-3 h-3 rounded-full bg-[#03045E] mr-1.5 sm:mr-2" ] []
                                , text "Follow-up Requests"
                                ]
                            ]
                        ]
                    ]
                , div [ class "lg:col-span-1" ]
                    [ -- Next Renewals section
                      div [ class "bg-white rounded-lg shadow p-4 sm:p-6" ]
                        [ h3 [ class "text-lg font-semibold mb-2 sm:mb-4" ] [ text "Next Renewals" ]
                        , div [ class "space-y-4" ]
                            [-- We'll add renewal items here later
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewTutorialModal : Html Msg
viewTutorialModal =
    div [ class "fixed inset-0 z-50 bg-gray-600 bg-opacity-50 flex items-center justify-center p-4" ]
        [ div [ class "bg-white p-4 sm:p-6 rounded-lg shadow-lg max-w-2xl w-full" ]
            [ div [ class "flex justify-between items-center mb-4" ]
                [ h2 [ class "text-lg sm:text-xl font-semibold text-[#03045E]" ] [ text "Welcome to MedicareMax!" ]
                , button
                    [ class "text-gray-400 hover:text-gray-600 text-xl p-1", onClick CloseTutorialModal ]
                    [ text "×" ]
                ]
            , div [ class "mb-4 sm:mb-6" ]
                [ iframe
                    [ src "https://www.youtube.com/embed/dQw4w9WgXcQ" -- Replace with actual tutorial video
                    , class "w-full aspect-video max-h-[50vh] sm:h-96"
                    , attribute "allowfullscreen" ""
                    , attribute "frameborder" "0"
                    ]
                    []
                ]
            , p [ class "mb-4 text-gray-600 text-sm sm:text-base" ]
                [ text "This quick setup tutorial will help you get started with MedicareMax and show you how to make the most of its features." ]
            , div [ class "flex justify-end" ]
                [ button
                    [ class "px-4 py-2 bg-[#03045E] text-white rounded-md hover:bg-opacity-90 w-full sm:w-auto"
                    , onClick CloseTutorialModal
                    ]
                    [ text "Close" ]
                ]
            ]
        ]


viewStatsCard : String -> String -> String -> Html Msg
viewStatsCard title value colorClass =
    div [ class "bg-white rounded-lg shadow-xl p-4 sm:p-6" ]
        [ div [ class "text-gray-600 text-xs sm:text-sm" ] [ text title ]
        , div [ class "text-2xl sm:text-4xl font-bold mt-1 sm:mt-2 text-[#03045E]" ] [ text value ]
        ]


viewChart : Model -> Html Msg
viewChart model =
    C.chart
        [ CA.height 300
        , CA.width 800 -- Fixed width, with overflow-x-auto on container
        , CA.margin { top = 10, bottom = 45, left = 30, right = 10 }

        -- CA.responsive is not available in this version
        ]
        [ C.xLabels
            [ CA.withGrid
            , CA.amount 12
            , CA.fontSize 12
            , CA.moveDown 35
            , CA.format
                (\x ->
                    case round x of
                        0 ->
                            "Jan"

                        1 ->
                            "Feb"

                        2 ->
                            "Mar"

                        3 ->
                            "Apr"

                        4 ->
                            "May"

                        5 ->
                            "Jun"

                        6 ->
                            "Jul"

                        7 ->
                            "Aug"

                        8 ->
                            "Sep"

                        9 ->
                            "Oct"

                        10 ->
                            "Nov"

                        11 ->
                            "Dec"

                        _ ->
                            ""
                )
            ]
        , C.yLabels [ CA.withGrid ]
        , C.bars []
            [ C.stacked
                [ C.bar .sends [ CA.color "#DCE2E5" ]
                , C.bar .views [ CA.color "#53389E" ]
                , C.bar .followUps [ CA.color "#03045E" ]
                ]
            ]
            chartData
        ]


chartData : List ChartData
chartData =
    [ { x = 0, sends = 10, views = 8, followUps = 5 }
    , { x = 1, sends = 15, views = 12, followUps = 8 }
    , { x = 2, sends = 8, views = 6, followUps = 4 }
    , { x = 3, sends = 12, views = 10, followUps = 7 }
    , { x = 4, sends = 20, views = 15, followUps = 10 }
    , { x = 5, sends = 18, views = 14, followUps = 9 }
    , { x = 6, sends = 25, views = 20, followUps = 15 }
    , { x = 7, sends = 22, views = 18, followUps = 12 }
    , { x = 8, sends = 28, views = 22, followUps = 16 }
    , { x = 9, sends = 30, views = 25, followUps = 18 }
    , { x = 10, sends = 35, views = 28, followUps = 20 }
    , { x = 11, sends = 40, views = 32, followUps = 25 }
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
