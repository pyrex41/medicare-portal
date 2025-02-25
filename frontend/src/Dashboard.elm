module Dashboard exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


type alias Model =
    { hovering : Maybe Point }


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hovering = Nothing }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnHover point ->
            ( { model | hovering = point }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Dashboard"
    , body =
        [ div [ class "p-6 max-w-7xl mx-auto" ]
            [ div [ class "grid grid-cols-3 gap-6" ]
                [ -- Stats cards
                  viewStatsCard "Quotes Sent" "912" "text-purple-600"
                , viewStatsCard "Quotes Viewed" "912" "text-purple-600"
                , viewStatsCard "Follow Ups Requested" "912" "text-purple-600"
                ]
            , div [ class "mt-8 grid grid-cols-4 gap-6" ]
                [ div [ class "col-span-3" ]
                    [ -- Chart section
                      div [ class "bg-white rounded-lg shadow p-6" ]
                        [ h3 [ class "text-lg font-semibold mb-4" ] [ text "Quote Results" ]
                        , div [ class "h-64" ]
                            [ viewChart model ]
                        , div [ class "flex justify-center mt-4 space-x-6 text-sm text-gray-600" ]
                            [ div [ class "flex items-center" ]
                                [ div [ class "w-3 h-3 rounded-full bg-purple-600 mr-2" ] []
                                , text "Sends"
                                ]
                            , div [ class "flex items-center" ]
                                [ div [ class "w-3 h-3 rounded-full bg-purple-400 mr-2" ] []
                                , text "Viewed"
                                ]
                            , div [ class "flex items-center" ]
                                [ div [ class "w-3 h-3 rounded-full bg-purple-200 mr-2" ] []
                                , text "Follow Ups"
                                ]
                            ]
                        ]
                    ]
                , div [ class "col-span-1" ]
                    [ -- Next Renewals section
                      div [ class "bg-white rounded-lg shadow p-6" ]
                        [ h3 [ class "text-lg font-semibold mb-4" ] [ text "Next Renewals" ]
                        , div [ class "space-y-4" ]
                            [-- We'll add renewal items here later
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewStatsCard : String -> String -> String -> Html Msg
viewStatsCard title value colorClass =
    div [ class "bg-white rounded-lg shadow p-6" ]
        [ div [ class "text-gray-600 text-sm" ] [ text title ]
        , div [ class ("text-4xl font-bold mt-2 " ++ colorClass) ] [ text value ]
        ]


viewChart : Model -> Html Msg
viewChart model =
    C.chart
        [ CA.height 300
        , CA.width 800
        , CA.margin { top = 10, bottom = 30, left = 30, right = 10 }
        ]
        [ C.xLabels
            [ CA.withGrid
            , CA.amount 12
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
                [ C.bar .sends [ CA.color CA.purple ]
                , C.bar .views [ CA.color CA.pink ]
                , C.bar .followUps [ CA.color CA.purple, CA.opacity 0.4 ]
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
