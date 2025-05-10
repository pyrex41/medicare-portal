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
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Time


type alias Model =
    { hovering : Maybe Point
    , limitBanner : LimitBanner.Model
    , showTutorialModal : Bool
    , quotesSent : Int
    , quotesViewed : Int
    , followUpsRequested : Int
    , statsLoading : Bool
    , statsError : Maybe String
    , chartData : List ChartData
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
    | FetchDashboardStats
    | GotDashboardStats (Result Http.Error DashboardStatsResponse)


type alias Flags =
    { isPostPayment : Maybe Bool
    }


type alias DashboardStats =
    { quotesSent : Int
    , quotesViewed : Int
    , followUpsRequested : Int
    , chartData : List ChartData
    }


type alias DashboardStatsResponse =
    { success : Bool
    , stats : DashboardStats
    }


dashboardStatsDecoder : Decoder DashboardStats
dashboardStatsDecoder =
    Decode.succeed DashboardStats
        |> Pipeline.required "quotesSent" Decode.int
        |> Pipeline.required "quotesViewed" Decode.int
        |> Pipeline.required "followUpsRequested" Decode.int
        |> Pipeline.required "chartData" (Decode.list chartDataDecoder)


chartDataDecoder : Decoder ChartData
chartDataDecoder =
    Decode.succeed ChartData
        |> Pipeline.required "x" Decode.float
        |> Pipeline.required "sends" Decode.float
        |> Pipeline.required "views" Decode.float
        |> Pipeline.required "followUps" Decode.float


dashboardStatsResponseDecoder : Decoder DashboardStatsResponse
dashboardStatsResponseDecoder =
    Decode.succeed DashboardStatsResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "stats" dashboardStatsDecoder


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( limitBannerModel, limitBannerCmd ) =
            LimitBanner.init
    in
    ( { hovering = Nothing
      , limitBanner = limitBannerModel
      , showTutorialModal = Maybe.withDefault False flags.isPostPayment
      , quotesSent = 0
      , quotesViewed = 0
      , followUpsRequested = 0
      , statsLoading = True
      , statsError = Nothing
      , chartData = chartData
      }
    , Cmd.batch
        [ Cmd.map LimitBannerMsg limitBannerCmd
        , fetchDashboardStats
        ]
    )


fetchDashboardStats : Cmd Msg
fetchDashboardStats =
    Http.get
        { url = "/api/dashboard/stats"
        , expect = Http.expectJson GotDashboardStats dashboardStatsResponseDecoder
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "Bad status: " ++ String.fromInt code

        Http.BadBody message ->
            "Bad body: " ++ message


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

        FetchDashboardStats ->
            ( { model | statsLoading = True, statsError = Nothing }
            , fetchDashboardStats
            )

        GotDashboardStats result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model
                            | statsLoading = False
                            , quotesSent = response.stats.quotesSent
                            , quotesViewed = response.stats.quotesViewed
                            , followUpsRequested = response.stats.followUpsRequested
                            , chartData = response.stats.chartData
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | statsLoading = False, statsError = Just "Failed to load dashboard data." }
                        , Cmd.none
                        )

                Err httpError ->
                    ( { model | statsLoading = False, statsError = Just (httpErrorToString httpError) }
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
                  if model.statsLoading then
                    viewStatsCardWithSpinner "Quotes Sent" "text-purple-600"
                  else if model.statsError /= Nothing then
                    viewStatsCard "Quotes Sent" "Error" "text-red-600"
                  else 
                    viewStatsCard "Quotes Sent" (String.fromInt model.quotesSent) "text-purple-600"
                , if model.statsLoading then
                    viewStatsCardWithSpinner "Quotes Viewed" "text-blue-600"
                  else if model.statsError /= Nothing then
                    viewStatsCard "Quotes Viewed" "Error" "text-red-600"
                  else 
                    viewStatsCard "Quotes Viewed" (String.fromInt model.quotesViewed) "text-blue-600"
                , if model.statsLoading then
                    viewStatsCardWithSpinner "Upcoming Renewals" "text-green-600"
                  else if model.statsError /= Nothing then
                    viewStatsCard "Upcoming Renewals" "Error" "text-red-600"
                  else 
                    viewStatsCard "Upcoming Renewals" (String.fromInt model.followUpsRequested) "text-green-600"
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
                                , text "Upcoming Renewals"
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
    let
        subtitle =
            case title of
                "Quotes Sent" ->
                    "Total emails sent with quotes"
                
                "Quotes Viewed" ->
                    "Unique contacts who've viewed quotes"
                
                "Upcoming Renewals" ->
                    "Upcoming scheduled policy renewals"
                
                _ ->
                    ""
    in
    div [ class "bg-white rounded-lg shadow-xl p-4 sm:p-6" ]
        [ div [ class "text-gray-600 text-xs sm:text-sm" ] [ text title ]
        , div [ class "text-2xl sm:text-4xl font-bold mt-1 sm:mt-2 text-[#03045E]" ] [ text value ]
        , div [ class "text-gray-500 text-xs mt-1" ] [ text subtitle ]
        ]


viewStatsCardWithSpinner : String -> String -> Html Msg
viewStatsCardWithSpinner title colorClass =
    let
        subtitle =
            case title of
                "Quotes Sent" ->
                    "Total emails sent with quotes"
                
                "Quotes Viewed" ->
                    "Unique contacts who've viewed quotes"
                
                "Upcoming Renewals" ->
                    "Upcoming scheduled policy renewals"
                
                _ ->
                    ""
    in
    div [ class "bg-white rounded-lg shadow-xl p-4 sm:p-6" ]
        [ div [ class "text-gray-600 text-xs sm:text-sm" ] [ text title ]
        , div [ class ("text-2xl sm:text-4xl font-bold mt-1 sm:mt-2 flex items-center justify-center " ++ colorClass) ]
            [ div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-l-2 border-purple-500" ] [] ]
        , div [ class "text-gray-500 text-xs mt-1" ] [ text subtitle ]
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
            model.chartData
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
