module Dashboard exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Components.LimitBanner as LimitBanner exposing (LimitWarning(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Time



-- Port for sending data to Chartist.js


type alias Model =
    { limitBanner : LimitBanner.Model
    , showTutorialModal : Bool
    , quotesSent : Int
    , quotesViewed : Int
    , followUpsRequested : Int
    , healthQuestionsCompleted : Int
    , statsLoading : Bool
    , statsError : Maybe String
    , chartData : List ChartDataFromAPI
    }



-- This is the data structure from the API


type alias ChartDataFromAPI =
    { x : Float
    , sends : Float
    , views : Float
    , followUps : Float
    }



-- This is the data structure for Chartist.js


type alias ChartistJsData =
    { labels : List String
    , series : List (List Float)
    }


type Msg
    = NoOp
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
    , healthQuestionsCompleted : Int
    , chartData : List ChartDataFromAPI
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
        |> Pipeline.required "healthQuestionsCompleted" Decode.int
        |> Pipeline.required "chartData" (Decode.list chartDataFromAPIDecoder)


chartDataFromAPIDecoder : Decoder ChartDataFromAPI
chartDataFromAPIDecoder =
    Decode.succeed ChartDataFromAPI
        |> Pipeline.required "x" Decode.float
        |> Pipeline.required "sends" Decode.float
        |> Pipeline.required "views" Decode.float
        |> Pipeline.required "followUps" Decode.float


dashboardStatsResponseDecoder : Decoder DashboardStatsResponse
dashboardStatsResponseDecoder =
    Decode.succeed DashboardStatsResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "stats" dashboardStatsDecoder



-- Function to encode ChartistJsData to JSON


encodeChartistJsData : ChartistJsData -> Encode.Value
encodeChartistJsData data =
    Encode.object
        [ ( "labels", Encode.list Encode.string data.labels )
        , ( "series", Encode.list (Encode.list Encode.float) data.series )
        ]



-- Helper to format month float to short string (e.g., 0.0 -> "Jan")


formatMonthLabel : Float -> String
formatMonthLabel x =
    let
        monthIndex =
            round x
    in
    case monthIndex of
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( limitBannerModel, limitBannerCmd ) =
            LimitBanner.init
    in
    ( { limitBanner = limitBannerModel
      , showTutorialModal = Maybe.withDefault False flags.isPostPayment
      , quotesSent = 0
      , quotesViewed = 0
      , followUpsRequested = 0
      , healthQuestionsCompleted = 0
      , statsLoading = True
      , statsError = Nothing
      , chartData = []
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
                            , healthQuestionsCompleted = response.stats.healthQuestionsCompleted
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


viewChartist : String -> Html msg
viewChartist chartistDataJson =
    node "chartist-bar"
        [ attribute "data" chartistDataJson
        , attribute "style" "height: 100%; width: 100%; display: block;"
        ]
        []


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
                        , div [ class "h-64" ]
                            [ if model.statsLoading then
                                -- Chart spinner
                                div [ class "h-full w-full flex items-center justify-center" ]
                                    [ div [ class "animate-spin rounded-full h-12 w-12 border-t-2 border-l-2 border-purple-500" ] []
                                    , div [ class "ml-3 text-gray-500" ] [ text "Loading chart data..." ]
                                    ]

                              else if model.statsError /= Nothing then
                                -- Error message
                                div [ class "h-full w-full flex items-center justify-center" ]
                                    [ div [ class "text-red-500" ] [ text "Error loading chart data. Please try again." ] ]

                              else if List.isEmpty model.chartData then
                                -- No data message
                                div [ class "h-full w-full flex items-center justify-center" ]
                                    [ div [ class "text-gray-500" ] [ text "No data available to display." ] ]

                              else
                                let
                                    labels =
                                        List.map (.x >> formatMonthLabel) model.chartData

                                    seriesSents =
                                        List.map .sends model.chartData

                                    seriesViews =
                                        List.map .views model.chartData

                                    seriesFollowUps =
                                        List.map .followUps model.chartData

                                    chartistData =
                                        { labels = labels
                                        , series = [ seriesSents, seriesViews, seriesFollowUps ]
                                        }

                                    chartistJson =
                                        Encode.encode 0 (encodeChartistJsData chartistData)
                                in
                                viewChartist chartistJson
                            ]
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

                "Health Questions" ->
                    "Contacts who completed health questions"

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

                "Health Questions" ->
                    "Contacts who completed health questions"

                _ ->
                    ""
    in
    div [ class "bg-white rounded-lg shadow-xl p-4 sm:p-6" ]
        [ div [ class "text-gray-600 text-xs sm:text-sm" ] [ text title ]
        , div [ class ("text-2xl sm:text-4xl font-bold mt-1 sm:mt-2 flex items-center justify-center " ++ colorClass) ]
            [ div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-l-2 border-purple-500" ] [] ]
        , div [ class "text-gray-500 text-xs mt-1" ] [ text subtitle ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
