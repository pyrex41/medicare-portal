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


-- MODEL

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
    , selectedTimeFilter : TimeFilter
    , selectedChartView : ChartView
    , upcomingRenewals : List Renewal
    }



-- This is the data structure from the API
type alias ChartDataFromAPI =
    { x : Float -- timestamp or month index
    , sends : Float
    , views : Float
    , followUps : Float
    , healthCompleted : Float -- New field
    }



-- This is the data structure for Chartist.js
type alias ChartistJsData =
    { labels : List String
    , series : List (List Float)
    }

type TimeFilter
    = Last7Days
    | Last30Days
    | Last90Days
    | YearToDate
    | CustomRange Time.Posix Time.Posix

type ChartView
    = TrendView
    | FunnelView
    | ComparisonView

type alias Renewal =
    { id : String
    , name : String
    , email : String
    , phone : String
    , date : String
    , policyType : String
    }


-- MESSAGES

type Msg
    = NoOp
    | LimitBannerMsg LimitBanner.Msg
    | CloseTutorialModal
    | OpenTutorialModal
    | FetchDashboardStats
    | GotDashboardStats (Result Http.Error DashboardStatsResponse)
    | SelectTimeFilter TimeFilter
    | SelectChartView ChartView
    | FetchRenewals
    | GotRenewals (Result Http.Error RenewalResponse)
    | SendReminderToContact String
    | CallContact String


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

type alias RenewalResponse =
    { success : Bool
    , renewals : List Renewal
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
        |> Pipeline.optional "healthCompleted" Decode.float 0.0 -- Default to 0 if not present


dashboardStatsResponseDecoder : Decoder DashboardStatsResponse
dashboardStatsResponseDecoder =
    Decode.succeed DashboardStatsResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "stats" dashboardStatsDecoder

renewalDecoder : Decoder Renewal
renewalDecoder =
    Decode.succeed Renewal
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "phone" Decode.string
        |> Pipeline.required "date" Decode.string
        |> Pipeline.required "policyType" Decode.string

renewalResponseDecoder : Decoder RenewalResponse
renewalResponseDecoder =
    Decode.succeed RenewalResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "renewals" (Decode.list renewalDecoder)


-- HELPERS

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

-- Calculate percentages and rates
calculateViewRate : Model -> Float
calculateViewRate model =
    if model.quotesSent == 0 then
        0
    else
        toFloat model.quotesViewed / toFloat model.quotesSent * 100

calculateFollowUpRate : Model -> Float
calculateFollowUpRate model =
    if model.quotesViewed == 0 then
        0
    else
        toFloat model.followUpsRequested / toFloat model.quotesViewed * 100

calculateCompletionRate : Model -> Float
calculateCompletionRate model =
    if model.quotesSent == 0 then
        0
    else
        toFloat model.healthQuestionsCompleted / toFloat model.quotesSent * 100

-- Helper to get API endpoint based on time filter
timeFilterToApiParam : TimeFilter -> String
timeFilterToApiParam filter =
    case filter of
        Last7Days ->
            "7days"
        Last30Days ->
            "30days"
        Last90Days ->
            "90days"
        YearToDate ->
            "ytd"
        CustomRange start end ->
            "custom"  -- Would need to append date parameters


-- INIT

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
      , selectedTimeFilter = Last30Days
      , selectedChartView = TrendView
      , upcomingRenewals = []
      }
    , Cmd.batch
        [ Cmd.map LimitBannerMsg limitBannerCmd
        , fetchDashboardStats Last30Days
        , fetchRenewals
        ]
    )


-- HTTP

fetchDashboardStats : TimeFilter -> Cmd Msg
fetchDashboardStats timeFilter =
    let
        timeParam = timeFilterToApiParam timeFilter
        url = "/api/dashboard/stats?period=" ++ timeParam
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotDashboardStats dashboardStatsResponseDecoder
        }

fetchRenewals : Cmd Msg
fetchRenewals =
    Http.get
        { url = "/api/dashboard/renewals"
        , expect = Http.expectJson GotRenewals renewalResponseDecoder
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


-- UPDATE

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
            , fetchDashboardStats model.selectedTimeFilter
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

        SelectTimeFilter timeFilter ->
            ( { model | selectedTimeFilter = timeFilter, statsLoading = True }
            , fetchDashboardStats timeFilter
            )

        SelectChartView chartView ->
            ( { model | selectedChartView = chartView }
            , Cmd.none
            )

        FetchRenewals ->
            ( model
            , fetchRenewals
            )

        GotRenewals result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | upcomingRenewals = response.renewals }
                        , Cmd.none
                        )
                    else
                        ( model
                        , Cmd.none
                        )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        SendReminderToContact contactId ->
            -- This would call an API to send a reminder
            ( model
            , Cmd.none
            )

        CallContact contactId ->
            -- This would integrate with a calling system or log the call
            ( model
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


viewChartist : String -> ChartView -> Html msg
viewChartist chartistDataJson chartView =
    let
        chartType =
            case chartView of
                TrendView ->
                    "chartist-line"
                FunnelView ->
                    "chartist-funnel"
                ComparisonView ->
                    "chartist-bar"
    in
    node chartType
        [ attribute "data" chartistDataJson
        , attribute "style" "height: 100%; width: 100%; display: block;"
        ]
        []


-- VIEW

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
            , viewDashboardHeader model
            , viewStatsCards model
            , viewMainContent model
            ]
        ]
    }

viewDashboardHeader : Model -> Html Msg
viewDashboardHeader model =
    div [ class "flex flex-col sm:flex-row justify-between items-center mb-6" ]
        [ h1 [ class "text-2xl font-bold text-gray-800 mb-4 sm:mb-0" ] 
             [ text "MedicareMax Dashboard" ]
        , div [ class "flex space-x-2" ]
            [ select
                [ class "bg-white border border-gray-300 rounded-md px-3 py-2 text-sm"
                , onInput (\value -> 
                    case value of
                        "7days" -> SelectTimeFilter Last7Days
                        "30days" -> SelectTimeFilter Last30Days
                        "90days" -> SelectTimeFilter Last90Days
                        "ytd" -> SelectTimeFilter YearToDate
                        _ -> NoOp
                  )
                ]
                [ option [ value "7days" ] [ text "Last 7 Days" ]
                , option [ value "30days", selected (model.selectedTimeFilter == Last30Days) ] [ text "Last 30 Days" ]
                , option [ value "90days" ] [ text "Last 90 Days" ]
                , option [ value "ytd" ] [ text "Year to Date" ]
                ]
            , button 
                [ class "bg-[#03045e] text-white px-4 py-2 rounded-md text-sm hover:bg-opacity-90"
                , onClick FetchDashboardStats
                ] 
                [ text "Refresh" ]
            ]
        ]

viewStatsCards : Model -> Html Msg
viewStatsCards model =
    let
        viewRate = calculateViewRate model |> round
        followUpRate = calculateFollowUpRate model |> round
        completionRate = calculateCompletionRate model |> round
    in
    div [ class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 sm:gap-6 mb-6" ]
        [ -- Quotes Sent card
          if model.statsLoading then
            viewStatsCardWithSpinner "Quotes Sent" "text-[#03045e]"
          else if model.statsError /= Nothing then
            viewStatsCard "Quotes Sent" "Error" "text-red-600" "Failed to load data"
          else
            viewStatsCard "Quotes Sent" (String.fromInt model.quotesSent) "text-[#03045e]" "Total emails sent with quotes"
        
        -- View Rate card
        , if model.statsLoading then
            viewStatsCardWithSpinner "View Rate" "text-[#0077b6]"
          else if model.statsError /= Nothing then
            viewStatsCard "View Rate" "Error" "text-red-600" "Failed to load data"
          else
            viewStatsCard "View Rate" (String.fromInt viewRate ++ "%") "text-[#0077b6]" (String.fromInt model.quotesViewed ++ " quotes viewed")
        
        -- Upcoming Emails card
        , if model.statsLoading then
            viewStatsCardWithSpinner "Upcoming Emails" "text-[#00b4d8]"
          else if model.statsError /= Nothing then
            viewStatsCard "Upcoming Emails" "Error" "text-red-600" "Failed to load data"
          else
            viewStatsCard "Upcoming Emails" (String.fromInt model.followUpsRequested) "text-[#00b4d8]" "Upcoming scheduled emails"
        
        -- Completion Rate card
        , if model.statsLoading then
            viewStatsCardWithSpinner "Completion Rate" "text-[#48cae4]"
          else if model.statsError /= Nothing then
            viewStatsCard "Completion Rate" "Error" "text-red-600" "Failed to load data"
          else
            viewStatsCard "Completion Rate" (String.fromInt completionRate ++ "%") "text-[#48cae4]" (String.fromInt model.healthQuestionsCompleted ++ " health questions completed")
        ]

viewMainContent : Model -> Html Msg
viewMainContent model =
    div [ class "grid grid-cols-1 lg:grid-cols-3 gap-6" ]
        [ -- Chart section
          div [ class "lg:col-span-2 bg-white rounded-lg shadow-xl p-4" ]
            [ div [ class "flex justify-between items-center mb-4" ]
                [ h2 [ class "text-lg font-semibold text-gray-800" ] [ text "Performance Metrics" ]
                , div [ class "flex space-x-2" ]
                    [ button 
                        [ class ("px-3 py-1 text-sm rounded-md " ++ 
                            if model.selectedChartView == TrendView then 
                                "bg-[#03045e] text-white" 
                            else 
                                "bg-gray-100")
                        , onClick (SelectChartView TrendView)
                        ]
                        [ text "Trend" ]
                    , button 
                        [ class ("px-3 py-1 text-sm rounded-md " ++ 
                            if model.selectedChartView == FunnelView then 
                                "bg-[#03045e] text-white" 
                            else 
                                "bg-gray-100")
                        , onClick (SelectChartView FunnelView)
                        ]
                        [ text "Funnel" ]
                    , button 
                        [ class ("px-3 py-1 text-sm rounded-md " ++ 
                            if model.selectedChartView == ComparisonView then 
                                "bg-[#03045e] text-white" 
                            else 
                                "bg-gray-100")
                        , onClick (SelectChartView ComparisonView)
                        ]
                        [ text "Comparison" ]
                    ]
                ]
            , div [ class "h-64" ]
                [ if model.statsLoading then
                    -- Chart spinner
                    div [ class "h-full w-full flex items-center justify-center" ]
                        [ div [ class "animate-spin rounded-full h-12 w-12 border-t-2 border-l-2 border-[#03045e]" ] []
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
                        -- Different data formatting based on chart type
                        chartistData =
                            case model.selectedChartView of
                                FunnelView ->
                                    -- For funnel view, we need a simpler data structure
                                    -- We'll use the most recent data points
                                    let
                                        latestData =
                                            List.reverse model.chartData |> List.head
                                    in
                                    case latestData of
                                        Just latest ->
                                            -- Pass the actual values from the model here, not from chartData
                                            { labels = [ "Quotes Sent", "Quotes Viewed", "Upcoming Emails", "Health Completed" ]
                                            , series = [ [ toFloat model.quotesSent, toFloat model.quotesViewed, toFloat model.followUpsRequested, toFloat model.healthQuestionsCompleted ] ]
                                            }
                                        Nothing ->
                                            -- Even if there's no chart data, we can still use the model values
                                            { labels = [ "Quotes Sent", "Quotes Viewed", "Upcoming Emails", "Health Completed" ]
                                            , series = [ [ toFloat model.quotesSent, toFloat model.quotesViewed, toFloat model.followUpsRequested, toFloat model.healthQuestionsCompleted ] ]
                                            }

                                _ ->
                                    -- For trend and comparison views
                                    let
                                        labels =
                                            List.map (.x >> formatMonthLabel) model.chartData

                                        seriesSents =
                                            List.map .sends model.chartData

                                        seriesViews =
                                            List.map .views model.chartData

                                        seriesFollowUps =
                                            List.map .followUps model.chartData

                                        seriesHealthCompleted =
                                            List.map .healthCompleted model.chartData
                                    in
                                    { labels = labels
                                    , series = [ seriesSents, seriesViews, seriesFollowUps, seriesHealthCompleted ]
                                    }

                        chartistJson =
                            Encode.encode 0 (encodeChartistJsData chartistData)
                    in
                    viewChartist chartistJson model.selectedChartView
                ]
            , div [ class "flex flex-col sm:flex-row justify-center mt-4 space-y-2 sm:space-y-0 sm:space-x-8 text-sm text-gray-600 border-t border-gray-200 pt-4" ]
                [ div [ class "flex items-center" ]
                    [ div [ class "w-3 h-3 rounded-full bg-[#03045e] mr-1.5 sm:mr-2" ] []
                    , text "Quotes Sent"
                    ]
                , div [ class "flex items-center" ]
                    [ div [ class "w-3 h-3 rounded-full bg-[#0077b6] mr-1.5 sm:mr-2" ] []
                    , text "Quotes Viewed"
                    ]
                , div [ class "flex items-center" ]
                    [ div [ class "w-3 h-3 rounded-full bg-[#00b4d8] mr-1.5 sm:mr-2" ] []
                    , text "Upcoming Emails"
                    ]
                , div [ class "flex items-center" ]
                    [ div [ class "w-3 h-3 rounded-full bg-[#48cae4] mr-1.5 sm:mr-2" ] []
                    , text "Health Completed"
                    ]
                ]
            ]
        , -- Renewals section
          div [ class "lg:col-span-1 bg-white rounded-lg shadow-xl p-4" ]
            [ div [ class "flex justify-between items-center mb-4" ]
                [ h2 [ class "text-lg font-semibold text-gray-800" ] [ text "Upcoming Renewals" ]
                , span [ class "bg-[#03045e] text-white text-xs px-2 py-1 rounded-full" ] 
                    [ text (String.fromInt (List.length model.upcomingRenewals) ++ " Total") ]
                ]
            , if model.statsLoading then
                div [ class "h-full w-full flex items-center justify-center py-8" ]
                    [ div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-l-2 border-[#03045e]" ] [] ]
              else if List.isEmpty model.upcomingRenewals then
                div [ class "text-center py-8 text-gray-500" ]
                    [ text "No upcoming renewals at this time." ]
              else
                div [ class "space-y-4 max-h-[400px] overflow-y-auto" ]
                    (List.map viewRenewalItem model.upcomingRenewals)
            , div [ class "mt-4 text-center" ]
                [ button [ class "text-[#03045e] text-sm font-medium" ] 
                    [ text "View All Renewals" ]
                ]
            ]
        ]

viewRenewalItem : Renewal -> Html Msg
viewRenewalItem renewal =
    div [ class "border-b pb-3" ]
        [ div [ class "flex justify-between items-start" ]
            [ div []
                [ div [ class "font-medium" ] [ text renewal.name ]
                , div [ class "text-sm text-gray-500" ] [ text renewal.policyType ]
                ]
            , div [ class "text-sm font-medium text-[#03045e]" ] [ text renewal.date ]
            ]
        , div [ class "mt-2 flex items-center text-xs text-gray-500" ]
            [ -- Email icon
              span [ class "mr-1" ] [ text "📧" ]
            , text renewal.email
            ]
        , div [ class "mt-1 flex items-center text-xs text-gray-500" ]
            [ -- Phone icon
              span [ class "mr-1" ] [ text "📞" ]
            , text renewal.phone
            ]
        , div [ class "mt-2 flex space-x-2" ]
            [ button 
                [ class "bg-[#0077b6] text-white text-xs px-3 py-1 rounded"
                , onClick (SendReminderToContact renewal.id)
                ] 
                [ text "Send Reminder" ]
            , button 
                [ class "bg-gray-100 text-gray-800 text-xs px-3 py-1 rounded"
                , onClick (CallContact renewal.id)
                ] 
                [ text "Call" ]
            ]
        ]


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


viewStatsCard : String -> String -> String -> String -> Html Msg
viewStatsCard title value colorClass subtitle =
    div [ class "bg-white rounded-lg shadow-xl p-4 sm:p-6" ]
        [ div [ class "text-gray-600 text-xs sm:text-sm" ] [ text title ]
        , div [ class ("text-2xl sm:text-4xl font-bold mt-1 sm:mt-2 " ++ colorClass) ] [ text value ]
        , div [ class "text-gray-500 text-xs mt-1" ] [ text subtitle ]
        ]


viewStatsCardWithSpinner : String -> String -> Html Msg
viewStatsCardWithSpinner title colorClass =
    div [ class "bg-white rounded-lg shadow-xl p-4 sm:p-6" ]
        [ div [ class "text-gray-600 text-xs sm:text-sm" ] [ text title ]
        , div [ class ("text-2xl sm:text-4xl font-bold mt-1 sm:mt-2 flex items-center justify-center " ++ colorClass) ]
            [ div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-l-2 border-[#03045e]" ] [] ]
        , div [ class "text-gray-500 text-xs mt-1" ] [ text "Loading..." ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none