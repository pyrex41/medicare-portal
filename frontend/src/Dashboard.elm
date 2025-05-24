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
import String
import Time



-- MODEL


type alias Model =
    { limitBanner : LimitBanner.Model
    , showTutorialModal : Bool

    -- Stats (consolidated from both Performance and Activity)
    , quotesSent : Int
    , manualQuotesSent : Int
    , quotesViewed : Int
    , followUpsRequested : Int
    , healthQuestionsCompleted : Int
    , statsLoading : Bool
    , statsError : Maybe String
    , chartData : List ChartDataFromAPI

    -- Send Ranking
    , sendRankingData : List SendRankingItem
    , sendRankingSortBy : RankingSortOption
    , sendRankingLoading : Bool

    -- Upcoming Emails
    , upcomingRenewals : List Renewal
    , upcomingEmailsTotal : Int
    , upcomingEmailsPage : List UpcomingEmail
    , upcomingEmailsPageNum : Int
    , upcomingEmailsPageSize : Int

    -- Common
    , selectedTimeFilter : TimeFilter
    , customStartDateInput : String
    , customEndDateInput : String
    , showCustomDateInputs : Bool -- New field to control date input visibility
    , selectedChartView : ChartView
    }



-- New types for Send Ranking


type alias SendRankingItem =
    { rank : Int
    , sendDate : String
    , quotesSent : Int
    , quotesViewed : Int
    , healthCompleted : Int
    }


type RankingSortOption
    = SortByQuotesSent
    | SortByQuotesViewed
    | SortByChronological


type alias SendRankingResponse =
    { success : Bool
    , data : List SendRankingItem
    }



-- This is the data structure from the API for Performance Metrics


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
    | Today
    | Yesterday
    | CustomRange


type ChartView
    = FunnelView



-- | TrendView  -- Commented out - may add back later


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
    | GotDashboardStats (Result Http.Error DashboardStatsResponse)
    | SelectTimeFilter TimeFilter
    | UpdateCustomStartDate String
    | UpdateCustomEndDate String
    | SelectChartView ChartView
    | FetchRenewals
    | GotRenewals (Result Http.Error RenewalResponse)
    | SendReminderToContact String
    | CallContact String
    | RefreshData
    | ChangeSendRankingSort RankingSortOption -- New message
    | GotSendRankingData (Result Http.Error SendRankingResponse) -- New message


type alias Flags =
    { isPostPayment : Maybe Bool
    }


type alias DashboardStats =
    { quotesSent : Int
    , manualQuotesSent : Int
    , quotesViewed : Int
    , followUpsRequested : Int
    , healthQuestionsCompleted : Int
    , chartData : List ChartDataFromAPI
    , upcomingEmailsTotal : Int
    , upcomingEmailsPage : List UpcomingEmail
    }


type alias DashboardStatsResponse =
    { success : Bool
    , stats : DashboardStats
    }


type alias RenewalResponse =
    { success : Bool
    , renewals : List Renewal
    }


type alias UpcomingEmail =
    { id : Int
    , contactId : Int
    , emailType : String
    , scheduledSendDate : String
    , status : String
    , firstName : String
    , lastName : String
    }


dashboardStatsDecoder : Decoder DashboardStats
dashboardStatsDecoder =
    Decode.succeed DashboardStats
        |> Pipeline.required "quotesSent" Decode.int
        |> Pipeline.required "manualQuotesSent" Decode.int
        |> Pipeline.required "quotesViewed" Decode.int
        |> Pipeline.required "followUpsRequested" Decode.int
        |> Pipeline.required "healthQuestionsCompleted" Decode.int
        |> Pipeline.required "chartData" (Decode.list chartDataFromAPIDecoder)
        |> Pipeline.required "upcomingEmailsTotal" Decode.int
        |> Pipeline.required "upcomingEmailsPage" (Decode.list upcomingEmailDecoder)


chartDataFromAPIDecoder : Decoder ChartDataFromAPI
chartDataFromAPIDecoder =
    Decode.succeed ChartDataFromAPI
        |> Pipeline.required "x" Decode.float
        |> Pipeline.required "sends" Decode.float
        |> Pipeline.required "views" Decode.float
        |> Pipeline.required "followUps" Decode.float
        |> Pipeline.optional "healthCompleted" Decode.float 0.0



-- Default to 0 if not present


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


sendRankingItemDecoder : Decoder SendRankingItem
sendRankingItemDecoder =
    Decode.succeed SendRankingItem
        |> Pipeline.required "rank" Decode.int
        |> Pipeline.required "sendDate" Decode.string
        |> Pipeline.required "quotesSent" Decode.int
        |> Pipeline.required "quotesViewed" Decode.int
        |> Pipeline.required "healthCompleted" Decode.int


sendRankingResponseDecoder : Decoder SendRankingResponse
sendRankingResponseDecoder =
    Decode.succeed SendRankingResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "data" (Decode.list sendRankingItemDecoder)



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

        Today ->
            "today"

        Yesterday ->
            "yesterday"

        CustomRange ->
            "custom"



-- Would need to append date parameters
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
      , manualQuotesSent = 0
      , quotesViewed = 0
      , followUpsRequested = 0
      , healthQuestionsCompleted = 0
      , statsLoading = True
      , statsError = Nothing
      , chartData = []
      , sendRankingData = []
      , sendRankingSortBy = SortByChronological
      , sendRankingLoading = True
      , upcomingRenewals = []
      , upcomingEmailsTotal = 0
      , upcomingEmailsPage = []
      , upcomingEmailsPageNum = 1
      , upcomingEmailsPageSize = 20
      , selectedTimeFilter = Last30Days
      , customStartDateInput = ""
      , customEndDateInput = ""
      , showCustomDateInputs = False
      , selectedChartView = FunnelView
      }
    , Cmd.batch
        [ Cmd.map LimitBannerMsg limitBannerCmd
        , fetchDashboardStats Last30Days "" ""
        , fetchSendRankingData Last30Days "" ""
        ]
    )



-- HTTP


fetchDashboardStats : TimeFilter -> String -> String -> Cmd Msg
fetchDashboardStats timeFilter startDate endDate =
    let
        timeParam =
            timeFilterToApiParam timeFilter

        baseUrl =
            "/api/dashboard/stats?period=" ++ timeParam

        url =
            if timeFilter == CustomRange && not (String.isEmpty startDate) && not (String.isEmpty endDate) then
                baseUrl ++ "&startDate=" ++ startDate ++ "&endDate=" ++ endDate

            else
                baseUrl
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


fetchSendRankingData : TimeFilter -> String -> String -> Cmd Msg
fetchSendRankingData timeFilter startDate endDate =
    let
        timeParam =
            timeFilterToApiParam timeFilter

        baseUrl =
            "/api/dashboard/send-ranking?period=" ++ timeParam

        url =
            if timeFilter == CustomRange && not (String.isEmpty startDate) && not (String.isEmpty endDate) then
                baseUrl ++ "&startDate=" ++ startDate ++ "&endDate=" ++ endDate

            else
                baseUrl
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotSendRankingData sendRankingResponseDecoder
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

        GotDashboardStats result ->
            case result of
                Ok response ->
                    if response.success then
                        let
                            totalQuotesSent =
                                response.stats.quotesSent + response.stats.manualQuotesSent
                        in
                        ( { model
                            | statsLoading = False
                            , quotesSent = totalQuotesSent
                            , manualQuotesSent = response.stats.manualQuotesSent
                            , quotesViewed = response.stats.quotesViewed
                            , followUpsRequested = response.stats.followUpsRequested
                            , healthQuestionsCompleted = response.stats.healthQuestionsCompleted
                            , chartData = response.stats.chartData
                            , upcomingEmailsTotal = response.stats.upcomingEmailsTotal
                            , upcomingEmailsPage = response.stats.upcomingEmailsPage
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | statsLoading = False, statsError = Just "Failed to load dashboard data (API error)." }, Cmd.none )

                Err httpError ->
                    ( { model | statsLoading = False, statsError = Just (httpErrorToString httpError) }, Cmd.none )

        SelectTimeFilter timeFilter ->
            if timeFilter == CustomRange then
                -- Just update the filter and show inputs, don't fetch
                ( { model
                    | selectedTimeFilter = timeFilter
                    , showCustomDateInputs = True
                    , statsLoading = False
                  }
                , Cmd.none
                )

            else
                -- For non-custom filters, hide inputs and fetch data
                ( { model
                    | selectedTimeFilter = timeFilter
                    , customStartDateInput = ""
                    , customEndDateInput = ""
                    , showCustomDateInputs = False
                    , statsLoading = True
                    , sendRankingLoading = True
                    , statsError = Nothing
                  }
                , Cmd.batch
                    [ fetchDashboardStats timeFilter "" ""
                    , fetchSendRankingData timeFilter "" ""
                    ]
                )

        UpdateCustomStartDate dateStr ->
            ( { model | customStartDateInput = dateStr, selectedTimeFilter = CustomRange }, Cmd.none )

        UpdateCustomEndDate dateStr ->
            ( { model | customEndDateInput = dateStr, selectedTimeFilter = CustomRange }, Cmd.none )

        SelectChartView chartView ->
            ( { model | selectedChartView = chartView }
            , Cmd.none
            )

        FetchRenewals ->
            ( model, fetchRenewals )

        GotRenewals result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | upcomingRenewals = response.renewals }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SendReminderToContact contactId ->
            ( model, Cmd.none )

        CallContact contactId ->
            ( model, Cmd.none )

        ChangeSendRankingSort sortOption ->
            let
                sortedData =
                    case sortOption of
                        SortByQuotesSent ->
                            List.sortBy (negate << .quotesSent) model.sendRankingData

                        SortByQuotesViewed ->
                            List.sortBy (negate << .quotesViewed) model.sendRankingData

                        SortByChronological ->
                            List.sortWith (\a b -> compare b.sendDate a.sendDate) model.sendRankingData

                -- Re-rank after sorting
                rerankedData =
                    List.indexedMap (\idx item -> { item | rank = idx + 1 }) sortedData
            in
            ( { model
                | sendRankingSortBy = sortOption
                , sendRankingData = rerankedData
              }
            , Cmd.none
            )

        GotSendRankingData result ->
            case result of
                Ok response ->
                    if response.success then
                        let
                            -- Apply current sort
                            sortedData =
                                case model.sendRankingSortBy of
                                    SortByQuotesSent ->
                                        List.sortBy (negate << .quotesSent) response.data

                                    SortByQuotesViewed ->
                                        List.sortBy (negate << .quotesViewed) response.data

                                    SortByChronological ->
                                        List.sortWith (\a b -> compare b.sendDate a.sendDate) response.data

                            rankedData =
                                List.indexedMap (\idx item -> { item | rank = idx + 1 }) sortedData
                        in
                        ( { model
                            | sendRankingLoading = False
                            , sendRankingData = rankedData
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | sendRankingLoading = False }
                        , Cmd.none
                        )

                Err _ ->
                    ( { model | sendRankingLoading = False }
                    , Cmd.none
                    )

        RefreshData ->
            let
                ( startDate, endDate ) =
                    if model.selectedTimeFilter == CustomRange then
                        ( model.customStartDateInput, model.customEndDateInput )

                    else
                        ( "", "" )

                shouldFetch =
                    if model.selectedTimeFilter == CustomRange then
                        not (String.isEmpty startDate) && not (String.isEmpty endDate)

                    else
                        True

                fetchCmd =
                    if shouldFetch then
                        Cmd.batch
                            [ fetchDashboardStats model.selectedTimeFilter startDate endDate
                            , fetchSendRankingData model.selectedTimeFilter startDate endDate
                            ]

                    else
                        Cmd.none
            in
            ( { model
                | statsLoading = shouldFetch
                , sendRankingLoading = shouldFetch
                , statsError =
                    if shouldFetch then
                        Nothing

                    else
                        model.statsError
              }
            , fetchCmd
            )

        NoOp ->
            ( model, Cmd.none )


viewChartist : String -> ChartView -> Html msg
viewChartist chartistDataJson chartView =
    let
        chartType =
            case chartView of
                FunnelView ->
                    "chartist-funnel"

        -- TrendView ->  -- Commented out - may add back later
        --     "chartist-line"
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
            , div [ class "grid grid-cols-1 lg:grid-cols-3 gap-6 mt-6" ]
                [ -- Chart and Send Ranking section (spans 2 columns)
                  div [ class "lg:col-span-2 flex flex-col" ]
                    [ viewPerformanceChart model
                    , div [ class "flex-1" ] [ viewSendRanking model ]
                    ]
                , -- Upcoming Emails section (1 column) - height matches the left column exactly
                  div [ class "lg:col-span-1 flex" ]
                    [ viewUpcomingEmails model ]
                ]
            ]
        ]
    }


viewDashboardHeader : Model -> Html Msg
viewDashboardHeader model =
    div [ class "flex flex-col sm:flex-row justify-between items-center mb-6" ]
        [ h1 [ class "text-2xl font-bold text-gray-800 mb-4 sm:mb-0" ]
            [ text "Dashboard" ]
        , div [ class "flex flex-col sm:flex-row space-y-2 sm:space-y-0 sm:space-x-2 items-center" ]
            [ select
                [ class "bg-white border border-gray-300 rounded-md px-3 py-2 text-sm"
                , onInput
                    (\val ->
                        case val of
                            "today" ->
                                SelectTimeFilter Today

                            "yesterday" ->
                                SelectTimeFilter Yesterday

                            "7days" ->
                                SelectTimeFilter Last7Days

                            "30days" ->
                                SelectTimeFilter Last30Days

                            "90days" ->
                                SelectTimeFilter Last90Days

                            "ytd" ->
                                SelectTimeFilter YearToDate

                            "custom" ->
                                SelectTimeFilter CustomRange

                            _ ->
                                NoOp
                    )
                , value (timeFilterToHtmlValue model.selectedTimeFilter)
                ]
                [ option [ value "today" ] [ text "Today" ]
                , option [ value "yesterday" ] [ text "Yesterday" ]
                , option [ value "7days" ] [ text "Last 7 Days" ]
                , option [ value "30days" ] [ text "Last 30 Days" ]
                , option [ value "90days" ] [ text "Last 90 Days" ]
                , option [ value "ytd" ] [ text "Year to Date" ]
                , option [ value "custom" ] [ text "Custom Dates" ]
                ]

            -- Only show date inputs when Custom Dates is selected
            , if model.showCustomDateInputs then
                div [ class "flex space-x-2 items-center" ]
                    [ label [ class "text-sm" ] [ text "From:" ]
                    , input
                        [ type_ "date"
                        , class "bg-white border border-gray-300 rounded-md px-2 py-1.5 text-sm"
                        , value model.customStartDateInput
                        , onInput UpdateCustomStartDate
                        ]
                        []
                    , label [ class "text-sm" ] [ text "To:" ]
                    , input
                        [ type_ "date"
                        , class "bg-white border border-gray-300 rounded-md px-2 py-1.5 text-sm"
                        , value model.customEndDateInput
                        , onInput UpdateCustomEndDate
                        ]
                        []
                    ]

              else
                text ""
            , button
                [ class "bg-[#03045e] text-white px-4 py-2 rounded-md text-sm hover:bg-opacity-90"
                , onClick RefreshData
                ]
                [ text "Refresh" ]
            ]
        ]


viewStatsCards : Model -> Html Msg
viewStatsCards model =
    let
        viewRate =
            calculateViewRate model |> round

        followUpRate =
            calculateFollowUpRate model |> round

        completionRate =
            calculateCompletionRate model |> round
    in
    div [ class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 sm:gap-6 mb-6" ]
        [ -- Quotes Sent card
          if model.statsLoading then
            viewStatsCardWithSpinner "Quotes Sent" "text-[#03045e]"

          else if model.statsError /= Nothing then
            viewStatsCard "Quotes Sent" "0" "text-red-600" "Failed to load data"

          else
            viewStatsCard "Quotes Sent" (String.fromInt model.quotesSent) "text-[#03045e]" "Total emails sent with quotes"

        -- View Rate card
        , if model.statsLoading then
            viewStatsCardWithSpinner "View Rate" "text-[#0077b6]"

          else if model.statsError /= Nothing then
            viewStatsCard "View Rate" "0" "text-red-600" "Failed to load data"

          else
            viewStatsCard "View Rate" (String.fromInt viewRate ++ "%") "text-[#0077b6]" (String.fromInt model.quotesViewed ++ " quotes viewed")

        -- Upcoming Emails card
        , if model.statsLoading then
            viewStatsCardWithSpinner "Upcoming Emails" "text-[#00b4d8]"

          else if model.statsError /= Nothing then
            viewStatsCard "Upcoming Emails" "0" "text-red-600" "Failed to load data"

          else
            viewStatsCard "Upcoming Emails" (String.fromInt model.upcomingEmailsTotal) "text-[#00b4d8]" "Scheduled emails in next 30 days"

        -- Completion Rate card
        , if model.statsLoading then
            viewStatsCardWithSpinner "Completion Rate" "text-[#48cae4]"

          else if model.statsError /= Nothing then
            viewStatsCard "Completion Rate" "0" "text-red-600" "Failed to load data"

          else
            viewStatsCard "Completion Rate" (String.fromInt completionRate ++ "%") "text-[#48cae4]" (String.fromInt model.healthQuestionsCompleted ++ " health questions completed")
        ]


viewSendRanking : Model -> Html Msg
viewSendRanking model =
    div [ class "bg-white rounded-lg shadow-xl p-4 mt-6" ]
        [ div [ class "flex justify-between items-center mb-4" ]
            [ h2 [ class "text-lg font-semibold text-gray-800" ] [ text "Send Ranking" ]
            , div [ class "flex items-center space-x-2" ]
                [ span [ class "text-sm text-gray-500" ] [ text "Rank By" ]
                , select
                    [ class "bg-white border border-gray-300 rounded-md px-3 py-1 text-sm"
                    , value (sortOptionToString model.sendRankingSortBy)
                    , onInput
                        (\val ->
                            case val of
                                "quotes_viewed" ->
                                    ChangeSendRankingSort SortByQuotesViewed

                                "quotes_sent" ->
                                    ChangeSendRankingSort SortByQuotesSent

                                "chronological" ->
                                    ChangeSendRankingSort SortByChronological

                                _ ->
                                    NoOp
                        )
                    ]
                    [ option [ value "chronological" ] [ text "Chronological" ]
                    , option [ value "quotes_viewed" ] [ text "Quotes Viewed" ]
                    , option [ value "quotes_sent" ] [ text "Quotes Sent" ]
                    ]
                ]
            ]
        , if model.sendRankingLoading then
            div [ class "flex items-center justify-center py-8" ]
                [ div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-l-2 border-[#03045e]" ] [] ]

          else if List.isEmpty model.sendRankingData then
            div [ class "text-center py-8 text-gray-400" ]
                [ text "No send data available for the selected period." ]

          else
            div [ class "space-y-3" ]
                (List.map viewSendRankingItem model.sendRankingData)
        ]


viewSendRankingItem : SendRankingItem -> Html Msg
viewSendRankingItem item =
    div [ class "flex items-center space-x-4 p-3 rounded-lg bg-purple-50 border border-purple-100" ]
        [ div [ class "flex-shrink-0 w-12 h-12 flex items-center justify-center rounded-full bg-purple-100 text-purple-700 font-semibold" ]
            [ text ("#" ++ String.fromInt item.rank) ]
        , div [ class "flex-1 grid grid-cols-4 gap-4 text-sm" ]
            [ div []
                [ div [ class "font-medium text-gray-900" ] [ text item.sendDate ]
                , div [ class "text-xs text-gray-500" ] [ text "Send Date" ]
                ]
            , div [ class "text-center" ]
                [ div [ class "font-bold text-2xl text-[#03045e] mb-1" ] [ text (String.fromInt item.quotesSent) ]
                , div [ class "text-xs text-gray-500" ] [ text "Quotes Sent" ]
                ]
            , div [ class "text-center" ]
                [ div [ class "flex items-center justify-center mb-1" ]
                    [ div [ class "font-bold text-2xl text-[#03045e] w-10 text-right" ] [ text (String.fromInt item.quotesViewed) ]
                    , div [ class "inline-block bg-[#6366f1] text-white text-[10px] px-2 py-0.5 rounded ml-2 w-10 text-center" ]
                        [ text
                            (if item.quotesSent > 0 then
                                String.fromInt (round ((toFloat item.quotesViewed / toFloat item.quotesSent) * 100)) ++ "%"

                             else
                                "0%"
                            )
                        ]
                    ]
                , div [ class "text-xs text-gray-500" ] [ text "Quotes Viewed" ]
                ]
            , div [ class "text-center" ]
                [ div [ class "flex items-center justify-center mb-1" ]
                    [ div [ class "font-bold text-2xl text-[#48cae4] w-10 text-right" ] [ text (String.fromInt item.healthCompleted) ]
                    , div [ class "inline-block bg-[#06b6d4] text-white text-[10px] px-2 py-0.5 rounded ml-2 w-10 text-center" ]
                        [ text
                            (if item.quotesSent > 0 then
                                String.fromInt (round ((toFloat item.healthCompleted / toFloat item.quotesSent) * 100)) ++ "%"

                             else
                                "0%"
                            )
                        ]
                    ]
                , div [ class "text-xs text-gray-500" ] [ text "Health Completed" ]
                ]
            ]
        ]


viewPerformanceChart : Model -> Html Msg
viewPerformanceChart model =
    div [ class "bg-white rounded-lg shadow-xl p-4" ]
        [ div [ class "flex justify-between items-center mb-4" ]
            [ h2 [ class "text-lg font-semibold text-gray-800" ] [ text "Performance Metrics" ]

            -- Commented out - may add back later
            {- , div [ class "flex space-x-2" ]
               [ button
                   [ class
                       ("px-3 py-1 text-sm rounded-md "
                           ++ (if model.selectedChartView == FunnelView then
                                   "bg-[#03045e] text-white"

                               else
                                   "bg-gray-100"
                              )
                       )
                   , onClick (SelectChartView FunnelView)
                   ]
                   [ text "Funnel" ]
               , button
                   [ class
                       ("px-3 py-1 text-sm rounded-md "
                           ++ (if model.selectedChartView == TrendView then
                                   "bg-[#03045e] text-white"

                               else
                                   "bg-gray-100"
                              )
                       )
                   , onClick (SelectChartView TrendView)
                   ]
                   [ text "Trend" ]
               ]
            -}
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
                    -- For funnel view only (TrendView commented out)
                    chartistData =
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

                    {- TrendView logic commented out - may add back later
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

                           TrendView ->
                               -- For trend view
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
                    -}
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
                [ div [ class "w-3 h-3 rounded-full bg-[#48cae4] mr-1.5 sm:mr-2" ] []
                , text "Health Completed"
                ]
            ]
        ]


viewUpcomingEmails : Model -> Html Msg
viewUpcomingEmails model =
    div [ class "bg-white rounded-lg shadow-sm border border-gray-100 p-4 flex flex-col w-full self-stretch" ]
        [ div [ class "flex justify-between items-center mb-4 flex-shrink-0" ]
            [ h2 [ class "text-lg font-semibold text-gray-800" ] [ text "Upcoming Emails" ]
            , span [ class "bg-gray-100 text-gray-700 text-xs px-2 py-1 rounded-full font-medium" ]
                [ text (String.fromInt model.upcomingEmailsTotal ++ " total") ]
            ]
        , if model.statsLoading then
            div [ class "flex-1 flex items-center justify-center" ]
                [ div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-l-2 border-gray-400" ] [] ]

          else if model.statsError /= Nothing then
            div [ class "flex-1 flex items-center justify-center text-red-500" ]
                [ text "Error loading upcoming emails." ]

          else if List.isEmpty model.upcomingEmailsPage then
            div [ class "flex-1 flex items-center justify-center text-gray-400" ]
                [ text "No upcoming emails scheduled in the next 30 days." ]

          else
            div [ class "flex-1 overflow-y-auto min-h-0" ]
                (List.map viewUpcomingEmailItem model.upcomingEmailsPage)
        ]


viewUpcomingEmailItem : UpcomingEmail -> Html Msg
viewUpcomingEmailItem email =
    div [ class "flex justify-between items-start py-2.5 border-b border-gray-100 last:border-0" ]
        [ div [ class "flex-1 pr-3" ]
            [ div [ class "font-medium text-gray-900 text-sm mb-1" ] [ text (email.firstName ++ " " ++ email.lastName) ]
            , div [ class "flex items-center" ]
                [ span [ class "text-[10px] text-gray-500 mr-1.5" ] [ text "Email Type:" ]
                , span [ class "inline-block bg-blue-100 text-blue-800 text-[10px] px-1.5 py-0.5 rounded font-medium" ]
                    [ text (formatEmailType email.emailType) ]
                ]
            ]
        , div [ class "text-right flex-shrink-0" ]
            [ div [ class "text-sm font-medium text-gray-900" ] [ text (formatDate email.scheduledSendDate) ]
            , div [ class "text-[10px] text-gray-500" ] [ text "Schedule Date" ]
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


formatEmailType : String -> String
formatEmailType emailType =
    case emailType of
        "quote_email" ->
            "Quote Email"

        "QUOTE_EMAIL" ->
            "Quote Email"

        "follow_up_1" ->
            "Follow-up #1"

        "FOLLOW_UP_1" ->
            "Follow-up #1"

        "follow_up_2" ->
            "Follow-up #2"

        "FOLLOW_UP_2" ->
            "Follow-up #2"

        "follow_up_3" ->
            "Follow-up #3"

        "FOLLOW_UP_3" ->
            "Follow-up #3"

        "birthday" ->
            "Birthday"

        "BIRTHDAY" ->
            "Birthday"

        "anniversary" ->
            "Anniversary"

        "ANNIVERSARY" ->
            "Anniversary"

        "effective_date" ->
            "Effective Date"

        "EFFECTIVE_DATE" ->
            "Effective Date"

        _ ->
            emailType


formatDate : String -> String
formatDate dateStr =
    -- Simple formatting, could use a date library for better formatting
    String.left 10 dateStr


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


upcomingEmailDecoder : Decode.Decoder UpcomingEmail
upcomingEmailDecoder =
    Decode.map7 UpcomingEmail
        (Decode.field "id" Decode.int)
        (Decode.field "contact_id" Decode.int)
        (Decode.field "email_type" Decode.string)
        (Decode.field "scheduled_send_date" Decode.string)
        (Decode.field "status" Decode.string)
        (Decode.field "first_name" Decode.string)
        (Decode.field "last_name" Decode.string)


timeFilterToHtmlValue : TimeFilter -> String
timeFilterToHtmlValue filter =
    case filter of
        Today ->
            "today"

        Yesterday ->
            "yesterday"

        Last7Days ->
            "7days"

        Last30Days ->
            "30days"

        Last90Days ->
            "90days"

        YearToDate ->
            "ytd"

        CustomRange ->
            "custom"


sortOptionToString : RankingSortOption -> String
sortOptionToString sortOption =
    case sortOption of
        SortByQuotesSent ->
            "quotes_sent"

        SortByQuotesViewed ->
            "quotes_viewed"

        SortByChronological ->
            "chronological"
