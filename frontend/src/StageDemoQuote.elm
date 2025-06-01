module StageDemoQuote exposing
    ( CompareParams
    , Model
    , Msg(..)
    , PlanType(..)
    , init
    , subscriptions
    , update
    , view
    )

-- ADDED FOR STAGE DEMO

import BirthdayRules exposing (isInBirthdayRuleWindow)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import CarrierNaic exposing (Carrier(..), carrierDecoder, carrierToNaics, carrierToString, naicToCarrier, stringToCarrier)
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import List.Extra
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, height, stroke, strokeLinecap, viewBox, width)
import Task
import Time
import Url exposing (Url)
import Url.Parser as UrlParser
import Url.Parser.Query as Query
import Utils.DiscountDescription exposing (discountDescription)
import Utils.QuoteHeader exposing (viewHeader)



-- TYPES


type PlanType
    = PlanG
    | PlanN


type alias CompareParams =
    { quoteId : Maybe String
    , orgId : Maybe String
    , tid : Maybe String
    }


type alias ContactResponse =
    { contact : Maybe Contact
    , agent : Agent
    , orgSlug : String
    , orgName : String
    , orgLogo : Maybe String
    , orgPhone : Maybe String
    , orgSignature : Bool
    , orgSignatureText : Maybe String
    , carrierContracts : List Carrier
    , forceOrgSenderDetails : Bool
    }


type alias Agent =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , signature : String
    , useOrgSenderDetails : Bool
    , bookingLink : String
    }


type alias Contact =
    { id : Int
    , firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , age : Int
    , gender : String
    , tobacco : Bool
    , state : String
    , zipCode : String
    , county : Maybe String
    , currentCarrier : Maybe String
    , planType : Maybe String
    }


type alias CoverageItem =
    { name : String
    , percentageCovered : Int
    , note : Maybe String
    }


type alias CoverageList =
    List CoverageItem


type alias Plan =
    { price : Float
    , priceDiscount : Float
    , flag : Maybe String
    , age : Int
    , description : String
    , gender : String
    , id : Int
    , image : String
    , naic : String
    , name : String
    , planType : String
    , premiumStability : String
    , ratingCategory : String
    , score : Int
    , select : Bool
    , state : String
    , tobacco : Bool
    , coverageSummary : CoverageList
    , discountDescription : Maybe String
    , originalPlanName : Maybe String
    }


type alias Plans =
    { planG : List Plan
    , planN : List Plan
    }


type alias LocationUpdateResponse =
    { success : Bool
    , zipCode : String
    , state : String
    , counties : List String
    }


type alias Model =
    { tempId : String
    , isLoading : Bool
    , error : Maybe String
    , plans : Plans
    , state : Maybe String
    , county : Maybe String
    , zip : Maybe String
    , age : Maybe Int
    , gender : Maybe String
    , tobacco : Maybe Bool
    , selectedPlanType : PlanType
    , selectedPlan : Maybe Plan
    , showReviewVideo : Bool
    , showQualificationVideo : Bool
    , showGvsNVideo : Bool
    , showFAQ : Bool
    , currentCardIndex : Int
    , showRatesVideo : Bool
    , key : Nav.Key
    , showDiscount : Bool
    , currentCarrier : Maybe String
    , planType : Maybe String
    , dateOfBirth : Maybe String
    , quoteId : Maybe String
    , carrierContracts : List Carrier
    , currentDate : Maybe Date
    , effectiveDate : Maybe Date
    , discountCsvString : Maybe String
    , orgId : Maybe String
    , orgName : Maybe String
    , orgLogo : Maybe String
    , orgPhone : Maybe String
    , useOrg : Bool
    , orgSignatureText : Maybe String
    , name : Maybe String
    , contact : Maybe Contact
    , agent : Maybe Agent
    , orgSlug : Maybe String
    , loadingContact : Bool
    , showDiscountInfo : Bool
    , showLocationModal : Bool
    , editingZipCode : Maybe String
    , editingCounty : Maybe String
    , availableCounties : List String
    , locationUpdateError : Maybe String
    , submittingLocation : Bool
    , editingEffectiveDate : Maybe String
    , fetchNewPlans : Bool
    , activeTooltipPlan : Maybe Plan
    , forceOrgSenderDetails : Bool
    }


type Msg
    = GotDiscountCsvString (Result Http.Error String)
    | FetchedPlans (Result Http.Error (List QuoteResponse))
    | TogglePlanType
    | SelectPlan Plan
    | SelectPlanCard Plan
    | ScrollDown Dom.Viewport
    | OpenGvsNVideo
    | CloseGvsNVideo
    | ShowFAQ
    | CloseFAQ
    | NextCard
    | PreviousCard
    | CloseRatesVideo
    | NavigateTo String
    | ToggleDiscount
    | ToggleDiscountInfo
    | GotCurrentDate Date
    | ToggleMobileTooltip (Maybe Plan)
    | NavigateToHealth
    | NoOp


init : String -> Nav.Key -> ( Model, Cmd Msg )
init tempId key =
    let
        -- Empty model with loading state
        emptyModel =
            { tempId = tempId
            , isLoading = True
            , error = Nothing
            , plans = { planG = [], planN = [] }
            , state = Nothing
            , county = Nothing
            , zip = Nothing
            , age = Nothing
            , gender = Nothing
            , tobacco = Nothing
            , selectedPlanType = PlanG
            , selectedPlan = Nothing
            , showReviewVideo = False
            , showQualificationVideo = False
            , showGvsNVideo = False
            , showFAQ = False
            , currentCardIndex = 0
            , showRatesVideo = False
            , key = key
            , showDiscount = False
            , currentCarrier = Nothing
            , planType = Nothing
            , dateOfBirth = Nothing
            , quoteId = Nothing
            , carrierContracts = []
            , currentDate = Nothing
            , effectiveDate = Nothing
            , discountCsvString = Nothing
            , orgId = Nothing
            , orgName = Nothing
            , orgLogo = Nothing
            , orgPhone = Nothing
            , useOrg = False
            , orgSignatureText = Nothing
            , name = Nothing
            , contact = Nothing
            , agent = Nothing
            , orgSlug = Nothing
            , loadingContact = True
            , showDiscountInfo = False
            , showLocationModal = False
            , editingZipCode = Nothing
            , editingCounty = Nothing
            , availableCounties = []
            , locationUpdateError = Nothing
            , submittingLocation = False
            , editingEffectiveDate = Nothing
            , fetchNewPlans = False
            , activeTooltipPlan = Nothing
            , forceOrgSenderDetails = False
            }
    in
    ( emptyModel
    , Cmd.batch
        [ Task.perform GotCurrentDate Date.today
        , fetchDiscountCsvString
        , fetchPlans
        ]
    )



-- HTTP


fetchPlans : Cmd Msg
fetchPlans =
    Http.get
        { url = "/api/stage-demo/plans" -- Updated endpoint
        , expect = Http.expectJson FetchedPlans (D.list quoteResponseDecoder)
        }


fetchDiscountCsvString : Cmd Msg
fetchDiscountCsvString =
    Http.get
        { url = "/api/data/public/hhd.csv"
        , expect = Http.expectString GotDiscountCsvString
        }


getEffectiveDate : Date -> String
getEffectiveDate date =
    date
        |> Date.add Date.Months 1
        |> Date.floor Date.Month
        |> Date.toIsoString



-- DECODERS


type alias QuoteResponse =
    { naic : String
    , group : Int
    , companyName : String
    , quotes : List QuoteData
    }


type alias QuoteData =
    { rate : Float
    , discountRate : Float
    , discountCategory : Maybe String
    , age : Int
    , gender : String
    , plan : String
    , tobacco : Int
    , originalPlanName : Maybe String
    }


quoteDataDecoder : Decoder QuoteData
quoteDataDecoder =
    D.succeed QuoteData
        |> Pipeline.required "rate" D.float
        |> Pipeline.required "discount_rate" D.float
        |> Pipeline.required "discount_category" (D.nullable D.string)
        |> Pipeline.required "age" D.int
        |> Pipeline.required "gender" D.string
        |> Pipeline.required "plan" D.string
        |> Pipeline.required "tobacco" D.int
        |> Pipeline.optional "original_plan_name" (D.nullable D.string) Nothing


quoteResponseDecoder : Decoder QuoteResponse
quoteResponseDecoder =
    D.succeed QuoteResponse
        |> Pipeline.required "naic" D.string
        |> Pipeline.required "group" D.int
        |> Pipeline.required "companyName" D.string
        |> Pipeline.required "quotes" (D.list quoteDataDecoder)


blacklistCarriers : List Carrier
blacklistCarriers =
    [ Allstate ]


isCarrierSupported : String -> List Carrier -> Bool
isCarrierSupported naic carrierContracts =
    case naicToCarrier naic of
        Just carrierName ->
            List.member carrierName carrierContracts
                && not (List.member carrierName blacklistCarriers)

        Nothing ->
            False


filterPlansByCarrier : Plans -> List Carrier -> Plans
filterPlansByCarrier plans carrierContracts =
    { planG = List.filter (\plan -> isCarrierSupported plan.naic carrierContracts) plans.planG |> List.Extra.unique
    , planN = List.filter (\plan -> isCarrierSupported plan.naic carrierContracts) plans.planN |> List.Extra.unique
    }


groupQuotesByPlan : List QuoteResponse -> Model -> Plans
groupQuotesByPlan responses model =
    let
        convertToPlan : QuoteResponse -> QuoteData -> Plan
        convertToPlan response quote =
            let
                carrierImagePath =
                    case naicToCarrier response.naic of
                        Just carrier ->
                            "/images/" ++ (carrier |> carrierToString |> String.filter (\c -> c /= ' ')) ++ ".svg"

                        Nothing ->
                            -- Fallback to png if we can't match the carrier
                            "/images/medicare-max-logo.png"
            in
            { price = quote.rate / 100
            , priceDiscount = quote.discountRate / 100
            , flag = quote.discountCategory
            , age = quote.age
            , description = ""
            , gender = quote.gender
            , id = 0
            , image = carrierImagePath
            , naic = response.naic
            , name = response.companyName
            , planType = quote.plan
            , premiumStability = ""
            , ratingCategory = ""
            , score = 0
            , select = False
            , state = Maybe.withDefault "" model.state
            , tobacco = quote.tobacco == 1
            , coverageSummary =
                if String.toUpper quote.plan == "G" then
                    planGCoverageList

                else
                    planNCoverageList
            , discountDescription = Nothing
            , originalPlanName = quote.originalPlanName
            }

        allQuotes =
            List.concatMap
                (\response ->
                    List.concatMap
                        (\quote ->
                            let
                                upperPlan =
                                    String.toUpper quote.plan
                            in
                            if List.member upperPlan [ "G", "N" ] then
                                [ convertToPlan response quote ]

                            else
                                []
                        )
                        response.quotes
                )
                responses

        groupedPlanG =
            List.filter (\q -> String.toUpper q.planType == "G") allQuotes
                |> List.sortBy .price

        groupedPlanN =
            List.filter (\q -> String.toUpper q.planType == "N") allQuotes
                |> List.sortBy .price

        result =
            { planG = groupedPlanG
            , planN = groupedPlanN
            }
    in
    result



-- COVERAGE LISTS


planGCoverageList : CoverageList
planGCoverageList =
    [ { name = "Part A Deductible", percentageCovered = 100, note = Nothing }
    , { name = "Hospital Co-Pays", percentageCovered = 100, note = Nothing }
    , { name = "Skilled Nursing Facility Co-Pays", percentageCovered = 100, note = Nothing }
    , { name = "Part B Annual Deductible", percentageCovered = 0, note = Just "$240 annual deductible" }
    , { name = "Part B Coinsurance", percentageCovered = 100, note = Nothing }
    , { name = "Excess Charges", percentageCovered = 100, note = Nothing }
    , { name = "Foreign Travel Emergency", percentageCovered = 80, note = Nothing }
    ]


planNCoverageList : CoverageList
planNCoverageList =
    [ { name = "Part A Deductible", percentageCovered = 100, note = Nothing }
    , { name = "Hospital Co-Pays", percentageCovered = 100, note = Nothing }
    , { name = "Skilled Nursing Facility Co-Pays", percentageCovered = 100, note = Nothing }
    , { name = "Part B Annual Deductible", percentageCovered = 0, note = Just "$240 annual deductible" }
    , { name = "Part B Coinsurance", percentageCovered = 100, note = Just "w/ some copayments" }
    , { name = "Excess Charges", percentageCovered = 0, note = Nothing }
    , { name = "Foreign Travel Emergency", percentageCovered = 80, note = Nothing }
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDiscountCsvString (Ok discountCsvString) ->
            ( { model
                | discountCsvString = Just discountCsvString
              }
            , Cmd.none
            )

        GotDiscountCsvString (Err _) ->
            ( { model | error = Just "Failed to load discount CSV", isLoading = False }
            , Cmd.none
            )

        FetchedPlans (Ok responses) ->
            let
                newPlans =
                    groupQuotesByPlan responses model
            in
            ( { model
                | plans = newPlans
                , isLoading = False
              }
            , Cmd.none
            )

        FetchedPlans (Err httpError) ->
            ( { model
                | error = Just ("Failed to fetch plans: " ++ httpErrorToString httpError)
                , isLoading = False
              }
            , Cmd.none
            )

        ToggleDiscount ->
            ( { model | showDiscount = not model.showDiscount }
            , Cmd.none
            )

        ToggleDiscountInfo ->
            ( { model | showDiscountInfo = not model.showDiscountInfo }
            , Cmd.none
            )

        TogglePlanType ->
            ( { model
                | selectedPlanType = togglePlanType model.selectedPlanType
                , currentCardIndex = 0
                , selectedPlan = Nothing
              }
            , Cmd.none
            )

        SelectPlan plan ->
            ( { model | showQualificationVideo = True }
            , Cmd.batch
                [ Nav.pushUrl model.key
                    (case model.quoteId of
                        Just id ->
                            let
                                orgIdParam =
                                    case model.orgId of
                                        Just orgId ->
                                            "&orgId=" ++ orgId

                                        Nothing ->
                                            -- Try to extract orgId from the quoteId as a fallback
                                            case String.split "-" id |> List.head of
                                                Just extractedOrgId ->
                                                    "&orgId=" ++ extractedOrgId

                                                Nothing ->
                                                    ""
                            in
                            "/eligibility?id=" ++ id ++ orgIdParam

                        Nothing ->
                            let
                                orgIdParam =
                                    case model.orgId of
                                        Just orgId ->
                                            "?orgId=" ++ orgId

                                        Nothing ->
                                            ""
                            in
                            "/eligibility" ++ orgIdParam
                    )
                , Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)
                ]
            )

        SelectPlanCard plan ->
            ( { model
                | selectedPlan =
                    if Just plan == model.selectedPlan then
                        Nothing
                        -- Deselect if clicking the same plan again

                    else
                        Just plan

                -- Otherwise select the new plan
              }
            , Cmd.none
            )

        ScrollDown viewport ->
            ( model, Cmd.none )

        OpenGvsNVideo ->
            ( { model | showGvsNVideo = True }, Cmd.none )

        CloseGvsNVideo ->
            ( { model | showGvsNVideo = False }, Cmd.none )

        ShowFAQ ->
            ( { model | showFAQ = True }
            , Cmd.none
            )

        CloseFAQ ->
            ( { model | showFAQ = False }
            , Cmd.none
            )

        NextCard ->
            ( { model | currentCardIndex = Basics.min (model.currentCardIndex + 1) (List.length (getSelectedPlans model) - 1) }
            , Cmd.none
            )

        PreviousCard ->
            ( { model | currentCardIndex = Basics.max (model.currentCardIndex - 1) 0 }
            , Cmd.none
            )

        CloseRatesVideo ->
            ( { model | showRatesVideo = False }, Cmd.none )

        NavigateTo path ->
            ( model, Nav.pushUrl model.key path )

        GotCurrentDate date ->
            let
                effectiveDate =
                    date
                        |> Date.add Date.Months 1
                        |> Date.floor Date.Month
            in
            ( { model
                | currentDate = Just date
                , effectiveDate = Just effectiveDate
              }
            , Cmd.none
            )

        ToggleMobileTooltip plan ->
            ( { model | activeTooltipPlan = plan }
            , Cmd.none
            )

        NavigateToHealth ->
            ( model, Nav.pushUrl model.key ("/stage-demo/health/" ++ model.tempId) )

        NoOp ->
            ( model, Cmd.none )



-- HELPERS


togglePlanType : PlanType -> PlanType
togglePlanType planType =
    case planType of
        PlanG ->
            PlanN

        PlanN ->
            PlanG


getSelectedPlans : Model -> List Plan
getSelectedPlans model =
    let
        plans =
            case model.selectedPlan of
                Just plan ->
                    [ plan ]

                Nothing ->
                    case model.selectedPlanType of
                        PlanG ->
                            model.plans.planG

                        PlanN ->
                            model.plans.planN

        carrierNaics =
            model.currentCarrier
                |> Maybe.andThen stringToCarrier
                |> Maybe.map carrierToNaics

        filteredPlans =
            case carrierNaics of
                Just naicList ->
                    List.filter
                        (\plan ->
                            not (List.member plan.naic naicList)
                        )
                        plans

                Nothing ->
                    plans

        sortedAndLimited =
            List.sortBy .price filteredPlans
                |> List.take 3
    in
    sortedAndLimited



-- Get the top N cheapest plans for a specific plan type


getTopPlans : Model -> List Plan -> Int -> List Plan
getTopPlans model plans count =
    let
        carrierNaics =
            model.currentCarrier
                |> Maybe.andThen stringToCarrier
                |> Maybe.map carrierToNaics

        filteredPlans =
            case carrierNaics of
                Just naicList ->
                    List.filter
                        (\plan ->
                            not (List.member plan.naic naicList)
                        )
                        plans

                Nothing ->
                    plans
    in
    List.sortBy
        (\plan ->
            {--
            if model.showDiscount then
                plan.priceDiscount

            else
            --}
            plan.price
        )
        filteredPlans
        |> List.take count


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url ++ ". Please check the URL and try again."

        Http.Timeout ->
            "Request timed out. The server took too long to respond. Please try again later or check your internet connection."

        Http.NetworkError ->
            "Network error. Unable to connect to the server. Please check your internet connection and try again."

        Http.BadStatus statusCode ->
            "Bad status: " ++ String.fromInt statusCode ++ ". The server returned an unexpected status code. Please try again later or contact support if the issue persists."

        Http.BadBody message ->
            "Bad body: " ++ message ++ ". The server response was not in the expected format. Please try again or contact support if the issue persists."



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        closeModalOnEscape : String -> Msg
        closeModalOnEscape key =
            if key == "Escape" then
                case model of
                    _ ->
                        if model.showGvsNVideo then
                            CloseGvsNVideo

                        else if model.showRatesVideo then
                            CloseRatesVideo

                        else if model.showFAQ then
                            CloseFAQ

                        else
                            NoOp

            else
                NoOp

        shouldListenForEscape =
            model.showLocationModal
                || model.showGvsNVideo
                || model.showQualificationVideo
                || model.showRatesVideo
                || model.showFAQ
    in
    if shouldListenForEscape then
        Browser.Events.onKeyDown
            (D.map closeModalOnEscape (D.field "key" D.string))

    else
        Sub.none



-- VIEW


viewPersonalInfo : Model -> Html Msg
viewPersonalInfo model =
    div [ class "flex flex-col gap-4 sm:gap-10" ]
        [ div [ class "bg-white rounded-[10px] border border-[#DCE2E5] shadow-[0_1px_2px_rgba(16,24,40,0.05)]" ]
            [ -- Personal Quote Header
              div [ class "border-b border-[#DCE2E5] bg-[#F9F5FF] px-4 sm:px-6 py-4 rounded-t-[10px]" ]
                [ h2 [ class "text-2xl font-extrabold -tracking-[0.04em] text-[#101828] leading-[1.2]" ] [ text "Personal Quote" ]
                ]
            , div
                [ class "p-4 sm:p-6 flex flex-col sm:flex-row sm:justify-between sm:items-start gap-4 sm:gap-6 bg-white rounded-b-[10px]" ]
                [ div [ class "flex flex-col sm:flex-row gap-4 sm:gap-12" ]
                    [ -- Quote For section
                      div [ class "flex flex-col" ]
                        [ div [ class "mb-2" ]
                            [ p [ class "text-sm text-[#667085] mb-1" ] [ text "Quote For" ]
                            , p [ class "text-[16px] font-medium" ] [ text (Maybe.withDefault "Loading..." model.name) ]
                            , p [ class "text-[12px] text-[#667085]" ]
                                [ text
                                    (if String.contains "F" (model.gender |> Maybe.withDefault "" |> String.toUpper) then
                                        "F"

                                     else
                                        "M"
                                    )
                                , span [ class "text-[#475569] mx-2 font-medium" ] [ text "│" ]
                                , text
                                    (if Maybe.withDefault False model.tobacco then
                                        "Tobacco"

                                     else
                                        "Non-Tobacco"
                                    )
                                , span [ class "text-[#475569] mx-2 font-medium" ] [ text "│" ]
                                , text (String.fromInt (Maybe.withDefault 0 model.age))
                                , text " years"
                                , span [ class "text-[#475569] mx-2 font-medium" ] [ text "│" ]
                                , text (Maybe.withDefault "" model.state)
                                , text " "
                                , text (Maybe.withDefault "" model.zip)
                                ]
                            ]
                        , div [ class "flex flex-col gap-2" ]
                            [ div [ class "flex gap-2" ]
                                [ case model.effectiveDate of
                                    Just date ->
                                        p [ class "text-xs text-[#667085]" ]
                                            [ span [ class "font-medium" ] [ text "Effective Date: " ]
                                            , text (date |> Date.toIsoString |> formatEffectiveDate)
                                            ]

                                    Nothing ->
                                        text ""
                                , button [ class "text-xs text-[#2563EB] underline text-left" ] [ text "Change" ]
                                ]
                            , div [ class "hidden sm:block" ]
                                [ div [ class "flex flex-col mt-4 gap-1" ]
                                    [ p [ class "text-xs text-[#667085]" ] [ text "Need a Quote for Someone else?" ]
                                    , a [ href ("/self-onboarding/" ++ Maybe.withDefault "" model.orgSlug), class "text-xs text-[#667085] underline" ] [ text "Start Here" ]
                                    ]
                                ]
                            ]
                        ]

                    -- Mobile divider
                    , div [ class "block sm:hidden h-[1px] bg-[#DCE2E5] my-4" ] []

                    -- Desktop divider
                    , div [ class "hidden sm:block w-[1px] bg-[#DCE2E5]" ] []

                    -- Quote From section
                    , div [ class "flex flex-col min-w-[200px]" ]
                        [ p [ class "text-sm text-[#667085] mb-1" ] [ text "Quote From" ]
                        , let
                            -- Determine whether to use org or agent details
                            effectiveUseOrg =
                                if model.forceOrgSenderDetails then
                                    True

                                else
                                    case model.agent of
                                        Just agent ->
                                            agent.useOrgSenderDetails

                                        Nothing ->
                                            model.useOrg

                            -- fallback to old logic
                          in
                          if effectiveUseOrg then
                            -- Organization information
                            div []
                                [ case model.orgSignatureText of
                                    Just signature ->
                                        div [ class "mb-2" ]
                                            [ text signature ]

                                    Nothing ->
                                        p [ class "text-[16px] font-medium mb-2" ]
                                            [ text (Maybe.withDefault "Organization" model.orgName) ]
                                , case model.orgPhone of
                                    Just phone ->
                                        div [ class "flex flex-col gap-3" ]
                                            [ a
                                                [ href ("tel:" ++ String.filter (\c -> c >= '0' && c <= '9') phone)
                                                , class "flex items-center gap-1.5 bg-[#F9F5FF] px-2.5 py-2 rounded hover:bg-[#F4EBFF] transition-colors min-w-[200px] w-fit"
                                                ]
                                                [ svg [ Svg.Attributes.width "16", Svg.Attributes.height "16", Svg.Attributes.viewBox "0 0 12 12", Svg.Attributes.fill "none" ]
                                                    [ path
                                                        [ Svg.Attributes.fillRule "evenodd"
                                                        , Svg.Attributes.clipRule "evenodd"
                                                        , Svg.Attributes.d "M1.75442 1.13022C2.80442 0.0802194 4.57692 0.160219 5.30817 1.47022L5.7138 2.19709C6.19067 3.05209 5.98755 4.13147 5.2888 4.83897C5.24752 4.90156 5.22497 4.97463 5.2238 5.04959C5.21567 5.20959 5.27255 5.58022 5.84692 6.15397C6.42067 6.72772 6.79067 6.78522 6.9513 6.77709C7.02626 6.77592 7.09934 6.75337 7.16192 6.71209C7.8688 6.01334 8.9488 5.81022 9.8038 6.28709L10.5307 6.69334C11.8407 7.42459 11.9207 9.19584 10.8707 10.2465C10.3088 10.8077 9.56255 11.3071 8.68442 11.3402C7.38442 11.3896 5.22442 11.0533 3.08567 8.91522C0.947548 6.77647 0.611298 4.61709 0.660673 3.31647C0.693798 2.43834 1.19317 1.69147 1.75442 1.13022ZM4.48942 1.92709C4.11442 1.25584 3.10817 1.10209 2.41755 1.79334C1.93317 2.27772 1.61755 2.81209 1.59755 3.35147C1.5563 4.43647 1.82442 6.32772 3.7488 8.25147C5.6738 10.1765 7.56442 10.4446 8.6488 10.4033C9.18817 10.3827 9.7238 10.0677 10.2075 9.58334C10.8988 8.89209 10.745 7.88584 10.0738 7.51147L9.34692 7.10584C8.89505 6.85397 8.25942 6.93959 7.8138 7.38584C7.77005 7.42959 7.4913 7.68959 6.99692 7.71334C6.49067 7.73834 5.87755 7.51084 5.18442 6.81709C4.49005 6.12334 4.26255 5.51022 4.28755 5.00334C4.3113 4.50897 4.57192 4.23022 4.61505 4.18647C5.0613 3.74084 5.14692 3.10584 4.89505 2.65397L4.48942 1.92709Z"
                                                        , Svg.Attributes.fill "#03045E"
                                                        ]
                                                        []
                                                    ]
                                                , span [ class "text-sm text-[#03045E]" ]
                                                    [ text (formatPhoneNumber phone) ]
                                                ]
                                            ]

                                    Nothing ->
                                        text ""
                                ]

                          else
                            -- Agent information
                            div []
                                [ p [ class "text-[16px] font-medium mb-2" ]
                                    [ text
                                        (case model.agent of
                                            Just agent ->
                                                agent.firstName ++ " " ++ agent.lastName

                                            Nothing ->
                                                "Loading..."
                                        )
                                    ]
                                , div [ class "flex flex-col gap-3" ]
                                    [ a
                                        [ href
                                            (case model.agent of
                                                Just agent ->
                                                    "mailto:" ++ agent.email

                                                Nothing ->
                                                    "#"
                                            )
                                        , class "flex items-center gap-1.5 bg-[#F9F5FF] px-2.5 py-2 rounded hover:bg-[#F4EBFF] transition-colors min-w-[200px] w-fit"
                                        ]
                                        [ svg [ Svg.Attributes.width "16", Svg.Attributes.height "16", Svg.Attributes.viewBox "0 0 12 12", Svg.Attributes.fill "none" ]
                                            [ path [ Svg.Attributes.d "M1 6C1 4.1145 1 3.1715 1.586 2.586C2.1715 2 3.1145 2 5 2H7C8.8855 2 9.8285 2 10.414 2.586C11 3.1715 11 4.1145 11 6C11 7.8855 11 8.8285 10.414 9.414C9.8285 10 8.8855 10 7 10H5C3.1145 10 2.1715 10 1.586 9.414C1 8.8285 1 7.8855 1 6Z", Svg.Attributes.stroke "#03045E" ] []
                                            , path [ Svg.Attributes.d "M3 4L4.0795 4.9C4.998 5.665 5.457 6.0475 6 6.0475C6.543 6.0475 7.0025 5.665 7.9205 4.8995L9 4", Svg.Attributes.stroke "#03045E", Svg.Attributes.strokeLinecap "round", Svg.Attributes.strokeLinejoin "round" ] []
                                            ]
                                        , span [ class "text-sm text-[#03045E]" ]
                                            [ text
                                                (case model.agent of
                                                    Just agent ->
                                                        agent.email

                                                    Nothing ->
                                                        "Loading..."
                                                )
                                            ]
                                        ]
                                    , a
                                        [ href
                                            (case model.agent of
                                                Just agent ->
                                                    "tel:" ++ String.filter (\c -> c >= '0' && c <= '9') agent.phone

                                                Nothing ->
                                                    "#"
                                            )
                                        , class "flex items-center gap-1.5 bg-[#F9F5FF] px-2.5 py-2 rounded hover:bg-[#F4EBFF] transition-colors min-w-[200px] w-fit"
                                        ]
                                        [ svg [ Svg.Attributes.width "16", Svg.Attributes.height "16", Svg.Attributes.viewBox "0 0 12 12", Svg.Attributes.fill "none" ]
                                            [ path
                                                [ Svg.Attributes.fillRule "evenodd"
                                                , Svg.Attributes.clipRule "evenodd"
                                                , Svg.Attributes.d "M1.75442 1.13022C2.80442 0.0802194 4.57692 0.160219 5.30817 1.47022L5.7138 2.19709C6.19067 3.05209 5.98755 4.13147 5.2888 4.83897C5.24752 4.90156 5.22497 4.97463 5.2238 5.04959C5.21567 5.20959 5.27255 5.58022 5.84692 6.15397C6.42067 6.72772 6.79067 6.78522 6.9513 6.77709C7.02626 6.77592 7.09934 6.75337 7.16192 6.71209C7.8688 6.01334 8.9488 5.81022 9.8038 6.28709L10.5307 6.69334C11.8407 7.42459 11.9207 9.19584 10.8707 10.2465C10.3088 10.8077 9.56255 11.3071 8.68442 11.3402C7.38442 11.3896 5.22442 11.0533 3.08567 8.91522C0.947548 6.77647 0.611298 4.61709 0.660673 3.31647C0.693798 2.43834 1.19317 1.69147 1.75442 1.13022ZM4.48942 1.92709C4.11442 1.25584 3.10817 1.10209 2.41755 1.79334C1.93317 2.27772 1.61755 2.81209 1.59755 3.35147C1.5563 4.43647 1.82442 6.32772 3.7488 8.25147C5.6738 10.1765 7.56442 10.4446 8.6488 10.4033C9.18817 10.3827 9.7238 10.0677 10.2075 9.58334C10.8988 8.89209 10.745 7.88584 10.0738 7.51147L9.34692 7.10584C8.89505 6.85397 8.25942 6.93959 7.8138 7.38584C7.77005 7.42959 7.4913 7.68959 6.99692 7.71334C6.49067 7.73834 5.87755 7.51084 5.18442 6.81709C4.49005 6.12334 4.26255 5.51022 4.28755 5.00334C4.3113 4.50897 4.57192 4.23022 4.61505 4.18647C5.0613 3.74084 5.14692 3.10584 4.89505 2.65397L4.48942 1.92709Z"
                                                , Svg.Attributes.fill "#03045E"
                                                ]
                                                []
                                            ]
                                        , span [ class "text-sm text-[#03045E]" ]
                                            [ text
                                                (case model.agent of
                                                    Just agent ->
                                                        formatPhoneNumber agent.phone

                                                    Nothing ->
                                                        "Loading..."
                                                )
                                            ]
                                        ]
                                    ]
                                ]
                        ]

                    -- Desktop divider before video
                    , div [ class "hidden sm:block w-[1px] bg-[#DCE2E5]" ] []

                    -- Video button section
                    , div [ class "hidden sm:flex flex-col justify-center items-center cursor-pointer gap-2 bg-[#F9F5FF] rounded-[10px] p-4 border border-[#DCE2E5] min-w-[200px] min-h-[160px]", onClick OpenGvsNVideo ]
                        [ p [ class "text-base font-bold text-[#03045E] -tracking-[0.03em] leading-[1.21] text-center" ] [ text "Learn About Plan G vs Plan N" ]
                        , div [ class "w-[33px] h-[33px] rounded-full border border-[#03045E] flex items-center justify-center" ]
                            [ div [ class "w-0 h-0 border-t-[8px] border-t-transparent border-l-[12px] border-l-[#03045E] border-b-[8px] border-b-transparent ml-1" ] []
                            ]
                        , p [ class "text-xs text-[#667085] -tracking-[0.03em] leading-[1.21] text-center" ] [ text "Watch the Video" ]
                        ]
                    ]
                ]
            , div [ class "block sm:hidden mt-2 mb-6 justify-center flex items-center w-full" ]
                [ div [ class "text-sm text-[#667085] text-center flex flex-wrap justify-center items-center gap-1" ]
                    [ text "Need a Quote for Someone else? "
                    , a [ href ("/self-onboarding/" ++ Maybe.withDefault "" model.orgSlug), class " underline" ] [ text "Start Here" ]
                    ]
                ]
            ]
        ]


viewPlanCard : Model -> String -> Plan -> Html Msg
viewPlanCard model planTypeCode plan =
    let
        isSelected =
            model.selectedPlan == Just plan

        displayPlanBadgeText =
            case plan.originalPlanName of
                Just originalName ->
                    formatOriginalPlanName originalName

                Nothing ->
                    "PLAN " ++ planTypeCode

        ( badgeTextColor, badgeBgColor ) =
            if planTypeCode == "G" then
                ( "text-[#363F72]", "bg-[#F8F9FC]" )

            else
                ( "text-[#363F72]", "bg-[#F8F9FC]" )

        borderClass =
            if isSelected then
                "border-2 border-[#2563EB]"

            else
                "border border-[#D4D4D4]"

        isTooltipActive =
            case model.activeTooltipPlan of
                Just activePlan ->
                    activePlan.id == plan.id

                Nothing ->
                    False
    in
    div [ class "flex flex-col" ]
        [ div
            [ class ("relative bg-white rounded-lg " ++ borderClass ++ " overflow-hidden cursor-pointer w-[calc(100vw-72px)] sm:w-[340px]")
            , onClick (SelectPlanCard plan)
            ]
            [ -- Top row with Plan type badge and radio
              div [ class "flex items-center justify-between p-2 sm:p-4" ]
                [ div [ class ("px-2.5 py-0.5 rounded-lg text-xs font-medium leading-5 " ++ badgeTextColor ++ " " ++ badgeBgColor) ]
                    [ text displayPlanBadgeText ]
                , div [ class "flex items-center gap-1.5" ]
                    [ span [ class "text-sm sm:text-sm font-medium text-[#667085]" ] [ text "Select This Plan" ]
                    , if isSelected then
                        svg [ Svg.Attributes.width "18", Svg.Attributes.height "19", Svg.Attributes.viewBox "0 0 14 15", Svg.Attributes.fill "none" ]
                            [ Svg.rect [ Svg.Attributes.x "0.5", Svg.Attributes.y "1", Svg.Attributes.width "13", Svg.Attributes.height "13", Svg.Attributes.rx "6.5", Svg.Attributes.fill "#F9F5FF" ] []
                            , Svg.rect [ Svg.Attributes.x "0.5", Svg.Attributes.y "1", Svg.Attributes.width "13", Svg.Attributes.height "13", Svg.Attributes.rx "6.5", Svg.Attributes.stroke "#7F56D9" ] []
                            , Svg.path [ Svg.Attributes.d "M10.5 5.25L6 9.75L3.5 7.25", Svg.Attributes.stroke "#7F56D9", Svg.Attributes.strokeWidth "1.6666", Svg.Attributes.strokeLinecap "round", Svg.Attributes.strokeLinejoin "round" ] []
                            ]

                      else
                        svg [ Svg.Attributes.width "18", Svg.Attributes.height "19", Svg.Attributes.viewBox "0 0 14 15", Svg.Attributes.fill "none" ]
                            [ Svg.rect [ Svg.Attributes.x "0.5", Svg.Attributes.y "1", Svg.Attributes.width "13", Svg.Attributes.height "13", Svg.Attributes.rx "6.5", Svg.Attributes.fill "white" ] []
                            , Svg.rect [ Svg.Attributes.x "0.5", Svg.Attributes.y "1", Svg.Attributes.width "13", Svg.Attributes.height "13", Svg.Attributes.rx "6.5", Svg.Attributes.stroke "#D4D4D4" ] []
                            ]
                    ]
                ]

            -- Carrier Logo
            , div [ class "px-4 flex justify-center items-center min-h-[120px] py-4" ]
                [ img [ src plan.image, alt (plan.name ++ " logo"), class "h-20 max-w-[240px] object-contain" ] [] ]

            -- Rates
            , div [ class "flex justify-between items-center px-6 py-4 bg-[#F9FAFB]" ]
                [ div [ class "flex items-center" ]
                    [ span [ class "text-sm font-medium text-[#667085]" ] [ text "Standard:" ]
                    , span [ class "text-lg font-bold text-[#667085] ml-1" ] [ text ("$" ++ String.fromInt (floor plan.price)) ]
                    ]
                , div [ class "flex items-center" ]
                    [ div [ class "w-[1px] h-[24px] bg-[#DCE2E5] mx-4" ] [] ]
                , div [ class "flex items-center relative group" ]
                    [ span [ class "text-sm font-medium text-[#667085]" ] [ text "Discount:" ]
                    , span [ class "text-lg font-bold text-[#667085] ml-1" ] [ text ("$" ++ String.fromInt (floor plan.priceDiscount)) ]
                    , case plan.discountDescription of
                        Just description ->
                            div [ class "inline-flex ml-1 relative" ]
                                [ -- Desktop hover tooltip (hidden on mobile)
                                  div [ class "hidden sm:inline-flex text-blue-500 cursor-help" ]
                                    [ svg [ Svg.Attributes.width "16", Svg.Attributes.height "16", Svg.Attributes.viewBox "0 0 24 24", Svg.Attributes.fill "none" ]
                                        [ Svg.circle [ Svg.Attributes.cx "12", Svg.Attributes.cy "12", Svg.Attributes.r "10", Svg.Attributes.stroke "currentColor", Svg.Attributes.strokeWidth "2" ] []
                                        , Svg.path [ Svg.Attributes.d "M12 8v4", Svg.Attributes.stroke "currentColor", Svg.Attributes.strokeWidth "2", Svg.Attributes.strokeLinecap "round" ] []
                                        , Svg.path [ Svg.Attributes.d "M12 16h.01", Svg.Attributes.stroke "currentColor", Svg.Attributes.strokeWidth "2", Svg.Attributes.strokeLinecap "round" ] []
                                        ]
                                    , div [ class "absolute bottom-full mb-2 right-0 w-48 p-2 bg-gray-800 text-white text-xs rounded shadow-lg opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all duration-200" ]
                                        [ text description
                                        , div [ class "absolute right-3 top-full -mt-1 border-4 border-transparent border-t-gray-800" ] []
                                        ]
                                    ]

                                -- Mobile tap tooltip icon (hidden on desktop)
                                , div
                                    [ class "sm:hidden inline-flex text-blue-500 cursor-pointer z-10"
                                    , Html.Events.stopPropagationOn "click"
                                        (D.succeed ( ToggleMobileTooltip (Just plan), True ))
                                    ]
                                    [ svg [ Svg.Attributes.width "16", Svg.Attributes.height "16", Svg.Attributes.viewBox "0 0 24 24", Svg.Attributes.fill "none" ]
                                        [ Svg.circle [ Svg.Attributes.cx "12", Svg.Attributes.cy "12", Svg.Attributes.r "10", Svg.Attributes.stroke "currentColor", Svg.Attributes.strokeWidth "2" ] []
                                        , Svg.path [ Svg.Attributes.d "M12 8v4", Svg.Attributes.stroke "currentColor", Svg.Attributes.strokeWidth "2", Svg.Attributes.strokeLinecap "round" ] []
                                        , Svg.path [ Svg.Attributes.d "M12 16h.01", Svg.Attributes.stroke "currentColor", Svg.Attributes.strokeWidth "2", Svg.Attributes.strokeLinecap "round" ] []
                                        ]
                                    ]
                                ]

                        Nothing ->
                            text ""
                    ]
                ]
            ]
        , div
            [ class
                ("overflow-hidden transition-all duration-300 ease-in-out "
                    ++ (if isSelected then
                            "max-h-[100px] opacity-100 mt-8 mb-8"

                        else
                            "max-h-0 opacity-0 mt-0 mb-0"
                       )
                )
            ]
            [ div [ class "flex justify-center" ]
                [ button
                    [ class "w-[200px] bg-[#03045E] text-white text-sm font-medium px-4 py-4 rounded-lg hover:bg-[#02034D] transition-colors"
                    , onClick (SelectPlan plan)
                    ]
                    [ text "See if I Qualify" ]
                ]
            ]
        ]


viewPlansSection : Model -> Html Msg
viewPlansSection model =
    let
        hasPlanG =
            not (List.isEmpty model.plans.planG)

        hasPlanN =
            not (List.isEmpty model.plans.planN)

        hasAnyPlans =
            hasPlanG || hasPlanN

        hasOriginalPlanNames =
            List.any (\p -> p.originalPlanName /= Nothing) model.plans.planG
                || List.any (\p -> p.originalPlanName /= Nothing) model.plans.planN

        stateSpecificNotice =
            if hasOriginalPlanNames then
                div [ class "bg-blue-50 border border-blue-200 rounded-lg p-4 mb-4" ]
                    [ div [ class "flex items-start gap-3" ]
                        [ svg [ Svg.Attributes.width "20", Svg.Attributes.height "20", Svg.Attributes.viewBox "0 0 20 20", Svg.Attributes.fill "none", Svg.Attributes.class "flex-shrink-0 mt-0.5" ]
                            [ path
                                [ Svg.Attributes.d "M10 0C4.477 0 0 4.477 0 10s4.477 10 10 10 10-4.477 10-10S15.523 0 10 0zm1 15H9v-2h2v2zm0-4H9V5h2v6z"
                                , Svg.Attributes.fill "#2563EB"
                                ]
                                []
                            ]
                        , div [ class "flex-1 space-y-2" ]
                            [ h4 [ class "text-sm font-semibold text-blue-900 mb-1" ]
                                [ text "State-Specific Plan Names" ]
                            , p [ class "text-sm text-blue-800" ]
                                [ text "Your state uses slightly different variations from standardized Medicare Supplement plans. The plans shown reflect these state-specific names and do include additional optional riders that may be available in your state. Premiums may vary accordingly."
                                ]
                            , p [ class "text-sm text-blue-800" ]
                                [ text "We would be happy to discuss these details further with you to help you find the best plan for your needs."
                                ]
                            ]
                        ]
                    ]

            else
                text ""

        planGHeader =
            case List.head model.plans.planG of
                Just firstPlanG ->
                    firstPlanG.originalPlanName
                        |> Maybe.map formatOriginalPlanName
                        |> Maybe.withDefault "Plan G"
                        |> (\name -> name ++ " Monthly Premiums")

                Nothing ->
                    "Plan G Monthly Premiums"

        planNHeader =
            case List.head model.plans.planN of
                Just firstPlanN ->
                    firstPlanN.originalPlanName
                        |> Maybe.map formatOriginalPlanName
                        |> Maybe.withDefault "Plan N"
                        |> (\name -> name ++ " Monthly Premiums")

                Nothing ->
                    "Plan N Monthly Premiums"
    in
    if not hasAnyPlans then
        -- No plans found, display a message with contact info and button
        div [ class "bg-white rounded-[10px] border border-[#DCE2E5] shadow-[0_1px_2px_rgba(16,24,40,0.05)] mt-6 p-6 sm:p-8 text-center" ]
            [ div [ class "w-12 h-12 rounded-full bg-[#F9F5FF] flex items-center justify-center mx-auto mb-4" ]
                [ svg [ Svg.Attributes.width "26", Svg.Attributes.height "26", Svg.Attributes.viewBox "0 0 24 24", Svg.Attributes.fill "none" ]
                    [ path
                        [ Svg.Attributes.d "M12 22C17.5228 22 22 17.5228 22 12C22 6.47715 17.5228 2 12 2C6.47715 2 2 6.47715 2 12C2 17.5228 6.47715 22 12 22Z"
                        , Svg.Attributes.stroke "#7F56D9"
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeLinejoin "round"
                        ]
                        []
                    , path
                        [ Svg.Attributes.d "M12 8V12"
                        , Svg.Attributes.stroke "#7F56D9"
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeLinejoin "round"
                        ]
                        []
                    , path
                        [ Svg.Attributes.d "M12 16H12.01"
                        , Svg.Attributes.stroke "#7F56D9"
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeLinejoin "round"
                        ]
                        []
                    ]
                ]
            , h3 [ class "text-lg sm:text-xl font-bold text-[#101828] -tracking-[0.02em] mb-2" ]
                [ text "We couldn't find plans for your specific criteria." ]
            , p [ class "text-sm sm:text-base text-[#667085] mb-4" ]
                [ text "Please give us a call at ", span [ class "font-semibold" ] [ text (getPhoneNumberForDisplay model |> formatPhoneNumber) ], text ". We're happy to help you find the best plan for your needs." ]
            , a
                [ href (getScheduleLink model)
                , class "whitespace-nowrap bg-[#03045E] text-white px-5 sm:px-4 py-3 sm:py-2 rounded-lg hover:bg-[#02034D] transition-colors text-sm sm:text-base w-full sm:w-auto text-center max-w-xs mx-auto block font-semibold"
                ]
                [ text "Connect With Us" ]
            ]

    else
        div [ class "flex flex-col gap-4 sm:gap-0" ]
            [ -- State-specific notice
              stateSpecificNotice

            -- Plan G Section - Desktop and Mobile
            , if hasPlanG then
                div [ class "bg-white rounded-[10px] border border-[#DCE2E5] shadow-[0_1px_2px_rgba(16,24,40,0.05)]" ]
                    [ -- Header (desktop only)
                      div [ class "hidden sm:flex px-4 sm:px-6 py-4 flex-row items-center justify-between border-b border-[#DCE2E5] bg-[#F9F5FF] rounded-t-[10px]" ]
                        [ div [ class "flex items-end gap-3" ]
                            [ h2 [ class "text-2xl font-extrabold -tracking-[0.04em] text-[#101828] leading-[1.2]" ] [ text "Recommended Plans for You" ]
                            , p [ class "text-[16px] font-medium text-[#667085] -tracking-[0.04em] leading-[1.2] pb-[2px]" ] [ text "Select one to see if you qualify" ]
                            ]
                        ]

                    -- Mobile header
                    , div [ class "block sm:hidden px-4 py-4 border-b border-[#DCE2E5] bg-[#F9F5FF] rounded-t-[10px]" ]
                        [ h2 [ class "text-2xl font-extrabold -tracking-[0.04em] text-[#101828] leading-[1.2]" ] [ text "Recommended Plans" ]
                        , p [ class "text-[16px] font-medium text-[#667085] -tracking-[0.04em] leading-[1.2]" ] [ text "Select one to continue" ]
                        ]

                    -- Plan G Section
                    , div [ class "px-3 sm:px-4 py-6 bg-white" ]
                        [ h3 [ class "text-xl font-extrabold -tracking-[0.02em] mb-6 text-[#101828]" ] [ text planGHeader ]
                        , div [ class "flex flex-wrap gap-8 justify-center sm:justify-start sm:pl-8" ]
                            (List.map (viewPlanCard model "G") (getTopPlans model model.plans.planG 3))
                        ]
                    ]

              else
                text ""

            -- Mobile video button (standalone between Plan G and Plan N)
            -- Only show if both G and N plans are available
            , if hasPlanG && hasPlanN then
                div [ class "block sm:hidden py-4 px-3" ]
                    [ div [ class "mx-auto max-w-[280px] bg-[#F9F5FF] rounded-[10px] p-4 flex flex-row items-center cursor-pointer gap-4", onClick OpenGvsNVideo ]
                        [ div [ class "w-[33px] h-[33px] rounded-full border border-[#03045E] flex items-center justify-center flex-shrink-0" ]
                            [ div [ class "w-0 h-0 border-t-[8px] border-t-transparent border-l-[12px] border-l-[#03045E] border-b-[8px] border-b-transparent ml-1" ] []
                            ]
                        , div [ class "flex flex-col items-start" ]
                            [ p [ class "text-[16px] font-bold text-[#03045E] -tracking-[0.03em] leading-[1.21] text-left" ] [ text "Learn About Plan G vs N" ]
                            , p [ class "text-[12px] text-[#667085] -tracking-[0.03em] leading-[1.21]" ] [ text "Watch the Video" ]
                            ]
                        ]
                    ]

              else
                text ""

            -- Plan N Section (separate container for mobile)
            , if hasPlanN then
                div [ class "bg-white rounded-[10px] border border-[#DCE2E5] shadow-[0_1px_2px_rgba(16,24,40,0.05)]" ]
                    [ -- Plan N Section Header (Mobile only)
                      div [ class "block sm:hidden px-4 py-4 border-b border-[#DCE2E5] bg-[#F9F5FF] rounded-t-[10px]" ]
                        [ h2 [ class "text-2xl font-extrabold -tracking-[0.04em] text-[#101828] leading-[1.2]" ] [ text "Plan N Options" ]
                        ]

                    -- Plan N Content
                    , div [ class "px-3 sm:px-4 py-6 bg-white" ]
                        [ h3 [ class "text-xl font-extrabold -tracking-[0.02em] mb-6 text-[#101828]" ] [ text planNHeader ]
                        , div [ class "flex flex-wrap gap-8 justify-center sm:justify-start sm:pl-8" ]
                            (List.map (viewPlanCard model "N") (getTopPlans model model.plans.planN 3))
                        ]
                    ]

              else
                text ""

            -- Medicare Advantage Off Ramp Section
            -- This will also be hidden if no plans were found initially by the top-level `if not hasAnyPlans`
            , viewMedicareAdvantageOffRamp model
            ]



-- Medicare Advantage Off Ramp component


viewMedicareAdvantageOffRamp : Model -> Html Msg
viewMedicareAdvantageOffRamp model =
    div [ class "bg-white rounded-[10px] border border-[#DCE2E5] shadow-[0_1px_2px_rgba(16,24,40,0.05)] mt-6" ]
        [ div [ class "px-4 sm:px-6 py-4 sm:py-5 flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4 sm:gap-0" ]
            [ div [ class "flex items-start gap-4" ]
                [ -- Icon for the section
                  div [ class "w-12 h-12 rounded-full bg-[#F9F5FF] flex items-center justify-center flex-shrink-0" ]
                    [ svg [ Svg.Attributes.width "26", Svg.Attributes.height "26", Svg.Attributes.viewBox "0 0 24 24", Svg.Attributes.fill "none" ]
                        [ path
                            [ Svg.Attributes.d "M12 22C17.5228 22 22 17.5228 22 12C22 6.47715 17.5228 2 12 2C6.47715 2 2 6.47715 2 12C2 17.5228 6.47715 22 12 22Z"
                            , Svg.Attributes.stroke "#7F56D9"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            ]
                            []
                        , path
                            [ Svg.Attributes.d "M12 8V12"
                            , Svg.Attributes.stroke "#7F56D9"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            ]
                            []
                        , path
                            [ Svg.Attributes.d "M12 16H12.01"
                            , Svg.Attributes.stroke "#7F56D9"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            ]
                            []
                        ]
                    ]
                , div [ class "flex flex-col" ]
                    [ h3 [ class "text-lg sm:text-xl font-bold text-[#101828] -tracking-[0.02em]" ]
                        [ text "Looking to dramatically lower your monthly costs?" ]
                    , p [ class "text-sm sm:text-base text-[#667085]" ]
                        [ text "Medicare Advantage plans offer $0 monthly premiums with drug coverage included. These plans have improved a lot over recent years, with more benefits and flexibility than ever. If saving money is your priority, let's find the right plan for you." ]
                    ]
                ]
            , div [ class "sm:ml-4 flex justify-center sm:justify-start" ]
                [ a
                    [ href
                        (case model.quoteId of
                            Just id ->
                                let
                                    orgQ =
                                        String.split "-" id
                                            |> List.head
                                            |> Maybe.map
                                                (\org ->
                                                    "?org=" ++ org ++ "&"
                                                )
                                            |> Maybe.withDefault "?"
                                in
                                "/schedule" ++ orgQ ++ "id=" ++ id ++ "&status=decline"

                            Nothing ->
                                "/contact"
                        )
                    , class "whitespace-nowrap bg-[#03045E] text-white px-5 sm:px-4 py-3 sm:py-2 rounded-lg hover:bg-[#02034D] transition-colors text-sm sm:text-base w-full sm:w-auto text-center"
                    ]
                    [ text "Explore Options" ]
                ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Quote - Medicare Max"
    , body =
        [ viewHeader model.orgLogo model.orgName
        , div [ class "bg-white min-h-screen pb-12 scroll-smooth" ]
            [ if model.loadingContact || model.isLoading then
                viewLoading

              else
                div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-3 space-y-8 sm:space-y-10" ]
                    [ -- Personal Quote Card
                      viewPersonalInfo model

                    -- Plans Section (mobile video button moved inside viewPlansSection)
                    , viewPlansSection model
                    ]
            ]
        , viewGvsNModal model
        , viewMobileTooltip model
        ]
    }


viewLoading : Html Msg
viewLoading =
    div [ class "fixed inset-0 bg-white flex flex-col items-center justify-center gap-4 text-center" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-purple-600 border-t-transparent" ] []
        , p [ class "text-center text-lg font-medium text-gray-600" ]
            [ text "Loading your personalized quote..." ]
        , div
            [ class "sm:hidden opacity-0 transition-opacity duration-500 delay-[10000ms] mt-6 px-4 max-w-xs text-center text-sm text-gray-500"
            , style "animation" "fadeIn 0.5s 5s forwards"
            ]
            [ text "If page fails to load, please try refreshing or opening in your device's default browser." ]
        , Html.node "style"
            []
            [ text """
                @keyframes fadeIn {
                    from { opacity: 0; }
                    to { opacity: 1; }
                }
              """
            ]
        ]


viewError : String -> Maybe String -> Html Msg
viewError error orgSlug =
    div [ class "fixed inset-0 bg-white flex flex-col items-center justify-center gap-6 text-center px-4" ]
        [ div [ class "max-w-lg" ]
            [ h1 [ class "text-2xl font-semibold text-[#1A1A1A] mb-4" ]
                [ text "Unable to Load Quote" ]
            , p [ class "text-lg text-gray-600 mb-8" ]
                [ text "This quote link appears to be invalid or has expired. Please get a new quote to continue." ]
            , case orgSlug of
                Just slug ->
                    a
                        [ href ("/self-onboarding/" ++ slug)
                        , class "inline-block bg-[#03045E] text-white text-sm font-medium px-6 py-3 rounded hover:bg-[#02034D] transition-colors"
                        ]
                        [ text "Get a New Quote" ]

                Nothing ->
                    text ""
            ]
        ]


viewGvsNModal : Model -> Html Msg
viewGvsNModal model =
    if model.showGvsNVideo then
        div [ class "fixed inset-0 bg-black/30 flex items-center justify-center z-50 p-4 backdrop-blur-sm" ]
            [ div [ class "bg-white rounded-lg p-4 pt-8 sm:p-8 w-[95%] h-auto max-w-5xl mx-auto shadow-lg relative" ]
                [ button
                    [ class "absolute top-4 right-4 text-gray-500 hover:text-gray-700 text-xl p-1"
                    , onClick CloseGvsNVideo
                    ]
                    [ text "×" ]
                , h2 [ class "text-xl sm:text-2xl font-bold mb-1 sm:mb-2 text-center" ] [ text "Plan G vs. Plan N" ]
                , p [ class "mb-2 text-center text-sm sm:text-base" ] [ text "Watch this video to learn about the key differences between plans" ]
                , div [ class "w-full mx-auto my-auto h-[450px] sm:h-[500px] mt-2 sm:mt-4 sm:max-w-[90%]" ]
                    [ div [ class "w-full h-full" ]
                        [ iframe
                            -- [ src "https://player.vimeo.com/video/1075091871?autoplay=0&title=0&byline=0&portrait=0&responsive=1" -- education example
                            [ src "https://player.vimeo.com/video/1089216242?autoplay=0&title=0&byline=0&portrait=0&responsive=1"
                            , class "w-full h-full"
                            , attribute "frameborder" "0"
                            , attribute "allow" "autoplay; fullscreen; picture-in-picture"
                            , attribute "allowfullscreen" ""
                            ]
                            []
                        ]
                    ]
                ]
            ]

    else
        text ""


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    let
        cleanPhone =
            String.filter (\c -> c >= '0' && c <= '9') phone
    in
    String.slice 0 3 cleanPhone ++ "-" ++ String.slice 3 6 cleanPhone ++ "-" ++ String.slice 6 10 cleanPhone


contactResponseDecoder : Decoder ContactResponse
contactResponseDecoder =
    D.succeed ContactResponse
        |> Pipeline.optional "contact" (D.oneOf [ D.map Just contactDecoder, D.succeed Nothing ]) Nothing
        |> Pipeline.required "agent" agentDecoder
        |> Pipeline.required "orgSlug" D.string
        |> Pipeline.required "orgName" D.string
        |> Pipeline.required "orgLogo" (D.nullable D.string)
        |> Pipeline.optional "orgPhone" (D.nullable D.string) Nothing
        |> Pipeline.required "orgSignature" D.bool
        |> Pipeline.optional "orgSignatureText" (D.nullable D.string) Nothing
        |> Pipeline.required "carrierContracts" (D.list carrierDecoder)
        |> Pipeline.required "forceOrgSenderDetails" D.bool


contactDecoder : Decoder Contact
contactDecoder =
    D.succeed Contact
        |> Pipeline.required "id" D.int
        |> Pipeline.required "firstName" D.string
        |> Pipeline.required "lastName" D.string
        |> Pipeline.required "email" D.string
        |> Pipeline.required "phoneNumber" D.string
        |> Pipeline.required "age" D.int
        |> Pipeline.required "gender" D.string
        |> Pipeline.required "tobacco" D.bool
        |> Pipeline.required "state" D.string
        |> Pipeline.required "zipCode" D.string
        |> Pipeline.optional "county" (D.nullable D.string) Nothing
        |> Pipeline.optional "currentCarrier" (D.nullable D.string) Nothing
        |> Pipeline.optional "planType" (D.nullable D.string) Nothing


agentDecoder : Decoder Agent
agentDecoder =
    D.succeed Agent
        |> Pipeline.required "firstName" D.string
        |> Pipeline.required "lastName" D.string
        |> Pipeline.required "email" D.string
        |> Pipeline.required "phone" D.string
        |> Pipeline.optional "signature" D.string ""
        |> Pipeline.optional "useOrgSenderDetails" D.bool True
        |> Pipeline.optional "bookingLink" D.string ""


locationUpdateResponseDecoder : Decoder LocationUpdateResponse
locationUpdateResponseDecoder =
    D.map4 LocationUpdateResponse
        (D.field "success" D.bool)
        (D.field "zipCode" D.string)
        (D.field "state" D.string)
        (D.field "counties" (D.list D.string))



-- Helper function to format the date in a more readable way


formatEffectiveDate : String -> String
formatEffectiveDate isoDate =
    case Date.fromIsoString isoDate of
        Ok date ->
            Date.format "MMMM 1, yyyy" date

        Err _ ->
            isoDate



-- Helper function to get next N months of effective dates


getNextEffectiveDates : Date -> Int -> List String
getNextEffectiveDates currentDate count =
    List.range 0 (count - 1)
        |> List.map
            (\n ->
                currentDate
                    |> Date.add Date.Months n
                    |> Date.add Date.Months 1
                    |> Date.floor Date.Month
                    |> Date.toIsoString
            )



-- Mobile tooltip overlay


viewMobileTooltip : Model -> Html Msg
viewMobileTooltip model =
    case model.activeTooltipPlan of
        Just tooltipPlan ->
            case tooltipPlan.discountDescription of
                Just description ->
                    div
                        [ class "sm:hidden fixed inset-0 z-50 bg-black/50 flex items-center justify-center p-4"
                        , onClick (ToggleMobileTooltip Nothing)
                        ]
                        [ div
                            [ class "bg-white rounded-lg p-4 max-w-xs w-full shadow-lg text-sm relative"
                            , Html.Events.stopPropagationOn "click" (D.succeed ( NoOp, True ))
                            ]
                            [ button
                                [ class "absolute top-2 right-2 text-gray-500"
                                , onClick (ToggleMobileTooltip Nothing)
                                ]
                                [ text "×" ]
                            , div [ class "font-bold text-gray-800 text-base pb-2" ] [ text (tooltipPlan.name ++ " Discount") ]

                            --, div [ class "text-sm text-gray-700" ] [ text ("Plan " ++ tooltipPlan.planType ++ " Discount") ]
                            , div [ class "pt-3" ] [ text description ]
                            ]
                        ]

                Nothing ->
                    -- If no description, close the tooltip
                    div [ onClick (ToggleMobileTooltip Nothing) ] []

        Nothing ->
            -- No active tooltip
            text ""



-- Helper function to format state-specific plan names for display


formatOriginalPlanName : String -> String
formatOriginalPlanName originalName =
    case String.toUpper originalName of
        "MN_BASIC" ->
            "Minnesota Basic Plan"

        "MN_EXTB" ->
            "Minnesota Extended Basic Plan"

        "WI_BASE" ->
            "Wisconsin Basic Plan"

        "WI_HDED" ->
            "Wisconsin High Deductible Plan"

        "MA_CORE" ->
            "Massachusetts Core Plan"

        "MA_SUPP1" ->
            "Massachusetts Supplement 1 Plan"

        _ ->
            originalName



-- Helper to determine which phone number to display based on org/agent settings


getPhoneNumberForDisplay : Model -> String
getPhoneNumberForDisplay model =
    let
        effectiveUseOrg =
            if model.forceOrgSenderDetails then
                True

            else
                case model.agent of
                    Just agent ->
                        agent.useOrgSenderDetails

                    Nothing ->
                        model.useOrg
    in
    if effectiveUseOrg then
        Maybe.withDefault "" model.orgPhone

    else
        case model.agent of
            Just agent ->
                agent.phone

            Nothing ->
                ""



-- Helper to get the schedule/contact link


getScheduleLink : Model -> String
getScheduleLink model =
    case model.quoteId of
        Just id ->
            let
                orgQ =
                    String.split "-" id
                        |> List.head
                        |> Maybe.map
                            (\org ->
                                "?org=" ++ org ++ "&"
                            )
                        |> Maybe.withDefault "?"
            in
            "/schedule" ++ orgQ ++ "id=" ++ id ++ "&status=decline"

        -- Using "decline" status as per MA off-ramp button
        Nothing ->
            "/contact"
