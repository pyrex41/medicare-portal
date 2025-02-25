module Compare exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import CarrierNaic exposing (carrierToNaics, carrierToString, naicToCarrier, stringToCarrier)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Url exposing (Url)
import Url.Parser.Query as Query



-- TYPES


type PlanType
    = PlanG
    | PlanN


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
    }


type alias Plans =
    { planG : List Plan
    , planN : List Plan
    }


type alias Model =
    { isLoading : Bool
    , error : Maybe String
    , plans : Plans
    , state : String
    , county : String
    , zip : String
    , age : Int
    , gender : String
    , tobacco : Bool
    , selectedPlanType : PlanType
    , showReviewVideo : Bool
    , showQualificationVideo : Bool
    , showGvsNVideo : Bool
    , showFAQ : Bool
    , currentCardIndex : Int
    , showRatesVideo : Bool
    , key : Nav.Key
    , showDiscount : Bool
    , currentCarrier : Maybe String
    , dateOfBirth : String
    , quoteId : Maybe String
    , orgSettings : Maybe Settings
    , settingsLoaded : Bool
    }


type alias Settings =
    { stateLicenses : List String
    , carrierContracts : List String
    , stateCarrierSettings : List StateCarrierSetting
    , allowAgentSettings : Bool
    , emailSendBirthday : Bool
    , emailSendPolicyAnniversary : Bool
    , emailSendAep : Bool
    , smartSendEnabled : Bool
    }


type alias StateCarrierSetting =
    { state : String
    , carrier : String
    , active : Bool
    , targetGI : Bool
    }


type Msg
    = GotPlans (Result Http.Error Plans)
    | TogglePlanType
    | SelectPlan Plan
    | CloseReviewVideo
    | OpenGvsNVideo
    | CloseGvsNVideo
    | ShowQualificationVideo
    | CloseQualificationVideo
    | ShowFAQ
    | CloseFAQ
    | NextCard
    | PreviousCard
    | CloseRatesVideo
    | NavigateTo String
    | ToggleDiscount
    | GotOrgSettings (Result Http.Error Settings)


type alias Flags =
    { state : String
    , zip : String
    , county : String
    , gender : String
    , tobacco : Bool
    , age : Int
    , planType : String
    , currentCarrier : Maybe String
    , dateOfBirth : String
    , quoteId : Maybe String
    }



-- INIT


init : Flags -> Nav.Key -> ( Model, Cmd Msg )
init flags key =
    let
        defaultFlags =
            { state = "TX"
            , zip = "75201"
            , county = "Dallas"
            , gender = "Male"
            , tobacco = False
            , age = 65
            , planType = "G"
            , currentCarrier = Nothing
            , dateOfBirth = ""
            , quoteId = Nothing
            }

        model =
            { isLoading = True
            , error = Nothing
            , plans = { planG = [], planN = [] }
            , state = flags.state
            , county = flags.county
            , zip = flags.zip
            , age = flags.age
            , gender = flags.gender
            , tobacco = flags.tobacco
            , selectedPlanType = defaultPlanType flags
            , showReviewVideo = False
            , showQualificationVideo = False
            , showGvsNVideo = False
            , showFAQ = False
            , currentCardIndex = 0
            , showRatesVideo = False
            , key = key
            , showDiscount = False
            , currentCarrier = flags.currentCarrier
            , dateOfBirth = flags.dateOfBirth
            , quoteId = flags.quoteId
            , orgSettings = Nothing
            , settingsLoaded = False
            }
    in
    ( model
    , Http.get
        { url = "/api/settings"
        , expect = Http.expectJson GotOrgSettings settingsDecoder
        }
    )


defaultPlanType : Flags -> PlanType
defaultPlanType flags =
    case flags.planType of
        "G" ->
            PlanG

        "N" ->
            PlanN

        _ ->
            PlanG



-- HTTP


fetchPlans : Flags -> Model -> Cmd Msg
fetchPlans flags model =
    let
        _ =
            Debug.log "Attempting to fetch plans"
                { hasSettings = model.orgSettings /= Nothing
                , settingsLoaded = model.settingsLoaded
                }
    in
    if model.settingsLoaded then
        Http.request
            { method = "POST"
            , headers = []
            , url = "/api/quotes"
            , body = Http.jsonBody (buildPlansBody flags)
            , expect = Http.expectJson GotPlans (plansDecoder model)
            , timeout = Nothing
            , tracker = Nothing
            }

    else
        let
            _ =
                Debug.log "Skipping plans fetch - settings not loaded" ()
        in
        Cmd.none


buildPlansBody : Flags -> E.Value
buildPlansBody flags =
    E.object
        [ ( "zip_code", E.string flags.zip )
        , ( "state", E.string flags.state )
        , ( "county", E.string flags.county )
        , ( "age", E.int flags.age )
        , ( "gender"
          , E.string
                (if flags.gender == "Male" then
                    "M"

                 else
                    "F"
                )
          )
        , ( "tobacco", E.bool flags.tobacco )
        , ( "plans", E.list E.string [ "G", "N" ] )
        , ( "carriers", E.string "supported" )
        ]



-- DECODERS


plansDecoder : Model -> Decoder Plans
plansDecoder model =
    D.list quoteResponseDecoder
        |> D.map (\responses -> groupQuotesByPlan responses model)


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
    }


quoteResponseDecoder : Decoder QuoteResponse
quoteResponseDecoder =
    D.map4 QuoteResponse
        (D.field "naic" D.string)
        (D.field "group" D.int)
        (D.field "company_name" D.string)
        (D.field "quotes" (D.list quoteDataDecoder))


quoteDataDecoder : Decoder QuoteData
quoteDataDecoder =
    D.map7 QuoteData
        (D.field "rate" D.float)
        (D.field "discount_rate" D.float)
        (D.field "discount_category" (D.nullable D.string))
        (D.field "age" D.int)
        (D.field "gender" D.string)
        (D.field "plan" D.string)
        (D.field "tobacco" D.int)


groupQuotesByPlan : List QuoteResponse -> Model -> Plans
groupQuotesByPlan responses model =
    let
        _ =
            Debug.log "Raw responses from API" responses

        _ =
            Debug.log "Settings state when grouping quotes"
                { settingsLoaded = model.settingsLoaded
                , hasOrgSettings = model.orgSettings /= Nothing
                , carrierContracts = Maybe.map .carrierContracts model.orgSettings
                }

        isCarrierSupported : String -> Bool
        isCarrierSupported naic =
            let
                _ =
                    Debug.log "Checking carrier support for NAIC" naic

                carrierResult =
                    naicToCarrier naic
                        |> Debug.log "naicToCarrier result"

                _ =
                    Debug.log "orgSettings" model.orgSettings
            in
            case ( carrierResult, model.orgSettings ) of
                ( Just carrier, Just settings ) ->
                    let
                        carrierStr =
                            carrierToString carrier

                        _ =
                            Debug.log "Checking carrier support"
                                { naic = naic
                                , carrier = carrierStr
                                , carrierContracts = settings.carrierContracts
                                , supported = List.member carrierStr settings.carrierContracts
                                }
                    in
                    List.member carrierStr settings.carrierContracts

                _ ->
                    let
                        _ =
                            Debug.log "Carrier not supported - fallthrough case"
                                { naic = naic
                                , carrierResult = carrierResult
                                , hasSettings = model.orgSettings /= Nothing
                                }
                    in
                    False

        convertToPlan : QuoteResponse -> QuoteData -> Plan
        convertToPlan response quote =
            let
                carrierImagePath =
                    case naicToCarrier response.naic of
                        Just carrier ->
                            "/images/" ++ carrierToString carrier ++ ".svg"

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
            , state = model.state
            , tobacco = quote.tobacco == 1
            , coverageSummary =
                if String.toUpper quote.plan == "G" then
                    planGCoverageList

                else
                    planNCoverageList
            }

        allQuotes =
            List.concatMap
                (\response ->
                    if isCarrierSupported response.naic then
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
                            (response.quotes |> Debug.log "Response.quotes")

                    else
                        []
                )
                responses

        _ =
            Debug.log "All quotes after conversion and carrier filtering" allQuotes

        planG =
            List.filter (\q -> String.toUpper q.planType == "G") allQuotes
                |> List.sortBy .price

        planN =
            List.filter (\q -> String.toUpper q.planType == "N") allQuotes
                |> List.sortBy .price

        _ =
            Debug.log "Plan G quotes" planG

        _ =
            Debug.log "Plan N quotes" planN

        result =
            { planG = planG
            , planN = planN
            }

        _ =
            Debug.log "Final result" result
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
        GotOrgSettings (Ok settings) ->
            let
                flags =
                    { state = model.state
                    , zip = model.zip
                    , county = model.county
                    , age = model.age
                    , gender = model.gender
                    , tobacco = model.tobacco
                    , planType =
                        case model.selectedPlanType of
                            PlanG ->
                                "G"

                            PlanN ->
                                "N"
                    , currentCarrier = model.currentCarrier
                    , dateOfBirth = model.dateOfBirth
                    , quoteId = model.quoteId
                    }

                _ =
                    Debug.log "Settings loaded, fetching quotes" settings

                updatedModel =
                    { model | orgSettings = Just settings, settingsLoaded = True }
            in
            ( updatedModel
            , fetchPlans flags updatedModel
            )

        GotOrgSettings (Err _) ->
            ( { model | error = Just "Failed to load organization settings" }
            , Cmd.none
            )

        ToggleDiscount ->
            ( { model | showDiscount = not model.showDiscount }
            , Cmd.none
            )

        GotPlans result ->
            case result of
                Ok plans ->
                    let
                        _ =
                            Debug.log "Received plans" plans
                    in
                    ( { model
                        | plans = plans
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            Debug.log "Error getting plans" (httpErrorToString error)
                    in
                    ( { model
                        | error = Just (httpErrorToString error)
                        , isLoading = False
                      }
                    , Cmd.none
                    )

        TogglePlanType ->
            ( { model
                | selectedPlanType = togglePlanType model.selectedPlanType
                , currentCardIndex = 0
              }
            , Cmd.none
            )

        SelectPlan plan ->
            ( { model | showQualificationVideo = True }
            , Nav.pushUrl model.key
                (case model.quoteId of
                    Just id ->
                        "/eligibility?id=" ++ id

                    Nothing ->
                        "/eligibility"
                )
            )

        CloseReviewVideo ->
            ( { model | showReviewVideo = False }
            , Cmd.none
            )

        OpenGvsNVideo ->
            ( { model | showGvsNVideo = True }, Cmd.none )

        CloseGvsNVideo ->
            ( { model | showGvsNVideo = False }, Cmd.none )

        ShowQualificationVideo ->
            ( { model | showQualificationVideo = True }
            , Cmd.none
            )

        CloseQualificationVideo ->
            ( { model | showQualificationVideo = False }
            , Nav.pushUrl model.key "/eligibility"
            )

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
            case model.selectedPlanType of
                PlanG ->
                    model.plans.planG

                PlanN ->
                    model.plans.planN

        _ =
            Debug.log "Current carrier from URL" model.currentCarrier

        carrierNaics =
            model.currentCarrier
                |> Maybe.andThen stringToCarrier
                |> Maybe.map carrierToNaics
                |> Debug.log "Carrier NAICs to filter out"

        _ =
            Debug.log "All plans before filtering" plans

        filteredPlans =
            case carrierNaics of
                Just naicList ->
                    List.filter
                        (\plan ->
                            let
                                _ =
                                    Debug.log ("Checking plan NAIC " ++ plan.naic ++ " for " ++ plan.name)
                                        (not (List.member plan.naic naicList))
                            in
                            not (List.member plan.naic naicList)
                        )
                        plans

                Nothing ->
                    plans

        _ =
            Debug.log "Filtered plans before sorting" filteredPlans

        sortedAndLimited =
            List.sortBy .price filteredPlans
                |> List.take 3
                |> Debug.log "Final selected plans"
    in
    sortedAndLimited



-- Only take the three cheapest plans


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
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Compare Medicare Plans - Medicare Max"
    , body =
        [ div [ class "container mx-auto px-4 py-8" ]
            [ if model.isLoading then
                viewLoading

              else
                case model.error of
                    Just error ->
                        viewError error

                    Nothing ->
                        div [ class "flex flex-col gap-6 text-center mx-auto max-w-3xl" ]
                            [ h1 [ class "text-2xl font-semibold text-[#1A1A1A] mb-2" ]
                                [ text "Select a Plan from these recommendations" ]
                            , div [ class "mt-6" ]
                                [ viewPlanToggle model ]
                            , div [ class "flex justify-center mt-4" ]
                                [ viewPillButton "Learn About Plan G vs. Plan N" True OpenGvsNVideo ]
                            , viewCarousel model
                            ]
            ]
        , viewGvsNModal model
        , viewQualificationModal model
        , viewRatesModal model
        ]
    }


viewLoading : Html Msg
viewLoading =
    div [ class "flex flex-col items-center justify-center gap-4 text-center min-h-[400px]" ]
        [ div [ class "animate-spin text-brand w-10 h-10 border-4 border-current border-t-transparent rounded-full" ] []
        , p [ class "text-center text-lg font-medium text-neutral-600" ]
            [ text "Searching plans..." ]
        ]


viewError : String -> Html Msg
viewError error =
    div [ class "text-center text-xl font-bold text-red-600 mt-8 p-4 bg-red-50 rounded-lg" ]
        [ text error ]


viewPlanToggle : Model -> Html Msg
viewPlanToggle model =
    let
        ( planGClass, planNClass ) =
            case model.selectedPlanType of
                PlanG ->
                    ( "font-medium text-[#1A1A1A]", "text-[#666666]" )

                PlanN ->
                    ( "text-[#666666]", "font-medium text-[#1A1A1A]" )
    in
    div [ class "flex justify-center items-center gap-3 text-base" ]
        [ span [ class planGClass ] [ text (getPlanGName model) ]
        , button
            [ onClick TogglePlanType
            , class "w-12 h-6 bg-[#0066FF] rounded-full relative"
            ]
            [ div
                [ class "absolute top-0.5 left-0.5 bg-white w-5 h-5 rounded-full shadow-sm transform duration-300 ease-in-out"
                , class
                    (if model.selectedPlanType == PlanN then
                        "translate-x-6"

                     else
                        "translate-x-0"
                    )
                ]
                []
            ]
        , span [ class planNClass ] [ text (getPlanNName model) ]
        ]


getPlanGName : Model -> String
getPlanGName model =
    case List.head model.plans.planG of
        Just firstPlan ->
            if firstPlan.state == "MN" || firstPlan.state == "WI" then
                "Extended"

            else if firstPlan.state == "MA" then
                "Expanded"

            else
                "Plan G"

        Nothing ->
            "Plan G"


getPlanNName : Model -> String
getPlanNName model =
    case List.head model.plans.planN of
        Just firstPlan ->
            case firstPlan.state of
                "MN" ->
                    "Basic"

                "WI" ->
                    ""

                "MA" ->
                    "Core"

                _ ->
                    "Plan N"

        Nothing ->
            "Plan N"


viewCarousel : Model -> Html Msg
viewCarousel model =
    let
        currentPlans =
            getSelectedPlans model

        totalCards =
            List.length currentPlans
    in
    div [ class "relative w-full max-w-[640px] mx-auto mt-8" ]
        [ div [ class "absolute left-1/2 transform -translate-x-1/2 -top-6 z-10" ]
            [ viewCarouselDots model totalCards ]
        , div [ class "overflow-hidden" ]
            [ div
                [ class "flex transition-transform duration-300 ease-in-out"
                , style "transform" ("translateX(-" ++ String.fromInt (model.currentCardIndex * 100) ++ "%)")
                ]
                (List.map (viewPlanCard model) currentPlans)
            ]
        , viewCarouselControls model totalCards
        , div [ class "mt-8 text-center text-sm text-[#666666] max-w-lg mx-auto" ]
            [ p [ class "mb-2" ]
                [ text "These are the three least expensive plans available. All Medicare Supplement plans of the same letter (G or N) provide identical coverage, as mandated by federal law." ]
            , p []
                [ text "Our recommendation: Choose an established insurance company with a strong financial rating, then select their most affordable plan since the coverage will be identical to other companies offering the same plan letter." ]
            ]
        ]


viewCarouselDots : Model -> Int -> Html Msg
viewCarouselDots model totalCards =
    div [ class "flex space-x-3" ]
        (List.range 0 (totalCards - 1)
            |> List.map
                (\index ->
                    div
                        [ class "w-3 h-3 rounded-full transition-colors duration-200"
                        , class
                            (if index == model.currentCardIndex then
                                "bg-[#0066FF]"

                             else
                                "bg-[#E5E5E5]"
                            )
                        ]
                        []
                )
        )


viewCarouselControls : Model -> Int -> Html Msg
viewCarouselControls model totalCards =
    div [ class "absolute w-full flex justify-between items-center", style "top" "50%" ]
        [ button
            [ class "bg-[#F5F8FF] w-10 h-10 rounded-lg shadow-sm -ml-5 flex justify-center items-center disabled:opacity-50 disabled:cursor-not-allowed hover:bg-[#E5EFFF] transition-colors"
            , onClick PreviousCard
            , disabled (model.currentCardIndex == 0)
            ]
            [ text "←" ]
        , button
            [ class "bg-[#F5F8FF] w-10 h-10 rounded-lg shadow-sm -mr-5 flex justify-center items-center disabled:opacity-50 disabled:cursor-not-allowed hover:bg-[#E5EFFF] transition-colors"
            , onClick NextCard
            , disabled (model.currentCardIndex == totalCards - 1)
            ]
            [ text "→" ]
        ]


viewPlanCard : Model -> Plan -> Html Msg
viewPlanCard model plan =
    let
        displayPrice =
            if model.showDiscount then
                plan.priceDiscount

            else
                plan.price
    in
    div [ class "flex-shrink-0 w-full px-4 relative" ]
        [ div [ class "bg-white rounded-2xl shadow-sm p-8" ]
            [ div [ class "mb-8 flex justify-center items-center h-16" ]
                [ img [ src plan.image, alt (plan.name ++ " logo"), class "h-full object-contain" ] [] ]
            , div [ class "text-center mb-6" ]
                [ p [ class "text-[#1A1A1A]" ]
                    [ span [ class "text-[48px] font-bold leading-none" ]
                        [ text ("$" ++ String.fromFloat displayPrice) ]
                    , span [ class "text-lg text-[#666666] ml-1" ] [ text "/mo" ]
                    ]
                ]
            , div [ class "mb-6" ]
                [ label [ class "flex items-center justify-center text-sm text-[#666666] gap-2" ]
                    [ input
                        [ type_ "checkbox"
                        , class "w-4 h-4 rounded border-gray-300 text-[#0066FF] focus:ring-[#0066FF]"
                        , checked model.showDiscount
                        , onClick ToggleDiscount
                        ]
                        []
                    , text ("Apply " ++ calculateDiscount plan ++ "% Household Discount")
                    ]
                ]
            , button
                [ onClick (SelectPlan plan)
                , class "w-full bg-[#7C3AED] text-white py-4 px-4 rounded-lg hover:bg-[#6D28D9] transition-colors mb-8 font-medium text-base"
                ]
                [ text "See If I Qualify" ]
            , div [ class "border-t border-[#E5E5E5] pt-6" ]
                [ h3 [ class "font-medium text-base text-[#1A1A1A] text-left mb-4" ] [ text "GAPS Plan G Covers:" ]
                , ul [ class "space-y-3" ]
                    (List.map viewCoverageItem plan.coverageSummary)
                ]
            ]
        ]


calculateDiscount : Plan -> String
calculateDiscount plan =
    let
        discount =
            plan.priceDiscount / plan.price
    in
    (100 - (discount * 100 |> round)) |> String.fromInt


viewCoverageItem : CoverageItem -> Html Msg
viewCoverageItem item =
    li [ class "flex flex-col" ]
        [ div [ class "flex justify-between items-center" ]
            [ span [ class "text-sm text-left font-medium text-neutral-700" ] [ text item.name ]
            , div [ class "flex items-center" ]
                [ if item.percentageCovered == 0 then
                    span [ class "bg-medicare-danger-light text-medicare-danger text-xs font-medium px-2.5 py-1 rounded-full whitespace-nowrap" ]
                        [ text "NOT COVERED" ]

                  else if item.percentageCovered == 100 then
                    span [ class "bg-medicare-success-light text-medicare-success text-xs font-medium px-2.5 py-1 rounded-full" ]
                        [ text "COVERED" ]

                  else
                    span [ class "bg-medicare-success-light text-medicare-success text-xs font-medium px-2.5 py-1 rounded-full whitespace-nowrap" ]
                        [ text (String.fromInt item.percentageCovered ++ "% COVERED") ]
                ]
            ]
        , case item.note of
            Just noteText ->
                p [ class "text-xs text-neutral-500 mt-1 text-right" ] [ text noteText ]

            Nothing ->
                text ""
        ]


viewPillButton : String -> Bool -> Msg -> Html Msg
viewPillButton label isVideo msg =
    button
        [ class "mx-auto bg-white text-brand px-4 py-2 rounded-full border border-brand hover:bg-brand/5 transition-colors flex items-center justify-center gap-2"
        , onClick msg
        ]
        [ if isVideo then
            div [ class "flex items-center justify-center gap-1" ]
                [ text "▶"
                , span [ class "text-xs" ] [ text "Video" ]
                ]

          else
            text ""
        , text label
        ]


viewGvsNModal : Model -> Html Msg
viewGvsNModal model =
    if model.showGvsNVideo then
        div [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4" ]
            [ div [ class "bg-white rounded-lg p-8 w-[95%] max-w-5xl mx-auto flex flex-col items-center relative" ]
                [ button
                    [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700"
                    , onClick CloseGvsNVideo
                    ]
                    [ text "×" ]
                , h2 [ class "text-2xl font-bold mb-4 text-center" ] [ text "Plan G vs. Plan N" ]
                , p [ class "mb-4 text-center" ] [ text "Understanding the differences" ]
                , p [ class "mb-4 text-center" ] [ text "Watch this video to learn about the key differences between Plan G and Plan N" ]
                , div [ class "w-full max-w-3xl mx-auto" ]
                    [ div [ class "relative", style "padding-top" "125%" ]
                        [ iframe
                            [ src "https://player.vimeo.com/video/1018402330?autoplay=0&title=0&byline=0&portrait=0&responsive=1"
                            , class "w-full h-full absolute top-0 left-0"
                            , attribute "frameborder" "0"
                            , attribute "allow" "autoplay; fullscreen; picture-in-picture"
                            , attribute "allowfullscreen" ""
                            ]
                            []
                        ]
                    ]
                , button
                    [ class "bg-med-green-500 text-white px-6 py-2 rounded hover:bg-med-green-600 mt-4"
                    , onClick CloseGvsNVideo
                    ]
                    [ text "Continue" ]
                ]
            ]

    else
        text ""


viewQualificationModal : Model -> Html Msg
viewQualificationModal model =
    if model.showQualificationVideo then
        div [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4" ]
            [ div [ class "bg-white rounded-lg p-8 w-[95%] max-w-5xl mx-auto flex flex-col items-center relative" ]
                [ button
                    [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700"
                    , onClick CloseQualificationVideo
                    ]
                    [ text "×" ]
                , h1 [ class "text-2xl font-bold mb-4 text-center" ] [ text "Great Choice!" ]
                , h2 [ class "text-xl font-bold mb-4 text-center" ] [ text "Now let's see if you qualify" ]
                , p [ class "mb-4 text-center" ] [ text "Watch this video to understand the process of qualifying for the plan you selected" ]
                , div [ class "w-full max-w-3xl mx-auto" ]
                    [ div [ class "relative", style "padding-top" "125%" ]
                        [ iframe
                            [ src "https://player.vimeo.com/video/1018421414?autoplay=0&title=0&byline=0&portrait=0&responsive=1"
                            , class "w-full h-full absolute top-0 left-0"
                            , attribute "frameborder" "0"
                            , attribute "allow" "autoplay; fullscreen; picture-in-picture"
                            , attribute "allowfullscreen" ""
                            ]
                            []
                        ]
                    ]
                , button
                    [ class "bg-med-green-500 text-white px-6 py-2 rounded hover:bg-med-green-600 mt-4"
                    , onClick CloseQualificationVideo
                    ]
                    [ text "Continue" ]
                ]
            ]

    else
        text ""


viewRatesModal : Model -> Html Msg
viewRatesModal model =
    if model.showRatesVideo then
        let
            rateText =
                case List.head (getSelectedPlans model) of
                    Just plan ->
                        "$" ++ String.fromFloat plan.price

                    Nothing ->
                        ""

            countyText =
                model.county

            stateText =
                model.state

            planTypeText =
                case model.selectedPlanType of
                    PlanG ->
                        "Plan G"

                    PlanN ->
                        "Plan N"
        in
        div [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4" ]
            [ div [ class "bg-white rounded-lg p-8 w-[95%] max-w-5xl mx-auto flex flex-col items-center relative" ]
                [ button
                    [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700"
                    , onClick CloseRatesVideo
                    ]
                    [ text "×" ]
                , h1 [ class "text-2xl font-bold mb-4 text-center" ] [ text "Good News!" ]
                , h2 [ class "text-xl font-bold mb-4 text-center" ]
                    [ text ("We found " ++ planTypeText ++ " options as low as " ++ rateText ++ " in " ++ countyText ++ " County, " ++ stateText) ]
                , p [ class "mb-4 text-center" ] [ text "Watch this quick video for 3 things to consider while reviewing your quotes" ]
                , div [ class "w-full max-w-3xl mx-auto" ]
                    [ div [ class "relative", style "padding-top" "125%" ]
                        [ iframe
                            [ src "https://player.vimeo.com/video/1018421439?autoplay=0&title=0&byline=0&portrait=0&responsive=1"
                            , class "w-full h-full absolute top-0 left-0"
                            , attribute "frameborder" "0"
                            , attribute "allow" "autoplay; fullscreen; picture-in-picture"
                            , attribute "allowfullscreen" ""
                            ]
                            []
                        ]
                    ]
                , button
                    [ class "bg-med-green-500 text-white px-6 py-2 rounded hover:bg-med-green-600 mt-4"
                    , onClick CloseRatesVideo
                    ]
                    [ text "Continue" ]
                ]
            ]

    else
        text ""


settingsDecoder : Decoder Settings
settingsDecoder =
    let
        _ =
            Debug.log "Running settingsDecoder" ()
    in
    D.field "success" D.bool
        |> D.andThen
            (\success ->
                let
                    _ =
                        Debug.log "Settings success value" success
                in
                if success then
                    D.field "orgSettings" settingsObjectDecoder

                else
                    D.fail "Settings request was not successful"
            )


settingsObjectDecoder : Decoder Settings
settingsObjectDecoder =
    let
        _ =
            Debug.log "Running settingsObjectDecoder" ()
    in
    D.map8 Settings
        (D.field "stateLicenses" (D.list D.string)
            |> D.andThen
                (\licenses ->
                    let
                        _ =
                            Debug.log "Decoded stateLicenses" licenses
                    in
                    D.succeed licenses
                )
        )
        (D.field "carrierContracts" (D.list D.string)
            |> D.andThen
                (\contracts ->
                    let
                        _ =
                            Debug.log "Decoded carrierContracts" contracts
                    in
                    D.succeed contracts
                )
        )
        (D.field "stateCarrierSettings" (D.list stateCarrierSettingDecoder))
        (D.field "allowAgentSettings" D.bool)
        (D.field "emailSendBirthday" D.bool)
        (D.field "emailSendPolicyAnniversary" D.bool)
        (D.field "emailSendAep" D.bool)
        (D.field "smartSendEnabled" D.bool)


stateCarrierSettingDecoder : Decoder StateCarrierSetting
stateCarrierSettingDecoder =
    D.map4 StateCarrierSetting
        (D.field "state" D.string)
        (D.field "carrier" D.string)
        (D.field "active" D.bool)
        (D.field "targetGI" D.bool)
