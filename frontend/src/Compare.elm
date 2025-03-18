module Compare exposing
    ( CompareParams
    , Model
    , Msg(..)
    , PlanType(..)
    , fetchPlans
    , init
    , subscriptions
    , update
    , view
    )

import BirthdayRules exposing (isInBirthdayRuleWindow)
import Browser
import Browser.Navigation as Nav
import CarrierNaic exposing (carrierToNaics, carrierToString, naicToCarrier, stringToCarrier)
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Task
import Time
import Url exposing (Url)
import Url.Parser as UrlParser
import Url.Parser.Query as Query



-- TYPES


type PlanType
    = PlanG
    | PlanN


type alias CompareParams =
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
    , trackingId : Maybe String
    , orgId : Maybe String
    }


type alias Contact =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , dateOfBirth : String
    , gender : String
    , tobacco : Bool
    , state : String
    , zipCode : String
    , currentCarrier : Maybe String
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
    , dateOfBirth : String
    , quoteId : Maybe String
    , orgSettings : Maybe Settings
    , currentDate : Maybe Date
    , orgId : Maybe String
    , name : String
    , contact : Maybe Contact
    , orgSlug : Maybe String
    , loadingContact : Bool
    , showDiscountInfo : Bool
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
    | SelectPlanCard Plan
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
    | ToggleDiscountInfo
    | GotOrgSettings (Result Http.Error Settings)
    | GotCurrentDate Date
    | GotContactData (Result Http.Error ContactResponse)
    | NoOp


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


init : Nav.Key -> Maybe CompareParams -> ( Model, Cmd Msg )
init key maybeParams =
    let
        -- Get values from params or use defaults
        defaultParams =
            { state = "TX"
            , county = "Dallas"
            , zip = "75001"
            , age = 65
            , gender = "M"
            , tobacco = False
            , planType = "G"
            , currentCarrier = Nothing
            , dateOfBirth = ""
            , quoteId = Nothing
            , trackingId = Nothing
            , orgId = Nothing
            }

        params =
            maybeParams
                |> Maybe.withDefault defaultParams

        -- Extract org ID from quote ID if not provided explicitly
        -- Quote IDs are formatted as "orgId-contactId-hash"
        extractedOrgId =
            case ( params.orgId, params.quoteId ) of
                ( Nothing, Just quoteId ) ->
                    -- Try to extract orgId from the quoteId (first part before the first dash)
                    quoteId
                        |> String.split "-"
                        |> List.head

                ( orgId, _ ) ->
                    orgId

        -- Extract plan type directly from params
        initialPlanType =
            if params.planType == "N" then
                PlanN

            else
                PlanG

        -- Ensure age is at least 65 for Medicare supplement plans
        minimumAge =
            if params.age < 65 then
                65

            else
                params.age

        model =
            { isLoading = True
            , error = Nothing
            , plans = { planG = [], planN = [] }
            , state = params.state
            , county = params.county
            , zip = params.zip
            , age = minimumAge
            , gender =
                if params.gender == "Male" || params.gender == "M" then
                    "M"

                else
                    "F"
            , tobacco = params.tobacco
            , selectedPlanType = initialPlanType
            , selectedPlan = Nothing
            , showReviewVideo = False
            , showQualificationVideo = False
            , showGvsNVideo = False
            , showFAQ = False
            , currentCardIndex = 0
            , showRatesVideo = False
            , key = key
            , showDiscount = False
            , currentCarrier = params.currentCarrier
            , dateOfBirth = params.dateOfBirth
            , quoteId = params.quoteId
            , orgSettings = Nothing
            , currentDate = Nothing
            , orgId = extractedOrgId
            , name = "Loading..." -- Will be updated with real name
            , contact = Nothing
            , orgSlug = Nothing
            , loadingContact = True
            , showDiscountInfo = False
            }

        -- Commands to execute
        quoteCommand =
            case params.quoteId of
                Just quoteId ->
                    fetchContactData quoteId

                Nothing ->
                    Cmd.none

        initialCommands =
            [ fetchPlans model
            , quoteCommand
            , Task.perform GotCurrentDate Date.today
            ]

        -- If we have a quote ID, fetch contact data
        commands =
            case params.quoteId of
                Just quoteId ->
                    fetchContactData quoteId :: initialCommands

                Nothing ->
                    -- No quote ID, check if we have an org ID
                    case extractedOrgId of
                        Just orgId ->
                            -- We have an org ID but no quote ID - in a real implementation, we might
                            -- redirect to a self-service page, but for now we'll continue with default data
                            initialCommands

                        Nothing ->
                            -- No org ID either - this is an error case
                            [ Nav.pushUrl key "/404" ]
    in
    ( model, Cmd.batch commands )


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


fetchContactData : String -> Cmd Msg
fetchContactData quoteId =
    Http.get
        { url = "/api/quotes/decode/" ++ quoteId
        , expect = Http.expectJson GotContactData contactResponseDecoder
        }


fetchPlans : Model -> Cmd Msg
fetchPlans model =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/api/quotes"
        , body = Http.jsonBody (buildPlansBody model)
        , expect = Http.expectJson GotPlans (plansDecoder model)
        , timeout = Nothing
        , tracker = Nothing
        }


buildPlansBody : Model -> E.Value
buildPlansBody model =
    let
        -- Ensure age is at least 65 for Medicare supplement plans
        minimumAge =
            if model.age < 65 then
                65

            else
                model.age
    in
    E.object
        [ ( "zip_code", E.string model.zip )
        , ( "state", E.string model.state )
        , ( "county", E.string model.county )
        , ( "age", E.int minimumAge )
        , ( "gender"
          , E.string
                (if model.gender == "Male" then
                    "M"

                 else
                    "F"
                )
          )
        , ( "tobacco", E.bool model.tobacco )
        , ( "plans", E.list E.string [ "G", "N" ] )
        , ( "carriers", E.string "supported" )
        ]



-- DECODERS


plansDecoder : Model -> Decoder Plans
plansDecoder model =
    D.oneOf
        [ D.list quoteResponseDecoder
            |> D.map (\responses -> groupQuotesByPlan responses model)
        , D.succeed { planG = [], planN = [] }
        ]


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
        isCarrierSupported : String -> Bool
        isCarrierSupported naic =
            -- Always consider carriers supported until settings are loaded
            case model.orgSettings of
                Nothing ->
                    True

                Just settings ->
                    case naicToCarrier naic of
                        Just carrier ->
                            let
                                carrierStr =
                                    carrier
                                        |> carrierToString
                                        |> String.filter (\c -> c /= ' ')
                            in
                            List.member carrierStr settings.carrierContracts

                        Nothing ->
                            -- If we can't map the NAIC to a carrier, consider it supported
                            True

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

        planG =
            List.filter (\q -> String.toUpper q.planType == "G") allQuotes
                |> List.sortBy .price

        planN =
            List.filter (\q -> String.toUpper q.planType == "N") allQuotes
                |> List.sortBy .price

        result =
            { planG = planG
            , planN = planN
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
        GotOrgSettings (Ok settings) ->
            let
                updatedModel =
                    { model | orgSettings = Just settings }
            in
            ( updatedModel
            , fetchPlans updatedModel
            )

        GotOrgSettings (Err _) ->
            ( { model | error = Just "Failed to load organization settings" }
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

        GotPlans result ->
            case result of
                Ok plans ->
                    let
                        hasPlans =
                            not (List.isEmpty plans.planG && List.isEmpty plans.planN)

                        errorMsg =
                            if not hasPlans then
                                Just "No plans available for the selected criteria. Please try different parameters."

                            else
                                Nothing
                    in
                    ( { model
                        | plans = plans
                        , isLoading = False
                        , error = errorMsg
                      }
                    , Cmd.none
                    )

                Err error ->
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
                , selectedPlan = Nothing
              }
            , Cmd.none
            )

        SelectPlan plan ->
            ( { model | showQualificationVideo = True }
            , Nav.pushUrl model.key
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
            , Nav.pushUrl model.key
                (let
                    orgIdParam =
                        case model.orgId of
                            Just orgId ->
                                "?orgId=" ++ orgId

                            Nothing ->
                                -- Try to extract orgId from the quoteId as a fallback
                                case model.quoteId of
                                    Just id ->
                                        case String.split "-" id |> List.head of
                                            Just extractedOrgId ->
                                                "?orgId=" ++ extractedOrgId

                                            Nothing ->
                                                ""

                                    Nothing ->
                                        ""
                 in
                 "/eligibility" ++ orgIdParam
                )
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

        GotCurrentDate date ->
            ( { model | currentDate = Just date }, Cmd.none )

        GotContactData result ->
            case result of
                Ok response ->
                    let
                        contact =
                            response.contact

                        orgSlug =
                            response.orgSlug

                        -- Update model with contact data
                        updatedModel =
                            { model
                                | contact = Just contact
                                , orgSlug = Just orgSlug
                                , loadingContact = False
                                , name = contact.firstName ++ " " ++ contact.lastName
                                , gender = contact.gender
                                , tobacco = contact.tobacco
                                , state = contact.state
                                , zip = contact.zipCode
                                , dateOfBirth = contact.dateOfBirth
                                , currentCarrier = contact.currentCarrier
                            }
                    in
                    ( updatedModel, fetchPlans updatedModel )

                Err error ->
                    ( { model
                        | error = Just "Failed to load contact data. Please try again later."
                        , loadingContact = False
                      }
                    , Cmd.none
                    )

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
            if model.showDiscount then
                plan.priceDiscount

            else
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
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Quote - Medicare Max"
    , body =
        [ div [ class "bg-gray-50 min-h-screen pb-12" ]
            [ div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-3" ]
                [ div [ class "bg-white rounded-lg shadow-sm p-4 mb-3 max-w-2xl mx-auto" ]
                    [ div [ class "flex flex-col space-y-3" ]
                        [ -- Top row - Quote and date info
                          div [ class "flex justify-between" ]
                            [ div []
                                [ p [ class "text-xs text-gray-500" ] [ text "Quote for:" ]
                                , p [ class "text-sm font-medium" ] [ text model.name ]
                                ]
                            , div [ class "text-right" ]
                                [ p [ class "text-xs text-gray-500" ] [ text "Policy Effective Date:" ]
                                , p [ class "text-sm font-medium" ] [ text "April 2025" ]
                                ]
                            ]
                        , -- Middle row
                          div [ class "flex justify-between" ]
                            [ div []
                                [ p [ class "text-xs text-gray-500" ] [ text "Email:" ]
                                , p [ class "text-sm" ]
                                    [ text (model.contact |> Maybe.map .email |> Maybe.withDefault "loading...") ]
                                ]
                            , div [ class "text-right" ]
                                [ p [ class "text-xs text-gray-500" ] [ text "Birth Date:" ]
                                , p [ class "text-sm" ] [ text (formatBirthDate model.dateOfBirth) ]
                                ]
                            ]
                        , -- Bottom row
                          div [ class "flex justify-between" ]
                            [ div []
                                [ p [ class "text-xs text-gray-500" ] [ text "Phone:" ]
                                , p [ class "text-sm" ]
                                    [ text (model.contact |> Maybe.map .phone |> Maybe.map formatPhoneNumber |> Maybe.withDefault "loading...") ]
                                ]
                            , div [ class "text-right" ]
                                [ p [ class "text-xs text-gray-500" ] [ text "Details:" ]
                                , p [ class "text-sm" ]
                                    [ text
                                        (String.join ", "
                                            [ if model.gender == "M" then
                                                "Male"

                                              else
                                                "Female"
                                            , formatAddress model.state model.zip
                                            , if model.tobacco then
                                                "Tobacco User"

                                              else
                                                "Non-Tobacco User"
                                            ]
                                        )
                                    ]
                                ]
                            ]
                        , div [ class "flex justify-between" ]
                            [ div []
                                [ p [ class "text-xs text-gray-500" ] [ text "Zip Code:" ]
                                , p [ class "text-sm" ] [ text model.zip ]
                                ]
                            , -- Current carrier (only if available)
                              if model.currentCarrier /= Nothing then
                                div [ class "text-right" ]
                                    [ p [ class "text-xs text-gray-500" ] [ text "Current Carrier:" ]
                                    , p [ class "text-sm" ]
                                        [ text (Maybe.withDefault "" model.currentCarrier) ]
                                    ]

                              else
                                text ""
                            ]
                        ]
                    ]
                ]
            , -- Household Discount Checkbox
              div [ class "flex justify-center mb-3" ]
                [ div [ class "flex items-center" ]
                    [ input
                        [ type_ "checkbox"
                        , class "form-checkbox h-4 w-4"
                        , checked model.showDiscount
                        , onClick ToggleDiscount
                        ]
                        []
                    , span [ class "ml-3 text-sm text-gray-700" ]
                        [ text "Apply Household Discount" ]
                    , span
                        [ class "ml-2 text-xs text-purple-600 underline cursor-pointer"
                        , onClick ToggleDiscountInfo
                        ]
                        [ text "(what's this?)" ]
                    ]
                ]
            , if model.showDiscountInfo then
                div [ class "bg-purple-50 p-3 mt-2 rounded-md max-w-md mx-auto text-sm" ]
                    [ p [ class "mb-2 text-gray-700" ]
                        [ text "Some arriers offer premium discounts based on the number of people in the same household. Eligibility criteria and discount amounts vary by carrier." ]
                    , div [ class "flex justify-center" ]
                        [ button
                            [ class "text-xs text-purple-600 hover:text-purple-800 mt-1"
                            , onClick ToggleDiscountInfo
                            ]
                            [ text "Close" ]
                        ]
                    ]

              else
                text ""
            , -- G vs N Video Pill
              div [ class "flex justify-center mb-4" ]
                [ viewPillButton "Learn About Plan G vs. Plan N" True OpenGvsNVideo ]
            , -- Main Content
              if model.isLoading then
                viewLoading

              else
                case model.error of
                    Just error ->
                        viewError error

                    Nothing ->
                        div [ class "max-w-[700px] mx-auto px-2" ]
                            [ -- Plan G Section
                              viewPlanTypeSection model "Plan G Monthly Premiums" "G" model.plans.planG
                            , -- Plan N Section
                              viewPlanTypeSection model "Plan N Monthly Premiums" "N" model.plans.planN
                            , -- Continue button
                              div [ class "flex justify-center mt-12" ]
                                [ button
                                    [ class "bg-purple-500 hover:bg-purple-600 text-white font-bold px-8 py-2 rounded-md disabled:opacity-50 disabled:cursor-not-allowed"
                                    , disabled (model.selectedPlan == Nothing)
                                    , onClick
                                        (case model.selectedPlan of
                                            Just plan ->
                                                SelectPlan plan

                                            Nothing ->
                                                NoOp
                                        )
                                    ]
                                    [ text "Continue with Selected Plan" ]
                                ]
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
        [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-purple-600 border-t-transparent" ] []
        , p [ class "text-center text-lg font-medium text-gray-600" ]
            [ text "Searching plans..." ]
        ]


viewError : String -> Html Msg
viewError error =
    div [ class "flex flex-col gap-6 text-center mx-auto max-w-3xl" ]
        [ h1 [ class "text-2xl font-semibold text-[#1A1A1A] mb-2" ]
            [ text "Unable to Load Plans" ]
        , div [ class "text-center text-xl font-medium text-red-600 mt-8 p-4 bg-red-50 rounded-lg" ]
            [ text error ]
        , div [ class "mt-6 flex justify-center" ]
            [ button
                [ class "px-6 py-3 bg-purple-600 text-white rounded-lg hover:bg-purple-700 transition-colors"
                , onClick (NavigateTo "/")
                ]
                [ text "Return to Home" ]
            ]
        ]


viewPlanCard : Model -> Plan -> Html Msg
viewPlanCard model plan =
    let
        planType =
            if String.toUpper plan.planType == "G" then
                "Plan G"

            else
                "Plan N"

        bgColor =
            if plan.planType == "G" then
                "bg-slate-100"

            else
                "bg-stone-100"

        isSelected =
            model.selectedPlan == Just plan

        selectionClass =
            if isSelected then
                "ring-2 ring-purple-400 shadow-sm"

            else
                "hover:border-gray-300"

        totalPrice =
            "$" ++ formatFloat plan.price

        discountPrice =
            "$" ++ formatFloat plan.priceDiscount

        formatFloat : Float -> String
        formatFloat value =
            let
                valueAsString =
                    String.fromFloat value

                parts =
                    String.split "." valueAsString
            in
            case parts of
                [ whole, decimal ] ->
                    if String.length decimal == 1 then
                        whole ++ "." ++ decimal ++ "0"

                    else
                        valueAsString

                _ ->
                    valueAsString

        selectHeadClass =
            "text-xs text-gray-600 mb-0.5"

        softHeadClass =
            "text-xs text-gray-400 mb-0.5"

        selectBodyClass =
            "text-sm font-bold"

        softBodyClass =
            "text-xs font-semibold text-gray-400"
    in
    div
        [ class ("bg-white border border-gray-200 rounded-md shadow-sm cursor-pointer w-full max-w-[200px] " ++ selectionClass)
        , onClick (SelectPlanCard plan)
        ]
        [ div [ class "p-3" ]
            [ div [ class "flex items-start mb-2" ]
                [ div [ class ("text-dark font-medium text-xs px-3 py-1.5 rounded-full shadow-sm " ++ bgColor) ]
                    [ text planType ]
                ]
            , div [ class "flex justify-center items-center py-3" ]
                [ div [ class "text-center" ]
                    [ img [ src plan.image, alt (plan.name ++ " logo"), class "h-8 mx-auto object-contain" ] [] ]
                ]
            ]
        , div [ class "border-t border-gray-100 px-3 py-2 bg-gray-50 w-full" ]
            [ div [ class "flex justify-between items-center" ]
                [ div [ class "flex flex-col items-center" ]
                    [ p
                        [ class
                            (if model.showDiscount then
                                softHeadClass

                             else
                                selectHeadClass
                            )
                        ]
                        [ text "Standard" ]
                    , p
                        [ class
                            (if model.showDiscount then
                                softBodyClass

                             else
                                selectBodyClass
                            )
                        ]
                        [ text totalPrice ]
                    ]
                , div [ class "flex flex-col items-center" ]
                    [ p
                        [ class
                            (if model.showDiscount then
                                selectHeadClass

                             else
                                softHeadClass
                            )
                        ]
                        [ text "Discount" ]
                    , p
                        [ class
                            (if model.showDiscount then
                                selectBodyClass

                             else
                                softBodyClass
                            )
                        ]
                        [ text discountPrice ]
                    ]
                ]
            ]
        ]


viewGvsNModal : Model -> Html Msg
viewGvsNModal model =
    if model.showGvsNVideo then
        div [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4" ]
            [ div [ class "bg-white rounded-lg p-4 sm:p-8 w-[95%] max-w-5xl mx-auto flex flex-col items-center relative" ]
                [ button
                    [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700 text-xl p-1"
                    , onClick CloseGvsNVideo
                    ]
                    [ text "×" ]
                , h2 [ class "text-xl sm:text-2xl font-bold mb-2 sm:mb-4 text-center" ] [ text "Plan G vs. Plan N" ]
                , p [ class "mb-2 sm:mb-4 text-center text-sm sm:text-base" ] [ text "Understanding the differences" ]
                , p [ class "mb-3 sm:mb-4 text-center text-sm sm:text-base" ] [ text "Watch this video to learn about the key differences between Plan G and Plan N" ]
                , div [ class "w-full max-w-3xl mx-auto" ]
                    [ div [ class "relative", style "padding-top" "100%" ]
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
                    [ class "bg-purple-500 text-white px-4 sm:px-6 py-2 rounded hover:bg-purple-600 mt-4 w-full sm:w-auto"
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
            [ div [ class "bg-white rounded-lg p-4 sm:p-8 w-[95%] max-w-5xl mx-auto flex flex-col items-center relative" ]
                [ button
                    [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700 text-xl p-1"
                    , onClick CloseQualificationVideo
                    ]
                    [ text "×" ]
                , h1 [ class "text-xl sm:text-2xl font-bold mb-2 sm:mb-4 text-center" ] [ text "Great Choice!" ]
                , h2 [ class "text-lg sm:text-xl font-bold mb-2 sm:mb-4 text-center" ] [ text "Now let's see if you qualify" ]
                , p [ class "mb-3 sm:mb-4 text-center text-sm sm:text-base" ] [ text "Watch this video to understand the process of qualifying for the plan you selected" ]
                , div [ class "w-full max-w-3xl mx-auto" ]
                    [ div [ class "relative", style "padding-top" "100%" ]
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
                    [ class "bg-purple-500 text-white px-4 sm:px-6 py-2 rounded hover:bg-purple-600 mt-4 w-full sm:w-auto"
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
            [ div [ class "bg-white rounded-lg p-4 sm:p-8 w-[95%] max-w-5xl mx-auto flex flex-col items-center relative" ]
                [ button
                    [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700 text-xl p-1"
                    , onClick CloseRatesVideo
                    ]
                    [ text "×" ]
                , h1 [ class "text-xl sm:text-2xl font-bold mb-2 sm:mb-4 text-center" ] [ text "Good News!" ]
                , h2 [ class "text-base sm:text-xl font-bold mb-2 sm:mb-4 text-center leading-tight" ]
                    [ text ("We found " ++ planTypeText ++ " options as low as " ++ rateText ++ " in " ++ countyText ++ " County, " ++ stateText) ]
                , p [ class "mb-3 sm:mb-4 text-center text-sm sm:text-base" ] [ text "Watch this quick video for 3 things to consider while reviewing your quotes" ]
                , div [ class "w-full max-w-3xl mx-auto" ]
                    [ div [ class "relative", style "padding-top" "100%" ]
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
                    [ class "bg-purple-500 text-white px-4 sm:px-6 py-2 rounded hover:bg-purple-600 mt-4 w-full sm:w-auto"
                    , onClick CloseRatesVideo
                    ]
                    [ text "Continue" ]
                ]
            ]

    else
        text ""


settingsDecoder : Decoder Settings
settingsDecoder =
    D.field "success" D.bool
        |> D.andThen
            (\success ->
                if success then
                    D.field "orgSettings" settingsObjectDecoder

                else
                    D.fail "Settings request was not successful"
            )


settingsObjectDecoder : Decoder Settings
settingsObjectDecoder =
    D.map8 Settings
        (D.field "stateLicenses" (D.list D.string))
        (D.field "carrierContracts" (D.list D.string))
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


viewPlanTypeSection : Model -> String -> String -> List Plan -> Html Msg
viewPlanTypeSection model title code plans =
    div [ class "mb-8" ]
        [ h2 [ class "text-xl font-bold mb-4 text-center sm:text-left" ] [ text title ]
        , div [ class "grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 gap-4 justify-items-center" ]
            (List.map (viewPlanCard model) (getTopPlans model plans 3))
        ]



-- Helper function to format phone number as (555) 123-4567


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    if String.length phone >= 10 then
        let
            areaCode =
                String.slice 0 3 phone

            firstPart =
                String.slice 3 6 phone

            secondPart =
                String.slice 6 10 phone
        in
        "(" ++ areaCode ++ ") " ++ firstPart ++ "-" ++ secondPart

    else
        phone



-- Helper function to format birth date


formatBirthDate : String -> String
formatBirthDate date =
    if String.contains "-" date && String.length date >= 10 then
        let
            parts =
                String.split "-" date

            year =
                List.head parts |> Maybe.withDefault ""

            month =
                List.drop 1 parts |> List.head |> Maybe.withDefault ""

            day =
                List.drop 2 parts |> List.head |> Maybe.withDefault "" |> String.left 2

            monthName =
                case month of
                    "01" ->
                        "Jan"

                    "02" ->
                        "Feb"

                    "03" ->
                        "Mar"

                    "04" ->
                        "Apr"

                    "05" ->
                        "May"

                    "06" ->
                        "Jun"

                    "07" ->
                        "Jul"

                    "08" ->
                        "Aug"

                    "09" ->
                        "Sep"

                    "10" ->
                        "Oct"

                    "11" ->
                        "Nov"

                    "12" ->
                        "Dec"

                    _ ->
                        month
        in
        monthName ++ " " ++ day ++ ", " ++ year

    else
        date



-- Helper function to format address


formatAddress : String -> String -> String
formatAddress state zip =
    state ++ " " ++ zip



-- Add decoder for Contact data


type alias ContactResponse =
    { success : Bool
    , orgSlug : String
    , contact : Contact
    }


contactDecoder : Decoder Contact
contactDecoder =
    D.succeed Contact
        |> Pipeline.required "firstName" D.string
        |> Pipeline.required "lastName" D.string
        |> Pipeline.required "email" D.string
        |> Pipeline.required "phoneNumber" D.string
        |> Pipeline.required "dateOfBirth" D.string
        |> Pipeline.required "gender" D.string
        |> Pipeline.required "tobacco" D.bool
        |> Pipeline.required "state" D.string
        |> Pipeline.required "zipCode" D.string
        |> Pipeline.required "currentCarrier" (D.nullable D.string)


contactResponseDecoder : Decoder ContactResponse
contactResponseDecoder =
    D.map3 ContactResponse
        (D.field "success" D.bool)
        (D.field "orgSlug" D.string)
        (D.field "contact" contactDecoder)


viewPillButton : String -> Bool -> Msg -> Html Msg
viewPillButton label isVideo msg =
    button
        [ class "mx-auto bg-white text-purple-600 px-3 sm:px-4 py-1.5 sm:py-2 rounded-full border border-purple-600 hover:bg-purple-50 transition-colors flex items-center justify-center gap-1 sm:gap-2 text-xs sm:text-sm"
        , onClick msg
        ]
        [ if isVideo then
            div [ class "flex items-center justify-center gap-0.5 sm:gap-1" ]
                [ text "▶"
                , span [ class "text-[10px] sm:text-xs" ] [ text "Video" ]
                ]

          else
            text ""
        , text label
        ]
