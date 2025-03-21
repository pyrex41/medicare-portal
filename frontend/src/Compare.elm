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
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, height, stroke, strokeLinecap, viewBox, width)
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
    { quoteId : Maybe String
    , orgId : Maybe String
    }


type alias ContactResponse =
    { contact : Contact
    , agent : Agent
    , orgSlug : String
    , orgName : String
    , orgLogo : Maybe String
    , carrierContracts : List Carrier
    }


type alias Agent =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
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
    { isLoading : Bool
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
    , orgId : Maybe String
    , orgName : Maybe String
    , orgLogo : Maybe String
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
    | GotCarrierContracts (Result Http.Error (List Carrier))
    | GotCurrentDate Date
    | GotContactData (Result Http.Error ContactResponse)
    | ShowLocationModal
    | CloseLocationModal
    | UpdateZipCode String
    | UpdateCounty String
    | SubmitLocationUpdate
    | GotLocationUpdate (Result Http.Error LocationUpdateResponse)
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
        -- Empty model with loading state
        emptyModel =
            { isLoading = True
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
            , orgId = Nothing
            , orgName = Nothing
            , orgLogo = Nothing
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
            }
    in
    case maybeParams of
        Just params ->
            case params.quoteId of
                Just quoteId ->
                    -- We have a quote ID, fetch contact data
                    ( { emptyModel | quoteId = Just quoteId, orgId = params.orgId }
                    , Cmd.batch
                        [ fetchContactData quoteId
                        , Task.perform GotCurrentDate Date.today
                        ]
                    )

                Nothing ->
                    -- No quote ID, check if we have an org ID
                    case params.orgId of
                        Just orgId ->
                            -- Redirect to self-service onboarding
                            ( emptyModel
                            , Nav.pushUrl key ("/self-onboarding/" ++ orgId)
                            )

                        Nothing ->
                            -- No valid parameters
                            ( { emptyModel
                                | isLoading = False
                                , error = Just "Missing required parameters. Please provide either a quote ID or organization ID."
                              }
                            , Cmd.none
                            )

        Nothing ->
            -- No parameters provided
            ( { emptyModel
                | isLoading = False
                , error = Just "No parameters provided. Please provide either a quote ID or organization ID."
              }
            , Cmd.none
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


fetchContactData : String -> Cmd Msg
fetchContactData quoteId =
    Http.get
        { url = "/api/quotes/decode/" ++ quoteId
        , expect = Http.expectJson GotContactData contactResponseDecoder
        }


fetchPlans : Model -> Cmd Msg
fetchPlans model =
    case model.contact of
        Just contact ->
            Http.request
                { method = "POST"
                , headers = []
                , url = "/api/quotes"
                , body = Http.jsonBody (buildPlansBody contact)
                , expect = Http.expectJson GotPlans (plansDecoder model)
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            -- Don't fetch plans until we have contact data
            Cmd.none


buildPlansBody : Contact -> E.Value
buildPlansBody contact =
    E.object
        [ ( "zip_code", E.string contact.zipCode )
        , ( "state", E.string contact.state )
        , ( "county", E.string (Maybe.withDefault "" contact.county) )
        , ( "age", E.int contact.age )
        , ( "gender"
          , E.string
                (if contact.gender == "Male" then
                    "M"

                 else
                    "F"
                )
          )
        , ( "tobacco", E.bool contact.tobacco )
        , ( "plans", E.list E.string [ "G", "N" ] )
        , ( "carriers", E.string "supported" )
        ]


calculateAge : String -> Int
calculateAge dateOfBirth =
    -- TODO: Implement proper age calculation from dateOfBirth string
    -- For now, default to 65 which is the minimum age for Medicare
    65



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


isCarrierSupported : String -> List Carrier -> Bool
isCarrierSupported naic carrierContracts =
    case naicToCarrier naic of
        Just carrierName ->
            List.member carrierName carrierContracts

        Nothing ->
            False


filterPlansByCarrier : Plans -> List Carrier -> Plans
filterPlansByCarrier plans carrierContracts =
    { planG = List.filter (\plan -> isCarrierSupported plan.naic carrierContracts) plans.planG
    , planN = List.filter (\plan -> isCarrierSupported plan.naic carrierContracts) plans.planN
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
        GotCarrierContracts (Ok carrierContracts) ->
            let
                updatedModel =
                    { model | carrierContracts = carrierContracts }
            in
            ( updatedModel
            , fetchPlans updatedModel
            )

        GotCarrierContracts (Err _) ->
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
                Ok plansRaw ->
                    let
                        plans =
                            filterPlansByCarrier plansRaw model.carrierContracts

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
                        -- Update model with contact data
                        updatedModel =
                            { model
                                | contact = Just response.contact
                                , agent = Just response.agent
                                , orgSlug = Just response.orgSlug
                                , orgName = Just response.orgName
                                , orgLogo = response.orgLogo
                                , carrierContracts = response.carrierContracts
                                , loadingContact = False
                                , name = Just (response.contact.firstName ++ " " ++ response.contact.lastName)
                                , gender = Just response.contact.gender
                                , tobacco = Just response.contact.tobacco
                                , state = Just response.contact.state
                                , zip = Just response.contact.zipCode
                                , age = Just response.contact.age
                                , currentCarrier = response.contact.currentCarrier
                                , planType = response.contact.planType
                            }
                    in
                    ( updatedModel
                    , fetchPlans updatedModel
                    )

                Err error ->
                    ( { model
                        | error = Just "This quote link appears to be invalid or has expired. Please get a new quote to continue."
                        , loadingContact = False
                        , isLoading = False
                      }
                    , Cmd.none
                    )

        ShowLocationModal ->
            ( { model
                | showLocationModal = True
                , editingZipCode = model.zip
                , editingCounty = Nothing
                , availableCounties = []
                , locationUpdateError = Nothing
              }
            , Cmd.none
            )

        CloseLocationModal ->
            ( { model
                | showLocationModal = False
                , editingZipCode = Nothing
                , editingCounty = Nothing
                , availableCounties = []
                , locationUpdateError = Nothing
              }
            , Cmd.none
            )

        UpdateZipCode newZip ->
            ( { model | editingZipCode = Just newZip }
            , Cmd.none
            )

        UpdateCounty county ->
            ( { model | editingCounty = Just county }
            , Cmd.none
            )

        SubmitLocationUpdate ->
            case model.contact of
                Just contact ->
                    case model.orgSlug of
                        Just orgSlug ->
                            ( { model | submittingLocation = True, locationUpdateError = Nothing }
                            , Http.post
                                { url = "/api/self-service/update-location"
                                , body =
                                    Http.jsonBody
                                        (E.object
                                            [ ( "orgSlug", E.string orgSlug )
                                            , ( "contactId", E.string (String.fromInt contact.id) )
                                            , ( "zipCode", E.string (Maybe.withDefault "" model.editingZipCode) )
                                            , ( "county", E.string (Maybe.withDefault "" model.editingCounty) )
                                            ]
                                        )
                                , expect = Http.expectJson GotLocationUpdate locationUpdateResponseDecoder
                                }
                            )

                        Nothing ->
                            ( { model | locationUpdateError = Just "Organization ID not found" }
                            , Cmd.none
                            )

                Nothing ->
                    ( { model | locationUpdateError = Just "Contact information not found" }
                    , Cmd.none
                    )

        GotLocationUpdate result ->
            case result of
                Ok response ->
                    if response.success then
                        case response.counties of
                            [] ->
                                -- No counties returned, show error
                                ( { model
                                    | locationUpdateError = Just "No counties found for this ZIP code"
                                    , submittingLocation = False
                                  }
                                , Cmd.none
                                )

                            [ singleCounty ] ->
                                -- Only one county, use it and close modal
                                let
                                    -- Update contact with new location info
                                    updatedContact =
                                        model.contact
                                            |> Maybe.map
                                                (\contact ->
                                                    { contact
                                                        | zipCode = response.zipCode
                                                        , state = response.state
                                                        , county = Just singleCounty
                                                    }
                                                )

                                    updatedModel =
                                        { model
                                            | zip = Just response.zipCode
                                            , state = Just response.state
                                            , county = Just singleCounty
                                            , contact = updatedContact
                                            , showLocationModal = False
                                            , editingZipCode = Nothing
                                            , editingCounty = Nothing
                                            , availableCounties = []
                                            , locationUpdateError = Nothing
                                            , submittingLocation = False
                                        }
                                in
                                -- Reload the page to refresh everything
                                ( updatedModel
                                , Nav.reload
                                )

                            multipleCounties ->
                                -- Multiple counties, show dropdown
                                ( { model
                                    | availableCounties = multipleCounties
                                    , zip = Just response.zipCode
                                    , state = Just response.state
                                    , editingCounty = Nothing
                                    , submittingLocation = False
                                  }
                                , Cmd.none
                                )

                    else
                        ( { model
                            | locationUpdateError = Just "Failed to update location"
                            , submittingLocation = False
                          }
                        , Cmd.none
                        )

                Err error ->
                    ( { model
                        | locationUpdateError = Just (httpErrorToString error)
                        , submittingLocation = False
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
                        if model.showLocationModal then
                            CloseLocationModal

                        else if model.showGvsNVideo then
                            CloseGvsNVideo

                        else if model.showQualificationVideo then
                            CloseQualificationVideo

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
        [ div [ class "bg-white rounded-[10px] border-2 border-[#DCE2E5] shadow-[0_1px_2px_rgba(16,24,40,0.05)]" ]
            [ -- Personal Quote Header
              div
                [ class "border-b-2 border-[#DCE2E5] bg-[#F9F5FF] px-4 sm:px-6 py-3 rounded-t-[10px]" ]
                [ h2 [ class "text-xl sm:text-2xl font-extrabold tracking-tight leading-[2] -tracking-[0.04em]" ] [ text "Personal Quote" ]
                ]
            , div
                [ class "p-4 sm:p-6 flex flex-col sm:flex-row sm:justify-between sm:items-start gap-4 sm:gap-0 bg-white rounded-b-[10px]" ]
                [ -- Left side - Quote For
                  div [ class "flex flex-col" ]
                    [ div [ class "mb-2" ]
                        [ p [ class "text-sm text-[#667085] mb-1" ] [ text "Quote For" ]
                        , p [ class "text-[16px] font-medium" ] [ text (Maybe.withDefault "Loading..." model.name) ]
                        , p [ class "text-[12px] text-[#667085]" ]
                            [ text
                                (if Maybe.withDefault "" model.gender == "M" then
                                    "M"

                                 else
                                    "F"
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
                    , div [ class "flex items-center" ]
                        [ button [ class "text-xs text-[#2563EB] underline text-left", onClick ShowLocationModal ] [ text "Edit Location" ]
                        , span [ class "text-[#667085] mx-2 font-medium" ] [ text "│" ]
                        , a [ href ("/self-onboarding/" ++ Maybe.withDefault "" model.orgSlug), class "text-xs text-[#2563EB] underline text-left" ] [ text "Quote for a New Person" ]
                        ]
                    ]

                -- Right side container - Quote From and Video (desktop)
                , div [ class "flex flex-row justify-between items-start gap-16" ]
                    [ -- Quote From
                      div [ class "flex flex-col min-w-[200px]" ]
                        [ p [ class "text-sm text-[#667085] mb-1" ] [ text "Quote From" ]
                        , p [ class "text-[16px] font-medium mb-2" ]
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
                                , class "flex items-center gap-1.5 bg-[#F9F5FF] px-2.5 py-1 rounded hover:bg-[#F4EBFF] transition-colors"
                                ]
                                [ svg [ Svg.Attributes.width "12", Svg.Attributes.height "12", Svg.Attributes.viewBox "0 0 12 12", Svg.Attributes.fill "none" ]
                                    [ path [ Svg.Attributes.d "M1 6C1 4.1145 1 3.1715 1.586 2.586C2.1715 2 3.1145 2 5 2H7C8.8855 2 9.8285 2 10.414 2.586C11 3.1715 11 4.1145 11 6C11 7.8855 11 8.8285 10.414 9.414C9.8285 10 8.8855 10 7 10H5C3.1145 10 2.1715 10 1.586 9.414C1 8.8285 1 7.8855 1 6Z", Svg.Attributes.stroke "#03045E" ] []
                                    , path [ Svg.Attributes.d "M3 4L4.0795 4.9C4.998 5.665 5.457 6.0475 6 6.0475C6.543 6.0475 7.0025 5.665 7.9205 4.8995L9 4", Svg.Attributes.stroke "#03045E", Svg.Attributes.strokeLinecap "round", Svg.Attributes.strokeLinejoin "round" ] []
                                    ]
                                , span [ class "text-xs text-[#03045E]" ]
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
                                , class "flex items-center gap-1.5 bg-[#F9F5FF] px-2.5 py-1 rounded hover:bg-[#F4EBFF] transition-colors"
                                ]
                                [ svg [ Svg.Attributes.width "12", Svg.Attributes.height "12", Svg.Attributes.viewBox "0 0 12 12", Svg.Attributes.fill "none" ]
                                    [ path
                                        [ Svg.Attributes.fillRule "evenodd"
                                        , Svg.Attributes.clipRule "evenodd"
                                        , Svg.Attributes.d "M1.75442 1.13022C2.80442 0.0802194 4.57692 0.160219 5.30817 1.47022L5.7138 2.19709C6.19067 3.05209 5.98755 4.13147 5.2888 4.83897C5.24752 4.90156 5.22497 4.97463 5.2238 5.04959C5.21567 5.20959 5.27255 5.58022 5.84692 6.15397C6.42067 6.72772 6.79067 6.78522 6.9513 6.77709C7.02626 6.77592 7.09934 6.75337 7.16192 6.71209C7.8688 6.01334 8.9488 5.81022 9.8038 6.28709L10.5307 6.69334C11.8407 7.42459 11.9207 9.19584 10.8707 10.2465C10.3088 10.8077 9.56255 11.3071 8.68442 11.3402C7.38442 11.3896 5.22442 11.0533 3.08567 8.91522C0.947548 6.77647 0.611298 4.61709 0.660673 3.31647C0.693798 2.43834 1.19317 1.69147 1.75442 1.13022ZM4.48942 1.92709C4.11442 1.25584 3.10817 1.10209 2.41755 1.79334C1.93317 2.27772 1.61755 2.81209 1.59755 3.35147C1.5563 4.43647 1.82442 6.32772 3.7488 8.25147C5.6738 10.1765 7.56442 10.4446 8.6488 10.4033C9.18817 10.3827 9.7238 10.0677 10.2075 9.58334C10.8988 8.89209 10.745 7.88584 10.0738 7.51147L9.34692 7.10584C8.89505 6.85397 8.25942 6.93959 7.8138 7.38584C7.77005 7.42959 7.4913 7.68959 6.99692 7.71334C6.49067 7.73834 5.87755 7.51084 5.18442 6.81709C4.49005 6.12334 4.26255 5.51022 4.28755 5.00334C4.3113 4.50897 4.57192 4.23022 4.61505 4.18647C5.0613 3.74084 5.14692 3.10584 4.89505 2.65397L4.48942 1.92709Z"
                                        , Svg.Attributes.fill "#03045E"
                                        ]
                                        []
                                    ]
                                , span [ class "text-xs text-[#03045E]" ]
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

                    -- Video button (desktop only)
                    , div [ class "hidden sm:flex bg-[#F9F5FF] rounded-[10px] p-4 flex-col items-center cursor-pointer gap-2 border-2 border-[#DCE2E5] w-[180px]", onClick OpenGvsNVideo ]
                        [ p [ class "text-[14px] font-bold text-[#03045E] -tracking-[0.03em] leading-[1.21] text-center" ] [ text "Learn About Plan G vs Plan N" ]
                        , div [ class "w-[33px] h-[33px] rounded-full border-2 border-[#03045E] flex items-center justify-center" ]
                            [ div [ class "w-0 h-0 border-t-[8px] border-t-transparent border-l-[12px] border-l-[#03045E] border-b-[8px] border-b-transparent ml-1" ] []
                            ]
                        , p [ class "text-[8px] text-[#667085] -tracking-[0.03em] leading-[1.21]" ] [ text "Watch the Video" ]
                        ]
                    ]
                ]
            ]
        ]


viewPlanCard : Model -> String -> Plan -> Html Msg
viewPlanCard model planTypeCode plan =
    let
        isSelected =
            model.selectedPlan == Just plan

        ( badgeTextColor, badgeBgColor ) =
            if planTypeCode == "G" then
                ( "text-[#363F72]", "bg-[#F8F9FC]" )

            else
                ( "text-[#F8F9FC]", "bg-[#363F72]" )

        borderClass =
            if isSelected then
                "border-2 border-[#2563EB]"

            else
                "border border-[#D4D4D4]"
    in
    div
        [ class ("relative bg-white rounded-lg " ++ borderClass ++ " overflow-hidden cursor-pointer w-full sm:pb-[75%]") -- 4:3 aspect ratio on desktop only
        , onClick (SelectPlanCard plan)
        ]
        [ div [ class "sm:absolute sm:inset-0 flex flex-col" ]
            [ -- Top row with Plan type badge and radio
              div [ class "flex items-center justify-between p-3 sm:p-4" ]
                [ div [ class ("px-2.5 py-0.5 rounded-lg text-xs font-medium leading-5 " ++ badgeTextColor ++ " " ++ badgeBgColor) ]
                    [ text ("PLAN " ++ planTypeCode) ]
                , div [ class "flex items-center gap-1.5" ]
                    [ span [ class "text-xs font-medium text-[#667085]" ] [ text "Select Plan" ]
                    , if isSelected then
                        svg [ Svg.Attributes.width "16", Svg.Attributes.height "17", Svg.Attributes.viewBox "0 0 12 13", Svg.Attributes.fill "none" ]
                            [ Svg.rect [ Svg.Attributes.x "0.5", Svg.Attributes.y "1", Svg.Attributes.width "11", Svg.Attributes.height "11", Svg.Attributes.rx "5.5", Svg.Attributes.fill "#F9F5FF" ] []
                            , Svg.rect [ Svg.Attributes.x "0.5", Svg.Attributes.y "1", Svg.Attributes.width "11", Svg.Attributes.height "11", Svg.Attributes.rx "5.5", Svg.Attributes.stroke "#7F56D9" ] []
                            , Svg.path [ Svg.Attributes.d "M9 4.25L4.875 8.375L3 6.5", Svg.Attributes.stroke "#7F56D9", Svg.Attributes.strokeWidth "1.6666", Svg.Attributes.strokeLinecap "round", Svg.Attributes.strokeLinejoin "round" ] []
                            ]

                      else
                        svg [ Svg.Attributes.width "16", Svg.Attributes.height "17", Svg.Attributes.viewBox "0 0 12 13", Svg.Attributes.fill "none" ]
                            [ Svg.rect [ Svg.Attributes.x "0.5", Svg.Attributes.y "1", Svg.Attributes.width "11", Svg.Attributes.height "11", Svg.Attributes.rx "5.5", Svg.Attributes.fill "white" ] []
                            , Svg.rect [ Svg.Attributes.x "0.5", Svg.Attributes.y "1", Svg.Attributes.width "11", Svg.Attributes.height "11", Svg.Attributes.rx "5.5", Svg.Attributes.stroke "#667085" ] []
                            ]
                    ]
                ]

            -- Carrier Logo
            , div [ class "flex-1 px-4 flex justify-center items-center min-h-[50px] sm:min-h-[60px] max-h-[80px] mb-3 sm:mb-0" ]
                [ img [ src plan.image, alt (plan.name ++ " logo"), class "h-8 sm:h-10 max-w-[140px] sm:max-w-[160px] object-contain" ] [] ]

            -- Rates
            , div [ class "flex flex-row sm:flex-row justify-between px-4 sm:px-6 py-4 sm:py-4 bg-[#F9FAFB]" ]
                [ -- Standard Rate
                  div [ class "flex-1 flex flex-col sm:flex-col justify-center sm:justify-start items-start sm:items-start" ]
                    [ p [ class "text-[13px] sm:text-[10px] font-medium leading-5 text-[#667085] mb-1 sm:mb-0.5" ] [ text "Standard Rate:" ]
                    , p [ class "text-[17px] sm:text-lg font-bold leading-6 text-[#667085]" ] [ text ("$" ++ String.fromInt (floor plan.price)) ]
                    ]

                -- Vertical Divider (mobile and desktop)
                , div [ class "w-[1px] h-[40px] self-center bg-[#DCE2E5] mx-2" ] []

                -- Discount Rate
                , div [ class "flex-1 flex flex-col sm:flex-col justify-center sm:justify-end items-end sm:items-end" ]
                    [ p [ class "text-[13px] sm:text-[10px] font-medium leading-5 text-[#667085] mb-1 sm:mb-0.5" ] [ text "Discount Rate:" ]
                    , p [ class "text-[17px] sm:text-lg font-bold leading-6 text-[#667085]" ] [ text ("$" ++ String.fromInt (floor plan.priceDiscount)) ]
                    ]
                ]
            ]
        ]


viewPlansSection : Model -> Html Msg
viewPlansSection model =
    div [ class "bg-white rounded-[10px] border-2 border-[#DCE2E5] shadow-[0_1px_2px_rgba(16,24,40,0.05)]" ]
        [ -- Header (desktop only)
          div [ class "hidden sm:flex px-4 sm:px-6 py-4 flex-row items-center justify-between border-b-2 border-[#DCE2E5] bg-[#F9F5FF] rounded-t-[10px]" ]
            [ div [ class "flex items-end gap-3" ]
                [ h2 [ class "text-xl sm:text-2xl font-extrabold -tracking-[0.04em] text-[#101828] leading-[1.2]" ] [ text "Recommended Plans" ]
                , p [ class "text-[12px] font-medium text-[#667085] -tracking-[0.04em] leading-[1.2] pb-[2px]" ] [ text "Select one to continue" ]
                ]
            , button
                [ class "bg-[#03045E] text-white text-sm font-medium px-6 py-2 rounded disabled:opacity-50 disabled:cursor-not-allowed"
                , onClick (SelectPlan (Maybe.withDefault (Plan 0 0 Nothing 0 "" "" 0 "" "" "" "" "" "" 0 False "" False []) model.selectedPlan))
                , disabled (model.selectedPlan == Nothing)
                ]
                [ text "Qualify" ]
            ]

        -- Mobile header
        , div [ class "block sm:hidden px-4 py-4 border-b-2 border-[#DCE2E5] bg-[#F9F5FF] rounded-t-[10px]" ]
            [ h2 [ class "text-xl font-extrabold -tracking-[0.04em] text-[#101828] leading-[1.2]" ] [ text "Recommended Plans" ]
            , p [ class "text-[12px] font-medium text-[#667085] -tracking-[0.04em] leading-[1.2]" ] [ text "Select one to continue" ]
            ]

        -- Plan G Section
        , div [ class "p-4 sm:p-8 pb-2 bg-white" ]
            [ h3 [ class "text-base font-extrabold -tracking-[0.02em] mb-6 text-[#101828]" ] [ text "Plan G Monthly Premiums" ]
            , div [ class "grid grid-cols-1 sm:grid-cols-3 gap-4 sm:gap-6 px-2 sm:px-0" ]
                (List.map (viewPlanCard model "G") (getTopPlans model model.plans.planG 3))
            ]

        -- Plan N Section
        , div [ class "p-4 sm:p-8 pt-6 bg-white" ]
            [ h3 [ class "text-base font-extrabold -tracking-[0.02em] mb-6 text-[#101828]" ] [ text "Plan N Monthly Premiums" ]
            , div [ class "grid grid-cols-1 sm:grid-cols-3 gap-4 sm:gap-6 px-2 sm:px-0" ]
                (List.map (viewPlanCard model "N") (getTopPlans model model.plans.planN 3))
            ]

        -- Mobile continue button
        , div [ class "block sm:hidden p-4 bg-[#F9F5FF] rounded-b-[10px] border-t-2 border-[#DCE2E5]" ]
            [ button
                [ class "w-full bg-[#03045E] text-white text-sm font-medium px-6 py-2 rounded disabled:opacity-50 disabled:cursor-not-allowed"
                , onClick (SelectPlan (Maybe.withDefault (Plan 0 0 Nothing 0 "" "" 0 "" "" "" "" "" "" 0 False "" False []) model.selectedPlan))
                , disabled (model.selectedPlan == Nothing)
                ]
                [ text "Qualify" ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Quote - Medicare Max"
    , body =
        [ div [ class "flex justify-center items-center mt-8 mb-4" ]
            [ case model.orgLogo of
                Just logo ->
                    img [ src logo, alt "Organization Logo", class "h-16 max-w-[200px] object-contain" ] []

                Nothing ->
                    case model.orgName of
                        Just name ->
                            div [ class "text-2xl font-bold text-[#101828] leading-[1.2]" ] [ text name ]

                        Nothing ->
                            text ""
            ]
        , div [ class "bg-white min-h-screen pb-12" ]
            [ if model.loadingContact || model.isLoading then
                viewLoading

              else
                div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-3 space-y-6 sm:space-y-10" ]
                    [ -- Personal Quote Card
                      viewPersonalInfo model

                    -- Mobile video button
                    , div [ class "block sm:hidden" ]
                        [ div [ class "mx-auto max-w-[280px] bg-[#F9F5FF] rounded-[10px] p-4 flex flex-row items-center cursor-pointer gap-4 border-2 border-[#DCE2E5]", onClick OpenGvsNVideo ]
                            [ div [ class "w-[33px] h-[33px] rounded-full border-2 border-[#03045E] flex items-center justify-center flex-shrink-0" ]
                                [ div [ class "w-0 h-0 border-t-[8px] border-t-transparent border-l-[12px] border-l-[#03045E] border-b-[8px] border-b-transparent ml-1" ] []
                                ]
                            , div [ class "flex flex-col items-start" ]
                                [ p [ class "text-[14px] font-bold text-[#03045E] -tracking-[0.03em] leading-[1.21] text-left" ] [ text "Learn About Plan G vs Plan N" ]
                                , p [ class "text-[8px] text-[#667085] -tracking-[0.03em] leading-[1.21]" ] [ text "Watch the Video" ]
                                ]
                            ]
                        ]

                    -- Plans Section
                    , viewPlansSection model
                    ]
            ]
        , viewGvsNModal model
        , viewQualificationModal model
        , viewRatesModal model
        , viewLocationModal model
        ]
    }


viewLoading : Html Msg
viewLoading =
    div [ class "fixed inset-0 bg-white flex flex-col items-center justify-center gap-4 text-center" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-purple-600 border-t-transparent" ] []
        , p [ class "text-center text-lg font-medium text-gray-600" ]
            [ text "Loading your personalized quote..." ]
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
            [ div [ class "bg-white rounded-lg p-4 sm:p-8 w-[95%] max-w-5xl mx-auto shadow-lg" ]
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
                , div [ class "flex justify-center" ]
                    [ button
                        [ class "bg-purple-500 text-white px-4 sm:px-6 py-2 rounded hover:bg-purple-600 mt-4 w-full sm:w-auto"
                        , onClick CloseGvsNVideo
                        ]
                        [ text "Continue" ]
                    ]
                ]
            ]

    else
        text ""


viewQualificationModal : Model -> Html Msg
viewQualificationModal model =
    if model.showQualificationVideo then
        div [ class "fixed inset-0 bg-black/30 flex items-center justify-center z-50 p-4 backdrop-blur-sm" ]
            [ div [ class "bg-white rounded-lg p-4 sm:p-8 w-[95%] max-w-5xl mx-auto shadow-lg" ]
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
                Maybe.withDefault "" model.county

            stateText =
                Maybe.withDefault "" model.state

            planTypeText =
                case model.selectedPlanType of
                    PlanG ->
                        "Plan G"

                    PlanN ->
                        "Plan N"
        in
        div [ class "fixed inset-0 bg-black/30 flex items-center justify-center z-50 p-4 backdrop-blur-sm" ]
            [ div [ class "bg-white rounded-lg p-4 sm:p-8 w-[95%] max-w-5xl mx-auto shadow-lg" ]
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


viewLocationModal : Model -> Html Msg
viewLocationModal model =
    if model.showLocationModal then
        div [ class "fixed inset-0 bg-black/30 flex items-center justify-center z-50 p-4 backdrop-blur-sm" ]
            [ div [ class "bg-white rounded-lg p-6 w-[95%] max-w-md mx-auto shadow-lg" ]
                [ div [ class "flex justify-between items-center mb-4" ]
                    [ h2 [ class "text-xl font-extrabold -tracking-[0.04em] text-[#101828]" ] [ text "Update Location" ]
                    , button
                        [ class "text-[#667085] hover:text-[#101828] transition-colors"
                        , onClick CloseLocationModal
                        ]
                        [ text "×" ]
                    ]
                , div [ class "mb-4" ]
                    [ label [ class "block text-sm font-medium text-[#667085] mb-1" ]
                        [ text "ZIP Code" ]
                    , input
                        [ type_ "text"
                        , class "w-full px-3 py-2 border border-[#DCE2E5] rounded-md focus:outline-none focus:ring-1 focus:ring-[#03045E] focus:border-[#03045E]"
                        , value (Maybe.withDefault "" model.editingZipCode)
                        , onInput UpdateZipCode
                        , maxlength 5
                        , pattern "[0-9]*"
                        ]
                        []
                    ]
                , if not (List.isEmpty model.availableCounties) then
                    div [ class "mb-4" ]
                        [ label [ class "block text-sm font-medium text-[#667085] mb-1" ]
                            [ text "County" ]
                        , select
                            [ class "w-full px-3 py-2 border border-[#DCE2E5] rounded-md focus:outline-none focus:ring-1 focus:ring-[#03045E] focus:border-[#03045E]"
                            , onInput UpdateCounty
                            , value (Maybe.withDefault "" model.editingCounty)
                            ]
                            (option [ value "" ] [ text "Select a county" ]
                                :: List.map
                                    (\county ->
                                        option [ value county ]
                                            [ text county ]
                                    )
                                    model.availableCounties
                            )
                        ]

                  else
                    text ""
                , if model.locationUpdateError /= Nothing then
                    div [ class "mb-4 text-red-600 text-sm" ]
                        [ text (Maybe.withDefault "" model.locationUpdateError) ]

                  else
                    text ""
                , div [ class "flex justify-end gap-3" ]
                    [ button
                        [ class "px-4 py-2 text-[#667085] hover:text-[#101828] transition-colors"
                        , onClick CloseLocationModal
                        ]
                        [ text "Cancel" ]
                    , button
                        [ class "px-4 py-2 bg-[#03045E] text-white rounded hover:bg-[#02034D] transition-colors disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center min-w-[80px]"
                        , onClick SubmitLocationUpdate
                        , disabled
                            (String.length (Maybe.withDefault "" model.editingZipCode)
                                /= 5
                                || (not (List.isEmpty model.availableCounties)
                                        && Maybe.withDefault "" model.editingCounty
                                        == ""
                                   )
                                || model.submittingLocation
                            )
                        ]
                        [ if model.submittingLocation then
                            div [ class "animate-spin rounded-full h-4 w-4 border-2 border-white border-t-transparent mr-2" ] []

                          else
                            text ""
                        , text
                            (if model.submittingLocation then
                                "Updating..."

                             else
                                "Update"
                            )
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
    D.map6 ContactResponse
        (D.field "contact" contactDecoder)
        (D.field "agent" agentDecoder)
        (D.field "orgSlug" D.string)
        (D.field "orgName" D.string)
        (D.field "orgLogo" (D.nullable D.string))
        (D.field "carrierContracts" (D.list carrierDecoder))


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
    D.map4 Agent
        (D.field "firstName" D.string)
        (D.field "lastName" D.string)
        (D.field "email" D.string)
        (D.field "phone" D.string)


locationUpdateResponseDecoder : Decoder LocationUpdateResponse
locationUpdateResponseDecoder =
    D.map4 LocationUpdateResponse
        (D.field "success" D.bool)
        (D.field "zipCode" D.string)
        (D.field "state" D.string)
        (D.field "counties" (D.list D.string))
