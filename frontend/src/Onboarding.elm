module Onboarding exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import CarrierNaic exposing (Carrier(..), allCarriers, carrierToString)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import MyIcon
import Task
import Url exposing (Url)
import Url.Builder exposing (absolute, int, string)
import Url.Parser.Query as Query
import Utils.UrlStuff exposing (getQueryParams)



-- MODEL


type PaymentStatus
    = Loading
    | ReadyToComplete
    | Continuing
    | Error String


type alias CalculationInputs =
    { contacts : Int
    , averageAge : Float
    , rolloverPercent : Float
    , commissionRate : Float
    }


type alias Model =
    { user : User
    , paymentStatus : PaymentStatus
    , frame : Int
    , companyName : String
    , companyPhone : String
    , companyWebsite : String
    , primaryColor : String
    , secondaryColor : String
    , logo : Maybe String
    , uploadingLogo : Bool
    , key : Nav.Key
    , contactCount : String
    , calculatedPrice : Maybe Int
    , rolloverPercent : String
    , commissionRate : String
    , calculatedRevenue : Maybe Float
    , firstYearRevenue : Maybe Float
    , calculatorExpanded : Bool
    , selectedCarriers : List Carrier
    , useSmartSend : Bool
    , agents : List Agent
    , showAgentForm : Bool
    , newAgentFirstName : String
    , newAgentLastName : String
    , newAgentEmail : String
    , newAgentPhone : String
    , newAgentIsAdmin : Bool
    , loadingResumeData : Bool
    }


type alias Agent =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , isAdmin : Bool
    }


maxFrame : Int
maxFrame =
    5


dummyUser : User
dummyUser =
    { firstName = "John"
    , lastName = "Doe"
    , email = "john.doe@example.com"
    , phone = ""
    }


type alias User =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    }


init : Nav.Key -> Url -> ( Model, Cmd Msg )
init key url =
    let
        queryParams =
            url |> getQueryParams

        firstName =
            Dict.get "firstName" queryParams

        lastName =
            Dict.get "lastName" queryParams

        email =
            Dict.get "email" queryParams

        phone =
            Dict.get "phone" queryParams

        frame =
            case Dict.get "frame" queryParams of
                Just f ->
                    case String.toInt f of
                        Just i ->
                            Basics.clamp 1 maxFrame i

                        Nothing ->
                            1

                Nothing ->
                    1

        maybeUser =
            case ( firstName, lastName, email ) of
                ( Just f, Just l, Just e ) ->
                    Just { firstName = f, lastName = l, email = e, phone = phone |> Maybe.withDefault "" }

                _ ->
                    Nothing

        currentUser =
            maybeUser |> Maybe.withDefault dummyUser

        initialAgents =
            [ { firstName = currentUser.firstName
              , lastName = currentUser.lastName
              , email = currentUser.email
              , phone = currentUser.phone
              , isAdmin = True -- Make the initial user an admin by default
              }
            ]

        initialModel =
            { user = currentUser
            , paymentStatus = ReadyToComplete
            , frame = frame
            , companyName = ""
            , companyPhone = ""
            , companyWebsite = ""
            , primaryColor = "#6B46C1"
            , secondaryColor = "#9F7AEA"
            , logo = Nothing
            , uploadingLogo = False
            , key = key
            , contactCount = "500"
            , calculatedPrice = Just 60
            , rolloverPercent = "3"
            , commissionRate = "350"
            , calculatedRevenue = Just 175000
            , firstYearRevenue = Just 8750
            , calculatorExpanded = False
            , selectedCarriers = []
            , useSmartSend = True
            , agents = initialAgents
            , showAgentForm = False
            , newAgentFirstName = ""
            , newAgentLastName = ""
            , newAgentEmail = ""
            , newAgentPhone = ""
            , newAgentIsAdmin = True
            , loadingResumeData = False
            }

        redirectCommand =
            case maybeUser of
                Just user ->
                    -- Try to resume session
                    fetchResumeData user.email

                Nothing ->
                    Nav.pushUrl key "/signup"
    in
    ( { initialModel | loadingResumeData = maybeUser /= Nothing }, redirectCommand )



-- UPDATE


type Msg
    = NoOp
    | PaymentCompleted PaymentStatus
    | CompanyNameChanged String
    | PhoneChanged String
    | WebsiteChanged String
    | PrimaryColorChanged String
    | SecondaryColorChanged String
    | UploadLogo
    | GotLogo File
    | GotLogoUrl String
    | ContinueClicked
    | BackClicked
    | ContactCountChanged String
    | RolloverPercentChanged String
    | CommissionRateChanged String
    | ToggleCalculator
    | ToggleCarrier Carrier
    | ToggleSmartSend
    | ToggleAllCarriers
    | ShowAgentForm
    | HideAgentForm
    | AgentFirstNameChanged String
    | AgentLastNameChanged String
    | AgentEmailChanged String
    | AgentPhoneChanged String
    | AgentIsAdminToggled Bool
    | AddAgent
    | AgentsSaved (Result Http.Error ())
    | GotResumeData (Result Http.Error ResumeData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PaymentCompleted status ->
            ( { model | paymentStatus = status }, Cmd.none )

        CompanyNameChanged name ->
            ( { model | companyName = name }, Cmd.none )

        PhoneChanged phone ->
            ( { model | companyPhone = phone }, Cmd.none )

        WebsiteChanged website ->
            ( { model | companyWebsite = website }, Cmd.none )

        PrimaryColorChanged color ->
            ( { model | primaryColor = color }, Cmd.none )

        SecondaryColorChanged color ->
            ( { model | secondaryColor = color }, Cmd.none )

        UploadLogo ->
            ( model, Select.file [ "image/png", "image/jpeg" ] GotLogo )

        GotLogo file ->
            ( { model | uploadingLogo = True }, Task.perform GotLogoUrl (File.toUrl file) )

        GotLogoUrl url ->
            ( { model | logo = Just url, uploadingLogo = False }, Cmd.none )

        ToggleCalculator ->
            ( { model | calculatorExpanded = not model.calculatorExpanded }, Cmd.none )

        ContinueClicked ->
            let
                newFrame =
                    Basics.min maxFrame (model.frame + 1)
            in
            ( { model | frame = newFrame }
            , case model.frame of
                2 ->
                    Nav.load (buildUrl model newFrame)

                3 ->
                    Cmd.batch
                        [ saveCompanyDetails model
                        , Nav.pushUrl model.key (buildUrl model newFrame)
                        ]

                4 ->
                    Cmd.batch
                        [ saveLicensingSettings model
                        , Nav.pushUrl model.key (buildUrl model newFrame)
                        ]

                5 ->
                    Cmd.batch
                        [ saveAgents model
                        , Nav.pushUrl model.key "/login" -- Go to dashboard after onboarding is complete
                        ]

                _ ->
                    Nav.pushUrl model.key (buildUrl model newFrame)
            )

        BackClicked ->
            let
                newFrame =
                    Basics.max 1 (model.frame - 1)
            in
            ( { model | frame = newFrame }
            , case newFrame of
                2 ->
                    Nav.load (buildUrl model newFrame)

                _ ->
                    Nav.pushUrl model.key (buildUrl model newFrame)
            )

        ContactCountChanged count ->
            let
                maybeInt =
                    if String.isEmpty count then
                        Nothing

                    else
                        String.toInt count

                calculatedPrice =
                    maybeInt
                        |> Maybe.map calculatePrice

                -- Create a temporary model with the new count for parsing
                tempModel =
                    { model | contactCount = count }

                maybeInputs =
                    parseCalculationInputs tempModel

                ( calculatedRevenue, firstYearRevenue ) =
                    case maybeInputs of
                        Just inputs ->
                            let
                                ( rev, firstYear ) =
                                    calculateRevenue inputs
                            in
                            ( Just rev, Just firstYear )

                        Nothing ->
                            ( Nothing, Nothing )
            in
            ( { model
                | contactCount = count
                , calculatedPrice = calculatedPrice
                , calculatedRevenue = calculatedRevenue
                , firstYearRevenue = firstYearRevenue
              }
            , Cmd.none
            )

        RolloverPercentChanged percent ->
            let
                maybeInputs =
                    parseCalculationInputsWithRollover model percent

                ( calculatedRevenue, firstYearRevenue ) =
                    case maybeInputs of
                        Just inputs ->
                            let
                                ( rev, firstYear ) =
                                    calculateRevenue inputs
                            in
                            ( Just rev, Just firstYear )

                        Nothing ->
                            ( Nothing, Nothing )
            in
            ( { model
                | rolloverPercent = percent
                , calculatedRevenue = calculatedRevenue
                , firstYearRevenue = firstYearRevenue
              }
            , Cmd.none
            )

        CommissionRateChanged rate ->
            let
                maybeInputs =
                    parseCalculationInputsWithCommission model rate

                ( calculatedRevenue, firstYearRevenue ) =
                    case maybeInputs of
                        Just inputs ->
                            let
                                ( rev, firstYear ) =
                                    calculateRevenue inputs
                            in
                            ( Just rev, Just firstYear )

                        Nothing ->
                            ( Nothing, Nothing )
            in
            ( { model
                | commissionRate = rate
                , calculatedRevenue = calculatedRevenue
                , firstYearRevenue = firstYearRevenue
              }
            , Cmd.none
            )

        ToggleCarrier carrier ->
            let
                newSelectedCarriers =
                    if List.member carrier model.selectedCarriers then
                        List.filter (\c -> c /= carrier) model.selectedCarriers

                    else
                        carrier :: model.selectedCarriers
            in
            ( { model | selectedCarriers = newSelectedCarriers }, Cmd.none )

        ToggleSmartSend ->
            ( { model | useSmartSend = not model.useSmartSend }, Cmd.none )

        ToggleAllCarriers ->
            let
                newSelectedCarriers =
                    if List.length model.selectedCarriers == List.length allCarriers then
                        []

                    else
                        allCarriers
            in
            ( { model | selectedCarriers = newSelectedCarriers }, Cmd.none )

        ShowAgentForm ->
            ( { model | showAgentForm = True }, Cmd.none )

        HideAgentForm ->
            ( { model | showAgentForm = False }, Cmd.none )

        AgentFirstNameChanged firstName ->
            ( { model | newAgentFirstName = firstName }, Cmd.none )

        AgentLastNameChanged lastName ->
            ( { model | newAgentLastName = lastName }, Cmd.none )

        AgentEmailChanged email ->
            ( { model | newAgentEmail = email }, Cmd.none )

        AgentPhoneChanged phone ->
            ( { model | newAgentPhone = phone }, Cmd.none )

        AgentIsAdminToggled isAdmin ->
            ( { model | newAgentIsAdmin = True }, Cmd.none )

        AddAgent ->
            let
                newAgent =
                    { firstName = model.newAgentFirstName
                    , lastName = model.newAgentLastName
                    , email = model.newAgentEmail
                    , phone = model.newAgentPhone
                    , isAdmin = True -- Keep the field value as True for consistency
                    }

                isValid =
                    not (String.isEmpty (String.trim model.newAgentFirstName))
                        && not (String.isEmpty (String.trim model.newAgentLastName))
                        && not (String.isEmpty (String.trim model.newAgentEmail))
            in
            if isValid then
                ( { model
                    | agents = model.agents ++ [ newAgent ]
                    , showAgentForm = False
                    , newAgentFirstName = ""
                    , newAgentLastName = ""
                    , newAgentEmail = ""
                    , newAgentPhone = ""
                    , newAgentIsAdmin = True -- Keep the field value as True for consistency
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        AgentsSaved result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotResumeData result ->
            case result of
                Ok resumeData ->
                    if resumeData.onboardingComplete && resumeData.redirectToLogin then
                        -- Onboarding is complete, redirect to login
                        ( model, Nav.load "/login" )

                    else if resumeData.success then
                        -- Update model with resumed data
                        let
                            -- Convert carrier strings to Carrier type
                            selectedCarriers =
                                resumeData.carrierSettings.selectedCarriers
                                    |> List.filterMap
                                        (\carrierStr ->
                                            allCarriers
                                                |> List.filter (\c -> carrierToString c == carrierStr)
                                                |> List.head
                                        )

                            -- Create agents list with the current user marked as admin if not in list
                            updatedAgents =
                                if List.isEmpty resumeData.agents then
                                    model.agents

                                else
                                    resumeData.agents

                            -- Set payment status based on saved data
                            paymentStatus =
                                if resumeData.paymentStatus.paymentCompleted then
                                    Continuing

                                else
                                    model.paymentStatus
                        in
                        ( { model
                            | loadingResumeData = False
                            , companyName = resumeData.organization.name
                            , companyPhone = resumeData.organization.phone
                            , companyWebsite = resumeData.organization.website
                            , primaryColor = resumeData.organization.primaryColor
                            , secondaryColor = resumeData.organization.secondaryColor
                            , logo = resumeData.organization.logo
                            , selectedCarriers = selectedCarriers
                            , useSmartSend = resumeData.carrierSettings.useSmartSend
                            , agents = updatedAgents
                            , paymentStatus = paymentStatus
                          }
                        , Cmd.none
                        )

                    else
                        -- Failed to get resume data but not critical, continue with current model
                        ( { model | loadingResumeData = False }, Cmd.none )

                Err err ->
                    -- Error fetching resume data, continue with current model
                    ( { model | loadingResumeData = False }, Cmd.none )



-- Calculate the price based on the number of contacts


calculatePrice : Int -> Int
calculatePrice contacts =
    let
        basePrice =
            60

        -- Base price for up to 500 contacts
        additionalTiers =
            Basics.max 0 (ceiling (toFloat (Basics.max 0 (contacts - 500)) / 500))

        additionalPrice =
            additionalTiers * 40

        -- $40 for each additional 500 contacts
    in
    basePrice + additionalPrice



-- Calculate revenue based on contacts, average age, and rollover percent


calculateRevenue : CalculationInputs -> ( Float, Float )
calculateRevenue inputs =
    let
        -- Constants
        maxYears =
            6.0

        ltvDiscount =
            0.75

        -- account for cancellations
        -- Calculate LTV metrics
        ltvPerContact =
            inputs.commissionRate * maxYears * ltvDiscount

        -- Average remaining LTV based on current age
        avgLtv =
            ltvPerContact * (maxYears - inputs.averageAge) / maxYears

        -- Average LTV after rollover (adds another cycle)
        avgLtvNew =
            ltvPerContact * (maxYears - inputs.averageAge + maxYears) / maxYears

        -- Convert percentage to fraction
        rolloverFraction =
            inputs.rolloverPercent / 100.0

        -- Calculate future revenue
        oldFutureRevenue =
            avgLtv * toFloat inputs.contacts

        -- New future revenue (non-rollovers + rollovers)
        newFutureRevenue =
            (avgLtv * toFloat inputs.contacts * (1 - rolloverFraction))
                + (avgLtvNew * toFloat inputs.contacts * rolloverFraction)

        -- Additional revenue from rollovers
        additionalRevenue =
            newFutureRevenue - oldFutureRevenue

        -- First year additional revenue
        -- Based on the proportion of contacts at or beyond max age
        -- that would generate immediate additional revenue
        contactsAtMaxAge =
            if inputs.averageAge >= maxYears / 2 then
                -- Calculate percentage of contacts at or beyond max age
                -- based on even distribution assumption
                let
                    percentAtMaxAge =
                        (inputs.averageAge + (maxYears / 2))
                            / maxYears
                            |> Basics.min 1.0
                            |> Basics.max 0.0
                in
                percentAtMaxAge * toFloat inputs.contacts * rolloverFraction

            else
                -- Few or no contacts at max age yet
                0

        firstYearAdditional =
            contactsAtMaxAge * inputs.commissionRate
    in
    ( additionalRevenue, firstYearAdditional )



-- Handle navigation if needed
-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Onboarding"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex flex-col items-center py-12 px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex flex-col items-center w-full mb-8" ]
                [ img [ src "/images/medicare-max-logo.png", class "h-10 w-auto" ] [] ]
            , if model.loadingResumeData then
                -- Show loading indicator when resuming data
                div [ class "max-w-6xl w-full space-y-8 bg-white p-8 rounded-lg shadow-md flex flex-col items-center justify-center min-h-[400px]" ]
                    [ div [ class "animate-spin rounded-full h-12 w-12 border-b-2 border-indigo-700 mb-4" ] []
                    , p [ class "text-gray-600 text-center" ] [ text "Loading your settings..." ]
                    ]

              else
                div [ class "max-w-6xl w-full space-y-8 bg-white p-8 rounded-lg shadow-md" ]
                    [ case model.frame of
                        2 ->
                            viewStripe model

                        3 ->
                            viewCompany model

                        4 ->
                            viewLicensing model

                        5 ->
                            viewAddAgents model

                        _ ->
                            viewPricing model
                    , viewProgressDots model.frame
                    ]
            ]
        ]
    }


viewPricing : Model -> Html Msg
viewPricing model =
    div [ class "flex flex-col items-center" ]
        [ MyIcon.banknote 32 "#0F172A"
        , h2 [ class "text-2xl font-semibold text-gray-900 mt-6" ] [ text "Subscription Pricing" ]
        , p [ class "text-gray-500 mt-2 mb-6" ] [ text "Simple and transparent." ]
        , div [ class "w-full max-w-3xl mt-6" ]
            [ div [ class "flex flex-col md:flex-row gap-6" ]
                [ div [ class "bg-white overflow-hidden shadow rounded-lg divide-y divide-gray-200 md:w-1/2" ]
                    [ div [ class "px-4 py-5 sm:px-6 bg-indigo-50" ]
                        [ h3 [ class "text-lg leading-6 font-medium text-gray-900" ]
                            [ text "Base Subscription" ]
                        ]
                    , div [ class "px-4 py-5 sm:p-6" ]
                        [ div [ class "flex items-center justify-between" ]
                            [ div [ class "flex items-center" ]
                                [ span [ class "text-3xl font-bold text-gray-900" ] [ text "$60" ]
                                , span [ class "ml-2 text-gray-500" ] [ text "/month" ]
                                ]
                            , span [ class "bg-green-100 text-green-800 px-2 py-1 rounded-full text-sm font-medium" ]
                                [ text "First 500 contacts" ]
                            ]
                        , div [ class "mt-4" ]
                            [ p [ class "text-sm text-gray-500" ]
                                [ text "Our base subscription includes all features of the Medicare Max portal platform and allows you to manage up to 500 contacts." ]
                            ]
                        ]
                    ]
                , div [ class "bg-white overflow-hidden shadow rounded-lg divide-y divide-gray-200 md:w-1/2" ]
                    [ div [ class "px-4 py-5 sm:px-6 bg-indigo-50" ]
                        [ h3 [ class "text-lg leading-6 font-medium text-gray-900" ]
                            [ text "Additional Contacts" ]
                        ]
                    , div [ class "px-4 py-5 sm:p-6" ]
                        [ div [ class "flex items-center justify-between" ]
                            [ div [ class "flex items-center" ]
                                [ span [ class "text-3xl font-bold text-gray-900" ] [ text "$40" ]
                                , span [ class "ml-2 text-gray-500" ] [ text "/month" ]
                                ]
                            , span [ class "bg-blue-100 text-blue-800 px-2 py-1 rounded-full text-sm font-medium" ]
                                [ text "per 500 contacts" ]
                            ]
                        , div [ class "mt-4" ]
                            [ p [ class "text-sm text-gray-500" ]
                                [ text "For every additional 500 contacts (or portion thereof), we charge $40 per month." ]
                            ]
                        ]
                    ]
                ]
            , div [ class "bg-white overflow-hidden shadow rounded-lg divide-y divide-gray-200 mt-6" ]
                [ div
                    [ class "px-4 py-5 sm:px-6 bg-indigo-50 cursor-pointer hover:bg-indigo-100 transition-colors"
                    , onClick ToggleCalculator
                    ]
                    [ div [ class "flex justify-between items-center" ]
                        [ h3 [ class "text-lg leading-6 font-medium text-gray-900" ]
                            [ text "Price & Revenue Calculator" ]
                        , div
                            [ class "transform transition-transform duration-200"
                            , class
                                (if model.calculatorExpanded then
                                    "-rotate-90"

                                 else
                                    "rotate-90"
                                )
                            ]
                            [ MyIcon.chevronRight 24 "#4A5568" ]
                        ]
                    ]
                , if model.calculatorExpanded then
                    div [ class "px-4 py-5 sm:p-6" ]
                        [ div [ class "space-y-6" ]
                            [ div [ class "bg-white p-4 rounded-md shadow-sm border border-gray-200" ]
                                [ h4 [ class "text-md font-medium text-gray-800 mb-3" ] [ text "Inputs" ]
                                , div [ class "flex flex-wrap gap-4" ]
                                    [ div [ class "flex flex-col" ]
                                        [ label
                                            [ class "block text-sm font-medium text-gray-700 mb-1 cursor-pointer"
                                            , for "contact-count"
                                            ]
                                            [ text "Number of Contacts" ]
                                        , div [ class "w-[140px]" ]
                                            [ input
                                                [ class "w-full border border-gray-300 rounded-md shadow-sm py-1.5 px-2 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm text-center"
                                                , id "contact-count"
                                                , type_ "number"
                                                , placeholder "Enter number"
                                                , value model.contactCount
                                                , onInput ContactCountChanged
                                                , Html.Attributes.min "0"
                                                , Html.Attributes.step "1"
                                                ]
                                                []
                                            ]
                                        ]
                                    , div [ class "flex flex-col" ]
                                        [ label
                                            [ class "block text-sm font-medium text-gray-700 mb-1 cursor-pointer"
                                            , for "commission-rate"
                                            ]
                                            [ text "Commission Per Contact" ]
                                        , div [ class "flex rounded-md shadow-sm w-[140px]" ]
                                            [ div [ class "flex-shrink-0 inline-flex items-center px-2 rounded-l-md border border-r-0 border-gray-300 bg-indigo-100 text-indigo-800 text-sm font-medium" ]
                                                [ text "$" ]
                                            , input
                                                [ class "w-full border border-gray-300 rounded-none rounded-r-md shadow-sm py-1.5 px-2 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm text-center"
                                                , id "commission-rate"
                                                , type_ "number"
                                                , placeholder "Commission"
                                                , value model.commissionRate
                                                , onInput CommissionRateChanged
                                                , Html.Attributes.min "0"
                                                , Html.Attributes.step "5"
                                                ]
                                                []
                                            ]
                                        ]
                                    , div [ class "flex flex-col" ]
                                        [ label
                                            [ class "block text-sm font-medium text-gray-700 mb-1 cursor-pointer"
                                            , for "rollover-percent"
                                            ]
                                            [ text "Annual Rollover" ]
                                        , div [ class "flex rounded-md shadow-sm w-[140px]" ]
                                            [ div [ class "flex-shrink-0 inline-flex items-center px-2 rounded-l-md border border-r-0 border-gray-300 bg-indigo-100 text-indigo-800 text-sm font-medium" ]
                                                [ text "%" ]
                                            , input
                                                [ class "w-full border border-gray-300 rounded-none rounded-r-md shadow-sm py-1.5 px-2 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm text-center"
                                                , id "rollover-percent"
                                                , type_ "number"
                                                , placeholder "Rollover"
                                                , value model.rolloverPercent
                                                , onInput RolloverPercentChanged
                                                , Html.Attributes.min "0"
                                                , Html.Attributes.max "100"
                                                , Html.Attributes.step "0.1"
                                                ]
                                                []
                                            ]
                                        ]
                                    ]
                                ]
                            , div [ class "bg-white p-4 rounded-md shadow-sm border border-gray-200" ]
                                [ h4 [ class "text-md font-medium text-gray-800 mb-4" ] [ text "Subscription Cost" ]
                                , table [ class "w-full text-sm" ]
                                    [ tbody []
                                        [ tr [ class "border-b border-gray-200" ]
                                            [ td [ class "py-2 text-gray-600" ] [ text "Base subscription:" ]
                                            , td [ class "py-2 text-right font-medium" ] [ text "$60/month" ]
                                            ]
                                        , if model.calculatedPrice /= Just 60 then
                                            tr [ class "border-b border-gray-200" ]
                                                [ td [ class "py-2 text-gray-600" ] [ text "Additional contacts cost:" ]
                                                , td [ class "py-2 text-right font-medium" ]
                                                    [ text <| "$" ++ String.fromInt (Maybe.withDefault 0 model.calculatedPrice - 60) ++ "/month" ]
                                                ]

                                          else
                                            text ""
                                        , tr [ class "border-b border-gray-200" ]
                                            [ td [ class "py-2 font-medium" ] [ text "Monthly total:" ]
                                            , td [ class "py-2 text-right font-bold text-indigo-600" ]
                                                [ text <| "$" ++ String.fromInt (Maybe.withDefault 0 model.calculatedPrice) ]
                                            ]
                                        , tr []
                                            [ td [ class "py-2 font-medium" ] [ text "Annual total:" ]
                                            , td [ class "py-2 text-right font-bold text-indigo-600" ]
                                                [ text <| "$" ++ String.fromInt (Maybe.withDefault 0 model.calculatedPrice * 12) ]
                                            ]
                                        ]
                                    ]
                                ]
                            , div [ class "bg-white p-4 rounded-md shadow-sm border border-gray-200" ]
                                [ h4 [ class "text-md font-medium text-gray-800 mb-4" ] [ text "Revenue Impact" ]
                                , p [ class "text-xs text-gray-600 mb-4 italic" ]
                                    [ text "Calculations assume an average book age of 3 years, discounted to account for cancellations." ]
                                , let
                                    inputs =
                                        case parseCalculationInputs model of
                                            Just i ->
                                                i

                                            Nothing ->
                                                { contacts = 0
                                                , averageAge = 0
                                                , rolloverPercent = 0
                                                , commissionRate = 0
                                                }

                                    -- Constants
                                    maxYears =
                                        6.0

                                    ltvDiscountMultiplier =
                                        0.75

                                    -- Basic calculations
                                    ltvPerContact =
                                        inputs.commissionRate * maxYears * ltvDiscountMultiplier

                                    rolloverFraction =
                                        inputs.rolloverPercent / 100.0

                                    contactsRolledOver =
                                        toFloat inputs.contacts * rolloverFraction

                                    -- LTV calculations
                                    remainingYears =
                                        maxYears - inputs.averageAge

                                    additionalYearsForRolled =
                                        maxYears - remainingYears

                                    additionalYearsDiscount =
                                        (1 - ltvDiscountMultiplier) * additionalYearsForRolled / maxYears

                                    additionalYearsDiscountMultiplier =
                                        1 - additionalYearsDiscount

                                    ltvGainPerContact =
                                        inputs.commissionRate * additionalYearsForRolled * additionalYearsDiscountMultiplier

                                    ltvGainPerYear =
                                        ltvGainPerContact * contactsRolledOver

                                    contactsAtMaxAge =
                                        -- assumes equal distribution across 6 years
                                        toFloat inputs.contacts / 6.0

                                    firstYearPayout =
                                        contactsAtMaxAge * inputs.commissionRate * rolloverFraction

                                    -- Return on investment
                                    annualCost =
                                        toFloat (Maybe.withDefault 0 model.calculatedPrice * 12)

                                    roi =
                                        (ltvGainPerYear / annualCost) |> (\x -> toFloat (Basics.round (x * 10)) / 10)
                                  in
                                  div [ class "space-y-4" ]
                                    [ div [ class "grid grid-cols-1 gap-2" ]
                                        [ div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                                            [ div [ class "text-gray-600 truncate pr-4" ] [ text "Lifetime Revenue Per Contact:" ]
                                            , div [ class "text-right font-medium whitespace-nowrap" ]
                                                [ text <| "$" ++ formatPreciseMoney ltvPerContact ]
                                            ]
                                        , div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                                            [ div [ class "text-gray-600 truncate pr-4" ] [ text "Lifetime Gain Per Contact Rolled Over:" ]
                                            , div [ class "text-right font-medium whitespace-nowrap" ]
                                                [ text <| "$" ++ formatPreciseMoney ltvGainPerContact ]
                                            ]
                                        , div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                                            [ div [ class "text-gray-600 truncate pr-4" ] [ text "Contacts Rolled Over Per Year:" ]
                                            , div [ class "text-right font-medium whitespace-nowrap" ]
                                                [ text <| formatNumber contactsRolledOver ]
                                            ]
                                        , div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                                            [ div [ class "text-gray-600 truncate pr-4" ] [ text "Average First Year Payout:" ]
                                            , div [ class "text-right font-medium text-green-600 whitespace-nowrap" ]
                                                [ text <| "$" ++ formatPreciseMoney firstYearPayout ]
                                            ]
                                        , div [ class "flex justify-between items-center border-b border-gray-200 py-2" ]
                                            [ div [ class "text-gray-600 truncate pr-4" ] [ text "Lifetime Revenue Added Per Year:" ]
                                            , div [ class "text-right font-medium text-green-600 whitespace-nowrap" ]
                                                [ text <| "$" ++ formatPreciseMoney ltvGainPerYear ]
                                            ]
                                        , div [ class "flex justify-between items-center py-2" ]
                                            [ div [ class "font-medium text-gray-700 truncate pr-4" ] [ text "Return on Investment:" ]
                                            , div [ class "text-right font-bold text-indigo-700 whitespace-nowrap" ]
                                                [ text <| String.fromFloat roi ++ "x" ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]

                  else
                    text ""
                ]
            , div [ class "w-full flex justify-center mt-8" ]
                [ button
                    [ class "bg-indigo-900 text-white py-2 px-6 rounded-md hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    , onClick ContinueClicked
                    ]
                    [ text "Continue to Payment" ]
                ]
            ]
        ]


viewStripe : Model -> Html Msg
viewStripe model =
    case model.paymentStatus of
        Loading ->
            div [ class "mt-4 pt-3 pb-32 px-4 bg-white border border-gray-200 rounded-md min-h-[600px] flex flex-col items-center justify-center" ]
                [ div [ class "animate-spin rounded-full h-16 w-16 border-b-2 border-indigo-700 mb-6" ] []
                , p [ class "text-gray-600 text-center text-lg" ] [ text "Loading secure payment form..." ]
                , p [ class "text-sm text-gray-500 text-center mt-3 max-w-md" ]
                    [ text "Please wait while we connect to our payment processor. This may take a few moments." ]
                ]

        Error error ->
            div [ class "mt-4 p-3 bg-red-50 border border-red-200 rounded-md" ]
                [ p [ class "text-red-700" ] [ text "Error" ]
                , p [ class "mt-2 text-sm text-red-600" ]
                    [ text "If you're using an ad blocker, please disable it for this site as it may interfere with payment processing." ]
                ]

        Continuing ->
            div [ class "mt-4 pt-3 pb-32 px-4 bg-white border border-gray-200 rounded-md min-h-[600px] flex flex-col items-center justify-center" ]
                [ div [ class "flex flex-col items-center" ]
                    [ div [ class "rounded-full bg-green-100 p-6 mb-6" ]
                        [ MyIcon.banknote 48 "#047857" ]
                    , h3 [ class "text-xl font-semibold text-gray-800 mb-2" ]
                        [ text "Payment Already Processed" ]
                    , p [ class "text-gray-600 text-center max-w-md mb-8" ]
                        [ text "Your subscription payment has been successfully processed. You can continue with the next steps of your onboarding process." ]
                    , button
                        [ class "bg-indigo-900 text-white py-2 px-6 rounded-md hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        , onClick ContinueClicked
                        ]
                        [ text "Continue to Company Setup" ]
                    ]
                ]

        ReadyToComplete ->
            node "stripe-checkout"
                [ attribute "price-id" "price_1RBStWCBUPXAZKNGwpimWl7v" -- Base Subscription price
                , attribute "metered-price-id" "price_1RBSvJCBUPXAZKNGQ1U9Hl8i" -- Contact Tier price
                , attribute "return-url" ("http://localhost:5173" ++ buildUrl model 3) -- Return to frame 3 after payment
                , attribute "first-name" model.user.firstName
                , attribute "last-name" model.user.lastName
                , attribute "email" model.user.email
                ]
                []


viewCompany : Model -> Html Msg
viewCompany model =
    div [ class "flex flex-col items-center" ]
        [ div [ class "text-center mb-8 w-full" ]
            [ h2 [ class "text-3xl font-semibold text-gray-900 mb-2" ] [ text "Company Settings" ]
            , p [ class "text-gray-500" ] [ text "Upload your logo and set your brand color." ]
            ]
        , div [ class "w-full max-w-md space-y-8 bg-white p-6 rounded-lg shadow-sm" ]
            [ div [ class "space-y-2" ]
                [ label [ class "block text-sm font-medium text-gray-700", for "company-name" ] [ text "Company Name" ]
                , input
                    [ class "block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
                    , id "company-name"
                    , type_ "text"
                    , placeholder "Your Company Name"
                    , value model.companyName
                    , onInput CompanyNameChanged
                    ]
                    []
                ]
            , div [ class "flex space-x-4" ]
                [ div [ class "w-1/2 space-y-2" ]
                    [ label [ class "block text-sm font-medium text-gray-700", for "phone" ] [ text "Phone" ]
                    , input
                        [ class "block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
                        , id "phone"
                        , type_ "tel"
                        , placeholder "(555) 555-5555"
                        , value model.companyPhone
                        , onInput PhoneChanged
                        ]
                        []
                    ]
                , div [ class "w-1/2 space-y-2" ]
                    [ label [ class "block text-sm font-medium text-gray-700", for "website" ] [ text "Website" ]
                    , div [ class "flex rounded-md shadow-sm" ]
                        [ span [ class "inline-flex items-center px-3 rounded-l-md border border-r-0 border-gray-300 bg-gray-50 text-gray-500 text-sm" ]
                            [ text "https://" ]
                        , input
                            [ class "block w-full flex-1 rounded-none rounded-r-md border border-gray-300 px-3 py-2 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
                            , id "website"
                            , type_ "text"
                            , placeholder "www.example.com"
                            , value model.companyWebsite
                            , onInput WebsiteChanged
                            ]
                            []
                        ]
                    ]
                ]
            , div
                [ class "mt-8 p-6 border border-gray-200 rounded-lg border-dashed text-center cursor-pointer hover:bg-gray-50 transition-colors"
                , onClick UploadLogo
                ]
                [ case model.logo of
                    Just logoUrl ->
                        div [ class "flex flex-col items-center" ]
                            [ img
                                [ src logoUrl
                                , class "h-20 w-20 object-contain mb-4"
                                ]
                                []
                            , div [ class "text-indigo-600 font-medium" ] [ text "Change logo" ]
                            ]

                    Nothing ->
                        if model.uploadingLogo then
                            div [ class "flex flex-col items-center" ]
                                [ div [ class "animate-spin rounded-full h-10 w-10 border-t-2 border-b-2 border-indigo-500 mb-4" ] []
                                , div [ class "text-gray-500" ] [ text "Uploading..." ]
                                ]

                        else
                            div [ class "flex flex-col items-center" ]
                                [ div [ class "rounded-full bg-gray-100 p-3 mb-3" ]
                                    [ MyIcon.clipboardList 24 "#6366F1" ]
                                , div [ class "text-indigo-600 font-medium" ] [ text "Click to upload" ]
                                , div [ class "text-gray-500 text-sm mt-1" ] [ text "or drag and drop your logo" ]
                                , div [ class "text-gray-400 text-xs mt-2" ] [ text "SVG, JPG, or PNG (Recommended: 240px width x 60px height)" ]
                                ]
                ]
            , div [ class "space-y-4 mt-8" ]
                [ div [ class "space-y-2" ]
                    [ label [ class "block text-sm font-medium text-gray-700" ] [ text "Primary Brand Color" ]
                    , div [ class "flex items-center space-x-4" ]
                        [ input
                            [ type_ "color"
                            , class "w-10 h-10 p-0 border-0 rounded-md cursor-pointer"
                            , value model.primaryColor
                            , onInput PrimaryColorChanged
                            ]
                            []
                        , div [ class "text-sm text-gray-500" ] [ text "Click to change colors" ]
                        ]
                    ]
                , div [ class "space-y-2 mt-4" ]
                    [ label [ class "block text-sm font-medium text-gray-700" ] [ text "Secondary Brand Color" ]
                    , div [ class "flex items-center space-x-4" ]
                        [ input
                            [ type_ "color"
                            , class "w-10 h-10 p-0 border-0 rounded-md cursor-pointer"
                            , value model.secondaryColor
                            , onInput SecondaryColorChanged
                            ]
                            []
                        , div [ class "text-sm text-gray-500" ] [ text "Click to change colors" ]
                        ]
                    ]
                ]
            ]
        , div [ class "mt-10 w-full max-w-md" ]
            [ button
                [ class "w-full bg-indigo-900 text-white py-3 px-4 rounded-md hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 font-medium"
                , onClick ContinueClicked
                ]
                [ text "Continue" ]
            ]
        ]


viewProgressDots : Int -> Html Msg
viewProgressDots currentFrame =
    let
        frames =
            List.range 1 maxFrame

        isActive frame =
            frame == currentFrame

        backButton =
            if currentFrame > 1 then
                div [ class "cursor-pointer w-10 flex justify-center items-center", onClick BackClicked ]
                    [ MyIcon.chevronLeft 32 "#4B5563" ]

            else
                div [ class "cursor-not-allowed w-10 flex justify-center items-center" ]
                    [ MyIcon.chevronLeft 32 "#E5E7EB" ]

        rightButton =
            div [ class "cursor-not-allowed w-10 flex justify-center items-center" ]
                [ MyIcon.chevronRight 32 "#E5E7EB" ]

        dots =
            List.map
                (\frame ->
                    div
                        [ class
                            (if isActive frame then
                                "w-2 h-2 rounded-full bg-indigo-600"

                             else
                                "w-2 h-2 rounded-full bg-gray-300"
                            )
                        ]
                        []
                )
                frames
    in
    div [ class "flex justify-center items-center mt-8" ]
        [ backButton
        , div [ class "flex justify-center space-x-2" ] dots
        , rightButton
        ]


viewLicensing : Model -> Html Msg
viewLicensing model =
    div [ class "flex flex-col items-center" ]
        [ MyIcon.clipboardList 32 "#0F172A"
        , h2 [ class "text-2xl font-semibold text-gray-900 mt-6" ] [ text "Carrier Information" ]
        , p [ class "text-gray-500 mt-2 mb-6" ] [ text "Tell us about your carrier relationships." ]
        , div [ class "w-full max-w-md space-y-6" ]
            [ div [ class "space-y-4" ]
                [ h3 [ class "text-xl font-medium text-gray-800" ] [ text "Carrier Contracts" ]
                , div [ class "mt-4 space-y-2 bg-gray-50 rounded-md p-4" ]
                    [ div [ class "flex items-center mb-3" ]
                        [ div [ class "flex items-center h-5" ]
                            [ input
                                [ type_ "checkbox"
                                , class "h-4 w-4 text-indigo-600 border-gray-300 rounded focus:ring-indigo-500"
                                , checked (List.length model.selectedCarriers == List.length allCarriers)
                                , onClick ToggleAllCarriers
                                , id "select-all-carriers"
                                ]
                                []
                            ]
                        , div [ class "ml-3 text-sm" ]
                            [ label
                                [ class "font-medium text-gray-700 cursor-pointer"
                                , for "select-all-carriers"
                                ]
                                [ text "Select All Carriers" ]
                            ]
                        ]
                    , div [ class "grid grid-cols-2 gap-2" ]
                        (List.map
                            (\carrier ->
                                let
                                    carrierId =
                                        "carrier-" ++ (carrierToString carrier |> String.toLower |> String.replace " " "-")
                                in
                                div [ class "flex items-center" ]
                                    [ div [ class "flex items-center h-5" ]
                                        [ input
                                            [ type_ "checkbox"
                                            , class "h-4 w-4 text-indigo-600 border-gray-300 rounded focus:ring-indigo-500"
                                            , checked (List.member carrier model.selectedCarriers)
                                            , onClick (ToggleCarrier carrier)
                                            , id carrierId
                                            ]
                                            []
                                        ]
                                    , div [ class "ml-3 text-sm" ]
                                        [ label
                                            [ class "font-medium text-gray-700 cursor-pointer"
                                            , for carrierId
                                            ]
                                            [ text (carrierToString carrier) ]
                                        ]
                                    ]
                            )
                            allCarriers
                        )
                    ]
                ]
            , div [ class "space-y-4 mt-6" ]
                [ h3 [ class "text-xl font-medium text-gray-800" ] [ text "Guaranteed Issue Settings" ]
                , div [ class "flex items-start p-4 bg-blue-50 rounded-md border border-blue-200" ]
                    [ div [ class "flex items-center h-5 mt-1" ]
                        [ input
                            [ class "h-4 w-4 text-indigo-600 border-gray-300 rounded focus:ring-indigo-500"
                            , type_ "checkbox"
                            , id "smart-send"
                            , checked model.useSmartSend
                            , onClick ToggleSmartSend
                            ]
                            []
                        ]
                    , div [ class "ml-3" ]
                        [ label
                            [ class "font-medium text-gray-700 cursor-pointer"
                            , for "smart-send"
                            ]
                            [ text "Use SmartSend for Guaranteed Issue" ]
                        , p [ class "text-gray-600 text-sm mt-1" ]
                            [ text "When enabled, SmartSend will automatically avoid sending quotes to individuals in no-commission windows (for example, right before their birthday in Birthday Rule states)." ]
                        ]
                    ]
                ]
            , div [ class "w-full flex mt-8" ]
                [ button
                    [ class "w-full bg-indigo-900 text-white py-2 px-4 rounded-md hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    , onClick ContinueClicked
                    ]
                    [ text "Continue" ]
                ]
            ]
        ]


viewAddAgents : Model -> Html Msg
viewAddAgents model =
    div [ class "flex flex-col items-center" ]
        [ div [ class "text-center mb-8 w-full" ]
            [ h2 [ class "text-3xl font-semibold text-gray-900 mb-2" ] [ text "More Team Members?" ]
            , p [ class "text-gray-500" ] [ text "Add additional agents who will be using Medicare Max" ]
            ]
        , div [ class "w-full max-w-4xl" ]
            [ div [ class "grid grid-cols-1 gap-6 mb-8" ]
                (List.map (viewAgentCard model) model.agents)
            , if model.showAgentForm then
                viewAgentForm model

              else
                div [ class "flex justify-center" ]
                    [ button
                        [ class "flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        , onClick ShowAgentForm
                        ]
                        [ span [ class "mr-2" ] [ text "+" ]
                        , text "Add Another Agent"
                        ]
                    ]
            ]
        , div [ class "mt-10 w-full max-w-md" ]
            [ button
                [ class "w-full bg-indigo-900 text-white py-3 px-4 rounded-md hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 font-medium"
                , onClick ContinueClicked
                ]
                [ text "Complete Setup" ]
            ]
        ]


viewAgentCard : Model -> Agent -> Html Msg
viewAgentCard model agent =
    let
        isCurrentUser =
            agent.email == model.user.email

        -- Decode the email address for display
        displayEmail =
            case Url.percentDecode agent.email of
                Just decoded ->
                    decoded

                Nothing ->
                    agent.email
    in
    div [ class "bg-white shadow rounded-lg p-8" ]
        -- Increased padding from p-6 to p-8
        [ div [ class "flex items-start" ]
            -- Changed from items-center to items-start
            [ div [ class "w-16 h-16 rounded-full bg-indigo-100 flex items-center justify-center text-indigo-600 font-bold text-xl" ]
                -- Increased size and font
                [ text (String.left 1 agent.firstName ++ String.left 1 agent.lastName) ]
            , div [ class "ml-4 flex-grow" ]
                -- Added flex-grow
                [ div [ class "text-xl font-medium text-gray-900 mb-1" ]
                    -- Increased font size and added margin
                    [ text (agent.firstName ++ " " ++ agent.lastName) ]
                , div [ class "flex flex-col space-y-1 text-sm text-gray-500" ]
                    -- Added spacing
                    [ div [ class "break-all" ] [ text displayEmail ]
                    , div []
                        [ text
                            (if String.isEmpty agent.phone then
                                "No phone provided"

                             else
                                agent.phone
                            )
                        ]
                    ]
                ]
            , div [ class "ml-auto" ]
                [ span
                    [ class
                        (if agent.isAdmin then
                            "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-green-100 text-green-800"

                         else
                            "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-gray-100 text-gray-800"
                        )
                    ]
                    [ text
                        (if agent.isAdmin then
                            "Admin"

                         else
                            "Agent"
                        )
                    ]
                ]
            ]
        , if isCurrentUser then
            div [ class "mt-4 text-xs text-gray-400 italic" ]
                [ text "Current user (you)" ]

          else
            text ""
        ]


viewAgentForm : Model -> Html Msg
viewAgentForm model =
    div [ class "bg-white shadow rounded-lg p-6 mb-6" ]
        [ div [ class "flex justify-between items-center mb-4" ]
            [ h3 [ class "text-lg font-medium text-gray-900" ]
                [ text "Add New Agent" ]
            , button
                [ class "text-gray-400 hover:text-gray-500"
                , onClick HideAgentForm
                ]
                [ text "×" ]
            ]
        , div [ class "space-y-4" ]
            [ div [ class "grid grid-cols-2 gap-4" ]
                [ div []
                    [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                        [ text "First Name" ]
                    , input
                        [ class "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"
                        , type_ "text"
                        , placeholder "First name"
                        , value model.newAgentFirstName
                        , onInput AgentFirstNameChanged
                        ]
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                        [ text "Last Name" ]
                    , input
                        [ class "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"
                        , type_ "text"
                        , placeholder "Last name"
                        , value model.newAgentLastName
                        , onInput AgentLastNameChanged
                        ]
                        []
                    ]
                ]
            , div [ class "grid grid-cols-2 gap-4" ]
                [ div []
                    [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                        [ text "Email" ]
                    , input
                        [ class "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"
                        , type_ "email"
                        , placeholder "Email address"
                        , value model.newAgentEmail
                        , onInput AgentEmailChanged
                        ]
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                        [ text "Phone (optional)" ]
                    , input
                        [ class "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"
                        , type_ "tel"
                        , placeholder "Phone number"
                        , value model.newAgentPhone
                        , onInput AgentPhoneChanged
                        ]
                        []
                    ]
                ]
            , div [ class "pt-4" ]
                [ button
                    [ class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    , onClick AddAgent
                    ]
                    [ text "Add Agent" ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Helper function to build URL with frame parameter


buildUrl : Model -> Int -> String
buildUrl model frame =
    let
        -- Determine if the email appears to be already encoded
        emailIsEncoded =
            let
                decoded =
                    Url.percentDecode model.user.email
            in
            case decoded of
                Just d ->
                    d /= model.user.email

                -- If decoding changes the value, it was encoded
                Nothing ->
                    False

        -- Invalid percent encoding, treat as not encoded
        -- Base URL parameters without email
        baseParams =
            [ int "frame" frame
            , string "firstName" model.user.firstName
            , string "lastName" model.user.lastName
            , string "phone" model.user.phone
            ]

        -- If email is already encoded, manually construct the URL to avoid re-encoding
        url =
            if emailIsEncoded then
                absolute [ "onboarding" ] baseParams ++ "&email=" ++ model.user.email

            else
                absolute [ "onboarding" ] (baseParams ++ [ string "email" model.user.email ])
    in
    url



-- Helper function to format precise money values with 2 decimal places


formatPreciseMoney : Float -> String
formatPreciseMoney value =
    let
        -- Round to 2 decimal places
        roundedValue =
            round10 2 value

        -- Format the number with commas and 2 decimal places
        valueStr =
            String.fromFloat roundedValue
    in
    formatMoney valueStr



-- Helper function to format money values


formatMoney : String -> String
formatMoney valueStr =
    let
        parts =
            String.split "." valueStr

        intPart =
            List.head parts |> Maybe.withDefault ""

        decPart =
            List.drop 1 parts
                |> List.head
                |> Maybe.withDefault ""
                |> (\s ->
                        if String.length s == 0 then
                            "00"

                        else if String.length s == 1 then
                            s ++ "0"

                        else
                            String.left 2 s
                   )

        -- Format integer part with commas
        formattedInt =
            addCommas intPart
    in
    formattedInt ++ "." ++ decPart



-- Add commas to numbers for better readability


addCommas : String -> String
addCommas str =
    if String.length str <= 3 then
        str

    else
        let
            -- Recursively add commas
            addCommasHelper : String -> String -> String
            addCommasHelper acc remaining =
                if String.length remaining <= 3 then
                    remaining ++ acc

                else
                    let
                        len =
                            String.length remaining

                        front =
                            String.dropRight 3 remaining

                        back =
                            String.right 3 remaining
                    in
                    addCommasHelper ("," ++ back ++ acc) front
        in
        addCommasHelper "" str



-- Helper function to round to specific decimal places


round10 : Int -> Float -> Float
round10 n value =
    let
        factor =
            10 ^ n |> toFloat
    in
    (value * factor) |> round |> toFloat |> (\x -> x / factor)



-- Helper function to parse calculation inputs from the model


parseCalculationInputs : Model -> Maybe CalculationInputs
parseCalculationInputs model =
    let
        maybeContacts =
            String.toInt model.contactCount

        -- Fixed average age value
        averageAge =
            3.0

        maybeRollover =
            String.toFloat model.rolloverPercent

        maybeCommission =
            String.toFloat model.commissionRate
    in
    Maybe.map3
        (\c r rate ->
            { contacts = c
            , averageAge = averageAge
            , rolloverPercent = r
            , commissionRate = rate
            }
        )
        maybeContacts
        maybeRollover
        maybeCommission



-- Helper to parse with a specific rollover override


parseCalculationInputsWithRollover : Model -> String -> Maybe CalculationInputs
parseCalculationInputsWithRollover model rolloverStr =
    let
        maybeContacts =
            String.toInt model.contactCount

        -- Fixed average age value
        averageAge =
            3.0

        maybeRollover =
            String.toFloat rolloverStr

        maybeCommission =
            String.toFloat model.commissionRate
    in
    Maybe.map3
        (\c r rate ->
            { contacts = c
            , averageAge = averageAge
            , rolloverPercent = r
            , commissionRate = rate
            }
        )
        maybeContacts
        maybeRollover
        maybeCommission



-- Helper to parse with a specific commission rate override


parseCalculationInputsWithCommission : Model -> String -> Maybe CalculationInputs
parseCalculationInputsWithCommission model commissionStr =
    let
        maybeContacts =
            String.toInt model.contactCount

        -- Fixed average age value
        averageAge =
            3.0

        maybeRollover =
            String.toFloat model.rolloverPercent

        maybeCommission =
            String.toFloat commissionStr
    in
    Maybe.map3
        (\c r rate ->
            { contacts = c
            , averageAge = averageAge
            , rolloverPercent = r
            , commissionRate = rate
            }
        )
        maybeContacts
        maybeRollover
        maybeCommission



-- Format a number with appropriate precision


formatNumber : Float -> String
formatNumber value =
    if value == toFloat (round value) then
        -- It's a whole number - show no decimals
        String.fromInt (round value)

    else
        -- Show with appropriate precision
        String.fromFloat (round10 1 value)



-- Save company details to the API


saveCompanyDetails : Model -> Cmd Msg
saveCompanyDetails model =
    Http.post
        { url = "/api/onboarding/company"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string model.user.email )
                    , ( "companyName", Encode.string model.companyName )
                    , ( "companyPhone", Encode.string model.companyPhone )
                    , ( "companyWebsite", Encode.string model.companyWebsite )
                    , ( "primaryColor", Encode.string model.primaryColor )
                    , ( "secondaryColor", Encode.string model.secondaryColor )
                    , ( "logo", Maybe.withDefault Encode.null (Maybe.map Encode.string model.logo) )
                    ]
                )
        , expect = Http.expectWhatever (\_ -> NoOp)
        }



-- Save licensing settings to the API


saveLicensingSettings : Model -> Cmd Msg
saveLicensingSettings model =
    Http.post
        { url = "/api/onboarding/licensing"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string model.user.email )
                    , ( "selectedCarriers", Encode.list Encode.string (List.map carrierToString model.selectedCarriers) )
                    , ( "useSmartSend", Encode.bool model.useSmartSend )
                    ]
                )
        , expect = Http.expectWhatever (\_ -> NoOp)
        }



-- Save agents to the API


saveAgents : Model -> Cmd Msg
saveAgents model =
    Http.post
        { url = "/api/onboarding/agents"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string model.user.email )
                    , ( "agents"
                      , Encode.list
                            (\agent ->
                                Encode.object
                                    [ ( "firstName", Encode.string agent.firstName )
                                    , ( "lastName", Encode.string agent.lastName )
                                    , ( "email", Encode.string agent.email )
                                    , ( "phone", Encode.string agent.phone )
                                    , ( "isAdmin", Encode.bool agent.isAdmin )
                                    ]
                            )
                            model.agents
                      )
                    ]
                )
        , expect = Http.expectWhatever AgentsSaved
        }



-- Get saved onboarding data


fetchResumeData : String -> Cmd Msg
fetchResumeData email =
    -- Use a simple HTTP request with the raw email to avoid encoding issues
    -- The plus sign in emails is problematic with URL encoding
    -- So instead of using URL Builder, we'll manually construct the URL
    -- preserving the email exactly as received
    let
        -- Use encodeUri to properly encode the email preserving the + sign
        encodedEmail =
            -- Replace + with %2B to preserve it in the URL
            email
                |> String.replace "+" "%2B"
                |> Url.percentEncode
    in
    Http.get
        { url = "/api/onboarding/resume?email=" ++ encodedEmail
        , expect = Http.expectJson GotResumeData resumeDataDecoder
        }



-- Decoder for resume data from backend


resumeDataDecoder : Decoder ResumeData
resumeDataDecoder =
    Decode.succeed ResumeData
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "onboardingComplete" Decode.bool
        |> Pipeline.optional "redirectToLogin" Decode.bool False
        |> Pipeline.optional "organization" organizationDecoder defaultOrganization
        |> Pipeline.optional "user" userDecoder defaultUser
        |> Pipeline.optional "agents" (Decode.list agentDecoder) []
        |> Pipeline.optional "carrierSettings" carrierSettingsDecoder defaultCarrierSettings
        |> Pipeline.optional "paymentStatus" paymentStatusDecoder defaultPaymentStatus


type alias ResumeData =
    { success : Bool
    , onboardingComplete : Bool
    , redirectToLogin : Bool
    , organization : OrganizationData
    , user : UserData
    , agents : List Agent
    , carrierSettings : CarrierSettings
    , paymentStatus : PaymentStatusInfo
    }


type alias OrganizationData =
    { id : Int
    , name : String
    , website : String
    , phone : String
    , primaryColor : String
    , secondaryColor : String
    , logo : Maybe String
    }


type alias UserData =
    { id : Int
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    }


type alias CarrierSettings =
    { selectedCarriers : List String
    , useSmartSend : Bool
    }


type alias PaymentStatusInfo =
    { paymentCompleted : Bool
    , stripeCustomerId : Maybe String
    , stripeSubscriptionId : Maybe String
    }



-- Default values


defaultOrganization : OrganizationData
defaultOrganization =
    { id = 0
    , name = ""
    , website = ""
    , phone = ""
    , primaryColor = "#6B46C1"
    , secondaryColor = "#9F7AEA"
    , logo = Nothing
    }


defaultUser : UserData
defaultUser =
    { id = 0
    , firstName = ""
    , lastName = ""
    , email = ""
    , phone = ""
    }


defaultCarrierSettings : CarrierSettings
defaultCarrierSettings =
    { selectedCarriers = []
    , useSmartSend = True
    }


defaultPaymentStatus : PaymentStatusInfo
defaultPaymentStatus =
    { paymentCompleted = False
    , stripeCustomerId = Nothing
    , stripeSubscriptionId = Nothing
    }



-- Decoders


organizationDecoder : Decoder OrganizationData
organizationDecoder =
    Decode.succeed OrganizationData
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "website" Decode.string
        |> Pipeline.required "phone" Decode.string
        |> Pipeline.required "primaryColor" Decode.string
        |> Pipeline.required "secondaryColor" Decode.string
        |> Pipeline.optional "logo" (Decode.map Just Decode.string) Nothing


userDecoder : Decoder UserData
userDecoder =
    Decode.succeed UserData
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "firstName" Decode.string
        |> Pipeline.required "lastName" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "phone" Decode.string


agentDecoder : Decoder Agent
agentDecoder =
    Decode.succeed Agent
        |> Pipeline.required "firstName" Decode.string
        |> Pipeline.required "lastName" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "phone" Decode.string
        |> Pipeline.required "isAdmin" Decode.bool


carrierSettingsDecoder : Decoder CarrierSettings
carrierSettingsDecoder =
    Decode.succeed CarrierSettings
        |> Pipeline.required "selectedCarriers" (Decode.list Decode.string)
        |> Pipeline.required "useSmartSend" Decode.bool


paymentStatusDecoder : Decoder PaymentStatusInfo
paymentStatusDecoder =
    Decode.succeed PaymentStatusInfo
        |> Pipeline.required "paymentCompleted" Decode.bool
        |> Pipeline.optional "stripeCustomerId" (Decode.map Just Decode.string) Nothing
        |> Pipeline.optional "stripeSubscriptionId" (Decode.map Just Decode.string) Nothing
