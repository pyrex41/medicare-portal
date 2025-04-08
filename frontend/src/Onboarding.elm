module Onboarding exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import MyIcon
import Url exposing (Url)
import Url.Builder exposing (absolute, int, string)
import Url.Parser.Query as Query
import Utils.UrlStuff exposing (getQueryParams)



-- MODEL


type PaymentStatus
    = ReadyToComplete
    | Continuing
    | Error String


type alias Model =
    { user : User
    , paymentStatus : PaymentStatus
    , frame : Int
    , companyName : String
    , companyPhone : String
    , companyWebsite : String
    , key : Nav.Key
    , contactCount : String
    , calculatedPrice : Maybe Int
    , averageAge : String
    , rolloverPercent : String
    , calculatedRevenue : Maybe Float
    , firstYearRevenue : Maybe Float
    }


maxFrame : Int
maxFrame =
    4


dummyUser : User
dummyUser =
    { firstName = "John"
    , lastName = "Doe"
    , email = "john.doe@example.com"
    }


type alias User =
    { firstName : String
    , lastName : String
    , email : String
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
                    Just { firstName = f, lastName = l, email = e }

                _ ->
                    Nothing
    in
    ( { user = maybeUser |> Maybe.withDefault dummyUser
      , paymentStatus = ReadyToComplete
      , frame = frame
      , companyName = ""
      , companyPhone = ""
      , companyWebsite = ""
      , key = key
      , contactCount = "500"
      , calculatedPrice = Just 60
      , averageAge = "3"
      , rolloverPercent = "5"
      , calculatedRevenue = Just 175000
      , firstYearRevenue = Just 8750
      }
    , case maybeUser of
        Just _ ->
            Cmd.none

        Nothing ->
            Nav.pushUrl key "/signup"
    )



-- UPDATE


type Msg
    = NoOp
    | PaymentCompleted PaymentStatus -- Added to handle custom event from JS
    | CompanyNameChanged String
    | PhoneChanged String
    | WebsiteChanged String
    | ContinueClicked
    | BackClicked
    | ContactCountChanged String -- New message for contact count input
    | AverageAgeChanged String
    | RolloverPercentChanged String


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

        ContinueClicked ->
            let
                newFrame =
                    Basics.min maxFrame (model.frame + 1)
            in
            ( { model | frame = newFrame }
            , case newFrame of
                2 ->
                    Nav.load (buildUrl model newFrame)

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

                ( calculatedRevenue, firstYearRevenue ) =
                    case ( maybeInt, String.toFloat model.averageAge, String.toFloat model.rolloverPercent ) of
                        ( Just c, Just a, Just r ) ->
                            let
                                ( rev, firstYear ) =
                                    calculateRevenue c a r
                            in
                            ( Just rev, Just firstYear )

                        _ ->
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

        AverageAgeChanged age ->
            let
                ( calculatedRevenue, firstYearRevenue ) =
                    case ( String.toInt model.contactCount, String.toFloat age, String.toFloat model.rolloverPercent ) of
                        ( Just c, Just a, Just r ) ->
                            let
                                ( rev, firstYear ) =
                                    calculateRevenue c a r
                            in
                            ( Just rev, Just firstYear )

                        _ ->
                            ( Nothing, Nothing )
            in
            ( { model
                | averageAge = age
                , calculatedRevenue = calculatedRevenue
                , firstYearRevenue = firstYearRevenue
              }
            , Cmd.none
            )

        RolloverPercentChanged percent ->
            let
                ( calculatedRevenue, firstYearRevenue ) =
                    case ( String.toInt model.contactCount, String.toFloat model.averageAge, String.toFloat percent ) of
                        ( Just c, Just a, Just r ) ->
                            let
                                ( rev, firstYear ) =
                                    calculateRevenue c a r
                            in
                            ( Just rev, Just firstYear )

                        _ ->
                            ( Nothing, Nothing )
            in
            ( { model
                | rolloverPercent = percent
                , calculatedRevenue = calculatedRevenue
                , firstYearRevenue = firstYearRevenue
              }
            , Cmd.none
            )



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


calculateRevenue : Int -> Float -> Float -> ( Float, Float )
calculateRevenue contacts averageAge rolloverPercent =
    let
        -- Constants
        commissionPerYear =
            350.0

        maxYears =
            6.0

        ltvDiscount =
            0.75

        -- account for cancellations
        -- Calculate LTV metrics
        ltvPerContact =
            commissionPerYear * maxYears * ltvDiscount

        -- Average remaining LTV based on current age
        avgLtv =
            ltvPerContact * (maxYears - averageAge) / maxYears

        -- Average LTV after rollover (adds another cycle)
        avgLtvNew =
            ltvPerContact * (maxYears - averageAge + maxYears) / maxYears

        -- Convert percentage to fraction
        rolloverFraction =
            rolloverPercent / 100.0

        -- Calculate future revenue
        oldFutureRevenue =
            avgLtv * toFloat contacts

        -- New future revenue (non-rollovers + rollovers)
        newFutureRevenue =
            (avgLtv * toFloat contacts * (1 - rolloverFraction))
                + (avgLtvNew * toFloat contacts * rolloverFraction)

        -- Additional revenue from rollovers
        additionalRevenue =
            newFutureRevenue - oldFutureRevenue

        -- First year additional revenue
        -- Based on the proportion of contacts at or beyond max age
        -- that would generate immediate additional revenue
        contactsAtMaxAge =
            if averageAge >= maxYears / 2 then
                -- Calculate percentage of contacts at or beyond max age
                -- based on even distribution assumption
                let
                    percentAtMaxAge =
                        (averageAge + (maxYears / 2))
                            / maxYears
                            |> Basics.min 1.0
                            |> Basics.max 0.0
                in
                percentAtMaxAge * toFloat contacts * rolloverFraction

            else
                -- Few or no contacts at max age yet
                0

        firstYearAdditional =
            contactsAtMaxAge * commissionPerYear
    in
    ( additionalRevenue, firstYearAdditional )



-- Handle navigation if needed
-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Onboarding"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex items-center justify-center py-12 px-4 sm:px-6 lg:px-8" ]
            [ div [ class "max-w-6xl w-full space-y-8 bg-white p-8 rounded-lg shadow-md" ]
                [ div [ class "text-center" ]
                    [ h1 [ class "text-3xl font-bold text-gray-900 mb-4" ] [ text "Welcome to Onboarding" ]
                    , div [ class "mt-4" ]
                        [ p [ class "text-xl font-medium text-gray-700" ]
                            [ text (String.join " " [ model.user.firstName, model.user.lastName ]) ]
                        , p [ class "mt-2 text-gray-600" ]
                            [ text (model.user.email |> Url.percentDecode |> Maybe.withDefault "") ]
                        ]
                    ]
                , case model.frame of
                    2 ->
                        viewStripe model

                    3 ->
                        viewCompany model

                    4 ->
                        viewLicensing model

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
        [ MyIcon.banknote 24 "#0F172A"
        , h2 [ class "text-2xl font-semibold text-gray-900 mt-6" ] [ text "Subscription Pricing" ]
        , p [ class "text-gray-500 mt-2 mb-6" ] [ text "Simple, transparent pricing for your Medicare portal." ]
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
            , div [ class "grid grid-cols-1 md:grid-cols-2 gap-6 mt-6" ]
                [ div [ class "bg-white overflow-hidden shadow rounded-lg divide-y divide-gray-200" ]
                    [ div [ class "px-4 py-5 sm:px-6 bg-indigo-50" ]
                        [ h3 [ class "text-lg leading-6 font-medium text-gray-900" ]
                            [ text "Price Calculator" ]
                        ]
                    , div [ class "px-4 py-5 sm:p-6" ]
                        [ div [ class "space-y-4" ]
                            [ div [ class "space-y-2" ]
                                [ label [ class "block text-sm font-medium text-gray-700", for "contact-count" ]
                                    [ text "Number of Contacts" ]
                                , input
                                    [ class "mt-1 block w-full border border-gray-300 rounded-md shadow-sm py-2 px-3 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                                    , id "contact-count"
                                    , type_ "number"
                                    , placeholder "Enter number of contacts"
                                    , value model.contactCount
                                    , onInput ContactCountChanged
                                    , Html.Attributes.min "0"
                                    , Html.Attributes.step "1"
                                    ]
                                    []
                                ]
                            , div [ class "pt-4" ]
                                [ div [ class "flex items-center justify-between pb-2 border-b border-gray-200" ]
                                    [ span [ class "text-gray-600" ] [ text "Base subscription:" ]
                                    , span [ class "font-medium text-gray-900" ] [ text "$60/month" ]
                                    ]
                                , if model.calculatedPrice /= Just 60 then
                                    div [ class "flex items-center justify-between py-2 border-b border-gray-200" ]
                                        [ span [ class "text-gray-600" ]
                                            [ text <| "Additional contacts cost:" ]
                                        , span [ class "font-medium text-gray-900" ]
                                            [ text <| "$" ++ String.fromInt (Maybe.withDefault 0 model.calculatedPrice - 60) ++ "/month" ]
                                        ]

                                  else
                                    text ""
                                , div [ class "flex items-center justify-between py-2 mt-2" ]
                                    [ span [ class "text-lg font-semibold text-gray-900" ] [ text "Total:" ]
                                    , span [ class "text-xl font-bold text-indigo-600" ]
                                        [ text <| "$" ++ String.fromInt (Maybe.withDefault 0 model.calculatedPrice) ++ "/month" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "bg-white overflow-hidden shadow rounded-lg divide-y divide-gray-200" ]
                    [ div [ class "px-4 py-5 sm:px-6 bg-indigo-50" ]
                        [ h3 [ class "text-lg leading-6 font-medium text-gray-900" ]
                            [ text "Revenue Calculator" ]
                        ]
                    , div [ class "px-4 py-5 sm:p-6" ]
                        [ div [ class "space-y-4" ]
                            [ div [ class "grid grid-cols-2 gap-4" ]
                                [ div [ class "space-y-2" ]
                                    [ label [ class "block text-sm font-medium text-gray-700", for "average-age" ]
                                        [ text "Average Book Age (Years)" ]
                                    , input
                                        [ class "mt-1 block w-full border border-gray-300 rounded-md shadow-sm py-2 px-3 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                                        , id "average-age"
                                        , type_ "number"
                                        , placeholder "Average age in years"
                                        , value model.averageAge
                                        , onInput AverageAgeChanged
                                        , Html.Attributes.min "1"
                                        , Html.Attributes.max "6"
                                        , Html.Attributes.step "0.1"
                                        ]
                                        []
                                    ]
                                , div [ class "space-y-2" ]
                                    [ label [ class "block text-sm font-medium text-gray-700", for "rollover-percent" ]
                                        [ text "Annual Rollover (%)" ]
                                    , input
                                        [ class "mt-1 block w-full border border-gray-300 rounded-md shadow-sm py-2 px-3 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                                        , id "rollover-percent"
                                        , type_ "number"
                                        , placeholder "Rollover percentage"
                                        , value model.rolloverPercent
                                        , onInput RolloverPercentChanged
                                        , Html.Attributes.min "0"
                                        , Html.Attributes.max "100"
                                        , Html.Attributes.step "0.1"
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "mt-4 p-3 bg-gray-50 rounded-md" ]
                                [ div [ class "space-y-2" ]
                                    [ div [ class "flex items-center justify-between pb-2 border-b border-gray-200" ]
                                        [ span [ class "text-gray-600" ] [ text "Annual Commission Rate:" ]
                                        , span [ class "font-medium text-gray-900" ] [ text "$350 per contact" ]
                                        ]
                                    , div [ class "flex items-center justify-between py-2 border-b border-gray-200" ]
                                        [ span [ class "text-gray-600" ] [ text "First Year Additional Revenue:" ]
                                        , span [ class "font-medium text-green-600" ]
                                            [ text <| "$" ++ formatMoney (Maybe.withDefault 0 model.firstYearRevenue) ]
                                        ]
                                    , div [ class "flex items-center justify-between py-2 mt-2" ]
                                        [ span [ class "text-lg font-semibold text-gray-900" ] [ text "Additional Revenue:" ]
                                        , span [ class "text-xl font-bold text-green-600" ]
                                            [ text <| "$" ++ formatMoney (Maybe.withDefault 0 model.calculatedRevenue) ]
                                        ]
                                    , div [ class "flex items-center justify-between py-2 mt-2 border-t border-gray-200" ]
                                        [ span [ class "text-sm font-medium text-gray-700" ] [ text "Return on Investment:" ]
                                        , let
                                            roi =
                                                case ( model.calculatedRevenue, model.calculatedPrice ) of
                                                    ( Just rev, Just price ) ->
                                                        if price > 0 then
                                                            (rev / toFloat (price * 12)) * 100

                                                        else
                                                            0

                                                    _ ->
                                                        0
                                          in
                                          span [ class "text-sm font-bold text-indigo-700" ]
                                            [ text <| String.fromFloat (round10 2 roi) ++ "%" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
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
        Error error ->
            div [ class "mt-4 p-3 bg-red-50 border border-red-200 rounded-md" ]
                [ p [ class "text-red-700" ] [ text "Error" ]
                , p [ class "mt-2 text-sm text-red-600" ]
                    [ text "If you're using an ad blocker, please disable it for this site as it may interfere with payment processing." ]
                ]

        Continuing ->
            div [ class "mt-4 p-3 bg-blue-50 border border-blue-200 rounded-md" ]
                [ p [ class "text-blue-700" ] [ text "Resuming your previous setup" ]
                , p [ class "mt-2 text-sm text-blue-600" ]
                    [ text "If the checkout doesn't appear, please disable any ad blockers for this site." ]
                ]

        ReadyToComplete ->
            node "stripe-checkout"
                [ attribute "price-id" "price_1RBStWCBUPXAZKNGwpimWl7v" -- Base Subscription price
                , attribute "metered-price-id" "price_1RBSvJCBUPXAZKNGQ1U9Hl8i" -- Contact Tier price
                , attribute "return-url" "http://localhost:3000/return" -- Matches your backend config
                , attribute "first-name" model.user.firstName
                , attribute "last-name" model.user.lastName
                , attribute "email" model.user.email
                ]
                []


viewCompany : Model -> Html Msg
viewCompany model =
    div [ class "flex flex-col items-center" ]
        [ MyIcon.hospital 24 "#0F172A"
        , h2 [ class "text-2xl font-semibold text-gray-900 mt-6" ] [ text "Company Details" ]
        , p [ class "text-gray-500 mt-2 mb-6" ] [ text "Let's get to know your business." ]
        , div [ class "w-full max-w-md space-y-6" ]
            [ div [ class "space-y-2" ]
                [ label [ class "block text-sm font-medium text-gray-700", for "company-name" ] [ text "Company Name" ]
                , input
                    [ class "block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
                    , id "company-name"
                    , type_ "text"
                    , placeholder "Example Company"
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
                        , placeholder "xxx-xxx-xxxx"
                        , pattern "[0-9]{3}-[0-9]{3}-[0-9]{4}"
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
                            , placeholder "www.untitledul.com"
                            , value model.companyWebsite
                            , onInput WebsiteChanged
                            ]
                            []
                        ]
                    ]
                ]
            , div
                [ class "w-full flex space-x-4 mt-6" ]
                [ button
                    [ class
                        (if model.frame >= maxFrame then
                            "w-full bg-gray-400 text-white py-2 px-4 rounded-md cursor-not-allowed"

                         else
                            "w-full bg-indigo-900 text-white py-2 px-4 rounded-md hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        )
                    , onClick ContinueClicked
                    , disabled (model.frame >= maxFrame)
                    ]
                    [ text "Continue" ]
                ]
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
    div [] [ text "Licensing" ]



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
            ]

        -- If email is already encoded, manually construct the URL to avoid re-encoding
        url =
            if emailIsEncoded then
                absolute [ "onboarding" ] baseParams ++ "&email=" ++ model.user.email

            else
                absolute [ "onboarding" ] (baseParams ++ [ string "email" model.user.email ])
    in
    url



-- Helper function to format money values


formatMoney : Float -> String
formatMoney value =
    let
        rounded =
            round value
    in
    String.fromInt rounded |> addCommas



-- Add commas to numbers for better readability


addCommas : String -> String
addCommas str =
    let
        parts =
            String.split "." str

        intPart =
            List.head parts |> Maybe.withDefault ""

        decPart =
            List.drop 1 parts |> List.head |> Maybe.withDefault ""

        formattedInt =
            String.foldr
                (\c ( result, count ) ->
                    if count == 3 then
                        ( String.cons c (String.cons ',' result), 1 )

                    else
                        ( String.cons c result, count + 1 )
                )
                ( "", 0 )
                intPart
                |> Tuple.first
                |> String.reverse
                |> String.dropLeft 1
                |> String.reverse
    in
    if String.isEmpty decPart then
        formattedInt

    else
        formattedInt ++ "." ++ decPart



-- Helper function to round to specific decimal places


round10 : Int -> Float -> Float
round10 n value =
    let
        factor =
            10 ^ n |> toFloat
    in
    (value * factor) |> round |> toFloat |> (\x -> x / factor)
