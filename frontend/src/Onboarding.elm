port module Onboarding exposing (Model, Msg, init, subscriptions, update, view)

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



-- PORTS


port paymentCompleted : (PaymentResponse -> msg) -> Sub msg


port checkoutCompleted : (CheckoutData -> msg) -> Sub msg


port checkoutError : (ErrorData -> msg) -> Sub msg



-- MODEL


showStripe : Bool
showStripe =
    False


type alias PaymentResponse =
    { success : Bool
    , message : String
    , paymentCompleted : Bool
    }


type alias CheckoutData =
    { email : String
    , firstName : String
    , lastName : String
    , stripeCustomerId : String
    , stripeSubscriptionId : String
    , stripeUsageItemId : String
    }


type alias ErrorData =
    { success : Bool
    , message : String
    }


type alias Model =
    { user : User
    , frame : Int
    , companyName : String
    , companyPhone : String
    , companyWebsite : String
    , primaryColor : String
    , secondaryColor : String
    , logo : Maybe String
    , uploadingLogo : Bool
    , hover : Bool
    , key : Nav.Key
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
    , paymentStatus : PaymentStatus
    }


type PaymentStatus
    = ReadyToComplete
    | Continuing
    | Error ApiError
    | Loading
    | Success SubscriptionStatus
    | PaymentProcessing
    | PaymentCompleted


type ApiError
    = NetworkError
    | BadStatus Int String
    | BadPayload String
    | BadUrl String
    | Timeout


type alias SubscriptionStatus =
    { isActive : Bool
    , tier : String
    , currentPeriodEnd : Maybe Int
    , cancelAtPeriodEnd : Maybe Bool
    , paymentStatus : String
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
    4


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


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    let
        -- Filter out non-digit characters
        digitsOnly =
            String.filter Char.isDigit phone

        -- Limit to 10 digits
        limitedDigits =
            String.left 10 digitsOnly

        -- Format the phone number as needed
        formattedPhone =
            if String.length limitedDigits == 10 then
                "(" ++ String.left 3 limitedDigits ++ ") " ++ String.slice 3 6 limitedDigits ++ "-" ++ String.slice 6 10 limitedDigits

            else if String.length limitedDigits >= 7 then
                "(" ++ String.left 3 limitedDigits ++ ") " ++ String.slice 3 6 limitedDigits ++ "-" ++ String.slice 6 10 limitedDigits

            else if String.length limitedDigits >= 4 then
                "(" ++ String.left 3 limitedDigits ++ ") " ++ String.slice 3 10 limitedDigits

            else if String.length limitedDigits > 0 then
                "(" ++ limitedDigits

            else
                ""
    in
    formattedPhone


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
                |> Maybe.andThen Url.percentDecode
                |> Maybe.withDefault ""

        organizationName =
            Dict.get "organizationName" queryParams
                |> Maybe.andThen Url.percentDecode
                |> Maybe.withDefault ""

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
                    Just { firstName = f, lastName = l, email = e, phone = phone |> formatPhoneNumber }

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
            , frame = frame
            , companyName = organizationName
            , companyPhone = currentUser.phone
            , companyWebsite = ""
            , primaryColor = "#6B46C1"
            , secondaryColor = "#9F7AEA"
            , logo = Nothing
            , uploadingLogo = False
            , hover = False
            , key = key
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
            , paymentStatus = Loading
            }

        -- Check if this is a direct page load with frame > 1
        isDirectLoadWithHigherFrame =
            frame > 1 && (url.path == "/onboarding")

        redirectCommand =
            case maybeUser of
                Just user ->
                    fetchResumeData user.email

                Nothing ->
                    Nav.pushUrl key "/signup"
    in
    ( { initialModel | loadingResumeData = maybeUser /= Nothing }, redirectCommand )



-- UPDATE


type Msg
    = NoOp
    | CompanyNameChanged String
    | PhoneChanged String
    | WebsiteChanged String
    | PrimaryColorChanged String
    | SecondaryColorChanged String
    | UploadLogo
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotLogo File
    | GotLogoUrl String
    | ContinueClicked
    | BackClicked
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
    | CompanyDetailsSaved (Result Http.Error SaveResponse)
    | LicensingSaved (Result Http.Error SaveResponse)
    | AgentsSaved (Result Http.Error SaveResponse)
    | GotResumeData (Result Http.Error ResumeData)
    | OnboardingLoginCompleted (Result Http.Error OnboardingLoginResponse)
    | PaymentSaved (Result Http.Error SaveResponse)
    | GotSubscriptionStatus (Result Http.Error SubscriptionStatus)
    | PaymentProcessed (Result Http.Error PaymentResponse)
    | PaymentCompletedFromPort PaymentResponse
    | RefreshPage
    | CheckoutCompletedFromPort CheckoutData
    | CheckoutErrorFromPort ErrorData
    | ProcessCheckoutData CheckoutData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CompanyNameChanged name ->
            ( { model | companyName = name }, Cmd.none )

        PhoneChanged phone ->
            ( { model | companyPhone = formatPhoneNumber phone }, Cmd.none )

        WebsiteChanged website ->
            ( { model | companyWebsite = website }, Cmd.none )

        PrimaryColorChanged color ->
            ( { model | primaryColor = color }, Cmd.none )

        SecondaryColorChanged color ->
            ( { model | secondaryColor = color }, Cmd.none )

        UploadLogo ->
            ( model, Select.file [ "image/png" ] GotLogo )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        GotFiles file files ->
            ( { model | hover = False, uploadingLogo = True }, Task.perform GotLogoUrl (File.toUrl file) )

        GotLogo file ->
            ( { model | uploadingLogo = True }, Task.perform GotLogoUrl (File.toUrl file) )

        GotLogoUrl url ->
            ( { model | logo = Just url, uploadingLogo = False }, Cmd.none )

        ContinueClicked ->
            case model.frame of
                4 ->
                    case model.paymentStatus of
                        Success status ->
                            if status.isActive then
                                ( { model | frame = model.frame + 1 }
                                , completeOnboardingLogin model.user.email
                                )

                            else
                                ( { model | paymentStatus = PaymentProcessing }
                                , checkPaymentStatus model.user.email
                                )

                        PaymentCompleted ->
                            ( { model | frame = model.frame + 1 }
                            , completeOnboardingLogin model.user.email
                            )

                        _ ->
                            ( model, Cmd.none )

                1 ->
                    if String.isEmpty model.companyName || String.isEmpty model.companyPhone then
                        ( model, Cmd.none )

                    else
                        ( { model | loadingResumeData = True }
                        , Cmd.batch
                            [ saveCompanyDetails model
                            ]
                        )

                2 ->
                    if List.isEmpty model.selectedCarriers then
                        ( model, Cmd.none )

                    else
                        ( { model | frame = model.frame + 1 }
                        , saveLicensingSettings model
                        )

                3 ->
                    ( { model | frame = model.frame + 1, paymentStatus = ReadyToComplete }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        BackClicked ->
            let
                newFrame =
                    Basics.max 1 (model.frame - 1)
            in
            ( { model | frame = newFrame }
            , Nav.pushUrl model.key (buildUrl model newFrame)
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

        CompanyDetailsSaved result ->
            case result of
                Ok response ->
                    if response.success then
                        -- Move to frame 2 and clear loading state
                        ( { model | frame = 2, loadingResumeData = False }
                        , Nav.pushUrl model.key (buildUrl { model | frame = 2 } 2)
                        )

                    else
                        -- Failed to save but stay on current page
                        ( { model | loadingResumeData = False }, Cmd.none )

                Err _ ->
                    -- Error saving but stay on current page
                    ( { model | loadingResumeData = False }, Cmd.none )

        LicensingSaved result ->
            case result of
                Ok response ->
                    if response.success then
                        ( model, Cmd.none )

                    else
                        -- Failed to save, redirect to signup
                        ( model, Nav.pushUrl model.key "/signup" )

                Err _ ->
                    -- Error saving, redirect to signup
                    ( model, Nav.pushUrl model.key "/signup" )

        AgentsSaved _ ->
            ( model
            , completeOnboardingLogin model.user.email
            )

        GotResumeData result ->
            case result of
                Ok resumeData ->
                    if resumeData.onboardingComplete then
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
                        in
                        ( { model
                            | loadingResumeData = False
                            , companyName = resumeData.organization.name |> Url.percentDecode |> Maybe.withDefault resumeData.organization.name
                            , companyPhone = resumeData.organization.phone
                            , companyWebsite = resumeData.organization.website
                            , primaryColor = resumeData.organization.primaryColor
                            , secondaryColor = resumeData.organization.secondaryColor
                            , logo = resumeData.organization.logo
                            , selectedCarriers = selectedCarriers
                            , useSmartSend = resumeData.carrierSettings.useSmartSend
                            , agents = updatedAgents
                          }
                        , Cmd.none
                        )

                    else
                        -- Failed to get resume data, redirect to signup if not on frame 1
                        ( { model | loadingResumeData = False }
                        , if model.frame > 1 then
                            Nav.pushUrl model.key "/signup"

                          else
                            Cmd.none
                        )

                Err _ ->
                    -- Error fetching resume data, redirect to signup if not on frame 1
                    ( { model | loadingResumeData = False }
                    , if model.frame > 1 then
                        Nav.pushUrl model.key "/signup"

                      else
                        Cmd.none
                    )

        OnboardingLoginCompleted result ->
            case result of
                Ok response ->
                    if response.success then
                        ( model
                        , Nav.load response.redirectUrl
                        )

                    else
                        ( { model | loadingResumeData = False }
                        , Nav.pushUrl model.key "/signup"
                        )

                Err _ ->
                    ( { model | loadingResumeData = False }
                    , Nav.pushUrl model.key "/signup"
                    )

        PaymentSaved result ->
            case result of
                Ok response ->
                    if response.success then
                        ( model, Cmd.none )

                    else
                        -- Failed to save, redirect to signup
                        ( model, Nav.pushUrl model.key "/signup" )

                Err _ ->
                    -- Error saving, redirect to signup
                    ( model, Nav.pushUrl model.key "/signup" )

        GotSubscriptionStatus result ->
            case result of
                Ok status ->
                    ( { model | paymentStatus = Success status }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        PaymentProcessed result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | paymentStatus = PaymentCompleted }
                        , Cmd.batch
                            [ completeOnboardingLogin model.user.email
                            , fetchSubscriptionStatus
                            ]
                        )

                    else
                        ( { model | paymentStatus = Error (BadPayload "Payment processing failed. Please try again.") }
                        , Cmd.none
                        )

                Err _ ->
                    ( { model | paymentStatus = Error NetworkError }
                    , Cmd.none
                    )

        PaymentCompletedFromPort response ->
            if response.success then
                ( { model | paymentStatus = PaymentCompleted }
                , Cmd.batch
                    [ completeOnboardingLogin model.user.email
                    , fetchSubscriptionStatus
                    ]
                )

            else
                ( { model | paymentStatus = Error (BadPayload "Payment processing failed. Please try again.") }
                , Cmd.none
                )

        CheckoutCompletedFromPort checkoutData ->
            ( model, processCheckoutData checkoutData )

        CheckoutErrorFromPort errorData ->
            ( { model | paymentStatus = Error (BadPayload errorData.message) }, Cmd.none )

        ProcessCheckoutData checkoutData ->
            ( { model | paymentStatus = PaymentProcessing }
            , completePayment checkoutData
            )

        RefreshPage ->
            ( model, Nav.reload )



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



-- Helper function to check if carriers are selected


hasSelectedCarriers : Model -> Bool
hasSelectedCarriers model =
    not (List.isEmpty model.selectedCarriers)



-- Calculate revenue based on contacts, average age, and rollover percent
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
                        1 ->
                            viewCompany model

                        2 ->
                            viewLicensing model

                        3 ->
                            viewAddAgents model

                        4 ->
                            viewPayment model

                        _ ->
                            viewCompany model
                    , viewProgressDots model.frame
                    ]
            ]
        ]
    }


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
                [ class
                    ("mt-8 p-6 border rounded-lg border-dashed text-center cursor-pointer transition-colors "
                        ++ (if model.hover then
                                "border-indigo-600 bg-indigo-50"

                            else
                                "border-gray-200 hover:bg-gray-50"
                           )
                    )
                , onClick UploadLogo
                , hijackOn "dragenter" (Decode.succeed DragEnter)
                , hijackOn "dragover" (Decode.succeed DragEnter)
                , hijackOn "dragleave" (Decode.succeed DragLeave)
                , hijackOn "drop" dropDecoder
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
                                , div [ class "text-gray-400 text-xs mt-2" ] [ text "PNG only (Recommended: 240px width x 60px height)" ]
                                ]
                ]

            {--
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
            --}
            ]
        , div [ class "mt-10 w-full max-w-md" ]
            [ button
                [ class "w-full bg-[#03045e] text-white py-3 px-4 rounded-md hover:bg-[#02034e] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 font-medium"
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
        [ h2 [ class "text-2xl font-semibold text-gray-900 mt-6" ] [ text "Carrier Information" ]
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
                    [ class
                        (if hasSelectedCarriers model then
                            "w-full bg-[#03045e] text-white py-2 px-4 rounded-md hover:bg-[#02034e] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"

                         else
                            "w-full bg-[#03045e]/70 text-white py-2 px-4 rounded-md cursor-not-allowed"
                        )
                    , onClick ContinueClicked
                    , disabled (not (hasSelectedCarriers model))
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
                [ class "w-full bg-[#03045e] text-white py-3 px-4 rounded-md hover:bg-[#02034e] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 font-medium"
                , onClick ContinueClicked
                ]
                [ text "Continue" ]
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
                        [ class "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full px-3 py-2 text-base border-gray-300 rounded-md"
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
                        [ class "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full px-3 py-2 text-base border-gray-300 rounded-md"
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
                        [ class "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full px-3 py-2 text-base border-gray-300 rounded-md"
                        , type_ "email"
                        , placeholder "Email address"
                        , value model.newAgentEmail
                        , onInput AgentEmailChanged
                        ]
                        []
                    ]
                , div []
                    [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                        [ text "Phone" ]
                    , input
                        [ class "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full px-3 py-2 text-base border-gray-300 rounded-md"
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
                    [ class
                        (if
                            String.isEmpty (String.trim model.newAgentFirstName)
                                || String.isEmpty (String.trim model.newAgentLastName)
                                || String.isEmpty (String.trim model.newAgentEmail)
                         then
                            "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-400 cursor-not-allowed"

                         else
                            "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        )
                    , onClick AddAgent
                    , disabled
                        (String.isEmpty (String.trim model.newAgentFirstName)
                            || String.isEmpty (String.trim model.newAgentLastName)
                            || String.isEmpty (String.trim model.newAgentEmail)
                        )
                    ]
                    [ text "Add Agent" ]
                ]
            ]
        ]


viewPayment : Model -> Html Msg
viewPayment model =
    div [ class "flex flex-col items-center" ]
        [ div [ class "text-center mb-8 w-full" ]
            [ h2 [ class "text-3xl font-semibold text-gray-900 mb-2" ] [ text "Payment Information" ]
            , p [ class "text-gray-500" ] [ text "Set up your payment method to complete your account." ]
            ]
        , div [ class "w-full max-w-md space-y-6" ]
            [ case model.paymentStatus of
                Loading ->
                    div [ class "mt-4 p-3 bg-blue-50 border border-blue-200 rounded-md" ]
                        [ p [ class "text-blue-700" ] [ text "Loading subscription status..." ] ]

                Error apiError ->
                    div [ class "mt-4 p-3 bg-red-50 border border-red-200 rounded-md" ]
                        [ p [ class "text-red-700" ] [ text "Error" ]
                        , p [ class "mt-2 text-sm text-red-600" ]
                            [ text (apiErrorToString apiError)
                            , br [] []
                            , text "If you're using an ad blocker, please disable it for this site as it may interfere with payment processing."
                            ]
                        , div [ class "mt-4 flex justify-center" ]
                            [ button
                                [ class "px-4 py-2 bg-indigo-600 text-white rounded-md hover:bg-indigo-700"
                                , onClick RefreshPage
                                ]
                                [ text "Refresh Page" ]
                            ]
                        ]

                Success status ->
                    if status.isActive then
                        div [ class "mt-4 p-3 bg-green-50 border border-green-200 rounded-md" ]
                            [ p [ class "text-green-700 font-medium" ]
                                [ text ("Active subscription: " ++ status.tier) ]
                            , case status.currentPeriodEnd of
                                Just periodEnd ->
                                    p [ class "mt-2 text-sm text-green-600" ]
                                        [ text ("Current period ends: " ++ formatUnixTimestamp periodEnd) ]

                                Nothing ->
                                    text ""
                            , case status.cancelAtPeriodEnd of
                                Just True ->
                                    p [ class "mt-2 text-sm text-yellow-600" ]
                                        [ text "Your subscription will cancel at the end of the current period" ]

                                _ ->
                                    text ""
                            ]

                    else
                        node "stripe-checkout"
                            [ attribute "price-id" "price_1RHG4mCBUPXAZKNGem75yV4U" --"price_1RBStWCBUPXAZKNGwpimWl7v"
                            , attribute "metered-price-id" "price_1RHG7kCBUPXAZKNGd5YvIzsw" --"price_1RBSvJCBUPXAZKNGQ1U9Hl8i"

                            --, attribute "return-url" ("http://localhost:3000/walkthrough?payment_success=true&email=" ++ model.user.email)
                            , attribute "first-name" model.user.firstName
                            , attribute "last-name" model.user.lastName
                            , attribute "email" model.user.email
                            ]
                            []

                Continuing ->
                    div [ class "mt-4 p-3 bg-blue-50 border border-blue-200 rounded-md" ]
                        [ p [ class "text-blue-700" ] [ text "Resuming your previous setup" ]
                        , p [ class "mt-2 text-sm text-blue-600" ]
                            [ text "If the checkout doesn't appear, please disable any ad blockers for this site." ]
                        ]

                ReadyToComplete ->
                    node "stripe-checkout"
                        [ attribute "price-id" "price_1RHG4mCBUPXAZKNGem75yV4U" --"price_1RBStWCBUPXAZKNGwpimWl7v"
                        , attribute "metered-price-id" "price_1RHG7kCBUPXAZKNGd5YvIzsw" --"price_1RBSvJCBUPXAZKNGQ1U9Hl8i"

                        --, attribute "return-url" ("http://localhost:3000/walkthrough?payment_success=true&email=" ++ model.user.email)
                        , attribute "first-name" model.user.firstName
                        , attribute "last-name" model.user.lastName
                        , attribute "email" model.user.email
                        ]
                        []

                PaymentProcessing ->
                    div [ class "mt-4 p-3 bg-blue-50 border border-blue-200 rounded-md" ]
                        [ div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-b-2 border-blue-500 mx-auto mb-4" ] []
                        , p [ class "text-blue-700 text-center" ] [ text "Processing your payment..." ]
                        ]

                PaymentCompleted ->
                    div [ class "mt-4 p-3 bg-green-50 border border-green-200 rounded-md" ]
                        [ p [ class "text-green-700 font-medium text-center" ] [ text "Payment completed successfully!" ]
                        , p [ class "mt-2 text-sm text-green-600 text-center" ] [ text "Finalizing your account setup..." ]
                        ]
            ]
        ]



-- Helper function to format Unix timestamp


formatUnixTimestamp : Int -> String
formatUnixTimestamp timestamp =
    -- Convert Unix timestamp to readable date string
    -- For now just return the raw timestamp, you can enhance this later
    String.fromInt timestamp



-- Helper function to convert API errors to strings


apiErrorToString : ApiError -> String
apiErrorToString error =
    case error of
        NetworkError ->
            "Network error occurred. Please check your connection."

        BadStatus status message ->
            "Server error: " ++ String.fromInt status ++ " - " ++ message

        BadPayload message ->
            "Failed to process server response. "
                ++ (if String.contains "JSON" message then
                        "Please try refreshing the page."

                    else
                        message
                   )

        BadUrl url ->
            "Invalid API URL: " ++ url

        Timeout ->
            "Request timed out. Please try again."



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ paymentCompleted PaymentCompletedFromPort
        , checkoutCompleted CheckoutCompletedFromPort
        , checkoutError CheckoutErrorFromPort
        ]



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
-- Save company details to the API


saveCompanyDetails : Model -> Cmd Msg
saveCompanyDetails model =
    Http.post
        { url = "/api/onboarding/company"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string model.user.email )
                    , ( "firstName", Encode.string model.user.firstName )
                    , ( "lastName", Encode.string model.user.lastName )
                    , ( "phone", Encode.string model.user.phone )
                    , ( "companyName", Encode.string model.companyName )
                    , ( "companyPhone", Encode.string model.companyPhone )
                    , ( "companyWebsite", Encode.string model.companyWebsite )
                    , ( "primaryColor", Encode.string model.primaryColor )
                    , ( "secondaryColor", Encode.string model.secondaryColor )
                    , ( "logo", Maybe.withDefault Encode.null (Maybe.map Encode.string model.logo) )
                    ]
                )
        , expect = Http.expectJson CompanyDetailsSaved saveResponseDecoder
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
        , expect = Http.expectJson LicensingSaved saveResponseDecoder
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
        , expect = Http.expectJson AgentsSaved saveResponseDecoder
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
        |> Pipeline.optional "organization" organizationDecoder defaultOrganization
        |> Pipeline.optional "user" userDecoder defaultUser
        |> Pipeline.optional "agents" (Decode.list agentDecoder) []
        |> Pipeline.optional "carrierSettings" carrierSettingsDecoder defaultCarrierSettings


type alias ResumeData =
    { success : Bool
    , onboardingComplete : Bool
    , organization : OrganizationData
    , user : UserData
    , agents : List Agent
    , carrierSettings : CarrierSettings
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


type alias SaveResponse =
    { success : Bool
    , message : String
    }


saveResponseDecoder : Decoder SaveResponse
saveResponseDecoder =
    Decode.map2 SaveResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "message" Decode.string)


type alias OnboardingLoginResponse =
    { success : Bool
    , redirectUrl : String
    , email : String
    }


onboardingLoginResponseDecoder : Decoder OnboardingLoginResponse
onboardingLoginResponseDecoder =
    Decode.succeed OnboardingLoginResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "redirectUrl" Decode.string
        |> Pipeline.required "email" Decode.string


completeOnboardingLogin : String -> Cmd Msg
completeOnboardingLogin email =
    Http.post
        { url = "/api/auth/onboarding-login"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "emailRaw", Encode.string email )
                    ]
                )
        , expect = Http.expectJson OnboardingLoginCompleted onboardingLoginResponseDecoder
        }



-- Save payment settings to the API


savePaymentSettings : Model -> Cmd Msg
savePaymentSettings model =
    Http.post
        { url = "/api/onboarding/payment"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string model.user.email )
                    ]
                )
        , expect = Http.expectJson PaymentSaved saveResponseDecoder
        }



-- Add fetchSubscriptionStatus function


fetchSubscriptionStatus : Cmd Msg
fetchSubscriptionStatus =
    Http.get
        { url = "/api/stripe/subscription-status"
        , expect = Http.expectJson GotSubscriptionStatus subscriptionStatusDecoder
        }



-- Add subscription status decoder


subscriptionStatusDecoder : Decoder SubscriptionStatus
subscriptionStatusDecoder =
    Decode.field "data"
        (Decode.map5 SubscriptionStatus
            (Decode.field "isActive" Decode.bool)
            (Decode.field "tier" Decode.string)
            (Decode.maybe (Decode.field "currentPeriodEnd" Decode.int))
            (Decode.maybe (Decode.field "cancelAtPeriodEnd" Decode.bool))
            (Decode.field "paymentStatus" Decode.string)
        )



-- Add payment status check function


checkPaymentStatus : String -> Cmd Msg
checkPaymentStatus email =
    Http.get
        { url = "/api/stripe/payment-status"
        , expect = Http.expectJson PaymentProcessed paymentResponseDecoder
        }



-- Add payment response decoder


paymentResponseDecoder : Decoder PaymentResponse
paymentResponseDecoder =
    Decode.map3 PaymentResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "message" Decode.string)
        (Decode.field "paymentCompleted" Decode.bool)



-- Call the payment-complete endpoint with checkout data


completePayment : CheckoutData -> Cmd Msg
completePayment checkoutData =
    Http.post
        { url = "/api/stripe/payment-complete"
        , body = Http.jsonBody (encodeCheckoutData checkoutData)
        , expect = Http.expectJson PaymentProcessed paymentResponseDecoder
        }



-- Encode checkout data for the API


encodeCheckoutData : CheckoutData -> Encode.Value
encodeCheckoutData data =
    Encode.object
        [ ( "email", Encode.string data.email )
        , ( "firstName", Encode.string data.firstName )
        , ( "lastName", Encode.string data.lastName )
        , ( "stripeCustomerId", Encode.string data.stripeCustomerId )
        , ( "stripeSubscriptionId", Encode.string data.stripeSubscriptionId )
        , ( "stripeUsageItemId", Encode.string data.stripeUsageItemId )
        ]



-- Process checkout data by making the API call


processCheckoutData : CheckoutData -> Cmd Msg
processCheckoutData checkoutData =
    -- After a small delay to allow UI to update, process the checkout data
    Task.perform (\_ -> ProcessCheckoutData checkoutData) (Task.succeed ())



-- Helper functions for drag and drop


dropDecoder : Decode.Decoder Msg
dropDecoder =
    Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore GotFiles File.decoder)


hijackOn : String -> Decode.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
