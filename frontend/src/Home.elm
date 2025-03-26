module Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import Json.Decode as Decode
import Ports exposing (getOrgSlug, receiveOrgSlug)
import Time



-- MODEL


type alias Model =
    { key : Nav.Key
    , sessionState : SessionState
    , activeExperienceTab : ExperienceTab
    , carouselActive : Bool
    }


type SessionState
    = Unknown
    | Checking
    | Valid
    | Invalid


type ExperienceTab
    = Email
    | Quote
    | Underwriting


type Msg
    = CheckSession
    | GotSessionResponse (Result Http.Error SessionResponse)
    | NavigateTo String
    | NavigateSignup
    | SetExperienceTab ExperienceTab
    | StartCarousel
    | StopCarousel
    | RotateCarousel Time.Posix
    | NoOp


type alias SessionResponse =
    { valid : Bool }


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key
      , sessionState = Unknown
      , activeExperienceTab = Email
      , carouselActive = True
      }
    , checkSession
    )


checkSession : Cmd Msg
checkSession =
    Http.get
        { url = "/api/auth/session"
        , expect = Http.expectJson GotSessionResponse sessionResponseDecoder
        }


sessionResponseDecoder : Decode.Decoder SessionResponse
sessionResponseDecoder =
    Decode.map SessionResponse
        (Decode.field "valid" Decode.bool)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckSession ->
            ( { model | sessionState = Checking }
            , checkSession
            )

        GotSessionResponse result ->
            case result of
                Ok response ->
                    ( { model
                        | sessionState =
                            if response.valid then
                                Valid

                            else
                                Invalid
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | sessionState = Invalid }
                    , Cmd.none
                    )

        NavigateTo path ->
            case ( path, model.sessionState ) of
                ( "/login", Valid ) ->
                    -- If trying to go to login but already logged in, go to dashboard
                    ( model, Nav.pushUrl model.key "/dashboard" )

                _ ->
                    -- Otherwise go to requested path
                    ( model, Nav.pushUrl model.key path )

        NavigateSignup ->
            -- Direct navigation to onboarding plan
            ( model
            , Nav.pushUrl model.key "/onboarding/plan"
            )

        SetExperienceTab tab ->
            ( { model | activeExperienceTab = tab, carouselActive = False }
            , Cmd.none
            )

        StartCarousel ->
            ( { model | carouselActive = True }
            , Cmd.none
            )

        StopCarousel ->
            ( { model | carouselActive = False }
            , Cmd.none
            )

        RotateCarousel _ ->
            if model.carouselActive then
                let
                    nextTab =
                        case model.activeExperienceTab of
                            Email ->
                                Quote

                            Quote ->
                                Underwriting

                            Underwriting ->
                                Email
                in
                ( { model | activeExperienceTab = nextTab }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Medicare Max - Renew Your Medigap Clients on Autopilot"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ nav [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6" ]
                [ div [ class "flex justify-between items-center" ]
                    [ div [ class "flex items-center" ]
                        [ div [ class "flex items-center gap-2" ]
                            [ img
                                [ src "/images/medicare-max-logo.svg"
                                , class "h-8 w-auto"
                                , alt "Medicare Max logo"
                                ]
                                []
                            , span [ class "font-bold text-[#03045E] text-xl" ] [ text "Medicare Max" ]
                            ]
                        ]
                    , div [ class "flex items-center space-x-4" ]
                        [ button
                            [ onClick (NavigateTo "/login")
                            , class "text-gray-600 hover:text-gray-900 px-4 py-2 text-sm font-medium"
                            ]
                            [ text "Log in" ]
                        , button
                            [ onClick NavigateSignup
                            , class "bg-[#03045E] text-white px-4 py-2 rounded-lg text-sm font-medium hover:bg-[#1a1f5f] transition-colors duration-200"
                            ]
                            [ text "Sign up" ]
                        ]
                    ]
                ]
            , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 pt-16 pb-32" ]
                [ div [ class "lg:grid lg:grid-cols-12 lg:gap-8" ]
                    [ div [ class "sm:text-center md:max-w-2xl md:mx-auto lg:col-span-6 lg:text-left" ]
                        [ div [ class "inline-flex items-center space-x-2 bg-[#F9F5FF] rounded-full px-4 py-1.5 mb-8" ]
                            [ span [ class "text-sm font-medium text-[#03045E]" ] [ text "Old book of business?" ]
                            , span [ class "text-sm text-[#03045E]" ] [ text "Renew with ease" ]
                            ]
                        , h1
                            [ class "text-5xl tracking-tight font-bold text-[#141B29] sm:text-6xl md:text-6xl" ]
                            [ text "Renew Your Medigap Clients on Autopilot" ]
                        , p
                            [ class "mt-6 text-lg text-gray-600 leading-relaxed" ]
                            [ text "Our AI-powered system handles client outreach, quotes, health underwriting, and e-apps — magically resetting your residuals so you can focus on growing your book." ]
                        , div [ class "mt-10" ]
                            [ button
                                [ onClick NavigateSignup
                                , class "inline-flex items-center px-6 py-3 rounded-lg text-base font-medium text-white bg-[#03045E] hover:bg-[#1a1f5f] transition-colors duration-200"
                                ]
                                [ text "Join the Waitlist" ]
                            ]
                        ]
                    , div [ class "mt-16 relative sm:max-w-lg sm:mx-auto lg:mt-0 lg:max-w-none lg:mx-0 lg:col-span-6 lg:flex lg:items-center" ]
                        [ div [ class "relative mx-auto w-full rounded-2xl shadow-xl overflow-hidden bg-gray-100" ]
                            [ img
                                [ src "/images/hero.png"
                                , class "w-full"
                                , alt "Dashboard with agent statistics"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "bg-white py-16" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 text-center" ]
                    [ div [ class "mb-12" ]
                        [ span [ class "text-[#03045E] font-semibold text-base" ] [ text "It's quite simple" ]
                        , h2 [ class "mt-3 text-3xl sm:text-4xl font-semibold text-gray-900" ] [ text "Here's how it works" ]
                        , p [ class "mt-4 text-lg text-gray-600 max-w-2xl mx-auto" ] [ text "3 easy steps to setup your book and automate your retention." ]
                        ]
                    , div [ class "grid grid-cols-1 md:grid-cols-3 gap-8 mt-16" ]
                        [ div [ class "bg-white p-6 rounded-lg" ]
                            [ div [ class "bg-[#03045E] w-10 h-10 flex items-center justify-center rounded-lg mb-6" ]
                                [ span [ class "text-white" ] [ text "⚡" ]
                                ]
                            , h3 [ class "text-xl font-semibold text-gray-900" ] [ text "Upload" ]
                            , p [ class "mt-2 text-gray-600" ] [ text "Upload your book of business effortlessly. Our system tracks policy age, renewal windows, and client details automatically." ]
                            ]
                        , div [ class "bg-white p-6 rounded-lg" ]
                            [ div [ class "bg-[#03045E] w-10 h-10 flex items-center justify-center rounded-lg mb-6" ]
                                [ span [ class "text-white" ] [ text "📧" ]
                                ]
                            , h3 [ class "text-xl font-semibold text-gray-900" ] [ text "Engage" ]
                            , p [ class "mt-2 text-gray-600" ] [ text "Clients receive personalized quotes showing your preferred carriers, underwriting pre-checks, and renewal options at key moments—without manual outreach." ]
                            ]
                        , div [ class "bg-white p-6 rounded-lg" ]
                            [ div [ class "bg-[#03045E] w-10 h-10 flex items-center justify-center rounded-lg mb-6" ]
                                [ span [ class "text-white" ] [ text "📈" ]
                                ]
                            , h3 [ class "text-xl font-semibold text-gray-900" ] [ text "Retain & Reset" ]
                            , p [ class "mt-2 text-gray-600" ] [ text "Clients complete 95% of the underwriting and application on their own. You simply verify details in a quick 5-minute call and submit the application." ]
                            ]
                        ]
                    ]
                ]
            , div [ class "py-16 max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                [ div [ class "text-left mb-10" ]
                    [ h2 [ class "text-3xl sm:text-4xl font-semibold text-gray-900" ]
                        [ text "Your Client's Personalized Experience" ]
                    , p [ class "mt-3 text-base text-gray-600 max-w-3xl" ]
                        [ text "Hover over each section to see what your client will experience along their journey to resetting the clock." ]
                    ]
                , div [ class "flex flex-col lg:flex-row gap-8 lg:gap-12 items-start" ]
                    [ div [ class "w-full lg:w-3/5 space-y-8" ]
                        [ div
                            [ class (experienceTabClass model.activeExperienceTab Email)
                            , onMouseEnter (SetExperienceTab Email)
                            , onMouseLeave StartCarousel
                            ]
                            [ h3 [ class "text-xl font-semibold text-gray-900" ]
                                [ text "Your Client Receives a Personalized Email" ]
                            , p [ class "mt-2 text-gray-600" ]
                                [ text "Your client gets a tailored email with their name, plan info, and helpful next steps—making them feel seen, supported, and confident in their Medicare decision." ]
                            ]
                        , div
                            [ class (experienceTabClass model.activeExperienceTab Quote)
                            , onMouseEnter (SetExperienceTab Quote)
                            , onMouseLeave StartCarousel
                            ]
                            [ h3 [ class "text-xl font-semibold text-gray-900" ]
                                [ text "They Review the Quotes" ]
                            , p [ class "mt-2 text-gray-600" ]
                                [ text "Your client can easily review their personalized Medicare quotes—clear, side-by-side comparisons that make choosing the right plan simple and stress-free." ]
                            ]
                        , div
                            [ class (experienceTabClass model.activeExperienceTab Underwriting)
                            , onMouseEnter (SetExperienceTab Underwriting)
                            , onMouseLeave StartCarousel
                            ]
                            [ h3 [ class "text-xl font-semibold text-gray-900" ]
                                [ text "And Then Complete Underwriting & Schedules" ]
                            , p [ class "mt-2 text-gray-600" ]
                                [ text "Your client answers a few quick health questions and picks a time that works best for them—keeping the process smooth, secure, and completely on their terms." ]
                            ]
                        ]
                    , div [ class "w-full lg:w-2/5 flex items-center justify-end" ]
                        [ div [ class "relative w-full max-w-[450px] h-auto drop-shadow-2xl" ]
                            [ div [ class (phoneContentClass model.activeExperienceTab Email) ]
                                [ img
                                    [ src "/images/email.png"
                                    , class "w-full object-contain rounded-xl"
                                    , alt "Personalized email preview"
                                    ]
                                    []
                                ]
                            , div [ class (phoneContentClass model.activeExperienceTab Quote) ]
                                [ img
                                    [ src "/images/quote.png"
                                    , class "w-full object-contain rounded-xl"
                                    , alt "Medicare quote comparison"
                                    ]
                                    []
                                ]
                            , div [ class (phoneContentClass model.activeExperienceTab Underwriting) ]
                                [ img
                                    [ src "/images/underwriting.png"
                                    , class "w-full object-contain rounded-xl"
                                    , alt "Underwriting and scheduling interface"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "bg-white py-16" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 text-center" ]
                    [ span [ class "text-[#03045E] font-semibold text-base" ] [ text "Features" ]
                    , h2 [ class "mt-3 text-3xl sm:text-4xl font-semibold text-gray-900" ] [ text "All you need to reset your commissions" ]
                    , p [ class "mt-4 text-lg text-gray-600 max-w-2xl mx-auto" ] [ text "It's like having a new team member that's only focused on retention." ]
                    , div [ class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8 mt-16" ]
                        [ featureCard "Simple Quote to Call Flow" "Something about admin and agent settings" "💬"
                        , featureCard "Non-Commissionable Protection" "Something about GI and year in timing protection" "⚡"
                        , featureCard "Live Analytics" "Up to date analytics that measure quote sends, requests, and follow-ups along with who is up next for contact." "📊"
                        , featureCard "Activity Notifications" "Something around them getting notified whether a client requests a follow up or simply a quote so they know when to connect." "😊"
                        , featureCard "Carrier and Licensing Control" "Something about them deciding who to show to their clients." "⌘"
                        , featureCard "Agent or Agency Setup" "Something about admin and agent settings" "❤️"
                        ]
                    ]
                ]
            , div [ class "py-24 bg-white" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "flex flex-col lg:flex-row gap-12 items-center relative" ]
                        [ div [ class "w-full lg:w-1/2 space-y-8 z-10" ]
                            [ h2 [ class "text-4xl font-semibold text-gray-900" ]
                                [ text "It's like keeping money in your pocket." ]
                            , div [ class "space-y-4 pl-4" ]
                                [ div [ class "flex gap-3 items-start" ]
                                    [ div [ class "text-[#03045E] text-xl" ] [ text "✓" ]
                                    , p [ class "text-lg text-gray-600" ] [ text "White-Labeled Tools" ]
                                    ]
                                , div [ class "flex gap-3 items-start" ]
                                    [ div [ class "text-[#03045E] text-xl" ] [ text "✓" ]
                                    , p [ class "text-lg text-gray-600" ] [ text "5 Minute Setup" ]
                                    ]
                                , div [ class "flex gap-3 items-start" ]
                                    [ div [ class "text-[#03045E] text-xl" ] [ text "✓" ]
                                    , p [ class "text-lg text-gray-600" ] [ text "Automated Retention" ]
                                    ]
                                ]
                            , div [ class "mt-10" ]
                                [ button
                                    [ onClick NavigateSignup
                                    , class "inline-flex items-center px-6 py-3 rounded-lg text-base font-medium text-white bg-[#03045E] hover:bg-[#1a1f5f] transition-colors duration-200"
                                    ]
                                    [ text "Join the Waitlist" ]
                                ]
                            ]
                        , div [ class "w-full lg:w-3/4 lg:absolute lg:-right-12 lg:top-1/2 lg:transform lg:-translate-y-1/2" ]
                            [ img [ src "/images/flap.png", class "w-full h-auto rounded-lg", alt "Dashboard interface" ] [] ]
                        ]
                    ]
                ]
            , div [ class "py-16" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "text-center mb-12" ]
                        [ h2 [ class "text-3xl sm:text-4xl font-semibold text-gray-900" ] [ text "Frequently asked questions" ]
                        , p [ class "mt-4 text-lg text-gray-600" ] [ text "Everything you need to know about the product and billing." ]
                        ]
                    , div [ class "max-w-3xl mx-auto divide-y divide-gray-200" ]
                        [ faqItem "Is there a free trial available?" "Yes, you can try us for free for 14 days. If you want, we'll provide you with a free, personalized 30-minute onboarding call to get you up and running as soon as possible." True
                        , faqItem "How many emails can I send?" "" False
                        , faqItem "How do I know my Client data is protected?" "" False
                        , faqItem "Will I be notified when someone requests a quote?" "" False
                        , faqItem "Will the emails come from me?" "" False
                        , faqItem "Can I customize the emails that are sent?" "" False
                        ]
                    ]
                ]
            , div [ class "py-16 bg-white" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "bg-white rounded-lg p-12" ]
                        [ h2 [ class "text-3xl font-semibold text-gray-900" ] [ text "Want to be notified on launch day?" ]
                        , p [ class "mt-4 text-lg text-gray-600" ] [ text "Join agents all across the US ready to reset their books." ]
                        , div [ class "mt-8" ]
                            [ button
                                [ onClick NavigateSignup
                                , class "inline-flex items-center px-6 py-3 rounded-lg text-base font-medium text-white bg-[#03045E] hover:bg-[#1a1f5f] transition-colors duration-200"
                                ]
                                [ text "Join the Waitlist" ]
                            ]
                        ]
                    ]
                ]
            , footer [ class "bg-[#141B29] text-white py-16" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "flex flex-col md:flex-row justify-between pb-12" ]
                        [ div [ class "mb-8 md:mb-0" ]
                            [ div [ class "flex items-center gap-2" ]
                                [ img [ src "/images/medicare-max-logo-white.svg", class "h-8 w-auto", alt "Medicare Max logo" ] []
                                , span [ class "font-bold text-white text-xl" ] [ text "Medicare Max" ]
                                ]
                            , p [ class "mt-4 text-[#94969C]" ] [ text "Retention technology that has your back." ]
                            ]
                        , div [ class "flex gap-8" ]
                            [ a [ href "#", class "text-[#94969C] hover:text-white" ] [ text "Overview" ]
                            , a [ href "#", class "text-[#94969C] hover:text-white" ] [ text "Pricing" ]
                            , a [ href "#", class "text-[#94969C] hover:text-white" ] [ text "Contact" ]
                            ]
                        ]
                    , div [ class "border-t border-[#1F242F] pt-8 text-[#94969C]" ]
                        [ p [] [ text "© 2023 Medicare Max. All rights reserved." ] ]
                    ]
                ]
            ]
        ]
    }


imageVisibilityClass : ExperienceTab -> ExperienceTab -> String
imageVisibilityClass activeTab tab =
    if activeTab == tab then
        "absolute inset-0 w-full h-full object-cover transition-opacity duration-500 opacity-100"

    else
        "absolute inset-0 w-full h-full object-cover transition-opacity duration-500 opacity-0"


experienceTabClass : ExperienceTab -> ExperienceTab -> String
experienceTabClass activeTab tab =
    let
        baseClass =
            "border-l-4 pl-6 py-5 cursor-pointer transition-all duration-300 ease-in-out"
    in
    if activeTab == tab then
        baseClass ++ " border-[#03045E]"

    else
        baseClass ++ " border-gray-200 hover:border-gray-400"


phoneContentClass : ExperienceTab -> ExperienceTab -> String
phoneContentClass activeTab tab =
    if activeTab == tab then
        "absolute inset-0 w-full h-full transition-opacity duration-500 opacity-100"

    else
        "absolute inset-0 w-full h-full transition-opacity duration-500 opacity-0"


featureCard : String -> String -> String -> Html Msg
featureCard title description icon =
    div [ class "bg-white p-6 rounded-lg shadow-sm border border-gray-100" ]
        [ div [ class "bg-[#03045E] w-10 h-10 flex items-center justify-center rounded-lg mb-6 mx-auto" ]
            [ span [ class "text-white" ] [ text icon ]
            ]
        , h3 [ class "text-xl font-semibold text-gray-900 text-center" ] [ text title ]
        , p [ class "mt-2 text-gray-600 text-center" ] [ text description ]
        ]


faqItem : String -> String -> Bool -> Html Msg
faqItem question answer isOpen =
    div [ class "py-6" ]
        [ div [ class "flex justify-between items-start" ]
            [ h3 [ class "text-lg font-medium text-gray-900" ] [ text question ]
            , button [ class "ml-6 h-7 w-7 flex items-center justify-center rounded-full" ]
                [ if isOpen then
                    span [ class "text-gray-500" ] [ text "−" ]

                  else
                    span [ class "text-gray-500" ] [ text "+" ]
                ]
            ]
        , if isOpen then
            p [ class "mt-2 text-gray-600" ] [ text answer ]

          else
            text ""
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.carouselActive then
        Time.every 4000 RotateCarousel

    else
        Sub.none
