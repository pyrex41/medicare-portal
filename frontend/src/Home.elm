module Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import Json.Decode as Decode
import MyIcon exposing (activity, brightArrow, chatBubbles, commandKey, envelope, heartBubble, lightning, smilieyChat)
import Ports exposing (getOrgSlug, receiveOrgSlug)
import Set exposing (Set)
import Svg exposing (Svg)
import Time



-- MODEL


type alias Model =
    { key : Nav.Key
    , sessionState : SessionState
    , activeExperienceTab : ExperienceTab
    , carouselActive : Bool
    , expandedFaqs : Set String
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
    | ToggleFaq String
    | NoOp


type alias SessionResponse =
    { valid : Bool }


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key
      , sessionState = Unknown
      , activeExperienceTab = Email
      , carouselActive = True
      , expandedFaqs = Set.empty
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

        ToggleFaq id ->
            ( { model
                | expandedFaqs =
                    if Set.member id model.expandedFaqs then
                        Set.remove id model.expandedFaqs

                    else
                        Set.insert id model.expandedFaqs
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Medicare Max - Renew Your Medigap Clients on Autopilot"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ nav [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4 sm:py-6" ]
                [ div [ class "flex justify-between items-center" ]
                    [ div [ class "flex items-center" ]
                        [ img
                            [ src "/images/medicare-max-logo.png"
                            , class "h-6 sm:h-8 w-auto"
                            , alt "Medicare Max logo"
                            ]
                            []
                        ]
                    , div [ class "flex items-center space-x-2 sm:space-x-4" ]
                        [ button
                            [ onClick (NavigateTo "/login")
                            , class "text-gray-600 hover:text-gray-900 px-3 sm:px-4 py-2 text-sm font-medium"
                            ]
                            [ text "Log in" ]
                        , button
                            [ onClick NavigateSignup
                            , class "bg-[#03045E] text-white px-3 sm:px-4 py-2 rounded-lg text-sm font-medium hover:bg-[#1a1f5f] transition-colors duration-200"
                            ]
                            [ text "Sign up" ]
                        ]
                    ]
                ]
            , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 pt-8 sm:pt-16 pb-16 sm:pb-32" ]
                [ div [ class "grid grid-cols-1 lg:grid-cols-2 gap-8 lg:gap-16 items-center" ]
                    [ div [ class "relative bg-white rounded-2xl p-6 sm:p-8 lg:p-12" ]
                        [ div [ class "inline-flex items-center rounded-full bg-[#F9F5FF] px-2 sm:px-3 py-1 mb-6 sm:mb-8" ]
                            [ div [ class "bg-[#03045E] rounded-full px-2 sm:px-2.5 py-0.5" ]
                                [ span [ class "text-xs sm:text-sm font-medium text-white" ] [ text "Old book of business?" ]
                                ]
                            , div [ class "flex items-center ml-2" ]
                                [ span [ class "text-xs sm:text-sm font-medium text-[#03045E]" ] [ text "Renew with ease" ]
                                , span [ class "ml-1 text-[#03045E]" ] [ text "→" ]
                                ]
                            ]
                        , h1
                            [ class "text-3xl sm:text-4xl lg:text-5xl xl:text-6xl tracking-tight font-semibold text-[#141B29] leading-[1.2]" ]
                            [ text "Renew Your Medigap Clients on Autopilot" ]
                        , p
                            [ class "mt-4 sm:mt-6 text-base sm:text-lg text-[#475467] leading-[1.5]" ]
                            [ text "Our AI-powered system handles client outreach, quotes, health underwriting, and e-apps — magically resetting your residuals so you can focus on growing your book." ]
                        , div [ class "mt-6 sm:mt-10" ]
                            [ button
                                [ onClick NavigateSignup
                                , class "w-full sm:w-auto inline-flex items-center justify-center px-4 sm:px-6 py-3 sm:py-4 rounded-lg text-base sm:text-lg font-semibold text-white bg-[#03045E] hover:bg-[#1a1f5f] transition-colors duration-200"
                                ]
                                [ text "Join the Waitlist" ]
                            ]
                        ]
                    , div [ class "relative bg-white rounded-2xl p-4 sm:p-8 lg:p-12" ]
                        [ div [ class "mx-auto w-full relative rounded-lg overflow-hidden" ]
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
            , div [ class "bg-white py-8 sm:py-16" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "mb-8 sm:mb-12 text-left" ]
                        [ span [ class "text-[#03045E] font-semibold text-sm sm:text-base" ] [ text "It's quite simple" ]
                        , h2 [ class "mt-2 sm:mt-3 text-2xl sm:text-3xl md:text-4xl font-semibold text-gray-900" ] [ text "Here's how it works" ]
                        , p [ class "mt-2 sm:mt-4 text-base sm:text-lg text-gray-600" ] [ text "3 easy steps to setup your book and automate your retention." ]
                        ]
                    , div [ class "grid grid-cols-1 md:grid-cols-3 gap-4 sm:gap-8 mt-8 sm:mt-16" ]
                        [ div [ class "bg-[#F9FAFB] p-4 sm:p-6 rounded-lg" ]
                            [ div [ class "bg-[#03045E] w-10 sm:w-12 h-10 sm:h-12 flex items-center justify-center rounded-lg mb-4 sm:mb-6" ]
                                [ lightning 24 "#FFFFFF"
                                ]
                            , h3 [ class "text-lg sm:text-xl font-semibold text-gray-900 text-left" ] [ text "Upload" ]
                            , p [ class "mt-2 text-sm sm:text-base text-gray-600 text-left" ] [ text "Upload your book of business effortlessly. Our system tracks policy age, renewal windows, and client details automatically." ]
                            ]
                        , div [ class "bg-[#F9FAFB] p-4 sm:p-6 rounded-lg" ]
                            [ div [ class "bg-[#03045E] w-10 sm:w-12 h-10 sm:h-12 flex items-center justify-center rounded-lg mb-4 sm:mb-6" ]
                                [ envelope 24 "#FFFFFF"
                                ]
                            , h3 [ class "text-lg sm:text-xl font-semibold text-gray-900 text-left" ] [ text "Engage" ]
                            , p [ class "mt-2 text-sm sm:text-base text-gray-600 text-left" ] [ text "Clients receive personalized quotes showing your preferred carriers, underwriting pre-checks, and renewal options at key moments—without manual outreach." ]
                            ]
                        , div [ class "bg-[#F9FAFB] p-4 sm:p-6 rounded-lg" ]
                            [ div [ class "bg-[#03045E] w-10 sm:w-12 h-10 sm:h-12 flex items-center justify-center rounded-lg mb-4 sm:mb-6" ]
                                [ brightArrow 24 "#FFFFFF"
                                ]
                            , h3 [ class "text-lg sm:text-xl font-semibold text-gray-900 text-left" ] [ text "Retain & Reset" ]
                            , p [ class "mt-2 text-sm sm:text-base text-gray-600 text-left" ] [ text "Clients complete 95% of the underwriting and application on their own. You simply verify details in a quick 5-minute call and submit the application." ]
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
                        [ div [ class "relative w-full max-w-[450px] h-auto" ]
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
                        [ featureCard "Simple Quote to Call Flow" "Something about admin and agent settings" (chatBubbles 24 "#03045E")
                        , featureCard "Non-Commissionable Protection" "Something about GI and year in timing protection" (lightning 24 "#03045E")
                        , featureCard "Live Analytics" "Up to date analytics that measure quote sends, requests, and follow-ups along with who is up next for contact." (activity 24 "#03045E")
                        , featureCard "Activity Notifications" "Something around them getting notified whether a client requests a follow up or simply a quote so they know when to connect." (smilieyChat 24 "#03045E")
                        , featureCard "Carrier and Licensing Control" "Something about them deciding who to show to their clients." (commandKey 24 "#03045E")
                        , featureCard "Agent or Agency Setup" "Something about admin and agent settings" (heartBubble 24 "#03045E")
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
                        , p [ class "mt-4 text-lg text-gray-600" ] [ text "Everything you need to know." ]
                        ]
                    , div [ class "max-w-3xl mx-auto divide-y divide-gray-200" ]
                        [ faqItem "How many emails can I send?"
                            "Our system sends regular, automated emails to every client in your book of business. Each plan tier supports different numbers of contacts - so you can choose the right level for your agency size. The emails continue indefinitely to keep your book engaged and ready for renewal opportunities."
                            model
                        , faqItem "How do I know my Client data is protected?"
                            "We take data security extremely seriously. All client data is encrypted both in transit and at rest using industry-standard encryption protocols. Additionally, each agency gets their own dedicated database instance, ensuring complete data separation between different agencies' client records."
                            model
                        , faqItem "Will I be notified when someone requests a quote?"
                            "Yes, you'll receive real-time notifications whenever a client requests a quote or takes any significant action. You can customize your notification preferences in your dashboard settings."
                            model
                        , faqItem "Will the emails come from me?"
                            "Yes, all communications are white-labeled and will appear to come directly from you. You can customize the email sender name and signature to maintain your personal brand and relationship with your clients."
                            model
                        ]
                    ]
                ]
            , div [ class "py-16 bg-white" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "bg-white rounded-lg p-12 text-center max-w-3xl mx-auto" ]
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
            , footer [ class "bg-[#141B29] text-white py-8 sm:py-16" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "flex flex-col md:flex-row justify-between pb-8 sm:pb-12" ]
                        [ div [ class "mb-6 md:mb-0" ]
                            [ div [ class "flex items-center" ]
                                [ img [ src "/images/whiteIcon.svg", class "h-6 sm:h-8 w-auto", alt "Medicare Max logo" ] []
                                ]
                            , p [ class "mt-3 sm:mt-4 text-sm sm:text-base text-[#94969C]" ] [ text "Retention technology that has your back." ]
                            ]
                        , div [ class "flex gap-4 sm:gap-8" ]
                            [ a [ href "#", class "text-sm sm:text-base text-[#94969C] hover:text-white" ] [ text "Overview" ]
                            , a [ href "#", class "text-sm sm:text-base text-[#94969C] hover:text-white" ] [ text "Contact" ]
                            ]
                        ]
                    , div [ class "border-t border-[#1F242F] pt-6 sm:pt-8 text-[#94969C] text-sm sm:text-base" ]
                        [ p [] [ text "© 2025 Medicare Max. All rights reserved." ] ]
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


featureCard : String -> String -> Svg Msg -> Html Msg
featureCard title description icon =
    div [ class "bg-white p-4 sm:p-6 rounded-lg shadow-md" ]
        [ div [ class "w-10 sm:w-12 h-10 sm:h-12 mx-auto mb-4 sm:mb-6" ]
            [ icon ]
        , h3 [ class "text-lg sm:text-xl font-semibold text-gray-900 text-center" ] [ text title ]
        , p [ class "mt-2 text-sm sm:text-base text-gray-600 text-center" ] [ text description ]
        ]


faqItem : String -> String -> Model -> Html Msg
faqItem question answer model =
    let
        isExpanded =
            Set.member question model.expandedFaqs
    in
    div [ class "py-4 sm:py-6" ]
        [ div
            [ class "flex justify-between items-start cursor-pointer group"
            , onClick (ToggleFaq question)
            ]
            [ h3 [ class "text-base sm:text-lg font-medium text-gray-900 group-hover:text-gray-700 transition-colors duration-200" ] [ text question ]
            , button
                [ class "ml-4 sm:ml-6 h-6 sm:h-7 w-6 sm:w-7 flex items-center justify-center rounded-full group-hover:bg-gray-100 transition-colors duration-200"
                ]
                [ span
                    [ class "text-gray-500 text-lg sm:text-xl transition-transform duration-200"
                    , style "transform"
                        (if isExpanded then
                            "rotate(180deg)"

                         else
                            "rotate(0deg)"
                        )
                    ]
                    [ text
                        (if isExpanded then
                            "-"

                         else
                            "+"
                        )
                    ]
                ]
            ]
        , div
            [ class "mt-2 text-sm sm:text-base text-gray-600 overflow-hidden transition-all duration-300 ease-in-out"
            , style "max-height"
                (if isExpanded then
                    "500px"

                 else
                    "0"
                )
            , style "opacity"
                (if isExpanded then
                    "1"

                 else
                    "0"
                )
            ]
            [ text answer ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.carouselActive then
        Time.every 4000 RotateCarousel

    else
        Sub.none
