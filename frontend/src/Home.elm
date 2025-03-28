port module Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import Json.Decode as Decode
import MyIcon exposing (activity, brightArrow, chatBubbles, commandKey, envelope, heartBubble, lightning, smilieyChat)
import Ports exposing (getOrgSlug, receiveOrgSlug)
import Process
import Set exposing (Set)
import Svg exposing (Svg)
import Task
import Time


port viewingPhone : (Bool -> msg) -> Sub msg



-- MODEL


type alias Model =
    { key : Nav.Key
    , sessionState : SessionState
    , activeExperienceTab : ExperienceTab
    , carouselActive : Bool
    , expandedFaqs : Set String
    , expandedFeature : Maybe String
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
    | NavigateSignup
    | SetExperienceTab ExperienceTab
    | StartCarousel
    | StopCarousel
    | RotateCarousel Time.Posix
    | ToggleFaq String
    | ToggleFeature String
    | PhoneSectionVisible Bool
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
      , expandedFeature = Nothing
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

        NavigateSignup ->
            ( model
            , Nav.pushUrl model.key "/waitlist"
            )

        SetExperienceTab tab ->
            ( { model | activeExperienceTab = tab, carouselActive = False }
            , Cmd.batch
                [ Process.sleep 5000
                    |> Task.perform (\_ -> StartCarousel)
                ]
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

        ToggleFeature id ->
            ( { model
                | expandedFeature =
                    if model.expandedFeature == Just id then
                        Nothing

                    else
                        Just id
              }
            , Cmd.none
            )

        PhoneSectionVisible isVisible ->
            if isVisible then
                ( { model | activeExperienceTab = Email, carouselActive = True }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Medicare Max - Boost Your Medigap Renewals with AI"
    , body =
        [ div [ class "min-h-screen bg-white md:snap-y md:snap-mandatory overflow-y-auto h-screen scroll-smooth" ]
            [ nav [ class "max-w-7xl mx-auto px-6 sm:px-6 lg:px-8 py-4 sm:py-6 sticky top-0 z-50 bg-white hidden xl:block" ]
                [ div [ class "flex justify-between items-center" ]
                    [ div [ class "flex items-center" ]
                        [ img
                            [ src "/images/medicare-max-logo.png"
                            , class "h-6 sm:h-8 w-auto"
                            , alt "Medicare Max logo"
                            ]
                            []
                        ]
                    , div [ class "flex items-center" ]
                        [ button
                            [ onClick NavigateSignup
                            , class "bg-[#03045E] text-white px-4 sm:px-6 py-2 rounded-lg text-sm font-medium hover:bg-[#1a1f5f] transition-colors duration-200"
                            ]
                            [ text "Join the Waitlist" ]
                        ]
                    ]
                ]
            , div [ class "max-w-7xl mx-auto px-6 sm:px-6 lg:px-8 pt-8 sm:pt-16 pb-16 sm:pb-32 min-h-screen flex items-center md:snap-start" ]
                [ div [ class "grid grid-cols-1 lg:grid-cols-2 gap-8 lg:gap-16 items-center" ]
                    [ div [ class "relative" ]
                        [ nav [ class "mb-8 xl:hidden" ]
                            [ div [ class "flex justify-between items-center" ]
                                [ div [ class "flex items-center" ]
                                    [ img
                                        [ src "/images/medicare-max-logo.png"
                                        , class "h-6 sm:h-8 w-auto"
                                        , alt "Medicare Max logo"
                                        ]
                                        []
                                    ]
                                , div [ class "flex items-center" ]
                                    [ button
                                        [ onClick NavigateSignup
                                        , class "bg-[#03045E] text-white px-4 sm:px-6 py-2 rounded-lg text-sm font-medium hover:bg-[#1a1f5f] transition-colors duration-200"
                                        ]
                                        [ text "Join the Waitlist" ]
                                    ]
                                ]
                            ]
                        , div [ class "flex justify-center sm:justify-start w-full" ]
                            [ div [ class "inline-flex items-center rounded-full bg-[#F9F5FF] mt-16 sm:mt-0 px-0 sm:px-0 py-1 mb-6 sm:mb-8" ]
                                [ div [ class "bg-[#03045E] rounded-full px-3 sm:px-3.5 py-1" ]
                                    [ span [ class "text-xs sm:text-sm text-white" ] [ text "Old book of business?" ]
                                    ]
                                , div [ class "flex items-center px-2 sm:px-3" ]
                                    [ span [ class "text-xs sm:text-sm font-medium text-[#03045E]" ] [ text "Renew with ease" ]
                                    , span [ class "ml-1 text-[#03045E]" ] [ text "→" ]
                                    ]
                                ]
                            ]
                        , h1
                            [ class "text-3xl sm:text-4xl lg:text-5xl xl:text-6xl tracking-tight font-semibold text-[#141B29] leading-[1.2] text-center sm:text-left" ]
                            [ text "Boost Your Medigap Renewals with AI" ]
                        , p
                            [ class "mt-4 sm:mt-6 text-base sm:text-lg text-[#475467] leading-[1.5] text-center sm:text-left" ]
                            [ text "Our AI-powered system handles client outreach, quotes, health underwriting, and e-apps — magically resetting your residuals so you can focus on growing your book." ]
                        , div [ class "mt-6 sm:mt-10 flex justify-center sm:justify-start" ]
                            [ button
                                [ onClick NavigateSignup
                                , class "w-full sm:w-auto inline-flex items-center justify-center px-4 sm:px-6 py-3 sm:py-4 rounded-lg text-base sm:text-lg font-semibold text-white bg-[#03045E] hover:bg-[#1a1f5f] transition-colors duration-200"
                                ]
                                [ text "Join the Waitlist" ]
                            ]
                        ]
                    , div [ class "relative" ]
                        [ div [ class "mx-auto w-full max-w-sm sm:max-w-none relative rounded-lg overflow-hidden" ]
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
            , div [ class "bg-white py-8 sm:py-16 min-h-screen flex items-center md:snap-start" ]
                [ div [ class "max-w-7xl mx-auto px-6 sm:px-6 lg:px-8" ]
                    [ div [ class "mb-8 sm:mb-12 text-left" ]
                        [ span [ class "text-[#03045E] font-semibold text-sm sm:text-base" ] [ text "It's quite simple" ]
                        , h2 [ class "mt-2 sm:mt-3 text-2xl sm:text-3xl md:text-4xl font-semibold text-gray-900" ] [ text "Here's how it works" ]
                        , p [ class "mt-2 sm:mt-4 text-base sm:text-lg text-gray-600" ] [ text "3 easy steps to setup your book and automate your retention." ]
                        ]
                    , div [ class "grid grid-cols-1 md:grid-cols-3 gap-4 sm:gap-8 mt-8 sm:mt-16" ]
                        [ div [ class "bg-[#F9FAFB] p-4 sm:p-6 rounded-lg flex flex-row sm:flex-col items-center sm:items-center" ]
                            [ div [ class "bg-[#03045E] w-10 sm:w-12 h-10 sm:h-12 flex items-center justify-center rounded-lg mr-4 sm:mr-0 sm:mb-6 flex-shrink-0" ]
                                [ lightning 24 "#FFFFFF"
                                ]
                            , div [ class "flex-1 sm:text-center" ]
                                [ h3 [ class "text-lg sm:text-xl font-semibold text-gray-900 text-left sm:text-center" ] [ text "Upload" ]
                                , p [ class "mt-2 text-sm sm:text-base text-gray-600 text-left sm:text-center" ] [ text "Upload your book of business effortlessly. Our system tracks policy age, renewal windows, and client details automatically." ]
                                ]
                            ]
                        , div [ class "bg-[#F9FAFB] p-4 sm:p-6 rounded-lg flex flex-row sm:flex-col items-center sm:items-center" ]
                            [ div [ class "bg-[#03045E] w-10 sm:w-12 h-10 sm:h-12 flex items-center justify-center rounded-lg mr-4 sm:mr-0 sm:mb-6 flex-shrink-0" ]
                                [ envelope 24 "#FFFFFF"
                                ]
                            , div [ class "flex-1 sm:text-center" ]
                                [ h3 [ class "text-lg sm:text-xl font-semibold text-gray-900 text-left sm:text-center" ] [ text "Engage" ]
                                , p [ class "mt-2 text-sm sm:text-base text-gray-600 text-left sm:text-center" ] [ text "Clients receive personalized quotes showing your preferred carriers, underwriting pre-checks, and renewal options at key moments—without manual outreach." ]
                                ]
                            ]
                        , div [ class "bg-[#F9FAFB] p-4 sm:p-6 rounded-lg flex flex-row sm:flex-col items-center sm:items-center" ]
                            [ div [ class "bg-[#03045E] w-10 sm:w-12 h-10 sm:h-12 flex items-center justify-center rounded-lg mr-4 sm:mr-0 sm:mb-6 flex-shrink-0" ]
                                [ brightArrow 24 "#FFFFFF"
                                ]
                            , div [ class "flex-1 sm:text-center" ]
                                [ h3 [ class "text-lg sm:text-xl font-semibold text-gray-900 text-left sm:text-center" ] [ text "Retain & Reset" ]
                                , p [ class "mt-2 text-sm sm:text-base text-gray-600 text-left sm:text-center" ] [ text "Clients complete 95% of the underwriting and application on their own. You simply verify details in a quick 5-minute call and submit the application." ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "py-8 max-w-7xl mx-auto px-6 sm:px-6 lg:px-8 min-h-screen flex flex-col justify-center md:snap-start" ]
                [ div [ class "lg:block hidden text-left md:mb-10 max-w-3xl" ]
                    [ h2 [ class "text-2xl sm:text-3xl md:text-4xl font-semibold text-gray-900" ]
                        [ text "Your Client's Personalized Experience" ]
                    , p [ class "mt-3 text-base sm:text-lg text-gray-600" ]
                        [ text "Hover over each section to see what your client will experience along their journey to resetting the clock." ]
                    ]
                , div [ class "relative" ]
                    [ div [ class "absolute right-0 top-0 lg:-mt-40 lg:-mr-4 z-0 hidden lg:block" ]
                        [ div
                            [ class "relative h-[650px] w-[340px] lg:w-[380px] lg:h-[770px] rounded-[40px] transform lg:rotate-[3deg] overflow-hidden"
                            ]
                            [ div [ class (phoneContentClass model.activeExperienceTab Email) ]
                                [ img
                                    [ src "/images/email.jpeg"
                                    , class "absolute inset-0 w-full h-full object-cover rounded-[40px]"
                                    , alt "Personalized email preview"
                                    ]
                                    []
                                ]
                            , div [ class (phoneContentClass model.activeExperienceTab Quote) ]
                                [ img
                                    [ src "/images/quote.jpeg"
                                    , class "absolute inset-0 w-full h-full object-cover rounded-[40px]"
                                    , alt "Medicare quote comparison"
                                    ]
                                    []
                                ]
                            , div [ class (phoneContentClass model.activeExperienceTab Underwriting) ]
                                [ img
                                    [ src "/images/underwriting.jpeg"
                                    , class "absolute inset-0 w-full h-full object-cover rounded-[40px]"
                                    , alt "Underwriting and scheduling interface"
                                    ]
                                    []
                                ]
                            , div
                                [ class "absolute inset-0 pointer-events-none z-10"
                                , style "box-shadow" "inset 0 0 25px 20px #F9FAFB"
                                , style "border-radius" "40px"
                                ]
                                []
                            , div
                                [ class "absolute bottom-0 left-0 right-0 h-[30%] pointer-events-none"
                                , style "background" "linear-gradient(to top, #F9FAFB 0%, rgba(249, 250, 251, 0) 100%)"
                                ]
                                []
                            , div
                                [ class "absolute top-0 left-0 right-0 h-[20%] pointer-events-none"
                                , style "background" "linear-gradient(to bottom, #F9FAFB 0%, rgba(249, 250, 251, 0) 100%)"
                                ]
                                []
                            , div
                                [ class "absolute -left-[50px] -right-[50px] -bottom-[50px] h-[100px] pointer-events-none"
                                , style "background" "#F9FAFB"
                                , style "filter" "blur(40px)"
                                ]
                                []
                            ]
                        ]
                    , div [ class "lg:hidden flex flex-col items-center" ]
                        [ h2 [ class "text-2xl sm:text-3xl font-semibold text-gray-900 text-center mt-2 mb-4" ]
                            [ text "Your Client's Personalized Experience" ]
                        , div
                            [ class "relative h-[400px] w-[280px] rounded-[30px] overflow-hidden mb-4"
                            ]
                            [ div [ class (phoneContentClass model.activeExperienceTab Email) ]
                                [ img
                                    [ src "/images/email_crop.jpeg"
                                    , class "absolute inset-0 w-full h-full object-cover object-center rounded-[30px]"
                                    , alt "Personalized email preview"
                                    ]
                                    []
                                ]
                            , div [ class (phoneContentClass model.activeExperienceTab Quote) ]
                                [ img
                                    [ src "/images/quote_crop.jpeg"
                                    , class "absolute inset-0 w-full h-full object-cover object-center rounded-[30px]"
                                    , alt "Medicare quote comparison"
                                    ]
                                    []
                                ]
                            , div [ class (phoneContentClass model.activeExperienceTab Underwriting) ]
                                [ img
                                    [ src "/images/underwriting_crop.jpeg"
                                    , class "absolute inset-0 w-full h-full object-cover object-center rounded-[30px]"
                                    , alt "Underwriting and scheduling interface"
                                    ]
                                    []
                                ]
                            , div
                                [ class "absolute inset-0 pointer-events-none z-10"
                                , style "box-shadow" "inset 0 0 15px 10px #F9FAFB"
                                , style "border-radius" "30px"
                                ]
                                []
                            ]
                        ]
                    , div [ class "lg:hidden w-full" ]
                        [ div [ class "grid grid-cols-3 gap-1 mb-1" ]
                            [ div
                                [ class (tabContainerClass model.activeExperienceTab Email)
                                , onClick (SetExperienceTab Email)
                                ]
                                [ text "Email" ]
                            , div
                                [ class (tabContainerClass model.activeExperienceTab Quote)
                                , onClick (SetExperienceTab Quote)
                                ]
                                [ text "Quotes" ]
                            , div
                                [ class (tabContainerClass model.activeExperienceTab Underwriting)
                                , onClick (SetExperienceTab Underwriting)
                                ]
                                [ text "Underwriting" ]
                            ]
                        , div [ class "bg-white p-4 rounded-b-lg shadow-sm border-t-0" ]
                            [ div
                                [ class
                                    (if model.activeExperienceTab == Email then
                                        "block"

                                     else
                                        "hidden"
                                    )
                                ]
                                [ h3 [ class "text-lg font-semibold text-gray-900" ]
                                    [ text "Your Client Receives a Personalized Email" ]
                                , p [ class "mt-2 text-gray-600" ]
                                    [ text "Your client gets a tailored email with their name, plan info, and helpful next steps—making them feel seen, supported, and confident in their Medicare decision." ]
                                ]
                            , div
                                [ class
                                    (if model.activeExperienceTab == Quote then
                                        "block"

                                     else
                                        "hidden"
                                    )
                                ]
                                [ h3 [ class "text-lg font-semibold text-gray-900" ]
                                    [ text "They Review the Quotes" ]
                                , p [ class "mt-2 text-gray-600" ]
                                    [ text "Your client can easily review their personalized Medicare quotes—clear, side-by-side comparisons that make choosing the right plan simple and stress-free." ]
                                ]
                            , div
                                [ class
                                    (if model.activeExperienceTab == Underwriting then
                                        "block"

                                     else
                                        "hidden"
                                    )
                                ]
                                [ h3 [ class "text-lg font-semibold text-gray-900" ]
                                    [ text "And Then Complete Underwriting & Schedules" ]
                                , p [ class "mt-2 text-gray-600" ]
                                    [ text "Your client answers a few quick health questions and picks a time that works best for them—keeping the process smooth, secure, and completely on their terms." ]
                                ]
                            ]
                        ]
                    , div [ class "relative w-full lg:w-3/5 space-y-8 z-10 hidden lg:block" ]
                        [ div
                            [ class (experienceTabClass model.activeExperienceTab Email)
                            , onMouseEnter (SetExperienceTab Email)
                            , onMouseLeave StartCarousel
                            , onClick (SetExperienceTab Email)
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
                            , onClick (SetExperienceTab Quote)
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
                            , onClick (SetExperienceTab Underwriting)
                            ]
                            [ h3 [ class "text-xl font-semibold text-gray-900" ]
                                [ text "And Then Complete Underwriting & Schedules" ]
                            , p [ class "mt-2 text-gray-600" ]
                                [ text "Your client answers a few quick health questions and picks a time that works best for them—keeping the process smooth, secure, and completely on their terms." ]
                            ]
                        ]
                    ]
                ]
            , div [ class "py-12 sm:py-16 bg-white relative overflow-hidden min-h-[50vh] sm:min-h-screen flex items-center md:snap-start" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 relative z-20 w-full" ]
                    [ -- Mobile layout (hidden on md screens and up)
                      div [ class "md:hidden w-full" ]
                        [ div [ class "flex flex-col items-center" ]
                            [ div [ class "w-full mb-6" ]
                                [ div [ class "mx-auto max-w-xs" ]
                                    [ img [ src "/images/flap.png", class "w-full h-auto", alt "Dashboard interface" ] [] ]
                                ]
                            , div [ class "w-full text-center p-4" ]
                                [ h2 [ class "text-2xl font-semibold text-gray-900" ]
                                    [ text "It's like keeping money in your pocket." ]
                                , div [ class "space-y-3 mt-4" ]
                                    [ div [ class "flex gap-3 items-start" ]
                                        [ div [ class "text-[#03045E] text-lg flex-shrink-0" ] [ text "✓" ]
                                        , p [ class "text-base text-gray-600 text-left" ] [ text "White-Labeled Tools" ]
                                        ]
                                    , div [ class "flex gap-3 items-start" ]
                                        [ div [ class "text-[#03045E] text-lg flex-shrink-0" ] [ text "✓" ]
                                        , p [ class "text-base text-gray-600 text-left" ] [ text "5 Minute Setup" ]
                                        ]
                                    , div [ class "flex gap-3 items-start" ]
                                        [ div [ class "text-[#03045E] text-lg flex-shrink-0" ] [ text "✓" ]
                                        , p [ class "text-base text-gray-600 text-left" ] [ text "Automated Retention" ]
                                        ]
                                    ]
                                , div [ class "mt-6 flex justify-center" ]
                                    [ button
                                        [ onClick NavigateSignup
                                        , class "inline-flex items-center px-5 py-2 rounded-lg text-base font-medium text-white bg-[#03045E] hover:bg-[#1a1f5f] transition-colors duration-200"
                                        ]
                                        [ text "Join the Waitlist" ]
                                    ]
                                ]
                            ]
                        ]

                    -- Desktop layout (hidden on small screens)
                    , div [ class "hidden md:block relative w-full min-h-[700px]" ]
                        [ div [ class "absolute inset-0 z-0" ]
                            [ img [ src "/images/flap.png", class "w-full h-full object-contain object-right translate-x-20", alt "Dashboard interface" ] [] ]
                        , div [ class "relative z-10 max-w-lg p-8 rounded-lg" ]
                            [ h2 [ class "text-4xl font-semibold text-gray-900" ]
                                [ text "It's like keeping money in your pocket." ]
                            , div [ class "space-y-4 mt-6" ]
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
                ]
            , div [ class "py-8 sm:py-16 min-h-[50vh] sm:min-h-screen flex items-center md:snap-start" ]
                [ div [ class "max-w-7xl mx-auto px-6 sm:px-6 lg:px-8" ]
                    [ div [ class "text-center mb-8 sm:mb-12" ]
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
                    , div [ class "mt-16 text-center hidden md:block" ]
                        [ h3 [ class "text-2xl font-semibold text-gray-900" ] [ text "Want to be notified on launch day?" ]
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
            , div [ class "py-8 sm:py-14 min-h-[40vh] sm:min-h-screen flex items-center md:snap-start md:hidden" ]
                [ div [ class "max-w-7xl mx-auto px-6 sm:px-6 lg:px-8" ]
                    [ div [ class "text-center" ]
                        [ h3 [ class "text-2xl font-semibold text-gray-900" ] [ text "Want to be notified on launch day?" ]
                        , p [ class "mt-4 text-lg text-gray-600" ] [ text "Join agents all across the US ready to reset their books." ]
                        , div [ class "mt-8" ]
                            [ button
                                [ onClick NavigateSignup
                                , class "inline-flex items-center px-6 py-3 rounded-lg text-base font-medium text-white bg-[#03045E] hover:bg-[#1a1f5f] transition-colors duration-200 w-full sm:w-auto justify-center"
                                ]
                                [ text "Join the Waitlist" ]
                            ]
                        ]
                    ]
                ]
            , footer [ class "bg-[#141B29] text-white py-6 sm:py-8 md:py-16 md:snap-start" ]
                [ div [ class "max-w-7xl mx-auto px-6 sm:px-6 lg:px-8" ]
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
            "border-l-4 pl-4 sm:pl-6 py-4 sm:py-5 cursor-pointer transition-all duration-300 ease-in-out"
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


featureCard : Model -> String -> String -> String -> Svg Msg -> Html Msg
featureCard model id title description icon =
    let
        isExpanded =
            model.expandedFeature == Just id

        -- Base classes shared across all sizes
        baseCardClass =
            "rounded-lg transition-all duration-500 ease-in-out flex flex-col h-full"

        -- Mobile-specific classes (expanded state)
        mobileCardClass =
            if isExpanded then
                "md:hidden " ++ baseCardClass ++ " bg-white p-4 z-10 transform scale-102 shadow-lg"

            else
                "md:hidden " ++ baseCardClass ++ " bg-white p-3 hover:shadow-md"

        -- Desktop-specific classes (always showing description)
        desktopCardClass =
            "hidden md:flex " ++ baseCardClass ++ " bg-[#F9FAFB] p-4 sm:p-5"

        -- Mobile title classes
        mobileTitleClass =
            if isExpanded then
                "md:hidden text-base font-semibold text-[#03045E] text-center transition-colors duration-500"

            else
                "md:hidden text-base font-semibold text-gray-900 text-center transition-colors duration-500"

        -- Desktop title classes (always the same)
        desktopTitleClass =
            "hidden md:block text-base sm:text-lg font-semibold text-gray-900 text-center"

        -- Mobile icon container
        mobileIconContainerClass =
            if isExpanded then
                "md:hidden w-10 h-10 mx-auto mb-3 rounded-lg flex items-center justify-center shadow-md transition-all duration-500"

            else
                "md:hidden w-10 h-10 mx-auto mb-3 rounded-lg flex items-center justify-center shadow-sm transition-all duration-500"

        -- Desktop icon container
        desktopIconContainerClass =
            "hidden md:flex w-10 h-10 sm:w-11 sm:h-11 mx-auto mb-3 sm:mb-4 rounded-lg items-center justify-center shadow-md"
    in
    div []
        [ -- Mobile version with tap-to-expand
          div
            [ class mobileCardClass
            , style "box-shadow"
                (if isExpanded then
                    "0 10px 25px -5px rgba(0, 0, 0, 0.1), 0 8px 10px -6px rgba(0, 0, 0, 0.1)"

                 else
                    "0 1px 3px 0 rgba(0, 0, 0, 0.1)"
                )
            , onClick (ToggleFeature id)
            ]
            [ div [ class mobileIconContainerClass ] [ icon ]
            , h3 [ class mobileTitleClass ] [ text title ]
            , div
                [ class "md:hidden overflow-hidden transition-all duration-500 ease-in-out mt-2"
                , style "max-height"
                    (if isExpanded then
                        "200px"

                     else
                        "0px"
                    )
                , style "opacity"
                    (if isExpanded then
                        "1"

                     else
                        "0"
                    )
                , style "transform"
                    (if isExpanded then
                        "translateY(0)"

                     else
                        "translateY(-10px)"
                    )
                ]
                [ p [ class "text-xs text-[#03045E] text-center" ] [ text description ] ]
            ]

        -- Desktop version (always showing description)
        , div [ class desktopCardClass ]
            [ div [ class desktopIconContainerClass ] [ icon ]
            , h3 [ class desktopTitleClass ] [ text title ]
            , p [ class "hidden md:block mt-2 text-xs sm:text-sm text-gray-600 text-center" ] [ text description ]
            ]
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
    let
        carouselMsg =
            if model.carouselActive then
                Time.every 4000 RotateCarousel

            else
                Sub.none
    in
    Sub.batch
        [ carouselMsg
        , viewingPhone PhoneSectionVisible
        ]


mobileTabButtonClass : ExperienceTab -> ExperienceTab -> String
mobileTabButtonClass activeTab tab =
    let
        baseClass =
            "px-4 py-2 text-sm font-medium rounded-lg transition-colors duration-200"
    in
    if activeTab == tab then
        baseClass ++ " bg-[#03045E] text-white"

    else
        baseClass ++ " bg-gray-100 text-gray-700 hover:bg-gray-200"


tabContainerClass : ExperienceTab -> ExperienceTab -> String
tabContainerClass activeTab tab =
    let
        baseClass =
            "py-2 text-center text-sm font-medium cursor-pointer transition-colors duration-200 border-t-2"
    in
    if activeTab == tab then
        baseClass ++ " bg-white text-gray-900 border-t-2 border-[#03045E]"

    else
        baseClass ++ " bg-gray-100 text-gray-600 border-t-2 border-transparent"
