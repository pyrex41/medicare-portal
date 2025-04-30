module Schedule exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Decode.Pipeline as P
import Json.Encode as E
import List.Extra
import MyIcon
import Svg
import Svg.Attributes as SvgAttr
import Url
import Utils.QuoteHeader exposing (viewHeader)


type EligibilityStatus
    = Accept
    | Decline
    | Generic


type alias OrgInfo =
    { orgName : Maybe String
    , orgLogo : Maybe String
    , orgSlug : String
    , orgPhone : String
    , orgRedirectUrl : Maybe String
    }


type alias AgentInfo =
    { name : String
    , firstName : String
    , phone : String
    }


type alias ContactInfo =
    { email : String
    , firstName : String
    , lastName : String
    , phoneNumber : String
    }


type alias ScheduleInfo =
    { contact : ContactInfo
    , organization : OrgInfo
    , agent : AgentInfo
    , useOrg : Bool
    }


type alias Model =
    { name : Maybe String
    , email : Maybe String
    , phoneNumber : Maybe String
    , isSubmitting : Bool
    , isSubmittingAEP : Bool
    , isSubmittingFollowUp : Bool
    , error : Maybe String
    , success : Bool
    , quoteId : Maybe String
    , key : Nav.Key
    , status : EligibilityStatus
    , redirectUrl : Maybe String
    , scheduleInfo : Maybe ScheduleInfo
    , isLoading : Bool
    , demoMode : Bool
    , demoRedirectUrl : Maybe String
    }


type Msg
    = UpdateName String
    | UpdateEmail String
    | UpdatePhoneNumber String
    | SubmitForm
    | GotSubmitResponse (Result Http.Error SubmitResponse)
    | GotScheduleInfo (Result Http.Error ScheduleInfo)
    | RequestAEP
    | GotAEPResponse (Result Http.Error SubmitResponse)
    | RequestFollowUp
    | GotFollowUpResponse (Result Http.Error SubmitResponse)
    | CalendlyOpened


type alias SubmitResponse =
    { success : Bool
    , message : String
    }


init : Nav.Key -> Maybe String -> Maybe String -> ( Model, Cmd Msg )
init key maybeQuoteId maybeStatus =
    let
        status =
            case maybeStatus of
                Just "accept" ->
                    Accept

                Just "decline" ->
                    Decline

                _ ->
                    Generic

        commands =
            case maybeQuoteId of
                Just quoteId ->
                    [ Http.get
                        { url = "/api/schedule/info/" ++ quoteId
                        , expect = Http.expectJson GotScheduleInfo scheduleInfoDecoder
                        }
                    ]

                Nothing ->
                    []
    in
    ( { name = Nothing
      , email = Nothing
      , phoneNumber = Nothing
      , isSubmitting = False
      , isSubmittingAEP = False
      , isSubmittingFollowUp = False
      , error = Nothing
      , success = False
      , quoteId = maybeQuoteId
      , key = key
      , status = status
      , redirectUrl = Nothing --Just "https://calendly.com/josh-musick-medicaremax/medicare-max-demo?month=2025-04" --"https://calendly.com/medicareschool-max/30min"
      , scheduleInfo = Nothing
      , isLoading = True
      , demoMode = False -- sets CTA to demo mode always
      , demoRedirectUrl = Just "https://calendly.com/josh-musick-medicaremax/medicare-max-demo?month=2025-04" --"https://calendly.com/medicareschool-max/30min"
      }
    , Cmd.batch commands
    )


contactInfoDecoder : D.Decoder ContactInfo
contactInfoDecoder =
    D.map4 ContactInfo
        (D.field "email" D.string)
        (D.field "firstName" D.string)
        (D.field "lastName" D.string)
        (D.field "phoneNumber" D.string)


agentInfoDecoder : D.Decoder AgentInfo
agentInfoDecoder =
    D.map3 AgentInfo
        (D.field "name" D.string)
        (D.field "firstName" D.string)
        (D.field "phone" D.string)


orgInfoDecoder : D.Decoder OrgInfo
orgInfoDecoder =
    D.succeed OrgInfo
        |> P.required "name" (D.nullable D.string)
        |> P.required "logo" (D.nullable D.string)
        |> P.required "slug" D.string
        |> P.required "phone" D.string
        |> P.optional "redirectUrl" (D.nullable D.string) Nothing


scheduleInfoDecoder : D.Decoder ScheduleInfo
scheduleInfoDecoder =
    D.succeed ScheduleInfo
        |> P.required "contact" contactInfoDecoder
        |> P.required "organization" orgInfoDecoder
        |> P.required "agent" agentInfoDecoder
        |> P.optional "useOrg" D.bool True


submitResponseDecoder : D.Decoder SubmitResponse
submitResponseDecoder =
    D.map2 SubmitResponse
        (D.field "success" D.bool)
        (D.field "message" D.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = Just name }, Cmd.none )

        UpdateEmail email ->
            ( { model | email = Just email }, Cmd.none )

        UpdatePhoneNumber phoneNumber ->
            ( { model | phoneNumber = Just (stripPhoneNumber phoneNumber) }, Cmd.none )

        SubmitForm ->
            ( { model | isSubmitting = True }
            , Http.post
                { url = "/api/contact-request"
                , body = Http.jsonBody (encodeForm model)
                , expect = Http.expectJson GotSubmitResponse submitResponseDecoder
                }
            )

        GotSubmitResponse result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | isSubmitting = False, success = True }
                        , Cmd.none
                        )

                    else
                        ( { model | isSubmitting = False, error = Just response.message }
                        , Cmd.none
                        )

                Err _ ->
                    ( { model | isSubmitting = False, error = Just "Failed to submit form. Please try again." }
                    , Cmd.none
                    )

        GotScheduleInfo result ->
            case result of
                Ok info ->
                    let
                        safeRedirectUrl =
                            info.organization.orgRedirectUrl
                                |> Maybe.map
                                    (\url ->
                                        if String.startsWith "http" url then
                                            url

                                        else
                                            "https://" ++ url
                                    )
                    in
                    ( { model
                        | scheduleInfo = Just info
                        , email = Just info.contact.email
                        , name = Just (info.contact.firstName ++ " " ++ info.contact.lastName)
                        , phoneNumber = Just info.contact.phoneNumber
                        , isLoading = False
                        , redirectUrl = safeRedirectUrl
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | isLoading = False }, Cmd.none )

        RequestAEP ->
            case model.quoteId of
                Just quoteId ->
                    ( { model | isSubmittingAEP = True }
                    , Http.post
                        { url = "/api/schedule/aep-request/" ++ quoteId
                        , body = Http.emptyBody
                        , expect = Http.expectJson GotAEPResponse submitResponseDecoder
                        }
                    )

                Nothing ->
                    ( { model | error = Just "Unable to process AEP request" }, Cmd.none )

        GotAEPResponse result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | success = True, isSubmittingAEP = False }, Cmd.none )

                    else
                        ( { model | error = Just response.message, isSubmittingAEP = False }, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Failed to submit AEP request. Please try again.", isSubmittingAEP = False }
                    , Cmd.none
                    )

        RequestFollowUp ->
            case model.quoteId of
                Just quoteId ->
                    ( { model | isSubmittingFollowUp = True }
                    , Http.post
                        { url = "/api/schedule/request-follow-up/" ++ quoteId
                        , body = Http.emptyBody
                        , expect = Http.expectJson GotFollowUpResponse submitResponseDecoder
                        }
                    )

                Nothing ->
                    ( { model | error = Just "Unable to process follow-up request" }, Cmd.none )

        GotFollowUpResponse result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | success = True, isSubmittingFollowUp = False }, Cmd.none )

                    else
                        ( { model | error = Just response.message, isSubmittingFollowUp = False }, Cmd.none )

                Err error ->
                    ( { model | error = Just "Failed to submit follow-up request. Please try again.", isSubmittingFollowUp = False }
                    , Cmd.none
                    )

        CalendlyOpened ->
            ( { model | success = True }, Cmd.none )


stripPhoneNumber : String -> String
stripPhoneNumber phoneNumber =
    String.filter Char.isDigit phoneNumber


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    if String.isEmpty phone then
        ""

    else
        let
            digits =
                String.filter Char.isDigit phone
                    |> String.left 10

            len =
                String.length digits
        in
        if len == 0 then
            ""

        else if len <= 3 then
            "(" ++ digits

        else if len <= 6 then
            "(" ++ String.left 3 digits ++ ") " ++ String.dropLeft 3 digits

        else
            "(" ++ String.left 3 digits ++ ") " ++ String.slice 3 6 digits ++ "-" ++ String.dropLeft 6 digits


encodeForm : Model -> E.Value
encodeForm model =
    E.object
        [ ( "name", model.name |> Maybe.map Url.percentEncode |> Maybe.map E.string |> Maybe.withDefault E.null )
        , ( "email", model.email |> Maybe.map Url.percentEncode |> Maybe.map E.string |> Maybe.withDefault E.null )
        , ( "phoneNumber", model.phoneNumber |> Maybe.map stripPhoneNumber |> Maybe.map E.string |> Maybe.withDefault E.null )
        , ( "type"
          , E.string
                (case model.status of
                    Accept ->
                        "accept"

                    Decline ->
                        "decline"

                    Generic ->
                        "generic"
                )
          )
        , ( "quoteId", Maybe.map E.string model.quoteId |> Maybe.withDefault E.null )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = getTitle model.status
    , body =
        [ if model.isLoading then
            viewLoading

          else
            div [ class "min-h-screen bg-[#F9FAFB]" ]
                [ div [ class "max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-6 sm:py-12" ]
                    [ -- Organization Logo/Name
                      div [ class "flex justify-center items-center mb-8 px-4" ]
                        [ case model.scheduleInfo of
                            Just info ->
                                viewHeader info.organization.orgLogo info.organization.orgName

                            Nothing ->
                                text ""
                        ]
                    , if model.success then
                        div [ class "text-center" ]
                            [ h1 [ class "text-2xl sm:text-3xl font-bold text-gray-900 mb-3 sm:mb-4" ]
                                [ text "Thank You" ]
                            , p [ class "text-gray-600 text-base sm:text-lg" ]
                                [ text "We'll be in touch soon to discuss your options." ]
                            , div [ class "mt-8" ]
                                [ case model.scheduleInfo of
                                    Just info ->
                                        a
                                            [ href ("/self-onboarding/" ++ info.organization.orgSlug)
                                            , class "inline-flex items-center gap-2 text-[#03045E] hover:text-[#0077B6] transition-colors"
                                            ]
                                            [ MyIcon.clipboardPaste 24 "currentColor"
                                            , span [ class "font-medium" ] [ text "Help someone else get started" ]
                                            ]

                                    Nothing ->
                                        text ""
                                ]
                            ]

                      else if model.demoMode then
                        viewDemoCTA model

                      else
                        viewCTA model
                    ]
                ]
        ]
    }


viewDemoCTA : Model -> Html Msg
viewDemoCTA model =
    div [ class "flex flex-col max-w-xl mx-auto" ]
        [ div [ class "border border-[#DCE2E5] shadow-sm overflow-hidden rounded-lg" ]
            [ div [ class "bg-[#F9F5FF] p-6" ]
                [ h1 [ class "text-2xl sm:text-3xl font-extrabold text-black mb-4" ]
                    [ text "Let's Connect" ]
                , p [ class "text-black text-base leading-relaxed" ]
                    [ text "Interested in learning how you can use Medicare Max for your clients? Discover how our platform can help your agency maximize client retention, freeing up time to focus on what matters most." ]
                ]
            , div [ class "bg-white p-6 sm:p-8" ]
                [ p [ class "text-[#475467] text-sm mb-6" ]
                    [ text "Book a call with us to learn more" ]
                , case model.error of
                    Just error ->
                        div [ class "bg-red-50 border border-red-400 text-red-700 px-4 py-3 rounded mb-6 text-base" ]
                            [ text error ]

                    Nothing ->
                        text ""
                , div [ class "space-y-4" ]
                    [ a
                        [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                        , href (makeDemoCalendlyUrl model)
                        , target "_blank"
                        , onClick CalendlyOpened
                        ]
                        [ div [ class "flex items-center space-x-3" ]
                            [ span [ class "w-6 h-6 flex items-center justify-center" ]
                                [ MyIcon.calendarDays 24 "#03045E" ]
                            , span [ class "font-semibold text-base" ]
                                [ text "Schedule a Demo Call" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewCTA : Model -> Html Msg
viewCTA model =
    div [ class "flex flex-col max-w-xl mx-auto" ]
        [ div [ class "border border-[#DCE2E5] shadow-sm overflow-hidden rounded-lg" ]
            [ div [ class "bg-[#F9F5FF] p-6" ]
                [ h1 [ class "text-2xl sm:text-3xl font-extrabold text-black mb-4" ]
                    [ text (getHeading model.status) ]
                , p [ class "text-black text-base leading-relaxed" ]
                    [ text (getMessage model.status) ]
                ]
            , div [ class "bg-white p-6 sm:p-8" ]
                [ p [ class "text-[#667085] text-sm mb-6" ]
                    [ text "Select an Option Below" ]
                , case model.error of
                    Just error ->
                        div [ class "bg-red-50 border border-red-400 text-red-700 px-4 py-3 rounded mb-6 text-base" ]
                            [ text error ]

                    Nothing ->
                        text ""
                , case model.scheduleInfo of
                    Just info ->
                        case model.status of
                            Accept ->
                                viewAcceptButtons model info

                            Decline ->
                                viewDeclineButtons model info

                            Generic ->
                                viewGenericButtons model info

                    Nothing ->
                        text ""
                ]
            ]
        ]


viewAcceptButtons : Model -> ScheduleInfo -> Html Msg
viewAcceptButtons model info =
    if info.useOrg then
        viewAcceptButtonsOrg model info

    else
        viewAcceptButtonsAgent model info


viewAcceptButtonsAgent : Model -> ScheduleInfo -> Html Msg
viewAcceptButtonsAgent model info =
    div [ class "space-y-4" ]
        [ case model.redirectUrl of
            Just _ ->
                a
                    [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                    , href (makeCalendlyUrl model)
                    , target "_blank"
                    , onClick CalendlyOpened
                    ]
                    [ div [ class "flex items-center space-x-3" ]
                        [ span [ class "w-6 h-6 flex items-center justify-center" ]
                            [ MyIcon.calendarDays 24 "#03045E" ]
                        , span [ class "font-semibold text-base" ]
                            [ text ("Schedule a Call with " ++ info.agent.firstName) ]
                        ]
                    ]

            Nothing ->
                if model.isSubmittingFollowUp then
                    button
                        [ class "flex items-center justify-center w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] transition cursor-wait"
                        , type_ "button"
                        ]
                        [ div [ class "animate-spin rounded-full h-5 w-5 border-2 border-[#03045E] border-t-transparent" ] [] ]

                else
                    button
                        [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                        , type_ "button"
                        , onClick RequestFollowUp
                        ]
                        [ div [ class "flex items-center space-x-3" ]
                            [ span [ class "w-6 h-6 flex items-center justify-center" ]
                                [ MyIcon.phoneIncoming 24 "#03045E" ]
                            , span [ class "font-semibold text-base" ]
                                [ text ("Request a Call from " ++ info.agent.firstName) ]
                            ]
                        ]
        , if not (String.isEmpty info.agent.phone) then
            a
                [ href ("tel:" ++ info.agent.phone)
                , class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                ]
                [ div [ class "flex items-center space-x-3" ]
                    [ span [ class "w-6 h-6 flex items-center justify-center" ]
                        [ MyIcon.phoneOutgoing 24 "#03045E" ]
                    , span [ class "font-semibold text-base" ]
                        [ text ("Give " ++ info.agent.firstName ++ " Call: " ++ formatPhoneNumber info.agent.phone) ]
                    ]
                ]

          else
            text ""
        ]


viewAcceptButtonsOrg : Model -> ScheduleInfo -> Html Msg
viewAcceptButtonsOrg model info =
    div [ class "space-y-4" ]
        [ case info.organization.orgRedirectUrl of
            Just _ ->
                a
                    [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                    , href (makeCalendlyUrl model)
                    , target "_blank"
                    , onClick CalendlyOpened
                    ]
                    [ div [ class "flex items-center space-x-3" ]
                        [ span [ class "w-6 h-6 flex items-center justify-center" ]
                            [ MyIcon.calendarDays 24 "#03045E" ]
                        , span [ class "font-semibold text-base" ]
                            [ text "Schedule a Call" ]
                        ]
                    ]

            Nothing ->
                if model.isSubmittingFollowUp then
                    button
                        [ class "flex items-center justify-center w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] transition cursor-wait"
                        , type_ "button"
                        ]
                        [ div [ class "animate-spin rounded-full h-5 w-5 border-2 border-[#03045E] border-t-transparent" ] [] ]

                else
                    button
                        [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                        , type_ "button"
                        , onClick RequestFollowUp
                        ]
                        [ div [ class "flex items-center space-x-3" ]
                            [ span [ class "w-6 h-6 flex items-center justify-center" ]
                                [ MyIcon.phoneIncoming 24 "#03045E" ]
                            , span [ class "font-semibold text-base" ]
                                [ text "Request a Call" ]
                            ]
                        ]
        , if not (String.isEmpty info.organization.orgPhone) then
            a
                [ href ("tel:" ++ info.organization.orgPhone)
                , class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                ]
                [ div [ class "flex items-center space-x-3" ]
                    [ span [ class "w-6 h-6 flex items-center justify-center" ]
                        [ MyIcon.phoneOutgoing 24 "#03045E" ]
                    , span [ class "font-semibold text-base" ]
                        [ text ("Call Now: " ++ formatPhoneNumber info.organization.orgPhone) ]
                    ]
                ]

          else
            text ""
        ]


viewDeclineButtons : Model -> ScheduleInfo -> Html Msg
viewDeclineButtons model info =
    if info.useOrg then
        viewDeclineButtonsOrg model info

    else
        viewDeclineButtonsAgent model info


viewDeclineButtonsAgent : Model -> ScheduleInfo -> Html Msg
viewDeclineButtonsAgent model info =
    div [ class "space-y-4" ]
        [ a
            [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
            , href (makeCalendlyUrl model)
            , target "_blank"
            , onClick CalendlyOpened
            ]
            [ div [ class "flex items-center space-x-3" ]
                [ span [ class "w-6 h-6 flex items-center justify-center" ]
                    [ MyIcon.calendarDays 24 "#03045E" ]
                , span [ class "font-semibold text-base" ]
                    [ text ("Schedule a Call with " ++ info.agent.firstName) ]
                ]
            ]
        , if not (String.isEmpty info.agent.phone) then
            a
                [ href ("tel:" ++ info.agent.phone)
                , class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                ]
                [ div [ class "flex items-center space-x-3" ]
                    [ span [ class "w-6 h-6 flex items-center justify-center" ]
                        [ MyIcon.phoneOutgoing 24 "#03045E" ]
                    , span [ class "font-semibold text-base" ]
                        [ text ("Give " ++ info.agent.firstName ++ " a call: " ++ formatPhoneNumber info.agent.phone) ]
                    ]
                ]

          else
            text ""
        , div [ class "mt-8 mb-4" ]
            [ h3 [ class "text-[#03045E] font-bold text-base mb-3" ]
                [ text "Interested in an Advantage Plan?" ]
            , p [ class "text-[#03045E] text-sm mb-6 leading-relaxed" ]
                [ text "We can switch you to an Advantage Plan during the Annual Enrollment Period (Oct. 15 - Dec. 7) - Click below and we will be sure to reach out at the end of September to to begin the Advantage Plan Process." ]
            , if model.isSubmittingAEP then
                button
                    [ class "flex items-center justify-center w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] transition cursor-wait"
                    , type_ "button"
                    ]
                    [ div [ class "animate-spin rounded-full h-5 w-5 border-2 border-[#03045E] border-t-transparent" ] [] ]

              else
                button
                    [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                    , type_ "button"
                    , onClick RequestAEP
                    ]
                    [ div [ class "flex items-center space-x-3" ]
                        [ span [ class "w-6 h-6 flex items-center justify-center" ]
                            [ MyIcon.clipboardPlus 24 "#03045E" ]
                        , span [ class "font-semibold text-base" ]
                            [ text "Get on the Advantage Plan List" ]
                        ]
                    ]
            ]
        ]


viewDeclineButtonsOrg : Model -> ScheduleInfo -> Html Msg
viewDeclineButtonsOrg model info =
    div [ class "space-y-4" ]
        [ case info.organization.orgRedirectUrl of
            Just _ ->
                a
                    [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                    , href (makeCalendlyUrl model)
                    , target "_blank"
                    , onClick CalendlyOpened
                    ]
                    [ div [ class "flex items-center space-x-3" ]
                        [ span [ class "w-6 h-6 flex items-center justify-center" ]
                            [ MyIcon.calendarDays 24 "#03045E" ]
                        , span [ class "font-semibold text-base" ]
                            [ text "Schedule a Call" ]
                        ]
                    ]

            Nothing ->
                text ""
        , if model.isSubmittingAEP then
            button
                [ class "flex items-center justify-center w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] transition cursor-wait"
                , type_ "button"
                ]
                [ div [ class "animate-spin rounded-full h-5 w-5 border-2 border-[#03045E] border-t-transparent" ] [] ]

          else
            button
                [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                , type_ "button"
                , onClick RequestAEP
                ]
                [ div [ class "flex items-center space-x-3" ]
                    [ span [ class "w-6 h-6 flex items-center justify-center" ]
                        [ MyIcon.clipboardPlus 24 "#03045E" ]
                    , span [ class "font-semibold text-base" ]
                        [ text "Get on the Advantage Plan List" ]
                    ]
                ]
        , if not (String.isEmpty info.organization.orgPhone) then
            a
                [ href ("tel:" ++ info.organization.orgPhone)
                , class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                ]
                [ div [ class "flex items-center space-x-3" ]
                    [ span [ class "w-6 h-6 flex items-center justify-center" ]
                        [ MyIcon.phoneOutgoing 24 "#03045E" ]
                    , span [ class "font-semibold text-base" ]
                        [ text ("Call Now: " ++ formatPhoneNumber info.organization.orgPhone) ]
                    ]
                ]

          else
            text ""
        , div [ class "mt-8 mb-4" ]
            [ h3 [ class "text-[#03045E] font-bold text-base mb-3" ]
                [ text "Interested in an Advantage Plan?" ]
            , p [ class "text-[#03045E] text-sm mb-6 leading-relaxed" ]
                [ text "We can switch you to an Advantage Plan during the Annual Enrollment Period (Oct. 15 - Dec. 7) - Click below and we will be sure to reach out at the end of September to to begin the Advantage Plan Process." ]
            , if model.isSubmittingAEP then
                button
                    [ class "flex items-center justify-center w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] transition cursor-wait"
                    , type_ "button"
                    ]
                    [ div [ class "animate-spin rounded-full h-5 w-5 border-2 border-[#03045E] border-t-transparent" ] [] ]

              else
                button
                    [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                    , type_ "button"
                    , onClick RequestAEP
                    ]
                    [ div [ class "flex items-center space-x-3" ]
                        [ span [ class "w-6 h-6 flex items-center justify-center" ]
                            [ MyIcon.clipboardPlus 24 "#03045E" ]
                        , span [ class "font-semibold text-base" ]
                            [ text "Get on the Advantage Plan List" ]
                        ]
                    ]
            ]
        ]


viewGenericButtons : Model -> ScheduleInfo -> Html Msg
viewGenericButtons model info =
    if info.useOrg then
        viewGenericButtonsOrg model info

    else
        viewGenericButtonsAgent model info


viewGenericButtonsAgent : Model -> ScheduleInfo -> Html Msg
viewGenericButtonsAgent model info =
    div [ class "space-y-4" ]
        [ case model.redirectUrl of
            Just _ ->
                a
                    [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                    , href (makeCalendlyUrl model)
                    , target "_blank"
                    , onClick CalendlyOpened
                    ]
                    [ div [ class "flex items-center space-x-3" ]
                        [ span [ class "w-6 h-6 flex items-center justify-center" ]
                            [ MyIcon.calendarDays 24 "#03045E" ]
                        , span [ class "font-semibold text-base" ]
                            [ text ("Schedule a Call with " ++ info.agent.firstName) ]
                        ]
                    ]

            Nothing ->
                if model.isSubmittingFollowUp then
                    button
                        [ class "flex items-center justify-center w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] transition cursor-wait"
                        , type_ "button"
                        ]
                        [ div [ class "animate-spin rounded-full h-5 w-5 border-2 border-[#03045E] border-t-transparent" ] [] ]

                else
                    button
                        [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                        , type_ "button"
                        , onClick RequestFollowUp
                        ]
                        [ div [ class "flex items-center space-x-3" ]
                            [ span [ class "w-6 h-6 flex items-center justify-center" ]
                                [ MyIcon.phoneIncoming 24 "#03045E" ]
                            , span [ class "font-semibold text-base" ]
                                [ text ("Request a Call from " ++ info.agent.firstName) ]
                            ]
                        ]
        , if not (String.isEmpty info.agent.phone) then
            a
                [ href ("tel:" ++ info.agent.phone)
                , class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                ]
                [ div [ class "flex items-center space-x-3" ]
                    [ span [ class "w-6 h-6 flex items-center justify-center" ]
                        [ MyIcon.phoneOutgoing 24 "#03045E" ]
                    , span [ class "font-semibold text-base" ]
                        [ text ("Give " ++ info.agent.firstName ++ " a call: " ++ formatPhoneNumber info.agent.phone) ]
                    ]
                ]

          else
            text ""
        ]


viewGenericButtonsOrg : Model -> ScheduleInfo -> Html Msg
viewGenericButtonsOrg model info =
    div [ class "space-y-4" ]
        [ case info.organization.orgRedirectUrl of
            Just _ ->
                a
                    [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                    , href (makeCalendlyUrl model)
                    , target "_blank"
                    , onClick CalendlyOpened
                    ]
                    [ div [ class "flex items-center space-x-3" ]
                        [ span [ class "w-6 h-6 flex items-center justify-center" ]
                            [ MyIcon.calendarDays 24 "#03045E" ]
                        , span [ class "font-semibold text-base" ]
                            [ text "Schedule a Call" ]
                        ]
                    ]

            Nothing ->
                if model.isSubmittingFollowUp then
                    button
                        [ class "flex items-center justify-center w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] transition cursor-wait"
                        , type_ "button"
                        ]
                        [ div [ class "animate-spin rounded-full h-5 w-5 border-2 border-[#03045E] border-t-transparent" ] [] ]

                else
                    button
                        [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                        , type_ "button"
                        , onClick RequestFollowUp
                        ]
                        [ div [ class "flex items-center space-x-3" ]
                            [ span [ class "w-6 h-6 flex items-center justify-center" ]
                                [ MyIcon.phoneIncoming 24 "#03045E" ]
                            , span [ class "font-semibold text-base" ]
                                [ text "Request a Call" ]
                            ]
                        ]
        , if not (String.isEmpty info.organization.orgPhone) then
            a
                [ href ("tel:" ++ info.organization.orgPhone)
                , class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                ]
                [ div [ class "flex items-center space-x-3" ]
                    [ span [ class "w-6 h-6 flex items-center justify-center" ]
                        [ MyIcon.phoneOutgoing 24 "#03045E" ]
                    , span [ class "font-semibold text-base" ]
                        [ text ("Call Now: " ++ formatPhoneNumber info.organization.orgPhone) ]
                    ]
                ]

          else
            text ""
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "fixed inset-0 bg-white flex flex-col items-center justify-center gap-4 text-center" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-[#03045E] border-t-transparent" ] []
        , p [ class "text-center text-lg font-medium text-gray-600" ]
            [ text "Loading your schedule page..." ]
        ]


makeDemoCalendlyUrl : Model -> String
makeDemoCalendlyUrl model =
    makeCalendlyUrlHelper model model.demoRedirectUrl


makeCalendlyUrl : Model -> String
makeCalendlyUrl model =
    makeCalendlyUrlHelper model model.redirectUrl


makeCalendlyUrlHelper : Model -> Maybe String -> String
makeCalendlyUrlHelper model redirectUrl =
    case redirectUrl of
        Just url ->
            List.Extra.zip
                [ "email", "name", "location" ]
                [ model.email
                , model.name
                , model.phoneNumber
                ]
                |> List.filterMap
                    (\( key, value ) ->
                        case value of
                            Just s ->
                                case key of
                                    "location" ->
                                        Just (key ++ "=1" ++ Url.percentEncode s)

                                    _ ->
                                        Just (key ++ "=" ++ Url.percentEncode s)

                            Nothing ->
                                Nothing
                    )
                |> String.join "&"
                |> (\s -> url ++ "?" ++ s)

        Nothing ->
            "#"


getTitle : EligibilityStatus -> String
getTitle status =
    case status of
        Accept ->
            "Good News! - Medicare Max"

        Decline ->
            "Not Eligible - Medicare Max"

        Generic ->
            "Schedule Follow-up - Medicare Max"


getHeading : EligibilityStatus -> String
getHeading status =
    case status of
        Accept ->
            "Great News..."

        Decline ->
            "Here are some options..."

        Generic ->
            "Let's Connect"


getMessage : EligibilityStatus -> String
getMessage status =
    case status of
        Accept ->
            "Based on your answers, you look like a good candidate to switch plans. Let's schedule a follow-up to discuss your options or jump on a call now."

        Decline ->
            "Based on your answers, a Medicare Advantage Plan may be a better fit for you. We'd love to help you explore your options and find a plan that's a perfect fit for your needs."

        Generic ->
            "Let's schedule a follow-up call to discuss your Medicare options and find the best plan for your needs."


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
