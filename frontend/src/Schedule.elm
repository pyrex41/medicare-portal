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
    , useOrgSenderDetails : Bool
    , bookingLink : String
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
    , forceOrgSenderDetails : Bool
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
    , forceOrgSenderDetails : Bool
    }


type Msg
    = GotScheduleInfo (Result Http.Error ScheduleInfo)
    | CalendlyOpened


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
      , forceOrgSenderDetails = False
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
    D.succeed AgentInfo
        |> P.required "name" D.string
        |> P.required "firstName" D.string
        |> P.required "phone" D.string
        |> P.optional "use_org_sender_details" D.bool True
        |> P.optional "booking_link" D.string ""


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
        |> P.optional "force_org_sender_details" D.bool False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

                        -- Determine effective redirect URL based on sender settings
                        effectiveRedirectUrl =
                            if info.forceOrgSenderDetails then
                                safeRedirectUrl

                            else if info.agent.useOrgSenderDetails then
                                safeRedirectUrl

                            else if String.isEmpty info.agent.bookingLink then
                                -- When using agent settings but no booking link, don't show calendar button
                                Nothing

                            else
                                Just
                                    (if String.startsWith "http" info.agent.bookingLink then
                                        info.agent.bookingLink

                                     else
                                        "https://" ++ info.agent.bookingLink
                                    )
                    in
                    ( { model
                        | scheduleInfo = Just info
                        , email = Just info.contact.email
                        , name = Just (info.contact.firstName ++ " " ++ info.contact.lastName)
                        , phoneNumber = Just info.contact.phoneNumber
                        , isLoading = False
                        , redirectUrl = effectiveRedirectUrl
                        , forceOrgSenderDetails = info.forceOrgSenderDetails
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | isLoading = False }, Cmd.none )

        CalendlyOpened ->
            ( { model | success = True }, Cmd.none )


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

                      else
                        viewCTA model
                    ]
                ]
        ]
    }


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
                        viewGenericButtons model info

                    Nothing ->
                        text ""
                ]
            ]
        ]


viewGenericButtons : Model -> ScheduleInfo -> Html Msg
viewGenericButtons model info =
    let
        -- Determine whether to use org or agent details
        effectiveUseOrg =
            if model.forceOrgSenderDetails then
                True

            else
                info.agent.useOrgSenderDetails
    in
    if effectiveUseOrg then
        viewGenericButtonsOrg model info

    else
        viewGenericButtonsAgent model info


viewGenericButtonsAgent : Model -> ScheduleInfo -> Html Msg
viewGenericButtonsAgent model info =
    let
        -- Use agent phone if available, otherwise fall back to org phone
        effectivePhone =
            if String.isEmpty info.agent.phone then
                info.organization.orgPhone

            else
                info.agent.phone

        -- Determine the display name for calls
        phoneDisplayName =
            if String.isEmpty info.agent.phone then
                -- Using org phone, so don't personalize with agent name
                ""

            else
                info.agent.firstName ++ " "
    in
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
                text ""
        , if not (String.isEmpty effectivePhone) then
            a
                [ href ("tel:" ++ effectivePhone)
                , class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition"
                ]
                [ div [ class "flex items-center space-x-3" ]
                    [ span [ class "w-6 h-6 flex items-center justify-center" ]
                        [ MyIcon.phoneOutgoing 24 "#03045E" ]
                    , span [ class "font-semibold text-base" ]
                        [ text
                            (if String.isEmpty phoneDisplayName then
                                "Call Now: " ++ formatPhoneNumber effectivePhone

                             else
                                "Give " ++ phoneDisplayName ++ "a call: " ++ formatPhoneNumber effectivePhone
                            )
                        ]
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
                text ""
        , a
            ((if not (String.isEmpty info.organization.orgPhone) then
                [ href ("tel:" ++ info.organization.orgPhone) ]

              else
                []
             )
                ++ [ class "flex items-center justify-between w-full px-4 py-4 border border-[#03045E] rounded-md text-[#03045E] hover:bg-gray-50 transition" ]
            )
            [ div [ class "flex items-center space-x-3" ]
                [ span [ class "w-6 h-6 flex items-center justify-center" ]
                    [ MyIcon.phoneOutgoing 24 "#03045E" ]
                , span [ class "font-semibold text-base" ]
                    [ if not (String.isEmpty info.organization.orgPhone) then
                        text ("Call Now: " ++ formatPhoneNumber info.organization.orgPhone)

                      else
                        text "Give us a call!"
                    ]
                ]
            ]
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "fixed inset-0 bg-white flex flex-col items-center justify-center gap-4 text-center" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-[#03045E] border-t-transparent" ] []
        , p [ class "text-center text-lg font-medium text-gray-600" ]
            [ text "Loading your schedule page..." ]
        ]


makeCalendlyUrl : Model -> String
makeCalendlyUrl model =
    makeCalendlyUrlHelper model model.redirectUrl


makeCalendlyUrlHelper : Model -> Maybe String -> String
makeCalendlyUrlHelper model redirectUrl =
    let
        -- Determine which redirect URL to use based on sender settings
        effectiveRedirectUrl =
            case ( model.scheduleInfo, redirectUrl ) of
                ( Just info, _ ) ->
                    let
                        effectiveUseOrg =
                            if model.forceOrgSenderDetails then
                                True

                            else
                                info.agent.useOrgSenderDetails
                    in
                    if effectiveUseOrg then
                        info.organization.orgRedirectUrl

                    else if String.isEmpty info.agent.bookingLink then
                        -- When using agent settings but no booking link, don't show calendar button
                        Nothing

                    else
                        Just info.agent.bookingLink

                ( Nothing, _ ) ->
                    redirectUrl
    in
    case effectiveRedirectUrl of
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
            "Medicare Advantage Follow-up - Medicare Max"

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
