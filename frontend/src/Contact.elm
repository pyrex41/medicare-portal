module Contact exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import EmailScheduler exposing (EmailSchedule, PlanType(..), ScheduledEmail, getScheduledEmails, init, viewFutureActivity)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Process
import Task
import Time exposing (Month(..), Posix, Zone)



-- TYPES


type alias Contact =
    { id : Int
    , firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , state : String
    , contactOwnerId : Maybe Int
    , contactOwner : Maybe User
    , currentCarrier : String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : Bool
    , gender : String
    , zipCode : String
    , planType : String
    , status : String
    , agentId : Maybe Int
    , lastEmailed : Maybe String
    }


type alias User =
    { id : Int
    , email : String
    , firstName : String
    , lastName : String
    , isAdmin : Bool
    , isAgent : Bool
    , organizationId : Int
    , isActive : Bool
    , phone : String
    }


type alias ContactForm =
    { id : Maybe Int
    , firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , state : String
    , contactOwnerId : Maybe Int
    , currentCarrier : String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : Bool
    , gender : String
    , zipCode : String
    , planType : String
    }


emptyForm : ContactForm
emptyForm =
    { id = Nothing
    , firstName = ""
    , lastName = ""
    , email = ""
    , phoneNumber = ""
    , state = ""
    , contactOwnerId = Nothing
    , currentCarrier = ""
    , effectiveDate = ""
    , birthDate = ""
    , tobaccoUser = False
    , gender = "M"
    , zipCode = ""
    , planType = ""
    }



-- MODEL


type alias Activity =
    { submissionDate : String
    , status : ActivityStatus
    , carrierSelected : Maybe String
    , planSelected : Maybe String
    , quoteAmount : Maybe Float
    }


type ActivityStatus
    = QuoteCreated
    | EmailOpened
    | EmailSent Int -- Int represents which email number (1, 2, etc.)


type Modal
    = NoModal
    | EditModal
    | DeleteConfirmModal


type alias Model =
    { key : Nav.Key
    , contact : Maybe Contact
    , showModal : Modal
    , editForm : ContactForm
    , isSubmittingForm : Bool
    , error : Maybe String
    , activities : List Activity
    , isCheckingEmail : Bool
    , emailExists : Bool
    , isDeletingContact : Bool
    , emailSchedule : EmailSchedule
    , quoteUrl : Maybe String
    , isGeneratingQuote : Bool
    , healthStatus : Maybe HealthStatus
    , followUps : List FollowUpRequest
    , timeZone : Zone
    , showAllFollowUps : Bool
    , orgSettings : Maybe Settings
    , emailSendSuccess : Bool
    }


type alias HealthStatus =
    { status : String
    , answers : Maybe (Dict String Bool)
    }


type alias FollowUpRequest =
    { type_ : String
    , quoteId : String
    , createdAt : Posix
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



-- DECODERS


contactDecoder : Decoder Contact
contactDecoder =
    Decode.succeed Contact
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "first_name" Decode.string
        |> Pipeline.required "last_name" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.optional "phone_number" Decode.string ""
        |> Pipeline.required "state" Decode.string
        |> Pipeline.optional "contact_owner_id" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "contact_owner" (Decode.nullable userDecoder) Nothing
        |> Pipeline.required "current_carrier" Decode.string
        |> Pipeline.required "effective_date" Decode.string
        |> Pipeline.required "birth_date" Decode.string
        |> Pipeline.required "tobacco_user" Decode.bool
        |> Pipeline.required "gender" Decode.string
        |> Pipeline.required "zip_code" Decode.string
        |> Pipeline.required "plan_type" Decode.string
        |> Pipeline.optional "status" Decode.string "New"
        |> Pipeline.required "agent_id" (Decode.nullable Decode.int)
        |> Pipeline.optional "last_emailed_date" (Decode.nullable Decode.string) Nothing


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "first_name" Decode.string
        |> Pipeline.required "last_name" Decode.string
        |> Pipeline.required "is_admin" Decode.bool
        |> Pipeline.required "is_agent" Decode.bool
        |> Pipeline.required "organization_id" Decode.int
        |> Pipeline.required "is_active" Decode.bool
        |> Pipeline.required "phone" Decode.string


healthStatusDecoder : Decoder HealthStatus
healthStatusDecoder =
    Decode.map2 HealthStatus
        (Decode.field "status" Decode.string)
        (Decode.field "answers" (Decode.nullable (Decode.dict Decode.bool)))


settingsDecoder : Decoder Settings
settingsDecoder =
    Decode.field "success" Decode.bool
        |> Decode.andThen
            (\success ->
                if success then
                    Decode.field "orgSettings" settingsObjectDecoder

                else
                    Decode.fail "Settings request was not successful"
            )


settingsObjectDecoder : Decoder Settings
settingsObjectDecoder =
    Decode.map8 Settings
        (Decode.field "stateLicenses" (Decode.list Decode.string))
        (Decode.field "carrierContracts" (Decode.list Decode.string))
        (Decode.field "stateCarrierSettings" (Decode.list stateCarrierSettingDecoder))
        (Decode.field "allowAgentSettings" Decode.bool)
        (Decode.field "emailSendBirthday" Decode.bool)
        (Decode.field "emailSendPolicyAnniversary" Decode.bool)
        (Decode.field "emailSendAep" Decode.bool)
        (Decode.field "smartSendEnabled" Decode.bool)


stateCarrierSettingDecoder : Decoder StateCarrierSetting
stateCarrierSettingDecoder =
    Decode.map4 StateCarrierSetting
        (Decode.field "state" Decode.string)
        (Decode.field "carrier" Decode.string)
        (Decode.field "active" Decode.bool)
        (Decode.field "targetGI" Decode.bool)



-- INIT


type alias ZipInfo =
    { state : String
    , counties : List String
    , cities : List String
    }


init : Nav.Key -> String -> ( Model, Cmd Msg )
init key contactId =
    let
        initialSchedule =
            EmailScheduler.init
                (String.toInt contactId |> Maybe.withDefault 0)
                (Date.fromCalendarDate 2024 Jan 1)
                (Date.fromCalendarDate 2024 Jan 1)
                (Date.fromCalendarDate 2024 Jan 1)
                NoPlan
                ""
                -- Empty initial state
                []
                -- Empty initial state carrier settings
                []

        -- Empty initial state licenses
    in
    ( { key = key
      , contact = Nothing
      , showModal = NoModal
      , editForm = emptyForm
      , isSubmittingForm = False
      , error = Nothing
      , activities = []
      , isCheckingEmail = False
      , emailExists = False
      , isDeletingContact = False
      , emailSchedule = initialSchedule
      , quoteUrl = Nothing
      , isGeneratingQuote = False
      , healthStatus = Nothing
      , followUps = []
      , timeZone = Time.utc
      , showAllFollowUps = False
      , orgSettings = Nothing
      , emailSendSuccess = False
      }
    , Cmd.batch
        [ Http.get
            { url = "/api/contacts/" ++ contactId
            , expect = Http.expectJson GotContact contactDecoder
            }
        , Http.get
            { url = "/api/contacts/" ++ contactId ++ "/eligibility"
            , expect = Http.expectJson GotHealthStatus healthStatusDecoder
            }
        , Http.get
            { url = "/api/contacts/" ++ contactId ++ "/follow-ups"
            , expect = Http.expectJson GotFollowUps followUpsDecoder
            }
        , Task.perform GotCurrentTime Date.today
        , Task.perform GotTimeZone Time.here
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | GotContact (Result Http.Error Contact)
    | GotCurrentTime Date
    | GotTimeZone Zone
    | ShowEditModal
    | CloseModal
    | BackToContacts
    | UpdateEditForm ContactFormField String
    | SubmitEditForm
    | ContactUpdated (Result Http.Error Contact)
    | CheckEmail String
    | EmailChecked (Result Http.Error { exists : Bool })
    | LookupZipCode String
    | GotZipLookup (Result Http.Error ZipInfo)
    | ShowDeleteConfirmModal
    | DeleteContact
    | ContactDeleted (Result Http.Error DeleteResponse)
    | GenerateQuoteLink
    | GotQuoteLink (Result Http.Error { quoteId : String, redirectUrl : String })
    | GotHealthStatus (Result Http.Error HealthStatus)
    | GotFollowUps (Result Http.Error (List FollowUpRequest))
    | ToggleFollowUps
    | GotOrgSettings (Result Http.Error Settings)
    | SendQuoteEmail
    | QuoteEmailSent (Result Http.Error { success : Bool, message : String })
    | ResetEmailSendState


type ContactFormField
    = FirstName
    | LastName
    | Email
    | PhoneNumber
    | State
    | ContactOwnerId
    | CurrentCarrier
    | EffectiveDate
    | BirthDate
    | TobaccoUser
    | Gender
    | ZipCode
    | PlanType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotContact (Ok contact) ->
            case Date.fromIsoString contact.effectiveDate of
                Ok effectiveDate ->
                    case Date.fromIsoString contact.birthDate of
                        Ok birthDate ->
                            let
                                planType =
                                    case contact.planType of
                                        "Plan N" ->
                                            PlanN

                                        "N" ->
                                            PlanN

                                        "Plan G" ->
                                            PlanG

                                        "G" ->
                                            PlanG

                                        _ ->
                                            NoPlan

                                ( stateCarrierSettings, stateLicenses ) =
                                    case model.orgSettings of
                                        Just settings ->
                                            ( settings.stateCarrierSettings, settings.stateLicenses )

                                        Nothing ->
                                            ( [], [] )

                                newSchedule =
                                    EmailScheduler.init
                                        contact.id
                                        effectiveDate
                                        birthDate
                                        model.emailSchedule.currentDate
                                        planType
                                        contact.state
                                        stateCarrierSettings
                                        stateLicenses
                            in
                            ( { model | contact = Just contact, emailSchedule = newSchedule }
                            , Cmd.batch
                                [ Http.get
                                    { url = "/api/quotes/generate/" ++ String.fromInt contact.id
                                    , expect = Http.expectJson GotQuoteLink quoteLinkDecoder
                                    }
                                , Http.get
                                    { url = "/api/settings"
                                    , expect = Http.expectJson GotOrgSettings settingsDecoder
                                    }
                                ]
                            )

                        Err _ ->
                            ( { model | error = Just "Invalid birth date format" }, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Invalid effective date format" }, Cmd.none )

        GotContact (Err _) ->
            ( { model | error = Just "Failed to load contact" }, Cmd.none )

        GotCurrentTime today ->
            let
                currentSchedule =
                    model.emailSchedule

                newSchedule =
                    { currentSchedule | currentDate = today }
            in
            ( { model | emailSchedule = newSchedule }
            , Cmd.none
            )

        GotTimeZone zone ->
            ( { model | timeZone = zone }, Cmd.none )

        ShowEditModal ->
            case model.contact of
                Just contact ->
                    ( { model
                        | showModal = EditModal
                        , editForm =
                            { id = Just contact.id
                            , firstName = contact.firstName
                            , lastName = contact.lastName
                            , email = contact.email
                            , phoneNumber = contact.phoneNumber
                            , state = contact.state
                            , contactOwnerId = contact.contactOwnerId
                            , currentCarrier = contact.currentCarrier
                            , effectiveDate = contact.effectiveDate
                            , birthDate = contact.birthDate
                            , tobaccoUser = contact.tobaccoUser
                            , gender = contact.gender
                            , zipCode = contact.zipCode
                            , planType = contact.planType
                            }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        CloseModal ->
            ( { model | showModal = NoModal }, Cmd.none )

        BackToContacts ->
            ( model, Nav.pushUrl model.key "/contacts" )

        UpdateEditForm field value ->
            let
                form =
                    model.editForm

                updatedForm =
                    case field of
                        FirstName ->
                            { form | firstName = value }

                        LastName ->
                            { form | lastName = value }

                        Email ->
                            { form | email = value }

                        PhoneNumber ->
                            { form | phoneNumber = String.filter Char.isDigit value |> String.left 10 }

                        State ->
                            { form | state = value }

                        ContactOwnerId ->
                            { form | contactOwnerId = String.toInt value }

                        CurrentCarrier ->
                            { form | currentCarrier = value }

                        EffectiveDate ->
                            { form | effectiveDate = value }

                        BirthDate ->
                            { form | birthDate = value }

                        TobaccoUser ->
                            { form | tobaccoUser = value == "true" }

                        Gender ->
                            { form | gender = value }

                        ZipCode ->
                            { form | zipCode = value }

                        PlanType ->
                            { form | planType = value }

                cmd =
                    if field == ZipCode && String.length value == 5 then
                        LookupZipCode value
                            |> Task.succeed
                            |> Task.perform identity

                    else if field == Email && String.length value > 0 then
                        checkEmail value

                    else
                        Cmd.none
            in
            ( { model
                | editForm = updatedForm
                , isCheckingEmail = field == Email && String.length value > 0
                , emailExists = False
                , error = Nothing
              }
            , cmd
            )

        SubmitEditForm ->
            case model.editForm.id of
                Just id ->
                    ( { model | isSubmittingForm = True }
                    , Http.request
                        { method = "PUT"
                        , headers = []
                        , url = "/api/contacts/" ++ String.fromInt id
                        , body = Http.jsonBody (encodeContactForm model.editForm)
                        , expect = Http.expectJson ContactUpdated contactDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                Nothing ->
                    ( model, Cmd.none )

        ContactUpdated (Ok contact) ->
            ( { model
                | contact = Just contact
                , showModal = NoModal
                , isSubmittingForm = False
                , error = Nothing
              }
            , Cmd.none
            )

        ContactUpdated (Err _) ->
            ( { model
                | isSubmittingForm = False
                , error = Just "Failed to update contact"
              }
            , Cmd.none
            )

        CheckEmail email ->
            ( { model | isCheckingEmail = True }
            , checkEmail email
            )

        EmailChecked (Ok response) ->
            ( { model
                | isCheckingEmail = False
                , emailExists = response.exists
                , error =
                    if response.exists then
                        Just "A contact with this email already exists"

                    else
                        Nothing
              }
            , Cmd.none
            )

        EmailChecked (Err _) ->
            ( { model
                | isCheckingEmail = False
                , error = Just "Failed to check email. Please try again."
              }
            , Cmd.none
            )

        LookupZipCode zipCode ->
            ( model
            , Http.get
                { url = "/api/zip-lookup/" ++ zipCode
                , expect = Http.expectJson GotZipLookup zipInfoDecoder
                }
            )

        GotZipLookup (Ok zipInfo) ->
            let
                form =
                    model.editForm

                updatedForm =
                    { form | state = zipInfo.state }
            in
            ( { model | editForm = updatedForm }
            , Cmd.none
            )

        GotZipLookup (Err _) ->
            ( model, Cmd.none )

        ShowDeleteConfirmModal ->
            ( { model | showModal = DeleteConfirmModal }, Cmd.none )

        DeleteContact ->
            case model.contact of
                Just contact ->
                    ( { model | isDeletingContact = True }
                    , deleteContact contact.id
                    )

                Nothing ->
                    ( model, Cmd.none )

        ContactDeleted (Ok response) ->
            if response.success then
                ( model, Nav.pushUrl model.key "/contacts" )

            else
                ( { model | isDeletingContact = False, error = Just "Failed to delete contact" }, Cmd.none )

        ContactDeleted (Err _) ->
            ( { model | isDeletingContact = False, error = Just "Failed to delete contact" }, Cmd.none )

        GenerateQuoteLink ->
            case model.contact of
                Just contact ->
                    ( { model | isGeneratingQuote = True }
                    , Http.get
                        { url = "/api/quotes/generate/" ++ String.fromInt contact.id
                        , expect = Http.expectJson GotQuoteLink quoteLinkDecoder
                        }
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotQuoteLink (Ok response) ->
            ( { model
                | quoteUrl =
                    case model.contact of
                        Just contact ->
                            Just ("/quote?id=" ++ response.quoteId ++ "&planType=" ++ contact.planType)

                        Nothing ->
                            Just ("/quote?id=" ++ response.quoteId)
                , isGeneratingQuote = False
              }
            , Cmd.none
            )

        GotQuoteLink (Err _) ->
            ( { model
                | error = Just "Failed to generate quote link"
                , isGeneratingQuote = False
              }
            , Cmd.none
            )

        GotHealthStatus (Ok status) ->
            ( { model | healthStatus = Just status }
            , Cmd.none
            )

        GotHealthStatus (Err _) ->
            ( model, Cmd.none )

        GotFollowUps (Ok followUps) ->
            ( { model | followUps = followUps }
            , Cmd.none
            )

        GotFollowUps (Err _) ->
            ( model, Cmd.none )

        ToggleFollowUps ->
            ( { model | showAllFollowUps = not model.showAllFollowUps }, Cmd.none )

        GotOrgSettings (Ok settings) ->
            let
                currentSchedule =
                    model.emailSchedule

                updatedSchedule =
                    { currentSchedule
                        | stateCarrierSettings = settings.stateCarrierSettings
                        , stateLicenses = settings.stateLicenses
                    }
            in
            ( { model | orgSettings = Just settings, emailSchedule = updatedSchedule }
            , Cmd.none
            )

        GotOrgSettings (Err _) ->
            ( { model | error = Just "Failed to load organization settings" }
            , Cmd.none
            )

        SendQuoteEmail ->
            case model.contact of
                Just contact ->
                    ( { model | isGeneratingQuote = True, emailSendSuccess = False }
                    , Http.post
                        { url = "/api/contacts/" ++ String.fromInt contact.id ++ "/send-quote-email"
                        , body = Http.emptyBody
                        , expect =
                            Http.expectJson QuoteEmailSent
                                (Decode.map2 (\s m -> { success = s, message = m })
                                    (Decode.field "success" Decode.bool)
                                    (Decode.field "message" Decode.string)
                                )
                        }
                    )

                Nothing ->
                    ( model, Cmd.none )

        QuoteEmailSent (Ok response) ->
            ( { model
                | isGeneratingQuote = False
                , emailSendSuccess = response.success
                , error =
                    if response.success then
                        Nothing

                    else
                        Just response.message
              }
            , if response.success then
                Process.sleep 5000
                    |> Task.perform (\_ -> ResetEmailSendState)

              else
                Cmd.none
            )

        QuoteEmailSent (Err _) ->
            ( { model
                | isGeneratingQuote = False
                , emailSendSuccess = False
                , error = Just "Failed to send quote email"
              }
            , Cmd.none
            )

        ResetEmailSendState ->
            ( { model | emailSendSuccess = False }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Contact Details"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
                [ viewBackButton
                , case model.contact of
                    Just contact ->
                        div []
                            [ viewHeader contact model
                            , viewContactSummary contact model.quoteUrl model.isGeneratingQuote model.healthStatus model.followUps model.timeZone model.showAllFollowUps
                            , if model.orgSettings /= Nothing && isStateActive model.emailSchedule then
                                div [ class "bg-white rounded-lg border border-gray-200 p-6 mb-8" ]
                                    [ viewFutureActivity (getScheduledEmails model.emailSchedule) ]

                              else
                                div [ class "bg-white rounded-lg border border-gray-200 p-6 mb-8" ]
                                    [ h2 [ class "text-lg font-medium mb-4" ] [ text "Future Activity" ]
                                    , div [ class "flex justify-center items-center py-8" ]
                                        [ viewSpinner
                                        , span [ class "ml-3 text-gray-500" ] [ text "Loading future activities..." ]
                                        ]
                                    ]
                            , viewActivity model.activities
                            ]

                    Nothing ->
                        viewLoading
                ]
            ]
        , viewModals model
        ]
    }


viewBackButton : Html Msg
viewBackButton =
    button
        [ class "mb-6 inline-flex items-center text-sm text-gray-600 hover:text-gray-900"
        , onClick BackToContacts
        ]
        [ span [ class "mr-2" ] [ text "←" ]
        , text "Back to Contacts"
        ]


viewHeader : Contact -> Model -> Html Msg
viewHeader contact model =
    div [ class "flex justify-between items-center mb-8" ]
        [ div [ class "flex items-center gap-4" ]
            [ h1 [ class "text-2xl font-semibold" ]
                [ text (contact.firstName ++ " " ++ contact.lastName) ]
            , viewStatus contact.status
            ]
        , div [ class "flex gap-2" ]
            [ button
                [ class "px-4 py-2 text-sm font-medium text-blue-600 hover:text-blue-700 bg-blue-50 hover:bg-blue-100 rounded-lg transition-colors duration-200 flex items-center gap-2"
                , onClick
                    (if model.isGeneratingQuote || model.emailSendSuccess then
                        NoOp

                     else
                        SendQuoteEmail
                    )
                ]
                (if model.isGeneratingQuote then
                    [ viewSpinner
                    , text "Sending..."
                    ]

                 else if model.emailSendSuccess then
                    [ span [ class "text-green-600" ] [ text "✓" ]
                    , text "Email Sent"
                    ]

                 else
                    [ text "Send Email" ]
                )
            , button
                [ class "px-4 py-2 text-sm font-medium text-purple-600 hover:text-purple-700 bg-purple-50 hover:bg-purple-100 rounded-lg transition-colors duration-200 flex items-center gap-2"
                , onClick ShowEditModal
                ]
                [ text "Edit" ]
            , button
                [ class "px-4 py-2 text-sm font-medium text-red-600 hover:text-red-700 bg-red-50 hover:bg-red-100 rounded-lg transition-colors duration-200 flex items-center gap-2"
                , onClick ShowDeleteConfirmModal
                ]
                [ text "Delete" ]
            ]
        ]


viewContactSummary : Contact -> Maybe String -> Bool -> Maybe HealthStatus -> List FollowUpRequest -> Zone -> Bool -> Html Msg
viewContactSummary contact quoteUrl isGeneratingQuote healthStatus followUps zone showAllFollowUps =
    let
        followUpsSection =
            if not (List.isEmpty followUps) then
                div [ class "bg-white rounded-lg border border-gray-200 p-6 mb-8" ]
                    [ div [ class "flex justify-between items-center mb-6" ]
                        [ h2 [ class "text-lg font-medium" ] [ text "Follow-up Requests" ] ]
                    , div [ class "space-y-4" ]
                        (List.take
                            (if showAllFollowUps then
                                List.length followUps

                             else
                                2
                            )
                            followUps
                            |> List.map (viewFollowUpRequest zone)
                        )
                    , if not showAllFollowUps && List.length followUps > 2 then
                        div [ class "mt-4 text-center" ]
                            [ button
                                [ class "text-sm text-purple-600 hover:text-purple-800"
                                , onClick ToggleFollowUps
                                ]
                                [ text ("Show " ++ String.fromInt (List.length followUps - 2) ++ " More") ]
                            ]

                      else if showAllFollowUps then
                        div [ class "mt-4 text-center" ]
                            [ button
                                [ class "text-sm text-purple-600 hover:text-purple-800"
                                , onClick ToggleFollowUps
                                ]
                                [ text "Show Less" ]
                            ]

                      else
                        text ""
                    ]

            else
                text ""
    in
    div []
        [ div [ class "bg-white rounded-lg border border-gray-200 p-6 mb-8" ]
            [ h2 [ class "text-lg font-medium mb-6" ] [ text "Contact Summary" ]
            , div [ class "grid grid-cols-2 gap-x-8 gap-y-6" ]
                [ viewField "Date of Birth" contact.birthDate
                , viewField "Contact Owner" (Maybe.map .firstName contact.contactOwner |> Maybe.withDefault "Default")
                , viewField "Phone Number" (formatPhoneNumber contact.phoneNumber)
                , viewField "Email" contact.email
                , viewField "Current Carrier" contact.currentCarrier
                , viewField "Gender" contact.gender
                , viewField "Tobacco Use"
                    (if contact.tobaccoUser then
                        "Yes"

                     else
                        "No"
                    )
                , viewField "State" contact.state
                , viewField "Zip Code" contact.zipCode
                , viewField "Effective Date" contact.effectiveDate
                , viewField "Plan Type" contact.planType
                , viewQuoteField quoteUrl isGeneratingQuote
                , viewHealthStatusField healthStatus
                ]
            ]
        , followUpsSection
        ]


viewQuoteField : Maybe String -> Bool -> Html Msg
viewQuoteField quoteUrl isGeneratingQuote =
    div []
        [ div [ class "text-sm font-medium text-gray-500" ] [ text "Quote Link" ]
        , div [ class "mt-1" ]
            [ case quoteUrl of
                Just url ->
                    a
                        [ href url
                        , class "text-sm text-blue-600 hover:text-blue-800 underline"
                        , target "_blank"
                        ]
                        [ text "View Quote" ]

                Nothing ->
                    if isGeneratingQuote then
                        viewSpinner

                    else
                        text "-"
            ]
        ]


viewHealthStatusField : Maybe HealthStatus -> Html Msg
viewHealthStatusField maybeStatus =
    div []
        [ div [ class "text-sm font-medium text-gray-500" ] [ text "Health Status" ]
        , div [ class "mt-1" ]
            [ case maybeStatus of
                Just status ->
                    case status.status of
                        "pass" ->
                            div [ class "flex items-center text-green-600 text-sm" ]
                                [ span [ class "mr-1" ] [ text "✓" ]
                                , text "Pass"
                                ]

                        "flagged" ->
                            div [ class "flex items-center text-red-600 text-sm" ]
                                [ span [ class "mr-1" ] [ text "✕" ]
                                , text "Issue Flagged"
                                ]

                        _ ->
                            div [ class "text-gray-600 text-sm" ]
                                [ text "Incomplete" ]

                Nothing ->
                    div [ class "text-gray-600 text-sm" ]
                        [ text "Loading..." ]
            ]
        ]


viewField : String -> String -> Html Msg
viewField label value =
    div []
        [ div [ class "text-sm font-medium text-gray-500" ] [ text label ]
        , div [ class "mt-1 text-sm text-gray-900" ] [ text value ]
        ]


viewActivity : List Activity -> Html Msg
viewActivity activities =
    div [ class "bg-white rounded-lg border border-gray-200 p-6" ]
        [ h2 [ class "text-lg font-medium mb-6" ] [ text "Past Activity" ]
        , table [ class "min-w-full" ]
            [ thead [ class "bg-gray-50" ]
                [ tr []
                    [ th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ] [ text "Submission Date" ]
                    , th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ] [ text "Status" ]
                    , th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ] [ text "Carrier Selected" ]
                    , th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ] [ text "Plan Selected" ]
                    , th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ] [ text "Quote Amount" ]
                    ]
                ]
            , tbody [ class "divide-y divide-gray-200" ]
                (List.map viewActivityRow activities)
            ]
        ]


viewActivityRow : Activity -> Html Msg
viewActivityRow activity =
    tr [ class "hover:bg-gray-50" ]
        [ td [ class "px-3 py-2 text-sm text-gray-900" ] [ text activity.submissionDate ]
        , td [ class "px-3 py-2 text-sm" ] [ viewActivityStatus activity.status ]
        , td [ class "px-3 py-2 text-sm text-gray-900" ] [ text (Maybe.withDefault "-" activity.carrierSelected) ]
        , td [ class "px-3 py-2 text-sm text-gray-900" ] [ text (Maybe.withDefault "-" activity.planSelected) ]
        , td [ class "px-3 py-2 text-sm text-gray-900" ]
            [ text
                (activity.quoteAmount
                    |> Maybe.map (\amount -> "$" ++ String.fromFloat amount)
                    |> Maybe.withDefault "-"
                )
            ]
        ]


viewActivityStatus : ActivityStatus -> Html Msg
viewActivityStatus status =
    let
        ( bgColor, textColor, statusText ) =
            case status of
                QuoteCreated ->
                    ( "bg-green-50", "text-green-700", "Quote Created" )

                EmailOpened ->
                    ( "bg-red-50", "text-red-700", "Email Opened" )

                EmailSent n ->
                    ( "bg-blue-50", "text-blue-700", "Email #" ++ String.fromInt n ++ " Sent" )
    in
    div [ class ("inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium " ++ bgColor ++ " " ++ textColor) ]
        [ text statusText ]


viewStatus : String -> Html Msg
viewStatus status =
    let
        ( bgColor, textColor ) =
            case status of
                "Quote Created" ->
                    ( "bg-green-50", "text-green-700" )

                "Opened Email" ->
                    ( "bg-red-50", "text-red-700" )

                "Email #2 Sent" ->
                    ( "bg-blue-50", "text-blue-700" )

                "Email #1 Sent" ->
                    ( "bg-blue-50", "text-blue-700" )

                "In Queue" ->
                    ( "bg-orange-50", "text-orange-700" )

                _ ->
                    ( "bg-gray-50", "text-gray-700" )
    in
    div [ class ("inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium " ++ bgColor ++ " " ++ textColor) ]
        [ text status ]


viewHealthStatus : Maybe HealthStatus -> Html Msg
viewHealthStatus maybeStatus =
    div [ class "p-4 rounded-lg flex items-center" ]
        [ div [ class "text-sm font-medium mr-2" ] [ text "Health Status:" ]
        , case maybeStatus of
            Just status ->
                case status.status of
                    "pass" ->
                        div [ class "flex items-center text-green-600" ]
                            [ span [ class "mr-2" ] [ text "✓" ]
                            , text "Pass"
                            ]

                    "flagged" ->
                        div [ class "flex items-center text-red-600" ]
                            [ span [ class "mr-2" ] [ text "✕" ]
                            , text "Issue Flagged"
                            ]

                    _ ->
                        div [ class "flex items-center text-gray-600" ]
                            [ text "Incomplete" ]

            Nothing ->
                div [ class "flex items-center text-gray-600" ]
                    [ text "Loading..." ]
        ]


viewModals : Model -> Html Msg
viewModals model =
    case model.showModal of
        NoModal ->
            text ""

        EditModal ->
            viewEditModal model

        DeleteConfirmModal ->
            viewDeleteConfirmModal model


viewEditModal : Model -> Html Msg
viewEditModal model =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-5xl w-full mx-4 shadow-xl relative" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ text "×" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Edit Contact" ]
            , viewContactForm model.editForm model.isSubmittingForm model
            ]
        ]


viewContactForm : ContactForm -> Bool -> Model -> Html Msg
viewContactForm form isSubmitting model =
    Html.form [ onSubmit SubmitEditForm ]
        [ div [ class "grid grid-cols-2 gap-x-8 gap-y-6" ]
            [ viewFormInput "First Name" "text" form.firstName FirstName True model
            , viewFormInput "Last Name" "text" form.lastName LastName True model
            , viewFormInput "Email" "email" form.email Email True model
            , viewFormInput "Phone Number" "text" (formatPhoneNumber form.phoneNumber) PhoneNumber True model
            , viewFormInput "Current Carrier" "text" form.currentCarrier CurrentCarrier True model
            , viewFormInput "Plan Type" "text" form.planType PlanType True model
            , viewFormInput "Effective Date" "date" form.effectiveDate EffectiveDate True model
            , viewFormInput "Birth Date" "date" form.birthDate BirthDate True model
            , viewFormRadioGroup "Tobacco User"
                (if form.tobaccoUser then
                    "true"

                 else
                    "false"
                )
                TobaccoUser
                [ ( "true", "Yes" ), ( "false", "No" ) ]
            , viewFormRadioGroup "Gender" form.gender Gender [ ( "M", "Male" ), ( "F", "Female" ) ]
            , div [ class "col-span-2 grid grid-cols-2 gap-x-8" ]
                [ viewFormInput "ZIP Code" "text" form.zipCode ZipCode True model
                , viewFormInput "State" "text" form.state State True model
                ]
            ]
        , div [ class "mt-10 flex justify-end space-x-4" ]
            [ button
                [ type_ "button"
                , onClick CloseModal
                , class "px-6 py-3 bg-white text-gray-700 text-sm font-medium rounded-lg border-2 border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200 focus:ring-4 focus:ring-purple-100"
                ]
                [ text "Cancel" ]
            , if isSubmitting then
                div [ class "px-6 py-3 flex items-center space-x-2" ] [ viewSpinner ]

              else
                button
                    [ type_ "submit"
                    , class "px-6 py-3 bg-purple-500 text-white text-sm font-medium rounded-lg hover:bg-purple-600 transition-colors duration-200 focus:ring-4 focus:ring-purple-200"
                    ]
                    [ text "Save Changes" ]
            ]
        ]


viewFormInput : String -> String -> String -> ContactFormField -> Bool -> Model -> Html Msg
viewFormInput labelText inputType inputValue field isRequired model =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , if field == Email then
            div [ class "relative" ]
                [ Html.input
                    [ type_ inputType
                    , class
                        ("w-full px-4 py-3 bg-white border-[2.5px] rounded-lg text-gray-700 placeholder-gray-400 shadow-sm transition-all duration-200 "
                            ++ (if model.emailExists then
                                    "border-red-300 hover:border-red-400 focus:border-red-500 focus:ring-2 focus:ring-red-200"

                                else
                                    "border-purple-300 hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200"
                               )
                        )
                    , Html.Attributes.value inputValue
                    , onInput (UpdateEditForm field)
                    , onBlur (CheckEmail inputValue)
                    , required isRequired
                    ]
                    []
                , if model.isCheckingEmail then
                    div [ class "absolute right-3 top-3" ]
                        [ viewSpinner ]

                  else if model.emailExists then
                    div [ class "absolute right-3 top-3 text-red-500" ]
                        [ text "✕" ]

                  else if String.length inputValue > 0 then
                    div [ class "absolute right-3 top-3 text-green-500" ]
                        [ text "✓" ]

                  else
                    text ""
                ]

          else if field == State then
            Html.input
                [ type_ inputType
                , class "w-full px-4 py-3 bg-white border-[2.5px] border-gray-200 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
                , Html.Attributes.value inputValue
                , Html.Attributes.disabled True
                , required isRequired
                ]
                []

          else
            Html.input
                [ type_ inputType
                , class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
                , Html.Attributes.value inputValue
                , onInput (UpdateEditForm field)
                , required isRequired
                ]
                []
        ]


viewFormRadioGroup : String -> String -> ContactFormField -> List ( String, String ) -> Html Msg
viewFormRadioGroup labelText selectedValue field options =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , div [ class "flex gap-4" ]
            (List.map
                (\( val, txt ) ->
                    label
                        [ class
                            ("flex items-center px-4 py-2 rounded-lg border-2 cursor-pointer transition-all duration-200 "
                                ++ (if selectedValue == val then
                                        "border-purple-500 bg-purple-50 text-purple-700"

                                    else
                                        "border-gray-200 hover:border-purple-200"
                                   )
                            )
                        ]
                        [ input
                            [ type_ "radio"
                            , value val
                            , checked (selectedValue == val)
                            , onInput (UpdateEditForm field)
                            , class "sr-only"
                            ]
                            []
                        , text txt
                        ]
                )
                options
            )
        ]


encodeContactForm : ContactForm -> Encode.Value
encodeContactForm form =
    Encode.object
        [ ( "first_name", Encode.string form.firstName )
        , ( "last_name", Encode.string form.lastName )
        , ( "email", Encode.string form.email )
        , ( "phone_number", Encode.string (String.filter Char.isDigit form.phoneNumber |> String.left 10) )
        , ( "state", Encode.string form.state )
        , ( "contact_owner_id", Maybe.map Encode.int form.contactOwnerId |> Maybe.withDefault Encode.null )
        , ( "current_carrier", Encode.string form.currentCarrier )
        , ( "effective_date", Encode.string form.effectiveDate )
        , ( "birth_date", Encode.string form.birthDate )
        , ( "tobacco_user", Encode.bool form.tobaccoUser )
        , ( "gender", Encode.string form.gender )
        , ( "zip_code", Encode.string form.zipCode )
        , ( "plan_type", Encode.string form.planType )
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "flex justify-center items-center h-64" ]
        [ div [ class "animate-spin rounded-full h-8 w-8 border-2 border-purple-500 border-t-transparent" ] [] ]


viewSpinner : Html msg
viewSpinner =
    div [ class "animate-spin rounded-full h-5 w-5 border-2 border-purple-500 border-t-transparent" ] []


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.showModal /= NoModal then
        Browser.Events.onKeyDown
            (Decode.map
                (\key ->
                    if key == "Escape" then
                        CloseModal

                    else
                        NoOp
                )
                (Decode.field "key" Decode.string)
            )

    else
        Sub.none


checkEmail : String -> Cmd Msg
checkEmail email =
    Http.get
        { url = "/api/contacts/check-email/" ++ email
        , expect = Http.expectJson EmailChecked (Decode.map (\exists -> { exists = exists }) (Decode.field "exists" Decode.bool))
        }


lookupZipCode : String -> Cmd Msg
lookupZipCode zipCode =
    Http.get
        { url = "/api/zip-lookup/" ++ zipCode
        , expect = Http.expectJson GotZipLookup zipInfoDecoder
        }


zipInfoDecoder : Decode.Decoder ZipInfo
zipInfoDecoder =
    Decode.succeed ZipInfo
        |> Pipeline.required "state" Decode.string
        |> Pipeline.required "counties" (Decode.list Decode.string)
        |> Pipeline.required "cities" (Decode.list Decode.string)


deleteContact : Int -> Cmd Msg
deleteContact contactId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/contacts"
        , body = Http.jsonBody (Encode.list Encode.int [ contactId ])
        , expect = Http.expectJson ContactDeleted deleteResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type alias DeleteResponse =
    { success : Bool
    , deletedIds : List Int
    , message : String
    }


deleteResponseDecoder : Decode.Decoder DeleteResponse
deleteResponseDecoder =
    Decode.map3 DeleteResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "deleted_ids" (Decode.list Decode.int))
        (Decode.field "message" Decode.string)


viewDeleteConfirmModal : Model -> Html Msg
viewDeleteConfirmModal model =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-8 max-w-md w-full mx-4 shadow-xl relative" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ text "×" ]
            , h2 [ class "text-xl font-semibold text-gray-900 mb-4" ]
                [ text "Delete Contact" ]
            , p [ class "text-sm text-gray-600 mb-6" ]
                [ text "Are you sure you want to delete this contact? This action cannot be undone." ]
            , div [ class "flex justify-end space-x-4" ]
                [ button
                    [ class "px-4 py-2 text-gray-700 text-sm font-medium rounded-lg border-2 border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200"
                    , onClick CloseModal
                    ]
                    [ text "Cancel" ]
                , if model.isDeletingContact then
                    div [ class "px-4 py-2 flex items-center" ]
                        [ viewSpinner ]

                  else
                    button
                        [ class "px-4 py-2 bg-red-600 text-white text-sm font-medium rounded-lg hover:bg-red-700 transition-colors duration-200"
                        , onClick DeleteContact
                        ]
                        [ text "Delete" ]
                ]
            ]
        ]


quoteLinkDecoder : Decode.Decoder { quoteId : String, redirectUrl : String }
quoteLinkDecoder =
    Decode.map2 (\id url -> { quoteId = id, redirectUrl = url })
        (Decode.field "quoteId" Decode.string)
        (Decode.field "redirectUrl" Decode.string)


followUpsDecoder : Decoder (List FollowUpRequest)
followUpsDecoder =
    Decode.list
        (Decode.map3 FollowUpRequest
            (Decode.field "type" Decode.string)
            (Decode.field "quoteId" Decode.string)
            (Decode.field "createdAt" posixDecoder)
        )


posixDecoder : Decoder Posix
posixDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                let
                    parts =
                        String.split " " str

                    datePart =
                        List.head parts |> Maybe.withDefault ""

                    timePart =
                        List.drop 1 parts |> List.head |> Maybe.withDefault ""

                    dateParts =
                        String.split "-" datePart

                    year =
                        List.head dateParts |> Maybe.andThen String.toInt |> Maybe.withDefault 1970

                    month =
                        List.drop 1 dateParts |> List.head |> Maybe.andThen String.toInt |> Maybe.withDefault 1

                    day =
                        List.drop 2 dateParts |> List.head |> Maybe.andThen String.toInt |> Maybe.withDefault 1

                    timeComponents =
                        String.split ":" timePart

                    hour =
                        List.head timeComponents |> Maybe.andThen String.toInt |> Maybe.withDefault 0

                    minute =
                        List.drop 1 timeComponents |> List.head |> Maybe.andThen String.toInt |> Maybe.withDefault 0

                    second =
                        List.drop 2 timeComponents |> List.head |> Maybe.andThen (String.split "." >> List.head) |> Maybe.andThen String.toInt |> Maybe.withDefault 0

                    -- Calculate milliseconds since epoch
                    msPerDay =
                        86400000

                    msPerHour =
                        3600000

                    msPerMinute =
                        60000

                    msPerSecond =
                        1000

                    -- Start with Unix epoch (1970-01-01) and add days
                    daysFromEpoch =
                        (year - 1970)
                            * 365
                            + ((year - 1969) // 4)
                            + (case month of
                                1 ->
                                    0

                                2 ->
                                    31

                                3 ->
                                    59

                                4 ->
                                    90

                                5 ->
                                    120

                                6 ->
                                    151

                                7 ->
                                    181

                                8 ->
                                    212

                                9 ->
                                    243

                                10 ->
                                    273

                                11 ->
                                    304

                                12 ->
                                    334

                                _ ->
                                    0
                              )
                            + day
                            - 1

                    timestamp =
                        daysFromEpoch
                            * msPerDay
                            + hour
                            * msPerHour
                            + minute
                            * msPerMinute
                            + second
                            * msPerSecond
                in
                Decode.succeed (Time.millisToPosix timestamp)
            )


viewFollowUpRequest : Zone -> FollowUpRequest -> Html Msg
viewFollowUpRequest zone followUp =
    div [ class "flex items-center justify-between py-3 border-b border-gray-100 last:border-0" ]
        [ div [ class "flex items-center space-x-4" ]
            [ div [ class "text-sm text-gray-600" ]
                [ text (formatDate zone followUp.createdAt) ]
            , div [ class "text-sm font-medium" ]
                [ text
                    (case followUp.type_ of
                        "accept" ->
                            "Accepted - Ready to Switch"

                        "decline" ->
                            "Declined - Looking for Alternatives"

                        _ ->
                            "General Follow-up Request"
                    )
                ]
            ]
        ]


formatDate : Zone -> Posix -> String
formatDate zone time =
    let
        year =
            String.fromInt (Time.toYear zone time)

        month =
            case Time.toMonth zone time of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"

        day =
            String.padLeft 2 '0' (String.fromInt (Time.toDay zone time))

        hour =
            Time.toHour zone time

        ( displayHour, amPm ) =
            if hour == 0 then
                ( "12", "AM" )

            else if hour < 12 then
                ( String.fromInt hour, "AM" )

            else if hour == 12 then
                ( "12", "PM" )

            else
                ( String.fromInt (hour - 12), "PM" )

        minute =
            String.padLeft 2 '0' (String.fromInt (Time.toMinute zone time))
    in
    year ++ "-" ++ month ++ "-" ++ day ++ " at " ++ displayHour ++ ":" ++ minute ++ " " ++ amPm



-- HELPERS


stringToPosix : String -> Date
stringToPosix dateString =
    case Date.fromIsoString dateString of
        Ok date ->
            date

        Err _ ->
            -- Default to Unix epoch if invalid date
            Date.fromCalendarDate 1970 Jan 1


isStateActive : EmailSchedule -> Bool
isStateActive schedule =
    List.member schedule.state schedule.stateLicenses


updateContact : Contact -> List Contact -> List Contact
updateContact updated contacts =
    List.map
        (\contact ->
            if contact.id == updated.id then
                updated

            else
                contact
        )
        contacts
