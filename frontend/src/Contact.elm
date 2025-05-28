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
import Utils.Formatters exposing (formatPhoneNumber)
import Utils.MyDate exposing (dateFromMonthDayYear)



-- TYPES


type alias Contact =
    { id : Int
    , firstName : Maybe String
    , lastName : Maybe String
    , email : String
    , phoneNumber : Maybe String
    , state : Maybe String
    , contactOwnerId : Maybe Int
    , contactOwner : Maybe User
    , currentCarrier : Maybe String
    , effectiveDate : Maybe String
    , birthDate : Maybe String
    , tobaccoUser : Maybe Bool
    , gender : Maybe String
    , zipCode : Maybe String
    , planType : Maybe String
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


type QuestionType
    = MainQuestion
    | FollowUpQuestion Int -- Parent question ID


type alias EligibilityQuestion =
    { id : Int
    , text : String
    , questionType : QuestionType
    , answer : Maybe (Either Bool String)
    , followUpQuestions : List EligibilityFollowUp
    }


type alias EligibilityFollowUp =
    { id : Int
    , text : String
    , answer : Maybe (Either Bool String)
    }


type Either a b
    = Left a
    | Right b


type Modal
    = NoModal
    | EditModal
    | DeleteConfirmModal
    | HealthAssessmentModal


type alias EmailTrackingRecord =
    { emailType : String
    , scheduledDate : String
    , sendStatus : String
    , sendMode : String
    }


type alias Model =
    { key : Nav.Key
    , contact : Maybe Contact
    , showModal : Modal
    , editForm : ContactForm
    , isSubmittingForm : Bool
    , error : Maybe String
    , emailTrackingRecords : List EmailTrackingRecord
    , isCheckingEmail : Bool
    , emailExists : Bool
    , isDeletingContact : Bool
    , emailSchedule : EmailSchedule
    , quoteUrl : Maybe String
    , isGeneratingQuote : Bool
    , healthStatus : Maybe HealthStatus
    , eligibilityQuestions : List EligibilityQuestion
    , followUps : List FollowUpRequest
    , timeZone : Zone
    , showAllFollowUps : Bool
    , orgSettings : Maybe Settings
    , emailSendSuccess : Bool
    , demoMode : Bool
    }


type alias HealthStatus =
    { status : String
    , answers : Maybe String
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
        |> Pipeline.required "first_name" (Decode.nullable Decode.string)
        |> Pipeline.required "last_name" (Decode.nullable Decode.string)
        |> Pipeline.required "email" Decode.string
        |> Pipeline.optional "phone_number" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "state" (Decode.nullable Decode.string)
        |> Pipeline.optional "contact_owner_id" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "contact_owner" (Decode.nullable userDecoder) Nothing
        |> Pipeline.optional "current_carrier" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "effective_date" (Decode.nullable Decode.string)
        |> Pipeline.required "birth_date" (Decode.nullable Decode.string)
        |> Pipeline.required "tobacco_user" (Decode.nullable Decode.bool)
        |> Pipeline.required "gender" (Decode.nullable Decode.string)
        |> Pipeline.required "zip_code" (Decode.nullable Decode.string)
        |> Pipeline.optional "plan_type" (Decode.nullable Decode.string) Nothing
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
        (Decode.field "answers" (Decode.nullable Decode.string))


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
    Decode.succeed StateCarrierSetting
        |> Pipeline.optional "state" Decode.string ""
        |> Pipeline.required "carrier" Decode.string
        |> Pipeline.required "active" Decode.bool
        |> Pipeline.required "targetGI" Decode.bool


emailTrackingDecoder : Decoder EmailTrackingRecord
emailTrackingDecoder =
    Decode.succeed EmailTrackingRecord
        |> Pipeline.required "email_type" Decode.string
        |> Pipeline.required "scheduled_date" Decode.string
        |> Pipeline.required "send_status" Decode.string
        |> Pipeline.required "send_mode" Decode.string



-- INIT


type alias ZipInfo =
    { state : String
    , counties : List String
    , cities : List String
    }


init : Nav.Key -> String -> Bool -> ( Model, Cmd Msg )
init key contactId demoMode =
    let
        initialSchedule =
            EmailScheduler.init
                (String.toInt contactId |> Maybe.withDefault 0)
                (Date.fromCalendarDate 2024 Jan 1)
                (Date.fromCalendarDate 2024 Jan 1)
                (Date.fromCalendarDate 2024 Jan 1)
                NoPlan
                ""
                []
                []
    in
    ( { key = key
      , contact = Nothing
      , showModal = NoModal
      , editForm = emptyForm
      , isSubmittingForm = False
      , error = Nothing
      , emailTrackingRecords = []
      , isCheckingEmail = False
      , emailExists = False
      , isDeletingContact = False
      , emailSchedule = initialSchedule
      , quoteUrl = Nothing
      , isGeneratingQuote = False
      , healthStatus = Nothing
      , eligibilityQuestions = []
      , followUps = []
      , timeZone = Time.utc
      , showAllFollowUps = False
      , orgSettings = Nothing
      , emailSendSuccess = False
      , demoMode = demoMode
      }
    , Cmd.batch
        [ Http.get
            { url = "/api/contacts/" ++ contactId
            , expect = Http.expectJson GotContact contactDecoder
            }
        , Http.get
            { url = "/api/contacts/email-tracking/" ++ contactId
            , expect =
                Http.expectJson GotEmailTracking
                    (Decode.map2 (\s r -> { success = s, trackingRecords = r })
                        (Decode.field "success" Decode.bool)
                        (Decode.field "trackingRecords" (Decode.list emailTrackingDecoder))
                    )
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
    | ShowHealthAssessmentModal
    | GotFollowUps (Result Http.Error (List FollowUpRequest))
    | ToggleFollowUps
    | GotOrgSettings (Result Http.Error Settings)
    | SendQuoteEmail
    | QuoteEmailSent (Result Http.Error { success : Bool, message : String, trackingRecord : Maybe EmailTrackingRecord })
    | GotEmailTracking (Result Http.Error { success : Bool, trackingRecords : List EmailTrackingRecord })
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


safeStringToDate : String -> Result String Date
safeStringToDate dateString =
    case Date.fromIsoString dateString of
        Ok date ->
            Ok date

        Err error ->
            case dateFromMonthDayYear dateString of
                Ok date ->
                    Ok date

                Err _ ->
                    Err error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotContact (Ok contact) ->
            let
                defaultDate =
                    Date.fromCalendarDate 1970 Jan 1

                effectiveDateResult =
                    case contact.effectiveDate of
                        Just date ->
                            if String.isEmpty date then
                                Ok defaultDate

                            else
                                safeStringToDate date

                        Nothing ->
                            Ok defaultDate

                birthDateResult =
                    case contact.birthDate of
                        Just date ->
                            if String.isEmpty date then
                                Ok defaultDate

                            else
                                safeStringToDate date

                        Nothing ->
                            Ok defaultDate
            in
            case effectiveDateResult of
                Ok effectiveDate ->
                    case birthDateResult of
                        Ok birthDate ->
                            let
                                planType =
                                    case contact.planType of
                                        Just s ->
                                            case String.toLower s of
                                                "plan n" ->
                                                    PlanN

                                                "n" ->
                                                    PlanN

                                                "plan g" ->
                                                    PlanG

                                                "g" ->
                                                    PlanG

                                                _ ->
                                                    NoPlan

                                        Nothing ->
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
                                        (Maybe.withDefault "" contact.state)
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
                            ( { model | contact = Just contact, error = Just "Invalid birth date format" }
                            , Cmd.none
                            )

                Err _ ->
                    ( { model | contact = Just contact, error = Just "Invalid effective date format" }
                    , Cmd.none
                    )

        GotContact (Err error) ->
            case error of
                Http.BadStatus 404 ->
                    ( { model | error = Just "Contact not found" }
                    , Cmd.none
                    )

                _ ->
                    ( { model | error = Just "Failed to load contact" }
                    , Cmd.none
                    )

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
                            , firstName = Maybe.withDefault "" contact.firstName
                            , lastName = Maybe.withDefault "" contact.lastName
                            , email = contact.email
                            , phoneNumber = Maybe.withDefault "" contact.phoneNumber
                            , state = Maybe.withDefault "" contact.state
                            , contactOwnerId = contact.contactOwnerId
                            , currentCarrier = Maybe.withDefault "" contact.currentCarrier
                            , effectiveDate = Maybe.withDefault "" contact.effectiveDate
                            , birthDate = Maybe.withDefault "" contact.birthDate
                            , tobaccoUser = Maybe.withDefault False contact.tobaccoUser
                            , gender = Maybe.withDefault "" contact.gender
                            , zipCode = Maybe.withDefault "" contact.zipCode
                            , planType = contact.planType |> Maybe.withDefault ""
                            }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        CloseModal ->
            ( { model | showModal = NoModal }
            , case model.contact of
                Just contact ->
                    Nav.pushUrl model.key ("/contact/" ++ String.fromInt contact.id)

                Nothing ->
                    Cmd.none
            )

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
                            let
                                orgId : Maybe String
                                orgId =
                                    response.quoteId
                                        |> String.split "-"
                                        |> List.head

                                outStr : String
                                outStr =
                                    "/compare?id=" ++ response.quoteId
                            in
                            Just outStr

                        Nothing ->
                            Just ("/compare?id=" ++ response.quoteId)
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
            let
                parsedQuestions =
                    case status.answers of
                        Just answersJson ->
                            parseEligibilityAnswers answersJson

                        Nothing ->
                            []
            in
            ( { model
                | healthStatus = Just status
                , eligibilityQuestions = parsedQuestions
              }
            , Cmd.none
            )

        GotHealthStatus (Err _) ->
            ( model, Cmd.none )

        ShowHealthAssessmentModal ->
            ( { model | showModal = HealthAssessmentModal }, Cmd.none )

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
                    let
                        orgId =
                            contact.contactOwner
                                |> Maybe.map (\owner -> owner.organizationId)
                                |> Maybe.withDefault 0

                        encodedBody =
                            Encode.object
                                [ ( "orgId", Encode.int orgId ) ]
                    in
                    ( { model | isGeneratingQuote = True, emailSendSuccess = False }
                    , Http.post
                        { url = "/api/contacts/" ++ String.fromInt contact.id ++ "/send-quote-email"
                        , body = Http.jsonBody encodedBody
                        , expect =
                            Http.expectJson QuoteEmailSent
                                (Decode.succeed (\s m r -> { success = s, message = m, trackingRecord = r })
                                    |> Pipeline.required "success" Decode.bool
                                    |> Pipeline.required "message" Decode.string
                                    |> Pipeline.optional "trackingRecord" (Decode.nullable emailTrackingDecoder) Nothing
                                )
                        }
                    )

                Nothing ->
                    ( model, Cmd.none )

        QuoteEmailSent (Ok response) ->
            let
                updatedRecords =
                    case response.trackingRecord of
                        Just record ->
                            record :: model.emailTrackingRecords

                        Nothing ->
                            model.emailTrackingRecords
            in
            ( { model
                | isGeneratingQuote = False
                , emailSendSuccess = response.success
                , error =
                    if response.success then
                        Nothing

                    else
                        Just response.message
                , emailTrackingRecords = updatedRecords
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

        GotEmailTracking (Ok response) ->
            ( { model | emailTrackingRecords = response.trackingRecords }, Cmd.none )

        GotEmailTracking (Err _) ->
            ( model, Cmd.none )

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
                            , if model.error /= Nothing then
                                div [ class "mb-8 p-4 bg-red-50 border border-red-200 rounded-lg text-red-700" ]
                                    [ text (Maybe.withDefault "An error occurred" model.error)
                                    , button [ class "ml-2 underline", onClick BackToContacts ] [ text "Back to Contacts" ]
                                    ]

                              else
                                text ""
                            , viewContactSummary contact model.quoteUrl model.isGeneratingQuote model.healthStatus model.eligibilityQuestions model.followUps model.timeZone model.showAllFollowUps
                            , if model.orgSettings /= Nothing && model.error == Nothing then
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
                            , viewActivity model.emailTrackingRecords
                            ]

                    Nothing ->
                        case model.error of
                            Just errorMsg ->
                                div [ class "text-center py-12" ]
                                    [ div [ class "text-red-600 text-lg mb-4" ] [ text errorMsg ]
                                    , button
                                        [ class "px-4 py-2 text-sm font-medium text-blue-600 hover:text-blue-700 bg-blue-50 hover:bg-blue-100 rounded-lg transition-colors duration-200"
                                        , onClick BackToContacts
                                        ]
                                        [ text "Back to Contacts" ]
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
                [ text (Maybe.withDefault "" contact.firstName ++ " " ++ Maybe.withDefault "" contact.lastName) ]
            , viewEmailStatus contact.status
            ]
        , div [ class "flex gap-2" ]
            [ button
                [ class
                    (String.join " "
                        [ "px-4 py-2 text-sm font-medium rounded-lg transition-colors duration-200 flex items-center gap-2"
                        , if model.demoMode then
                            "text-blue-400 bg-blue-50 cursor-not-allowed"

                          else
                            "text-blue-600 hover:text-blue-700 bg-blue-50 hover:bg-blue-100"
                        ]
                    )
                , onClick
                    (if model.isGeneratingQuote || model.emailSendSuccess || model.demoMode then
                        NoOp

                     else
                        SendQuoteEmail
                    )
                , disabled model.demoMode
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
                    [ text "Send Quote" ]
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


viewContactSummary : Contact -> Maybe String -> Bool -> Maybe HealthStatus -> List EligibilityQuestion -> List FollowUpRequest -> Zone -> Bool -> Html Msg
viewContactSummary contact quoteUrl isGeneratingQuote healthStatus eligibilityQuestions followUps zone showAllFollowUps =
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
                [ viewField "Date of Birth" (Maybe.withDefault "" contact.birthDate)
                , viewField "Contact Owner" (Maybe.map .firstName contact.contactOwner |> Maybe.withDefault "Default")
                , viewField "Phone Number" (formatPhoneNumber (Maybe.withDefault "" contact.phoneNumber))
                , viewField "Email" contact.email
                , viewField "Current Carrier" (Maybe.withDefault "" contact.currentCarrier)
                , viewField "Gender" (Maybe.withDefault "" contact.gender)
                , viewField "Tobacco Use"
                    (if Maybe.withDefault False contact.tobaccoUser then
                        "Yes"

                     else
                        "No"
                    )
                , viewField "State" (Maybe.withDefault "" contact.state)
                , viewField "Zip Code" (Maybe.withDefault "" contact.zipCode)
                , viewField "Effective Date" (Maybe.withDefault "" contact.effectiveDate)
                , viewField "Plan Type" (Maybe.withDefault "" contact.planType)
                , viewQuoteField quoteUrl isGeneratingQuote
                , viewHealthStatusField healthStatus eligibilityQuestions
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


viewHealthStatusField : Maybe HealthStatus -> List EligibilityQuestion -> Html Msg
viewHealthStatusField maybeStatus questions =
    div []
        [ div [ class "text-sm font-medium text-gray-500" ] [ text "Health Status" ]
        , div [ class "mt-1" ]
            [ case maybeStatus of
                Just status ->
                    let
                        hasYesAnswers =
                            List.any
                                (\q ->
                                    case q.answer of
                                        Just (Left True) ->
                                            q.questionType == MainQuestion

                                        _ ->
                                            False
                                )
                                questions
                    in
                    div [ class "flex items-center" ]
                        [ if List.isEmpty questions then
                            -- No eligibility questions means not completed
                            div [ class "flex items-center text-gray-500 text-sm" ]
                                [ span [ class "mr-1" ] [ text "•" ]
                                , text "Not Completed"
                                ]

                          else if hasYesAnswers then
                            div [ class "flex items-center text-red-600 text-sm" ]
                                [ span [ class "mr-1" ] [ text "✕" ]
                                , text "Issue Flagged"
                                ]

                          else
                            div [ class "flex items-center text-green-600 text-sm" ]
                                [ span [ class "mr-1" ] [ text "✓" ]
                                , text "Pass"
                                ]
                        , button
                            [ class "ml-3 text-blue-600 text-sm hover:text-blue-800 underline"
                            , onClick ShowHealthAssessmentModal
                            ]
                            [ text "View Details" ]
                        ]

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


viewActivity : List EmailTrackingRecord -> Html Msg
viewActivity records =
    div [ class "bg-white rounded-lg border border-gray-200 p-6" ]
        [ h2 [ class "text-lg font-medium mb-6" ] [ text "Email Activity" ]
        , table [ class "min-w-full" ]
            [ thead [ class "bg-gray-50" ]
                [ tr []
                    [ th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ] [ text "Date" ]
                    , th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ] [ text "Email Type" ]
                    , th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ] [ text "Status" ]
                    , th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase" ] [ text "Mode" ]
                    ]
                ]
            , tbody [ class "divide-y divide-gray-200" ]
                (List.map viewEmailTrackingRow records)
            ]
        ]


viewEmailTrackingRow : EmailTrackingRecord -> Html Msg
viewEmailTrackingRow record =
    tr [ class "hover:bg-gray-50" ]
        [ td [ class "px-3 py-2 text-sm text-gray-900" ] [ text record.scheduledDate ]
        , td [ class "px-3 py-2 text-sm text-gray-900" ] [ text (formatEmailType record.emailType) ]
        , td [ class "px-3 py-2 text-sm" ] [ viewEmailStatus record.sendStatus ]
        , td [ class "px-3 py-2 text-sm text-gray-900" ] [ text (formatSendMode record.sendMode) ]
        ]


formatEmailType : String -> String
formatEmailType emailType =
    case emailType of
        "quote_email" ->
            "Quote Email"

        "follow_up_1" ->
            "Follow-up #1"

        "follow_up_2" ->
            "Follow-up #2"

        "follow_up_3" ->
            "Follow-up #3"

        "birthday" ->
            "Birthday Email"

        "anniversary" ->
            "Anniversary Email"

        _ ->
            emailType


formatSendMode : String -> String
formatSendMode mode =
    case mode of
        "production" ->
            "Production"

        "test" ->
            "Test"

        _ ->
            mode


viewEmailStatus : String -> Html Msg
viewEmailStatus status =
    let
        ( bgColor, textColor, statusText ) =
            case status of
                "sent" ->
                    ( "bg-green-50", "text-green-700", "Sent" )

                "scheduled" ->
                    ( "bg-blue-50", "text-blue-700", "Scheduled" )

                "failed" ->
                    ( "bg-red-50", "text-red-700", "Failed" )

                "opened" ->
                    ( "bg-purple-50", "text-purple-700", "Opened" )

                _ ->
                    ( "bg-gray-50", "text-gray-700", status )
    in
    div [ class ("inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium " ++ bgColor ++ " " ++ textColor) ]
        [ text statusText ]


viewModals : Model -> Html Msg
viewModals model =
    case model.showModal of
        NoModal ->
            text ""

        EditModal ->
            viewEditModal model

        DeleteConfirmModal ->
            viewDeleteConfirmModal model

        HealthAssessmentModal ->
            viewHealthAssessmentModal model


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
    let
        carrierOptions =
            ( "", "Select a carrier" ) :: List.map (\c -> ( c, c )) (model.orgSettings |> Maybe.map .carrierContracts |> Maybe.withDefault []) ++ [ ( "Other", "Other" ) ]

        planTypeOptions =
            [ ( "", "Select a plan type" ), ( "Plan N", "Plan N" ), ( "Plan G", "Plan G" ), ( "Other", "Other" ) ]

        agentOptions =
            case model.orgSettings of
                Just settings ->
                    case settings.allowAgentSettings of
                        True ->
                            -- If agent settings are allowed, show all agents (if available)
                            []

                        -- TODO: Populate with agents if available in model
                        False ->
                            []

                Nothing ->
                    []

        selectedCarrier =
            form.currentCarrier

        selectedPlanType =
            form.planType

        selectedAgentId =
            form.contactOwnerId |> Maybe.map String.fromInt |> Maybe.withDefault ""
    in
    Html.form [ onSubmit SubmitEditForm ]
        [ div [ class "grid grid-cols-1 sm:grid-cols-2 gap-x-4 gap-y-3" ]
            [ viewFormInput "First Name" "text" form.firstName FirstName model True
            , viewFormInput "Last Name" "text" form.lastName LastName model True
            , viewFormInput "Email" "email" form.email Email model True
            , viewFormInput "Phone Number" "text" form.phoneNumber PhoneNumber model False
            , viewFormSelect "Current Carrier" selectedCarrier CurrentCarrier model carrierOptions
            , viewFormSelect "Plan Type" selectedPlanType PlanType model planTypeOptions
            , viewFormInput "Effective Date" "date" form.effectiveDate EffectiveDate model False
            , viewFormInput "Birth Date" "date" form.birthDate BirthDate model False
            , viewFormRadioGroup "Tobacco User"
                (if form.tobaccoUser then
                    "true"

                 else
                    "false"
                )
                TobaccoUser
                model
                [ ( "true", "Yes" ), ( "false", "No" ) ]
            , viewFormRadioGroup "Gender" form.gender Gender model [ ( "M", "Male" ), ( "F", "Female" ) ]
            , div [ class "col-span-1 sm:col-span-2 grid grid-cols-1 sm:grid-cols-2 gap-x-4" ]
                [ viewZipCodeField model form
                , viewStateField form
                ]
            ]
        , if model.error /= Nothing && not model.emailExists then
            div [ class "mt-2 text-red-600 text-xs" ] [ text (Maybe.withDefault "" model.error) ]

          else
            text ""
        , div [ class "mt-4 flex justify-end space-x-2" ]
            [ button
                [ type_ "button"
                , onClick CloseModal
                , class "px-3 py-1.5 bg-white text-gray-700 text-sm font-medium rounded-md border border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200 focus:ring-1 focus:ring-purple-100"
                ]
                [ text "Cancel" ]
            , if isSubmitting then
                div [ class "px-3 py-1.5 flex items-center" ] [ viewSpinner ]

              else
                let
                    isValid =
                        String.length form.firstName > 0 && String.length form.lastName > 0 && String.length form.email > 0 && not model.emailExists && not model.isCheckingEmail
                in
                button
                    [ type_ "submit"
                    , class
                        ("px-3 py-1.5 text-white text-sm font-medium rounded-md transition-colors duration-200 focus:ring-1 focus:ring-purple-200 "
                            ++ (if isValid then
                                    "bg-purple-500 hover:bg-purple-600"

                                else
                                    "bg-gray-300 cursor-not-allowed"
                               )
                        )
                    , Html.Attributes.disabled (not isValid)
                    ]
                    [ text "Save Changes" ]
            ]
        ]


viewFormInput : String -> String -> String -> ContactFormField -> Model -> Bool -> Html Msg
viewFormInput labelText inputType inputValue field model isRequired =
    let
        displayValue =
            if field == PhoneNumber then
                Utils.Formatters.formatPhoneNumber inputValue

            else
                inputValue

        inputHandler =
            if field == PhoneNumber then
                \val -> UpdateEditForm field (String.filter Char.isDigit val |> String.left 10)

            else
                UpdateEditForm field

        placeholderText =
            if field == PhoneNumber then
                "(555) 555-5555"

            else
                ""
    in
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ] [ text labelText ]
        , Html.input
            [ type_ inputType
            , class "w-full px-2 py-1.5 bg-white border-[1.5px] border-purple-300 rounded-md text-sm text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-1 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , Html.Attributes.value displayValue
            , onInput inputHandler
            , required isRequired
            , placeholder placeholderText
            ]
            []
        ]


viewFormSelect : String -> String -> ContactFormField -> Model -> List ( String, String ) -> Html Msg
viewFormSelect labelText selectedValue field model options =
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ] [ text labelText ]
        , Html.select
            [ class "w-full px-2 py-1.5 bg-white border-[1.5px] border-purple-300 rounded-md text-sm text-gray-700 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-1 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , Html.Attributes.value selectedValue
            , onInput (UpdateEditForm field)
            ]
            (List.map (\( val, label ) -> Html.option [ value val, selected (val == selectedValue) ] [ text label ]) options)
        ]


viewFormRadioGroup : String -> String -> ContactFormField -> Model -> List ( String, String ) -> Html Msg
viewFormRadioGroup labelText selectedValue field model options =
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ] [ text labelText ]
        , div [ class "flex gap-2" ]
            (List.map
                (\( val, txt ) ->
                    label
                        [ class
                            ("flex items-center px-2 py-1 rounded-md border text-sm cursor-pointer transition-all duration-200 "
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
                            , onInput (\_ -> UpdateEditForm field val)
                            , class "sr-only"
                            ]
                            []
                        , text txt
                        ]
                )
                options
            )
        ]


viewZipCodeField : Model -> ContactForm -> Html Msg
viewZipCodeField model form =
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ] [ text "ZIP Code" ]
        , Html.input
            [ type_ "text"
            , class "w-full px-2 py-1.5 bg-white border-[1.5px] border-purple-300 rounded-md text-sm text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-1 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , Html.Attributes.value form.zipCode
            , onInput (UpdateEditForm ZipCode)
            ]
            []
        ]


viewStateField : ContactForm -> Html Msg
viewStateField form =
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ] [ text "State" ]
        , Html.input
            [ type_ "text"
            , class "w-full px-2 py-1.5 bg-white border-[1.5px] border-gray-200 rounded-md text-sm text-gray-700 placeholder-gray-400 shadow-sm focus:ring-1 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , Html.Attributes.value form.state
            , Html.Attributes.disabled True
            ]
            []
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "flex justify-center items-center h-64" ]
        [ div [ class "animate-spin rounded-full h-8 w-8 border-2 border-purple-500 border-t-transparent" ] [] ]


viewSpinner : Html msg
viewSpinner =
    div [ class "animate-spin rounded-full h-5 w-5 border-2 border-purple-500 border-t-transparent" ] []



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


isStateActive : EmailSchedule -> Bool
isStateActive schedule =
    List.member schedule.state schedule.stateLicenses



-- Helper function to parse eligibility answers from JSON string


parseEligibilityAnswers : String -> List EligibilityQuestion
parseEligibilityAnswers jsonStr =
    let
        jsonResult =
            Decode.decodeString (Decode.dict eligibilityQuestionDecoder) jsonStr
    in
    case jsonResult of
        Ok dict ->
            Dict.toList dict
                |> List.map
                    (\( idStr, question ) ->
                        let
                            id =
                                String.toInt idStr |> Maybe.withDefault 0
                        in
                        { question | id = id }
                    )
                |> List.sortBy .id

        Err _ ->
            []


eligibilityQuestionDecoder : Decode.Decoder EligibilityQuestion
eligibilityQuestionDecoder =
    Decode.map5 EligibilityQuestion
        (Decode.succeed 0)
        -- Temporary ID that will be replaced
        (Decode.field "question_text" Decode.string)
        (Decode.field "question_type" questionTypeDecoder)
        (Decode.field "answer" answerDecoder)
        (Decode.oneOf
            [ Decode.field "follow_up_questions" (Decode.list followUpQuestionDecoder)
            , Decode.succeed [] -- Default to empty list if missing
            ]
        )


followUpQuestionDecoder : Decode.Decoder EligibilityFollowUp
followUpQuestionDecoder =
    Decode.map3 EligibilityFollowUp
        (Decode.field "id" Decode.int)
        (Decode.field "question_text" Decode.string)
        (Decode.field "answer" answerDecoder)


questionTypeDecoder : Decode.Decoder QuestionType
questionTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\typeStr ->
                if typeStr == "main" then
                    Decode.succeed MainQuestion

                else if String.startsWith "followup_" typeStr then
                    let
                        parentIdStr =
                            String.dropLeft 9 typeStr

                        parentId =
                            String.toInt parentIdStr |> Maybe.withDefault 0
                    in
                    Decode.succeed (FollowUpQuestion parentId)

                else
                    -- Treat other question types as main questions instead of failing
                    Decode.succeed MainQuestion
            )


answerDecoder : Decode.Decoder (Maybe (Either Bool String))
answerDecoder =
    Decode.oneOf
        [ Decode.bool |> Decode.map (\b -> Just (Left b))
        , Decode.string |> Decode.map (\s -> Just (Right s))
        , Decode.null Nothing
        ]


viewHealthAssessmentModal : Model -> Html Msg
viewHealthAssessmentModal model =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-4 sm:p-8 z-50 overflow-auto" ]
        [ div [ class "bg-white rounded-xl shadow-xl w-full max-w-4xl max-h-[90vh] flex flex-col" ]
            [ div [ class "px-6 py-4 border-b border-gray-200 flex justify-between items-center" ]
                [ h2 [ class "text-xl font-semibold text-gray-900" ]
                    [ text "Health Assessment Results" ]
                , button
                    [ class "text-gray-400 hover:text-gray-600 focus:outline-none"
                    , onClick CloseModal
                    ]
                    [ text "×" ]
                ]
            , div [ class "p-6 overflow-auto flex-grow" ]
                [ viewHealthAssessmentContent model.eligibilityQuestions ]
            , div [ class "px-6 py-4 border-t border-gray-200 flex justify-end" ]
                [ button
                    [ class "px-4 py-2 bg-gray-100 text-gray-700 rounded-lg hover:bg-gray-200 font-medium"
                    , onClick CloseModal
                    ]
                    [ text "Close" ]
                ]
            ]
        ]


viewHealthAssessmentContent : List EligibilityQuestion -> Html Msg
viewHealthAssessmentContent questions =
    if List.isEmpty questions then
        div [ class "text-center p-8" ]
            [ div [ class "inline-flex items-center justify-center w-12 h-12 rounded-full bg-gray-100 text-gray-500 mb-4" ]
                [ text "!" ]
            , h3 [ class "text-lg font-medium text-gray-700 mb-2" ]
                [ text "Health Assessment Not Completed" ]
            , p [ class "text-gray-600" ]
                [ text "This contact has not yet completed their health assessment questionnaire." ]
            ]

    else
        let
            mainQuestions =
                List.filter (\q -> q.questionType == MainQuestion) questions
                    |> List.sortBy .id

            hasAnyYes =
                List.any
                    (\q ->
                        case q.answer of
                            Just (Left True) ->
                                q.questionType == MainQuestion

                            _ ->
                                False
                    )
                    questions

            statusBorderClass =
                if hasAnyYes then
                    "border-red-200 bg-red-50"

                else
                    "border-green-200 bg-green-50"

            statusIconClass =
                if hasAnyYes then
                    "bg-red-100 text-red-600"

                else
                    "bg-green-100 text-green-600"

            statusTitleClass =
                if hasAnyYes then
                    "text-red-800"

                else
                    "text-green-800"

            statusTextClass =
                if hasAnyYes then
                    "text-red-700"

                else
                    "text-green-700"
        in
        div []
            [ div [ class ("mb-6 p-4 rounded-lg border " ++ statusBorderClass) ]
                [ div [ class "flex items-center" ]
                    [ span [ class ("inline-flex items-center justify-center w-8 h-8 rounded-full " ++ statusIconClass) ]
                        [ if hasAnyYes then
                            text "!"

                          else
                            text "✓"
                        ]
                    , div [ class "ml-3" ]
                        [ h3 [ class ("font-medium " ++ statusTitleClass) ]
                            [ if hasAnyYes then
                                text "Health Issues Identified"

                              else
                                text "All Health Checks Passed"
                            ]
                        , p [ class ("text-sm " ++ statusTextClass) ]
                            [ if hasAnyYes then
                                text "This contact has flagged health conditions that may affect their eligibility."

                              else
                                text "This contact has no health conditions that would affect their eligibility."
                            ]
                        ]
                    ]
                ]
            , div [ class "space-y-6" ]
                (List.map (viewMainQuestionWithFollowups questions) mainQuestions)
            ]


viewMainQuestionWithFollowups : List EligibilityQuestion -> EligibilityQuestion -> Html Msg
viewMainQuestionWithFollowups allQuestions mainQuestion =
    let
        isYes =
            case mainQuestion.answer of
                Just (Left True) ->
                    True

                _ ->
                    False

        borderClass =
            if isYes then
                "border-red-300"

            else
                "border-gray-200"
    in
    div [ class ("rounded-lg border " ++ borderClass ++ " overflow-hidden") ]
        [ div
            [ class
                ("p-4 "
                    ++ (if isYes then
                            "bg-red-50"

                        else
                            "bg-gray-50"
                       )
                )
            ]
            [ div [ class "flex items-start" ]
                [ div [ class "flex-grow" ]
                    [ div [ class "font-medium mb-1" ]
                        [ text mainQuestion.text ]
                    , div
                        [ class
                            ("text-sm font-medium "
                                ++ (if isYes then
                                        "text-red-700"

                                    else
                                        "text-green-700"
                                   )
                            )
                        ]
                        [ text
                            (if isYes then
                                "Yes"

                             else
                                "No"
                            )
                        ]
                    ]
                ]
            ]
        , if isYes && not (List.isEmpty mainQuestion.followUpQuestions) then
            div [ class "divide-y divide-gray-100" ]
                (List.map viewFollowUpQuestionAnswer mainQuestion.followUpQuestions)

          else
            text ""
        ]


viewFollowUpQuestionAnswer : EligibilityFollowUp -> Html Msg
viewFollowUpQuestionAnswer followUp =
    div [ class "p-4 bg-white" ]
        [ div [ class "font-medium text-sm text-gray-700 mb-1" ]
            [ text followUp.text ]
        , div [ class "text-sm" ]
            [ case followUp.answer of
                Just (Left isYes) ->
                    div
                        [ class
                            (if isYes then
                                "text-red-600 font-medium"

                             else
                                "text-green-600 font-medium"
                            )
                        ]
                        [ text
                            (if isYes then
                                "Yes"

                             else
                                "No"
                            )
                        ]

                Just (Right textAnswer) ->
                    if String.isEmpty textAnswer then
                        div [ class "text-gray-500 italic" ]
                            [ text "No answer provided" ]

                    else
                        div [ class "text-gray-900 bg-gray-50 p-2 rounded border border-gray-200" ]
                            [ text textAnswer ]

                Nothing ->
                    div [ class "text-gray-500 italic" ]
                        [ text "No answer" ]
            ]
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
