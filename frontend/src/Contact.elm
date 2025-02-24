module Contact exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Date exposing (Date)
import Debug
import EmailScheduler exposing (EmailSchedule, PlanType(..), ScheduledEmail, getScheduledEmails, init, viewFutureActivity)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Task
import Time exposing (Month(..))



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
    in
    ( { key = key
      , contact = Nothing
      , showModal = NoModal
      , editForm = emptyForm
      , isSubmittingForm = False
      , error = Nothing
      , activities =
            [ { submissionDate = "11-24-2024"
              , status = QuoteCreated
              , carrierSelected = Just "Allstate"
              , planSelected = Just "Silver Tsunami xJ5"
              , quoteAmount = Just 120.0
              }
            , { submissionDate = "11-24-2024"
              , status = EmailOpened
              , carrierSelected = Nothing
              , planSelected = Nothing
              , quoteAmount = Nothing
              }
            , { submissionDate = "11-24-2024"
              , status = EmailSent 1
              , carrierSelected = Nothing
              , planSelected = Nothing
              , quoteAmount = Nothing
              }
            ]
      , isCheckingEmail = False
      , emailExists = False
      , isDeletingContact = False
      , emailSchedule = initialSchedule
      , quoteUrl = Nothing
      , isGeneratingQuote = False
      }
    , Cmd.batch
        [ Http.get
            { url = "/api/contacts/" ++ contactId
            , expect = Http.expectJson GotContact contactDecoder
            }
        , Task.perform GotCurrentTime Date.today
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | GotContact (Result Http.Error Contact)
    | GotCurrentTime Date
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

                                newSchedule =
                                    EmailScheduler.init
                                        contact.id
                                        effectiveDate
                                        birthDate
                                        model.emailSchedule.currentDate
                                        planType
                            in
                            ( { model | contact = Just contact, emailSchedule = newSchedule }
                            , Http.get
                                { url = "/api/quotes/generate/" ++ String.fromInt contact.id
                                , expect = Http.expectJson GotQuoteLink quoteLinkDecoder
                                }
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

                _ =
                    Debug.log "Current date updated" (Date.toIsoString today)
            in
            ( { model | emailSchedule = newSchedule }
            , Cmd.none
            )

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
                | quoteUrl = Just ("/quote?id=" ++ response.quoteId)
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
                            [ viewHeader contact
                            , viewContactSummary contact model.quoteUrl model.isGeneratingQuote
                            , div [ class "bg-white rounded-lg border border-gray-200 p-6 mb-8" ]
                                [ viewFutureActivity (getScheduledEmails model.emailSchedule) ]
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


viewHeader : Contact -> Html Msg
viewHeader contact =
    div [ class "flex justify-between items-center mb-8" ]
        [ div [ class "flex items-center gap-4" ]
            [ h1 [ class "text-2xl font-semibold" ]
                [ text (contact.firstName ++ " " ++ contact.lastName) ]
            , viewStatus contact.status
            ]
        , div [ class "flex gap-2" ]
            [ button
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


viewContactSummary : Contact -> Maybe String -> Bool -> Html Msg
viewContactSummary contact quoteUrl isGeneratingQuote =
    div [ class "bg-white rounded-lg border border-gray-200 p-6 mb-8" ]
        [ h2 [ class "text-lg font-medium mb-6" ] [ text "Contact Summary" ]
        , div [ class "grid grid-cols-2 gap-x-8 gap-y-6" ]
            [ viewField "Date of Birth" contact.birthDate
            , viewField "Contact Owner" (Maybe.map .firstName contact.contactOwner |> Maybe.withDefault "Default")
            , viewField "Phone Number" (formatPhoneNumber contact.phoneNumber)
            , viewField "Email" contact.email
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
            ]
        , case quoteUrl of
            Just url ->
                div [ class "mt-6 p-4 bg-blue-50 rounded-lg" ]
                    [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                        [ text "Quote Link" ]
                    , a
                        [ href url
                        , class "text-sm text-blue-600 hover:text-blue-800 underline break-all"
                        , target "_blank"
                        ]
                        [ text "View Quote" ]
                    ]

            Nothing ->
                if isGeneratingQuote then
                    div [ class "mt-6 flex justify-center" ]
                        [ viewSpinner ]

                else
                    text ""
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
        [ h2 [ class "text-lg font-medium mb-6" ] [ text "Activity" ]
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



-- HELPERS


stringToPosix : String -> Date
stringToPosix dateString =
    case Date.fromIsoString dateString of
        Ok date ->
            date

        Err _ ->
            -- Default to Unix epoch if invalid date
            Date.fromCalendarDate 1970 Jan 1
