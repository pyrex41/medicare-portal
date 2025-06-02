module Contacts exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

-- Proper imports for CSV parsing

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Csv.Decode as CsvDecode
import Csv.Parser as CsvParser
import CsvProcessor exposing (CarrierMapping, ColumnMapping, extractHeaders, extractUniqueValues, processCsvToContacts, suggestCarrierMappings, suggestColumnMappings)
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select as Select
import Html exposing (Html, button, col, colgroup, details, div, h1, h2, h3, input, label, nav, option, p, select, span, summary, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, disabled, placeholder, required, selected, title, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit, preventDefaultOn, stopPropagationOn)
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, nullable, string)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra
import Process
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, stroke, viewBox)
import Task
import Time
import TutorialModal exposing (viewTutorialModal)
import Url exposing (Url)
import Url.Builder as Url
import Utils.Formatters exposing (formatPhoneNumber)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = \flags url key -> init key ( Nothing, Nothing )
        , view = \model -> { title = "Dashboard", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \_ -> NoOp
        }



-- MODEL


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


type Modal
    = NoModal
    | ContactChoiceModal
    | AddModal
    | EditModal Contact
    | CsvUploadModal UploadState
    | DeleteConfirmModal
    | ReassignAgentModal


type alias Model =
    { contacts : List Contact
    , showTutorialModal : Bool -- ADDED
    , selectedContacts : List Int
    , showModal : Modal
    , searchQuery : String
    , addForm : ContactForm
    , editForm : ContactForm
    , sortColumn : Maybe SortColumn
    , sortDirection : SortDirection
    , activeFilters : Filters
    , openFilter : Maybe FilterType
    , currentTime : Time.Posix
    , isLoadingContacts : Bool
    , isUploadingCsv : Bool
    , isDeletingContacts : Bool
    , isSubmittingForm : Bool
    , isCheckingEmail : Bool
    , emailExists : Bool
    , currentUser : Maybe User
    , showProfileMenu : Bool
    , error : Maybe String
    , saveOnUpdate : Bool
    , expandedContactId : Maybe Int
    , availableFilters : AvailableFilters
    , carriers : List String
    , agents : List User
    , defaultAgentId : Maybe Int -- Added field for default agent ID
    , key : Nav.Key
    , pagination : PaginationState
    , quotesSent : Int
    , manualQuotesSent : Int
    , quotesViewed : Int
    , healthQuestionsCompleted : Int
    , isLoadingDashboardStats : Bool
    , dashboardStatsError : Maybe String
    , initialTotalContacts : Int
    }


type alias ContactForm =
    { id : Maybe Int
    , firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , state : String
    , contactOwnerId : Maybe Int
    , currentCarrier : Maybe String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : Bool
    , gender : String
    , zipCode : String
    , planType : Maybe String
    }


type SortColumn
    = NameCol
    | StatusCol
    | EmailCol
    | PhoneNumberCol
    | StateCol
    | ContactOwnerCol
    | CurrentCarrierCol
    | EffectiveDateCol


type SortDirection
    = Ascending
    | Descending


type alias Filters =
    { carriers : List String
    , states : List String
    , ageRange : Maybe ( Int, Int )
    , agents : List Int
    }


type alias ZipInfo =
    { state : String
    , counties : List String
    , cities : List String
    }


type alias UploadState =
    { dragOver : Bool
    , file : Maybe File
    , error : Maybe String
    , errorCsv : Maybe String
    , converted_carriers_csv : Maybe String
    , stats : Maybe UploadStats
    , overwriteDuplicates : Bool
    , selectedAgentId : Maybe Int
    , columnMapping : Maybe ColumnMapping
    , carrierMapping : Maybe CarrierMapping
    , detectedCarriers : List String
    , csvHeaders : List String
    , processingHeaders : Bool
    , extractingCarriers : Bool
    , invalidEmails : List { email : String, reason : String }
    }


type alias UploadStats =
    { totalRows : Int
    , errorRows : Int
    , validRows : Int
    , converted_carrier_rows : Int
    , supported_carriers : List { name : String, aliases : List String }
    }


type alias DeleteResponse =
    { success : Bool
    , deletedIds : List Int
    , message : String
    }


type alias ReassignResponse =
    { success : Bool
    , updatedIds : List Int
    , message : String
    }


type alias User =
    { id : Int
    , email : String
    , firstName : String
    , lastName : String
    , isAdmin : Bool
    , isAgent : Bool
    , isDefault : Bool
    , organizationId : Int
    , isActive : Bool
    , phone : String
    , carriers : List String
    , stateLicenses : List String
    }


type alias AvailableFilters =
    { carriers : List String
    , states : List String
    }


type alias ContactsResponse =
    { contacts : List Contact
    , filterOptions : AvailableFilters
    , total : Int
    , page : Int
    , limit : Int
    }


type alias PaginationState =
    { currentPage : Int
    , totalPages : Int
    , totalItems : Int
    , itemsPerPage : Int
    }


type alias ColumnMapping =
    { firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , currentCarrier : String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : String
    , gender : String
    , zipCode : String
    , planType : String
    }


type alias CarrierMapping =
    { detectedCarriers : List String
    , mappings : Dict String String -- Original carrier name -> Standardized carrier name
    }


type alias SuggestedMappings =
    { columnMappings : ColumnMapping
    , carrierMappings : Dict String String
    }


emptyAvailableFilters : AvailableFilters
emptyAvailableFilters =
    { carriers = []
    , states = []
    }


init : Nav.Key -> ( Maybe Bool, Maybe User ) -> ( Model, Cmd Msg )
init key ( maybePaymentSuccess, maybeUser ) =
    let
        model =
            { contacts = []
            , showTutorialModal = Maybe.withDefault False maybePaymentSuccess
            , selectedContacts = []
            , showModal = NoModal
            , searchQuery = ""
            , addForm = emptyForm
            , editForm = emptyForm
            , sortColumn = Nothing
            , sortDirection = Ascending
            , activeFilters = emptyFilters
            , openFilter = Nothing
            , currentTime = Time.millisToPosix 0
            , isLoadingContacts = True
            , isUploadingCsv = False
            , isDeletingContacts = False
            , isSubmittingForm = False
            , isCheckingEmail = False
            , emailExists = False
            , currentUser = maybeUser
            , showProfileMenu = False
            , error = Nothing
            , saveOnUpdate = False
            , expandedContactId = Nothing
            , availableFilters = emptyAvailableFilters
            , carriers = []
            , agents = []
            , defaultAgentId = Nothing -- Initialize defaultAgentId
            , key = key
            , pagination = { currentPage = 1, totalPages = 1, totalItems = 0, itemsPerPage = 100 }
            , quotesSent = 0
            , manualQuotesSent = 0
            , quotesViewed = 0
            , healthQuestionsCompleted = 0
            , isLoadingDashboardStats = True
            , dashboardStatsError = Nothing
            , initialTotalContacts = 0
            }
    in
    ( model
    , Cmd.batch
        [ fetchContacts model
        , fetchDashboardStats
        , fetchAgents
        , fetchCarriers
        ]
    )


emptyForm : ContactForm
emptyForm =
    { id = Nothing
    , firstName = ""
    , lastName = ""
    , email = ""
    , phoneNumber = ""
    , state = ""
    , contactOwnerId = Nothing
    , currentCarrier = Nothing
    , effectiveDate = ""
    , birthDate = ""
    , tobaccoUser = False
    , gender = ""
    , zipCode = ""
    , planType = Nothing
    }


emptyFilters : Filters
emptyFilters =
    { carriers = []
    , states = []
    , ageRange = Nothing
    , agents = []
    }


emptyUploadState : Model -> UploadState
emptyUploadState model =
    let
        selectedAgentId =
            case model.currentUser of
                Just user ->
                    if user.isAgent then
                        Just user.id

                    else
                        Nothing

                Nothing ->
                    Nothing

        overwriteOption =
            case model.currentUser of
                Just user ->
                    not (user.isAgent && not user.isAdmin)

                Nothing ->
                    True
    in
    { dragOver = False
    , file = Nothing
    , error = Nothing
    , errorCsv = Nothing
    , converted_carriers_csv = Nothing
    , stats = Nothing
    , overwriteDuplicates = overwriteOption
    , selectedAgentId = selectedAgentId
    , columnMapping = Nothing
    , carrierMapping = Nothing
    , detectedCarriers = []
    , csvHeaders = []
    , processingHeaders = False
    , extractingCarriers = False
    , invalidEmails = []
    }



-- UPDATE


type Msg
    = NoOp
    | OpenTutorialModal
    | CloseTutorialModal
    | ShowContactChoiceModal
    | ChooseSingleContact
    | ChooseMultipleContacts
    | ShowAddModal
    | ShowEditModal Contact
    | CloseModal
    | UpdateSearchQuery String
    | UpdateAddForm ContactFormField String
    | UpdateEditForm ContactFormField String
    | EmailBlur String -- New message for email blur events
    | SubmitAddForm
    | SubmitEditForm
    | CheckEmail String
    | EmailChecked (Result Http.Error { exists : Bool })
    | EmailCheckTimeout
    | GotContacts (Result Http.Error ContactsResponse)
    | ContactAdded (Result Http.Error Contact)
    | ContactUpdated (Result Http.Error Contact)
    | HandleKeyDown String
    | SetSort SortColumn
    | ToggleFilter FilterType String
    | SetAgeFilter Int Int
    | ClearFilters
    | LookupZipCode String
    | GotZipLookup (Result Http.Error ZipInfo)
    | Batch (List Msg)
    | ToggleFilterDropdown FilterType
    | SelectAllFilter FilterType Bool
    | CloseFilterDropdown
    | GotCurrentTime Time.Posix
    | ToggleSelectContact Int
    | SelectAllContacts
    | DeselectAllContacts
    | EmailSelectedCarriers
    | EmailSelectedContacts
    | ShowCsvUploadModal
    | DragEnter
    | DragLeave
    | FileDrop File
    | FileSelected File
    | ClickedSelectFile
    | UploadCsv
    | CsvUploaded (Result Http.Error UploadResponse)
    | DownloadErrorCsv String
    | DownloadCarrierConversionsCsv String
    | ShowDeleteConfirmModal
    | DeleteSelectedContacts
    | ContactsDeleted (Result Http.Error DeleteResponse)
    | ToggleOverwriteDuplicates Bool
    | GotCurrentUser (Result Http.Error User)
    | NavigateToContact Int
    | GotCarriers (Result Http.Error (List String))
    | GotAgents (Result Http.Error AgentsResponseWithDefault)
    | SelectUploadAgent Int
    | ShowReassignAgentModal
    | SelectReassignAgent Int
    | ReassignSelectedContacts
    | ContactsReassigned (Result Http.Error ReassignResponse)
    | ChangePage Int
    | ChangeItemsPerPage Int
    | UpdateColumnMapping String String
    | ExtractCsvHeaders File
    | CsvHeadersExtracted (Result String (List String))
    | SuggestedMappingsReceived (Result Http.Error SuggestedMappings)
    | ExtractCarrierValues
    | CarrierValuesExtracted (List String)
    | UpdateCarrierMapping String String
    | GotCarriersForMapping (List String) (Result Http.Error (List { name : String, aliases : List String }))
    | EmailValidationCompleted (List { email : String, reason : String })
      -- Dashboard stats messages
    | FetchDashboardStats
    | GotDashboardStats (Result Http.Error { success : Bool, stats : { quotesSent : Int, manualQuotesSent : Int, quotesViewed : Int, followUpsRequested : Int, healthQuestionsCompleted : Int } })


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


type FilterType
    = CarrierFilter
    | StateFilter
    | AgeFilter
    | AgentFilter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OpenTutorialModal ->
            ( { model | showTutorialModal = True }, Cmd.none )

        CloseTutorialModal ->
            ( { model | showTutorialModal = False }
            , Nav.pushUrl model.key "/contacts"
            )

        ShowContactChoiceModal ->
            ( { model | showModal = ContactChoiceModal }, Cmd.none )

        ChooseSingleContact ->
            ( { model | showModal = AddModal }, Cmd.none )

        ChooseMultipleContacts ->
            ( { model | showModal = CsvUploadModal (emptyUploadState model) }
            , Cmd.batch [ fetchAgents, fetchCarriers ]
            )

        ShowAddModal ->
            ( { model | showModal = AddModal }, Cmd.none )

        ShowEditModal contact ->
            ( { model
                | showModal = EditModal contact
                , editForm =
                    { id = Just contact.id
                    , firstName = Maybe.withDefault "" contact.firstName
                    , lastName = Maybe.withDefault "" contact.lastName
                    , email = contact.email
                    , phoneNumber = Maybe.withDefault "" contact.phoneNumber
                    , state = Maybe.withDefault "" contact.state
                    , contactOwnerId = contact.contactOwnerId
                    , currentCarrier = contact.currentCarrier
                    , effectiveDate = Maybe.withDefault "" contact.effectiveDate
                    , birthDate = Maybe.withDefault "" contact.birthDate
                    , tobaccoUser = Maybe.withDefault False contact.tobaccoUser
                    , gender = Maybe.withDefault "" contact.gender
                    , zipCode = Maybe.withDefault "" contact.zipCode
                    , planType = contact.planType
                    }
              }
            , Cmd.none
            )

        CloseModal ->
            ( { model
                | showModal = NoModal
                , addForm = emptyForm
                , editForm = emptyForm
                , isCheckingEmail = False
                , emailExists = False
                , error = Nothing
                , isSubmittingForm = False
              }
            , Cmd.none
            )

        UpdateSearchQuery query ->
            let
                updatedModel =
                    { model | searchQuery = query, isLoadingContacts = True }
            in
            ( updatedModel, fetchContacts updatedModel )

        UpdateAddForm field value ->
            let
                form =
                    model.addForm

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
                            { form | currentCarrier = Just value }

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
                            { form | planType = Just value }

                -- Update the command logic to check emails properly
                cmd =
                    if field == ZipCode && String.length value == 5 then
                        submitEditFormWithFlag updatedForm True

                    else
                        Cmd.none
            in
            case model.showModal of
                ContactChoiceModal ->
                    ( model, Cmd.none )

                AddModal ->
                    ( { model
                        | addForm = updatedForm
                        , isCheckingEmail = field == Email && String.contains "@" value && String.length value > 5
                        , emailExists = False
                        , error = Nothing
                      }
                    , cmd
                    )

                EditModal _ ->
                    ( { model | editForm = updatedForm }, cmd )

                NoModal ->
                    ( model, Cmd.none )

                CsvUploadModal _ ->
                    ( model, Cmd.none )

                DeleteConfirmModal ->
                    ( model, Cmd.none )

                ReassignAgentModal ->
                    ( model, Cmd.none )

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
                            { form | currentCarrier = Just value }

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
                            { form | planType = Just value }

                -- Update the command logic
                cmd =
                    if field == ZipCode && String.length value == 5 then
                        LookupZipCode value
                            |> Task.succeed
                            |> Task.perform identity

                    else
                        Cmd.none
            in
            case model.showModal of
                ContactChoiceModal ->
                    ( model, Cmd.none )

                AddModal ->
                    ( { model
                        | addForm = updatedForm
                        , isCheckingEmail = field == Email && String.contains "@" value && String.length value > 5
                        , emailExists = False
                        , error = Nothing
                      }
                    , cmd
                    )

                EditModal _ ->
                    ( { model
                        | editForm = updatedForm
                        , isCheckingEmail = field == Email && String.contains "@" value && String.length value > 5
                        , emailExists = False
                        , error = Nothing
                      }
                    , cmd
                    )

                NoModal ->
                    ( model, Cmd.none )

                CsvUploadModal _ ->
                    ( model, Cmd.none )

                DeleteConfirmModal ->
                    ( model, Cmd.none )

                ReassignAgentModal ->
                    ( model, Cmd.none )

        EmailBlur email ->
            if isValidEmail email then
                ( { model | isCheckingEmail = True }
                , checkEmail email
                )

            else
                ( model, Cmd.none )

        SubmitAddForm ->
            ( { model | isSubmittingForm = True }
            , submitAddForm model.addForm
            )

        SubmitEditForm ->
            ( { model
                | isSubmittingForm = True
                , saveOnUpdate = True
              }
            , submitEditFormWithFlag model.editForm False
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

        EmailChecked (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.BadUrl _ ->
                            "Invalid URL for email check"

                        Http.Timeout ->
                            "Email check timed out"

                        Http.NetworkError ->
                            "Network error during email check"

                        Http.BadStatus code ->
                            "Server error: " ++ String.fromInt code

                        Http.BadBody errBody ->
                            "Invalid response: " ++ errBody
            in
            ( { model
                | isCheckingEmail = False
                , error = Just ("Failed to check email: " ++ errorMsg)
              }
            , Cmd.none
            )

        EmailCheckTimeout ->
            ( { model
                | isCheckingEmail = False
                , error = Just "Email check timed out. Please continue."
              }
            , Cmd.none
            )

        GotContacts (Ok response) ->
            ( { model
                | contacts = response.contacts
                , isLoadingContacts = False
                , availableFilters = response.filterOptions
                , pagination =
                    { currentPage = response.page
                    , totalItems = response.total
                    , itemsPerPage = response.limit
                    , totalPages = ceiling (toFloat response.total / toFloat response.limit)
                    }
                , initialTotalContacts =
                    if model.initialTotalContacts == 0 then
                        response.total

                    else
                        model.initialTotalContacts
              }
            , Cmd.none
            )

        GotContacts (Err error) ->
            ( { model | error = Just "Failed to load contacts" }, Cmd.none )

        ContactAdded (Ok contact) ->
            ( { model
                | contacts = contact :: model.contacts
                , showModal = NoModal
                , addForm = emptyForm
                , isSubmittingForm = False
              }
            , Cmd.none
            )

        ContactAdded (Err _) ->
            ( { model | isSubmittingForm = False }
            , Cmd.none
            )

        ContactUpdated (Ok contact) ->
            let
                updatedContacts =
                    updateContact contact model.contacts

                updatedModel =
                    if model.saveOnUpdate then
                        -- Close the modal for a final save
                        { model
                            | contacts = updatedContacts
                            , showModal = NoModal
                            , editForm = emptyForm
                            , isSubmittingForm = False
                            , error = Nothing
                            , saveOnUpdate = False
                        }

                    else
                        -- Just update the state field in the form
                        { model
                            | contacts = updatedContacts
                            , editForm =
                                model.editForm
                                    |> (\form -> { form | state = Maybe.withDefault "" contact.state })
                            , isSubmittingForm = False
                            , error = Nothing
                        }
            in
            ( updatedModel, Cmd.none )

        ContactUpdated (Err error) ->
            ( { model
                | isSubmittingForm = False
                , error = Just "Failed to update contact. Please check the ZIP code is valid."
              }
            , Cmd.none
            )

        HandleKeyDown key ->
            if key == "Escape" then
                ( { model | showModal = NoModal }, Cmd.none )

            else
                ( model, Cmd.none )

        SetSort column ->
            let
                ( newColumn, newDirection ) =
                    case ( model.sortColumn, model.sortDirection ) of
                        ( Just currentColumn, direction ) ->
                            if currentColumn == column then
                                -- Toggle direction if same column
                                ( Just column
                                , if direction == Ascending then
                                    Descending

                                  else
                                    Ascending
                                )

                            else
                                -- New column, start with ascending
                                ( Just column, Ascending )

                        ( Nothing, _ ) ->
                            -- First time sorting, start with ascending
                            ( Just column, Ascending )
            in
            ( { model
                | sortColumn = newColumn
                , sortDirection = newDirection
              }
            , Cmd.none
            )

        ToggleFilter filterType value ->
            let
                updatedModel =
                    { model
                        | activeFilters = toggleFilter model.activeFilters filterType value
                        , isLoadingContacts = True
                    }
            in
            ( updatedModel, fetchContacts updatedModel )

        SetAgeFilter min max ->
            ( { model | activeFilters = setAgeFilter min max model.activeFilters }, Cmd.none )

        ClearFilters ->
            ( { model | activeFilters = emptyFilters }, Cmd.none )

        LookupZipCode zipCode ->
            ( model
            , Http.get
                { url = "/api/zip-lookup/" ++ zipCode
                , expect = Http.expectJson GotZipLookup zipInfoDecoder
                }
            )

        GotZipLookup (Ok zipInfo) ->
            let
                updateForm form =
                    { form | state = zipInfo.state }
            in
            case model.showModal of
                ContactChoiceModal ->
                    ( model, Cmd.none )

                AddModal ->
                    ( { model | addForm = updateForm model.addForm }, Cmd.none )

                EditModal _ ->
                    ( { model | editForm = updateForm model.editForm }, Cmd.none )

                NoModal ->
                    ( model, Cmd.none )

                CsvUploadModal _ ->
                    ( model, Cmd.none )

                DeleteConfirmModal ->
                    ( model, Cmd.none )

                ReassignAgentModal ->
                    ( model, Cmd.none )

        GotZipLookup (Err _) ->
            ( model, Cmd.none )

        Batch messages ->
            List.foldl
                (\msg_ ( model_, cmds ) ->
                    let
                        ( newModel, newCmd ) =
                            update msg_ model_
                    in
                    ( newModel, newCmd :: cmds )
                )
                ( model, [] )
                messages
                |> (\( m, cs ) -> ( m, Cmd.batch cs ))

        ToggleFilterDropdown filterType ->
            ( { model
                | openFilter =
                    if model.openFilter == Just filterType then
                        Nothing

                    else
                        Just filterType
              }
            , Cmd.none
            )

        SelectAllFilter filterType select ->
            let
                options =
                    case filterType of
                        CarrierFilter ->
                            model.availableFilters.carriers

                        StateFilter ->
                            model.availableFilters.states

                        AgentFilter ->
                            model.agents
                                --|> List.filter (\agent -> agent.isAgent)
                                |> List.map (\agent -> agent.firstName ++ " " ++ agent.lastName)

                        _ ->
                            []

                updatedFilters =
                    case filterType of
                        CarrierFilter ->
                            { carriers =
                                if select then
                                    options

                                else
                                    []
                            , states = model.activeFilters.states
                            , ageRange = model.activeFilters.ageRange
                            , agents = model.activeFilters.agents
                            }

                        StateFilter ->
                            { carriers = model.activeFilters.carriers
                            , states =
                                if select then
                                    options

                                else
                                    []
                            , ageRange = model.activeFilters.ageRange
                            , agents = model.activeFilters.agents
                            }

                        AgentFilter ->
                            { carriers = model.activeFilters.carriers
                            , states = model.activeFilters.states
                            , ageRange = model.activeFilters.ageRange
                            , agents =
                                if select then
                                    model.agents
                                        --|> List.filter (\agent -> agent.isAgent)
                                        |> List.map (\agent -> agent.id)

                                else
                                    []
                            }

                        _ ->
                            model.activeFilters

                updatedModel =
                    { model | activeFilters = updatedFilters }
            in
            ( updatedModel, fetchContacts updatedModel )

        CloseFilterDropdown ->
            -- Only close the dropdown, don't prevent further events
            ( { model | openFilter = Nothing }, Cmd.none )

        GotCurrentTime time ->
            ( { model | currentTime = time }, Cmd.none )

        ToggleSelectContact id ->
            ( { model
                | selectedContacts =
                    if List.member id model.selectedContacts then
                        List.filter (\x -> x /= id) model.selectedContacts

                    else
                        id :: model.selectedContacts
              }
            , Cmd.none
            )

        SelectAllContacts ->
            let
                visibleContacts =
                    model.contacts
                        |> filterContacts model.activeFilters model.searchQuery model.currentTime model.defaultAgentId
                        |> List.map .id
            in
            ( { model | selectedContacts = visibleContacts }
            , Cmd.none
            )

        DeselectAllContacts ->
            ( { model | selectedContacts = [] }
            , Cmd.none
            )

        EmailSelectedCarriers ->
            -- For now, just a placeholder that does nothing
            ( model, Cmd.none )

        EmailSelectedContacts ->
            -- For now, just a placeholder that does nothing
            ( model, Cmd.none )

        ShowCsvUploadModal ->
            ( { model | showModal = CsvUploadModal (emptyUploadState model) }
            , Cmd.batch [ fetchAgents, fetchCarriers ]
            )

        DragEnter ->
            case model.showModal of
                CsvUploadModal state ->
                    ( { model | showModal = CsvUploadModal { state | dragOver = True } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DragLeave ->
            case model.showModal of
                CsvUploadModal state ->
                    ( { model | showModal = CsvUploadModal { state | dragOver = False } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FileDrop file ->
            case model.showModal of
                CsvUploadModal state ->
                    ( { model | showModal = CsvUploadModal { state | file = Just file, dragOver = False, processingHeaders = True } }
                    , extractCsvHeaders file
                    )

                _ ->
                    ( model, Cmd.none )

        FileSelected file ->
            case model.showModal of
                CsvUploadModal state ->
                    ( { model | showModal = CsvUploadModal { state | file = Just file, processingHeaders = True } }
                    , extractCsvHeaders file
                    )

                _ ->
                    ( model, Cmd.none )

        ClickedSelectFile ->
            ( model
            , Select.file [ "text/csv" ] FileSelected
            )

        UploadCsv ->
            case model.showModal of
                CsvUploadModal state ->
                    case state.file of
                        Just file ->
                            ( { model
                                | showModal = CsvUploadModal { state | error = Nothing, errorCsv = Nothing, stats = Nothing }
                                , isUploadingCsv = True
                              }
                            , uploadCsv file state.overwriteDuplicates state.selectedAgentId model
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CsvUploaded (Ok response) ->
            let
                currentModal =
                    case model.showModal of
                        CsvUploadModal state ->
                            if response.success then
                                NoModal

                            else
                                CsvUploadModal
                                    { state
                                        | error = Just response.message
                                    }

                        _ ->
                            model.showModal

                updatedModel =
                    { model
                        | showModal = currentModal
                        , isUploadingCsv = False
                    }
            in
            ( updatedModel
            , if response.success then
                fetchContacts updatedModel

              else
                Cmd.none
            )

        CsvUploaded (Err httpError) ->
            let
                errorMessage =
                    case httpError of
                        Http.BadUrl url ->
                            "Invalid URL: " ++ url

                        Http.BadStatus statusCode ->
                            if statusCode == 400 then
                                "The CSV format is invalid. Please check that all required columns are present and data is in the correct format."

                            else if statusCode == 413 then
                                "The file is too large. Please try a smaller file or split your data into multiple uploads."

                            else if statusCode == 403 then
                                "You don't have permission to upload contacts. Please contact your administrator."

                            else
                                "Server error (status " ++ String.fromInt statusCode ++ "). Please try again later."

                        Http.BadBody responseBody ->
                            "The server response was not in the expected format: " ++ responseBody

                        Http.NetworkError ->
                            "Network error. Please check your connection and try again."

                        Http.Timeout ->
                            "The upload timed out. Please try again."
            in
            case model.showModal of
                CsvUploadModal state ->
                    ( { model
                        | showModal = CsvUploadModal { state | error = Just errorMessage }
                        , isUploadingCsv = False
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DownloadErrorCsv csvContent ->
            ( model
            , File.Download.string "upload_errors.csv" "text/csv" csvContent
            )

        DownloadCarrierConversionsCsv csvContent ->
            ( model
            , File.Download.string "carrier_conversions.csv" "text/csv" csvContent
            )

        ShowDeleteConfirmModal ->
            ( { model | showModal = DeleteConfirmModal }, Cmd.none )

        DeleteSelectedContacts ->
            ( { model | isDeletingContacts = True, showModal = NoModal }
            , if List.isEmpty model.selectedContacts then
                Cmd.none

              else
                deleteContacts model.selectedContacts
            )

        ContactsDeleted (Ok response) ->
            if response.success then
                let
                    updatedModel =
                        { model
                            | contacts = List.filter (\c -> not (List.member c.id response.deletedIds)) model.contacts
                            , selectedContacts = []
                            , isDeletingContacts = False
                        }
                in
                ( updatedModel
                , fetchContacts updatedModel
                )

            else
                ( { model | isDeletingContacts = False }, Cmd.none )

        ContactsDeleted (Err _) ->
            ( model, Cmd.none )

        ToggleOverwriteDuplicates value ->
            case model.showModal of
                CsvUploadModal state ->
                    ( { model | showModal = CsvUploadModal { state | overwriteDuplicates = value } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotCurrentUser (Ok user) ->
            ( { model | currentUser = Just user }, Cmd.none )

        GotCurrentUser (Err _) ->
            ( model, Cmd.none )

        NavigateToContact id ->
            ( model, Nav.pushUrl model.key ("/contact/" ++ String.fromInt id) )

        GotCarriers (Ok carriers) ->
            ( { model | carriers = carriers }
            , Cmd.none
            )

        GotCarriers (Err _) ->
            ( model, Cmd.none )

        GotAgents (Ok response) ->
            let
                parser : String -> String
                parser =
                    Url.percentDecode >> Maybe.withDefault ""

                parseAgent : User -> User
                parseAgent agent =
                    { agent | firstName = parser agent.firstName, lastName = parser agent.lastName, email = parser agent.email }
            in
            ( { model
                | agents = response.agents |> List.map parseAgent
                , defaultAgentId = response.defaultAgentId |> Maybe.andThen String.toInt
              }
            , Cmd.none
            )

        GotAgents (Err error) ->
            ( model, Cmd.none )

        SelectUploadAgent agentId ->
            case model.showModal of
                CsvUploadModal state ->
                    ( { model | showModal = CsvUploadModal { state | selectedAgentId = Just agentId } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ShowReassignAgentModal ->
            let
                -- Get the first agent ID as the default selected agent
                defaultAgentId =
                    model.agents
                        |> List.head
                        |> Maybe.map .id

                -- Update the edit form with the selected agent
                updatedForm =
                    model.editForm

                updatedFormWithAgent =
                    { updatedForm | contactOwnerId = defaultAgentId }
            in
            ( { model | showModal = ReassignAgentModal, editForm = updatedFormWithAgent }, Cmd.none )

        SelectReassignAgent agentId ->
            let
                updatedForm =
                    model.editForm

                updatedFormWithAgent =
                    { updatedForm | contactOwnerId = Just agentId }
            in
            ( { model | editForm = updatedFormWithAgent }
            , Cmd.none
            )

        ReassignSelectedContacts ->
            if List.isEmpty model.selectedContacts then
                ( model, Cmd.none )

            else
                case model.editForm.contactOwnerId of
                    Just agentId ->
                        if agentId == 0 then
                            -- If "Default" (0) is selected, pass null for agent_id
                            ( { model | showModal = NoModal }
                            , reassignContacts model.selectedContacts 0
                            )

                        else
                            -- Normal agent reassignment
                            ( { model | showModal = NoModal }
                            , reassignContacts model.selectedContacts agentId
                            )

                    Nothing ->
                        ( { model | error = Just "Please select an agent to reassign contacts to" }
                        , Cmd.none
                        )

        ContactsReassigned (Ok response) ->
            let
                updatedModel =
                    { model | showModal = NoModal, selectedContacts = [], editForm = emptyForm }
            in
            ( updatedModel, fetchContacts updatedModel )

        ContactsReassigned (Err _) ->
            ( { model | error = Just "Failed to reassign contacts", showModal = NoModal }, Cmd.none )

        ChangePage page ->
            let
                updatedModel =
                    { model
                        | pagination =
                            model.pagination
                                |> (\p -> { p | currentPage = page })
                        , isLoadingContacts = True
                    }
            in
            ( updatedModel
            , fetchContacts updatedModel
            )

        ChangeItemsPerPage limit ->
            let
                updatedModel =
                    { model
                        | pagination =
                            model.pagination
                                |> (\p -> { p | itemsPerPage = limit, currentPage = 1 })
                        , isLoadingContacts = True
                    }
            in
            ( updatedModel
            , fetchContacts updatedModel
            )

        UpdateColumnMapping field value ->
            case model.showModal of
                CsvUploadModal state ->
                    let
                        updatedMapping =
                            case state.columnMapping of
                                Just mapping ->
                                    Just
                                        (case field of
                                            "firstName" ->
                                                { mapping | firstName = value }

                                            "lastName" ->
                                                { mapping | lastName = value }

                                            "email" ->
                                                { mapping | email = value }

                                            "phoneNumber" ->
                                                { mapping | phoneNumber = value }

                                            "currentCarrier" ->
                                                { mapping | currentCarrier = value }

                                            "effectiveDate" ->
                                                { mapping | effectiveDate = value }

                                            "birthDate" ->
                                                { mapping | birthDate = value }

                                            "tobaccoUser" ->
                                                { mapping | tobaccoUser = value }

                                            "gender" ->
                                                { mapping | gender = value }

                                            "zipCode" ->
                                                { mapping | zipCode = value }

                                            "planType" ->
                                                { mapping | planType = value }

                                            _ ->
                                                mapping
                                        )

                                Nothing ->
                                    Just
                                        { firstName =
                                            if field == "firstName" then
                                                value

                                            else
                                                ""
                                        , lastName =
                                            if field == "lastName" then
                                                value

                                            else
                                                ""
                                        , email =
                                            if field == "email" then
                                                value

                                            else
                                                ""
                                        , phoneNumber =
                                            if field == "phoneNumber" then
                                                value

                                            else
                                                ""
                                        , currentCarrier =
                                            if field == "currentCarrier" then
                                                value

                                            else
                                                ""
                                        , effectiveDate =
                                            if field == "effectiveDate" then
                                                value

                                            else
                                                ""
                                        , birthDate =
                                            if field == "birthDate" then
                                                value

                                            else
                                                ""
                                        , tobaccoUser =
                                            if field == "tobaccoUser" then
                                                value

                                            else
                                                ""
                                        , gender =
                                            if field == "gender" then
                                                value

                                            else
                                                ""
                                        , zipCode =
                                            if field == "zipCode" then
                                                value

                                            else
                                                ""
                                        , planType =
                                            if field == "planType" then
                                                value

                                            else
                                                ""
                                        }
                    in
                    ( { model | showModal = CsvUploadModal { state | columnMapping = updatedMapping } }
                    , case ( field, state.file, updatedMapping ) of
                        ( "currentCarrier", Just file, Just mapping ) ->
                            if mapping.currentCarrier == "" then
                                Cmd.none

                            else
                                Task.perform
                                    (\content -> CarrierValuesExtracted (extractUniqueCarriers content mapping.currentCarrier))
                                    (File.toString file)

                        _ ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ExtractCsvHeaders file ->
            ( model
            , Task.perform
                (\content -> CsvHeadersExtracted (extractHeadersFromCsv content))
                (File.toString file)
            )

        CsvHeadersExtracted result ->
            case model.showModal of
                CsvUploadModal state ->
                    case result of
                        Ok headers ->
                            ( { model | showModal = CsvUploadModal { state | csvHeaders = headers, processingHeaders = False } }
                            , suggestMappings headers
                            )

                        Err error ->
                            ( { model | showModal = CsvUploadModal { state | error = Just ("Failed to parse CSV headers: " ++ error), processingHeaders = False } }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        SuggestedMappingsReceived result ->
            case model.showModal of
                CsvUploadModal state ->
                    case result of
                        Ok suggestions ->
                            let
                                hasCarrierColumn =
                                    case suggestions.columnMappings.currentCarrier of
                                        "" ->
                                            False

                                        _ ->
                                            True

                                updatedState =
                                    { state
                                        | columnMapping = Just suggestions.columnMappings
                                        , carrierMapping = Just { detectedCarriers = state.detectedCarriers, mappings = suggestions.carrierMappings }
                                        , extractingCarriers = hasCarrierColumn
                                    }

                                -- Create commands to run
                                commands =
                                    []

                                -- Add carrier extraction command if needed
                                commands1 =
                                    case ( hasCarrierColumn, state.file ) of
                                        ( True, Just file ) ->
                                            Task.perform
                                                (\content -> CarrierValuesExtracted (extractUniqueCarriers content suggestions.columnMappings.currentCarrier))
                                                (File.toString file)
                                                :: commands

                                        _ ->
                                            commands

                                -- Add email validation command
                                commands2 =
                                    case state.file of
                                        Just file ->
                                            if not (String.isEmpty suggestions.columnMappings.email) then
                                                let
                                                    dummyCarrierMapping =
                                                        { detectedCarriers = []
                                                        , mappings = Dict.empty
                                                        }
                                                in
                                                Task.perform
                                                    (\content ->
                                                        case CsvProcessor.processCsvToContacts content suggestions.columnMappings dummyCarrierMapping of
                                                            Ok result1 ->
                                                                EmailValidationCompleted (List.map (\contact -> { email = contact.email, reason = contact.reason }) result1.invalid)

                                                            Err _ ->
                                                                EmailValidationCompleted []
                                                    )
                                                    (File.toString file)
                                                    :: commands1

                                            else
                                                commands1

                                        Nothing ->
                                            commands1
                            in
                            ( { model | showModal = CsvUploadModal updatedState }
                            , Cmd.batch commands2
                            )

                        Err _ ->
                            ( { model | showModal = CsvUploadModal { state | error = Just "Failed to get column suggestions" } }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        ExtractCarrierValues ->
            case model.showModal of
                CsvUploadModal state ->
                    case state.file of
                        Just file ->
                            -- Get carrier column from the column mapping
                            let
                                carrierColumn =
                                    case state.columnMapping of
                                        Just mapping ->
                                            if String.isEmpty mapping.currentCarrier then
                                                Nothing

                                            else
                                                Just mapping.currentCarrier

                                        Nothing ->
                                            Nothing
                            in
                            case carrierColumn of
                                Just column ->
                                    ( { model | showModal = CsvUploadModal { state | extractingCarriers = True } }
                                    , Task.perform
                                        (\content -> CarrierValuesExtracted (extractUniqueCarriers content column))
                                        (File.toString file)
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CarrierValuesExtracted carrierValues ->
            case model.showModal of
                CsvUploadModal state ->
                    -- Instead of making a new API call, create a structure from the model's carriers list
                    let
                        -- Create a map of common carrier aliases for better matching
                        carrierAliases =
                            Dict.fromList
                                [ ( "Aetna", [ "aetna medicare", "aetna advantage", "aetna health", "aetna cvs" ] )
                                , ( "United Healthcare", [ "uhc", "united health", "united", "aarp", "aarp / uhc", "aarp/uhc" ] )
                                , ( "Humana", [ "humana gold", "humana gold plus", "humana choice", "humana value", "humana preferred" ] )
                                , ( "Cigna", [ "cigna healthspring", "cigna-healthspring", "cigna health", "connecticut general" ] )
                                , ( "Blue Cross Blue Shield", [ "bcbs", "blue cross", "blue shield", "anthem", "anthem bcbs" ] )
                                , ( "Wellcare", [ "wellcare by allwell", "allwell", "wellcare value", "wellcare essentials" ] )
                                , ( "Kaiser Permanente", [ "kaiser", "kp", "kaiser senior advantage" ] )
                                , ( "Anthem", [ "anthem blue", "anthem medicare", "anthem medigap" ] )
                                , ( "Molina", [ "molina healthcare", "molina advantage", "molina health" ] )
                                , ( "Ace Chubb", [ "ace / chubb", "ace/chubb", "ace chubb", "chubb" ] )
                                ]

                        -- Convert the simple carriers string list to the format expected by CsvProcessor
                        -- and enhance with known aliases
                        standardCarriers =
                            List.map
                                (\name ->
                                    { name = name
                                    , aliases = Dict.get name carrierAliases |> Maybe.withDefault []
                                    }
                                )
                                model.carriers

                        -- Use CsvProcessor to suggest carrier mappings
                        carrierMapping =
                            CsvProcessor.suggestCarrierMappings carrierValues standardCarriers

                        -- Add validation command if we have all necessary components
                        validateEmails =
                            case ( state.file, state.columnMapping ) of
                                ( Just file, Just colMapping ) ->
                                    if String.isEmpty colMapping.email then
                                        Cmd.none

                                    else
                                        Task.perform
                                            (\content ->
                                                case CsvProcessor.processCsvToContacts content colMapping carrierMapping of
                                                    Ok result ->
                                                        EmailValidationCompleted (List.map (\contact -> { email = contact.email, reason = contact.reason }) result.invalid)

                                                    Err _ ->
                                                        EmailValidationCompleted []
                                            )
                                            (File.toString file)

                                _ ->
                                    Cmd.none
                    in
                    ( { model
                        | showModal =
                            CsvUploadModal
                                { state
                                    | detectedCarriers = carrierValues
                                    , carrierMapping = Just carrierMapping
                                    , extractingCarriers = False
                                }
                      }
                    , validateEmails
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateCarrierMapping original mapped ->
            case model.showModal of
                CsvUploadModal state ->
                    case state.carrierMapping of
                        Just mapping ->
                            let
                                updatedMappings =
                                    Dict.insert original mapped mapping.mappings

                                updatedMapping =
                                    { mapping | mappings = updatedMappings }
                            in
                            ( { model | showModal = CsvUploadModal { state | carrierMapping = Just updatedMapping } }
                            , case ( original, state.file ) of
                                ( "currentCarrier", Just file ) ->
                                    Task.perform
                                        (\content -> CarrierValuesExtracted (extractUniqueCarriers content mapped))
                                        (File.toString file)

                                _ ->
                                    Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotCarriersForMapping detectedCarriers (Ok carriers) ->
            let
                -- Use CsvProcessor to suggest carrier mappings with fuzzy matching
                carrierMapping =
                    CsvProcessor.suggestCarrierMappings detectedCarriers carriers
            in
            case model.showModal of
                CsvUploadModal state ->
                    ( { model
                        | showModal =
                            CsvUploadModal
                                { state
                                    | carrierMapping = Just carrierMapping
                                    , extractingCarriers = False
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotCarriersForMapping _ (Err error) ->
            case model.showModal of
                CsvUploadModal state ->
                    ( { model
                        | showModal =
                            CsvUploadModal
                                { state
                                    | extractingCarriers = False
                                    , error = Just "Failed to fetch carrier data for matching"
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EmailValidationCompleted invalidEmails ->
            case model.showModal of
                CsvUploadModal state ->
                    ( { model | showModal = CsvUploadModal { state | invalidEmails = invalidEmails } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FetchDashboardStats ->
            ( { model | isLoadingDashboardStats = True, dashboardStatsError = Nothing }
            , fetchDashboardStats
            )

        GotDashboardStats result ->
            case result of
                Ok response ->
                    if response.success then
                        let
                            totalQuotesSent =
                                response.stats.quotesSent + response.stats.manualQuotesSent
                        in
                        ( { model
                            | isLoadingDashboardStats = False
                            , quotesSent = totalQuotesSent
                            , manualQuotesSent = response.stats.manualQuotesSent
                            , quotesViewed = response.stats.quotesViewed
                            , healthQuestionsCompleted = response.stats.healthQuestionsCompleted
                            , dashboardStatsError = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | isLoadingDashboardStats = False, dashboardStatsError = Just "Failed to load dashboard data." }
                        , Cmd.none
                        )

                Err httpError ->
                    ( { model | isLoadingDashboardStats = False, dashboardStatsError = Just "Error loading dashboard stats." }
                    , Cmd.none
                    )



-- TODO: Handle error
-- Add other update cases here...
-- VIEW


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-white" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
            [ -- Stats Section - Make more compact with reduced margins
              if model.showTutorialModal then
                viewTutorialModal CloseTutorialModal

              else
                text ""
            , div [ class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 sm:gap-6 mb-6" ]
                [ if model.initialTotalContacts == 0 && model.isLoadingContacts then
                    statsCardWithSpinner "Total Contacts"

                  else
                    statsCard "Total Contacts" (String.fromInt model.initialTotalContacts)
                , if model.isLoadingDashboardStats then
                    statsCardWithSpinner "Quotes Sent"

                  else if model.dashboardStatsError /= Nothing then
                    statsCard "Quotes Sent" "0"

                  else
                    statsCard "Quotes Sent" (String.fromInt model.quotesSent)
                , if model.isLoadingDashboardStats then
                    statsCardWithSpinner "Quotes Viewed"

                  else if model.dashboardStatsError /= Nothing then
                    statsCard "Quotes Viewed" "0"

                  else
                    statsCard "Quotes Viewed" (String.fromInt model.quotesViewed)
                , if model.isLoadingDashboardStats then
                    statsCardWithSpinner "Health Questions"

                  else if model.dashboardStatsError /= Nothing then
                    statsCard "Health Questions" "0"

                  else
                    statsCard "Health Questions" (String.fromInt model.healthQuestionsCompleted)
                ]
            , -- Table Container with overflow handling - reduced vertical spacing
              div [ class "overflow-x-auto max-w-7xl mx-auto min-h-[20rem]" ]
                [ -- Add a container around both the header and the table to ensure they have the same width
                  div [ class "w-full" ]
                    [ -- Contacts header and filters - reduced margin bottom
                      div [ class "flex justify-between items-center mb-3 w-full" ]
                        [ div [ class "flex items-center gap-2" ]
                            [ h1 [ class "text-base font-semibold" ] [ text "Contacts " ]
                            , span [ class "text-sm text-gray-500" ]
                                [ text ("(" ++ String.fromInt model.pagination.totalItems ++ ")") ]
                            ]
                        , div [ class "flex items-center gap-2" ]
                            [ -- Only show Agent filter for admins
                              if isAdminOrAdminAgent model.currentUser then
                                div [ class "relative" ]
                                    [ button
                                        [ class "inline-flex items-center gap-1 px-2 py-1 border rounded-md text-sm text-gray-700 hover:bg-gray-50"
                                        , onClick (ToggleFilterDropdown AgentFilter)
                                        , Html.Events.stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
                                        ]
                                        [ text "Agent"
                                        , viewIcon "M19 9l-7 7-7-7"
                                        ]
                                    , if model.openFilter == Just AgentFilter then
                                        viewFilterDropdown model AgentFilter

                                      else
                                        text ""
                                    ]

                              else
                                text ""
                            , div [ class "relative" ]
                                [ button
                                    [ class "inline-flex items-center gap-1 px-2 py-1 border rounded-md text-sm text-gray-700 hover:bg-gray-50"
                                    , onClick (ToggleFilterDropdown CarrierFilter)
                                    , Html.Events.stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
                                    ]
                                    [ text "Carrier"
                                    , viewIcon "M19 9l-7 7-7-7"
                                    ]
                                , if model.openFilter == Just CarrierFilter then
                                    viewFilterDropdown model CarrierFilter

                                  else
                                    text ""
                                ]
                            , div [ class "relative" ]
                                [ button
                                    [ class "inline-flex items-center gap-1 px-2 py-1 border rounded-md text-sm text-gray-700 hover:bg-gray-50"
                                    , onClick (ToggleFilterDropdown StateFilter)
                                    , Html.Events.stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
                                    ]
                                    [ text "State"
                                    , viewIcon "M19 9l-7 7-7-7"
                                    ]
                                , if model.openFilter == Just StateFilter then
                                    viewFilterDropdown model StateFilter

                                  else
                                    text ""
                                ]
                            , div [ class "relative" ]
                                [ input
                                    [ class "w-48 px-2 py-1 border rounded-md text-sm placeholder-gray-500 focus:ring-1 focus:ring-indigo-500 focus:border-indigo-500"
                                    , placeholder "Search contacts..."
                                    , value model.searchQuery
                                    , onInput UpdateSearchQuery
                                    ]
                                    []
                                ]
                            , -- Add the add contact button with a different style
                              button
                                [ class "px-2 py-1 bg-black text-white rounded-md text-sm hover:bg-gray-800 transition-colors"
                                , onClick ShowContactChoiceModal
                                ]
                                [ text "+ Add Contact" ]
                            ]
                        ]
                    , table [ class "w-full table-fixed" ]
                        [ colgroup []
                            [ col [ class "w-12" ] [] -- Checkbox (3rem = 48px)
                            , col [ class "w-40" ] [] -- Name (10rem = 160px)
                            , col [ class "w-56" ] [] -- Email (14rem = 224px)
                            , col [ class "w-32" ] [] -- Phone (8rem = 128px)
                            , col [ class "w-16" ] [] -- State (4rem = 64px)
                            , col [ class "w-36" ] [] -- Agent (9rem = 144px)
                            , col [ class "w-28" ] [] -- Carrier (7rem = 112px)
                            , col [ class "w-40" ] [] -- Eff. Date (10rem = 160px) - INCREASED
                            , col [ class "w-16" ] [] -- Edit (4rem = 64px)
                            ]
                        , thead [ class "bg-gray-50" ]
                            [ tr []
                                [ th [ class "sticky top-0 px-2 py-1 border-b border-gray-200 bg-gray-50" ]
                                    [ input
                                        [ type_ "checkbox"
                                        , class "rounded border-gray-300 text-purple-600 focus:ring-purple-500"
                                        , checked (not (List.isEmpty model.contacts) && List.length model.selectedContacts == List.length model.contacts)
                                        , onClick
                                            (if not (List.isEmpty model.contacts) && List.length model.selectedContacts == List.length model.contacts then
                                                DeselectAllContacts

                                             else
                                                SelectAllContacts
                                            )
                                        ]
                                        []
                                    ]
                                , tableHeader "Name"
                                , tableHeader "Email"
                                , tableHeader "Phone"
                                , tableHeader "State"
                                , tableHeader "Agent"
                                , tableHeader "Carrier"
                                , tableHeader "Eff. Date"
                                , tableHeader "Edit"
                                ]
                            ]
                        , tbody [ class "bg-white" ]
                            (if model.isLoadingContacts then
                                [ tr []
                                    [ td
                                        [ class "px-3 py-8 text-sm text-gray-500 text-center border-t border-gray-200"
                                        , attribute "colspan" "9"
                                        ]
                                        [ div [ class "flex items-center justify-center gap-3" ]
                                            [ viewSpinner
                                            , text "Loading contacts..."
                                            ]
                                        ]
                                    ]
                                ]

                             else if List.isEmpty model.contacts then
                                [ tr []
                                    [ td
                                        [ class "px-3 py-2 text-sm text-gray-500 text-center border-t border-gray-200"
                                        , attribute "colspan" "9"
                                        ]
                                        [ text "No contacts found" ]
                                    ]
                                ]

                             else
                                List.concatMap (viewTableRow model) model.contacts
                            )
                        ]
                    ]
                ]
            ]
        , viewModals model
        , if not (List.isEmpty model.selectedContacts) then
            viewBulkActionBar model

          else
            text ""
        , viewPaginationControls model -- Add pagination controls
        ]


viewBulkActionBar : Model -> Html Msg
viewBulkActionBar model =
    let
        isAdmin =
            isAdminOrAdminAgent model.currentUser
    in
    div
        [ class "fixed bottom-0 inset-x-0 bg-white border-t border-gray-200 shadow-lg transform transition-all duration-200 z-50" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4" ]
            [ div [ class "flex justify-between items-center" ]
                [ div [ class "flex items-center gap-4" ]
                    [ span [ class "text-sm text-gray-600" ]
                        [ text (String.fromInt (List.length model.selectedContacts) ++ " contacts selected") ]
                    ]
                , div [ class "flex items-center gap-3" ]
                    [ button
                        [ class "px-4 py-2 text-sm font-medium text-gray-700 hover:text-gray-900"
                        , onClick DeselectAllContacts
                        ]
                        [ text "Cancel" ]
                    , if isAdmin then
                        -- Reassign button (only for admins)
                        button
                            [ class "px-4 py-2 bg-purple-600 text-white text-sm font-medium rounded-lg hover:bg-purple-700 transition-colors duration-200 mr-2"
                            , onClick ShowReassignAgentModal
                            ]
                            [ text "Reassign Agent" ]

                      else
                        text ""
                    , button
                        [ class "px-4 py-2 bg-red-600 text-white text-sm font-medium rounded-lg hover:bg-red-700 transition-colors duration-200"
                        , onClick ShowDeleteConfirmModal
                        ]
                        [ if model.isDeletingContacts then
                            viewSpinner

                          else
                            text "Delete Selected"
                        ]
                    ]
                ]
            ]
        ]


statsCard : String -> String -> Html Msg
statsCard title value =
    div [ class "bg-white rounded-lg shadow-xl p-4 sm:p-6" ]
        [ div [ class "text-gray-600 text-xs sm:text-sm" ] [ text title ]
        , div [ class "text-2xl sm:text-4xl font-bold mt-1 sm:mt-2 text-[#03045E]" ] [ text value ]
        ]


statsCardWithSpinner : String -> Html Msg
statsCardWithSpinner title =
    div [ class "bg-white rounded-lg shadow-xl p-4 sm:p-6" ]
        [ div [ class "text-gray-600 text-xs sm:text-sm" ] [ text title ]
        , div [ class "text-2xl sm:text-4xl font-bold mt-1 sm:mt-2 text-[#03045E] flex items-center justify-center" ]
            [ div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-l-2 border-purple-500" ] [] ]
        ]


tableHeader : String -> Html Msg
tableHeader headerText =
    th [ class "px-2 py-1 text-left text-xs font-medium text-gray-500 uppercase tracking-wider border-b border-gray-200 bg-gray-50" ]
        [ text headerText ]


viewTableRow : Model -> Contact -> List (Html Msg)
viewTableRow model contact =
    let
        cellClass =
            "px-2 py-1 text-sm border-t border-gray-200"

        phoneCellClass =
            cellClass ++ " text-xs"

        compactCellClass =
            "px-1 py-1 text-sm border-t border-gray-200"

        -- Reduced padding for compact cells
        -- Added smaller text for phone numbers
        initials =
            String.left 1 (Maybe.withDefault "" contact.firstName) ++ String.left 1 (Maybe.withDefault "" contact.lastName)

        agentName =
            case contact.contactOwner of
                Just owner ->
                    owner.firstName ++ " " ++ owner.lastName

                Nothing ->
                    -- No contactOwner directly on the contact
                    case contact.agentId of
                        Just agentIdFromContact ->
                            -- Try to find the agent by ID from the contact
                            model.agents
                                |> List.filter (\agent -> agent.id == agentIdFromContact)
                                |> List.head
                                |> Maybe.map (\agent -> agent.firstName ++ " " ++ agent.lastName)
                                |> Maybe.withDefault ""

                        -- Fallback if agent ID on contact doesn't match any known agent
                        Nothing ->
                            -- No agentId on the contact, try to use the defaultAgentId from the model
                            case model.defaultAgentId of
                                Just defAgentId ->
                                    model.agents
                                        |> List.filter (\agent -> agent.id == defAgentId)
                                        |> List.head
                                        |> Maybe.map (\agent -> agent.firstName ++ " " ++ agent.lastName)
                                        |> Maybe.withDefault "Default"

                                -- Fallback if defaultAgentId doesn't match
                                Nothing ->
                                    -- No defaultAgentId in model, fall back to single agent logic or "Default"
                                    if List.length model.agents == 1 then
                                        case List.head model.agents of
                                            Just singleAgent ->
                                                singleAgent.firstName ++ " " ++ singleAgent.lastName

                                            Nothing ->
                                                "Default"

                                    else
                                        "Default"

        currentCarrier =
            Maybe.withDefault "" contact.currentCarrier
    in
    [ tr [ class "hover:bg-gray-50 transition-colors duration-200" ]
        [ td
            [ class (cellClass ++ " text-center")
            ]
            [ input
                [ type_ "checkbox"
                , class "rounded border-gray-300 text-purple-600 focus:ring-purple-500"
                , checked (List.member contact.id model.selectedContacts)
                , onClick (ToggleSelectContact contact.id)
                ]
                []
            ]
        , td [ class cellClass ]
            [ div [ class "flex items-center" ]
                [ div [ class "h-8 w-8 rounded-full bg-purple-100 flex items-center justify-center text-sm text-purple-700 font-medium uppercase" ]
                    [ text initials ]
                , div [ class "ml-3 text-sm text-gray-900" ]
                    [ button
                        [ class "text-left text-gray-900 hover:text-purple-600 transition-colors duration-200"
                        , onClick (NavigateToContact contact.id)
                        ]
                        [ text (Maybe.withDefault "" contact.firstName ++ " " ++ Maybe.withDefault "" contact.lastName) ]
                    ]
                ]
            ]
        , td [ class cellClass, Html.Attributes.title contact.email ]
            [ text (truncateEmail contact.email) ]
        , td [ class phoneCellClass ]
            [ text (formatPhoneNumber (Maybe.withDefault "" contact.phoneNumber)) ]
        , td [ class cellClass ]
            [ text (Maybe.withDefault "" contact.state) ]
        , td [ class cellClass ]
            [ text agentName ]
        , td [ class compactCellClass ]
            [ text currentCarrier ]
        , td [ class cellClass ]
            [ text (Maybe.withDefault "" contact.effectiveDate) ]
        , td [ class cellClass ]
            [ button
                [ class "text-purple-600 hover:text-purple-800 transition-colors duration-200"
                , onClick (NavigateToContact contact.id)
                ]
                [ viewIcon "M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z" ]
            ]
        ]
    ]


viewStatus : String -> Html Msg
viewStatus status =
    let
        ( bgColor, textColor, statusText ) =
            case status of
                "Quote Created" ->
                    ( "bg-green-50", "text-green-700", "Quote Created" )

                "Opened Email" ->
                    ( "bg-red-50", "text-red-700", "Opened Email" )

                "Email #2 Sent" ->
                    ( "bg-blue-50", "text-blue-700", "Email #2 Sent" )

                "Email #1 Sent" ->
                    ( "bg-blue-50", "text-blue-700", "Email #1 Sent" )

                "In Queue" ->
                    ( "bg-orange-50", "text-orange-700", "In Queue" )

                _ ->
                    ( "bg-gray-50", "text-gray-700", status )
    in
    div [ class ("inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium " ++ bgColor ++ " " ++ textColor) ]
        [ text statusText ]



-- HELPERS


truncateEmail : String -> String
truncateEmail email =
    if String.length email > 30 then
        String.left 30 email ++ "..."

    else
        email


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



-- HTTP


submitAddForm : ContactForm -> Cmd Msg
submitAddForm form =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/api/contacts/create"
        , body = Http.jsonBody (encodeContactForm form)
        , expect = Http.expectJson ContactAdded contactDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


encodeContactFormTemp : ContactForm -> Encode.Value
encodeContactFormTemp form =
    Encode.object
        [ ( "first_name", Encode.string form.firstName )
        , ( "last_name", Encode.string form.lastName )
        , ( "email", Encode.string form.email )
        , ( "phone_number", Encode.string (String.filter Char.isDigit form.phoneNumber |> String.left 10) )
        , ( "state", Encode.string form.state )
        , ( "effective_date", Encode.string form.effectiveDate )
        , ( "birth_date", Encode.string form.birthDate )
        , ( "tobacco_user", Encode.bool form.tobaccoUser )
        , ( "gender", Encode.string form.gender )
        , ( "zip_code", Encode.string form.zipCode )
        , ( "contact_owner_id", Encode.null )
        ]


fetchDashboardStats : Cmd Msg
fetchDashboardStats =
    Http.get
        { url = "/api/dashboard/stats"
        , expect = Http.expectJson GotDashboardStats dashboardStatsResponseDecoder
        }


dashboardStatsResponseDecoder : Decode.Decoder { success : Bool, stats : { quotesSent : Int, manualQuotesSent : Int, quotesViewed : Int, followUpsRequested : Int, healthQuestionsCompleted : Int } }
dashboardStatsResponseDecoder =
    Decode.map2
        (\success stats ->
            { success = success
            , stats = stats
            }
        )
        (Decode.field "success" Decode.bool)
        (Decode.field "stats" dashboardStatsDecoder)


dashboardStatsDecoder : Decode.Decoder { quotesSent : Int, manualQuotesSent : Int, quotesViewed : Int, followUpsRequested : Int, healthQuestionsCompleted : Int }
dashboardStatsDecoder =
    Decode.map5
        (\quotesSent manualQuotesSent quotesViewed followUpsRequested healthQuestionsCompleted ->
            { quotesSent = quotesSent
            , manualQuotesSent = manualQuotesSent
            , quotesViewed = quotesViewed
            , followUpsRequested = followUpsRequested
            , healthQuestionsCompleted = healthQuestionsCompleted
            }
        )
        (Decode.field "quotesSent" Decode.int)
        (Decode.field "manualQuotesSent" Decode.int)
        (Decode.field "quotesViewed" Decode.int)
        (Decode.field "followUpsRequested" Decode.int)
        (Decode.field "healthQuestionsCompleted" Decode.int)


submitEditFormWithFlag : ContactForm -> Bool -> Cmd Msg
submitEditFormWithFlag form isZipUpdate =
    case form.id of
        Just id ->
            let
                url =
                    if isZipUpdate then
                        "/api/contacts/" ++ String.fromInt id ++ "?zip_update=true"

                    else
                        "/api/contacts/" ++ String.fromInt id
            in
            Http.request
                { method = "PUT"
                , headers = []
                , url = url
                , body = Http.jsonBody (encodeContactForm form)
                , expect = Http.expectJson ContactUpdated contactDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none



-- JSON


contactDecoder : Decode.Decoder Contact
contactDecoder =
    Decode.succeed Contact
        |> Pipeline.required "id" Decode.int
        |> Pipeline.optional "first_name" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "last_name" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "email" Decode.string
        |> Pipeline.optional "phone_number"
            (Decode.string
                |> Decode.andThen
                    (\val -> Decode.succeed val)
                |> Decode.nullable
            )
            Nothing
        |> Pipeline.optional "state" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "contact_owner_id" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "contact_owner" (Decode.nullable userDecoder) Nothing
        |> Pipeline.optional "current_carrier" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "effective_date" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "birth_date" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "tobacco_user" (Decode.nullable Decode.bool) Nothing
        |> Pipeline.optional "gender" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "zip_code" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "plan_type" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "status" Decode.string "New"
        |> Pipeline.required "agent_id" (Decode.nullable Decode.int)
        |> Pipeline.required "last_emailed" (Decode.nullable Decode.string)


contactsDecoder : Decode.Decoder ContactsResponse
contactsDecoder =
    Decode.succeed ContactsResponse
        |> Pipeline.required "contacts" (Decode.list contactDecoder)
        |> Pipeline.required "filterOptions" filterOptionsDecoder
        |> Pipeline.required "total" Decode.int
        |> Pipeline.required "page" Decode.int
        |> Pipeline.required "limit" Decode.int


filterOptionsDecoder : Decode.Decoder AvailableFilters
filterOptionsDecoder =
    Decode.succeed AvailableFilters
        |> Pipeline.required "carriers" (Decode.list Decode.string)
        |> Pipeline.required "states" (Decode.list Decode.string)


encodeContactForm : ContactForm -> Encode.Value
encodeContactForm form =
    let
        planType =
            case form.planType of
                Just value ->
                    [ ( "plan_type", Encode.string value ) ]

                Nothing ->
                    []

        currentCarrier =
            case form.currentCarrier of
                Just value ->
                    [ ( "current_carrier", Encode.string value ) ]

                Nothing ->
                    []

        contactOwnerId =
            case form.contactOwnerId of
                Just value ->
                    [ ( "contact_owner_id", Encode.int value ) ]

                Nothing ->
                    []
    in
    Encode.object
        ([ ( "first_name", Encode.string form.firstName )
         , ( "last_name", Encode.string form.lastName )
         , ( "email", Encode.string form.email )
         , ( "phone_number", Encode.string (String.filter Char.isDigit form.phoneNumber |> String.left 10) )
         , ( "state", Encode.string form.state )
         , ( "effective_date", Encode.string form.effectiveDate )
         , ( "birth_date", Encode.string form.birthDate )
         , ( "tobacco_user", Encode.bool form.tobaccoUser )
         , ( "gender", Encode.string form.gender )
         , ( "zip_code", Encode.string form.zipCode )
         ]
            ++ planType
            ++ currentCarrier
            ++ contactOwnerId
        )


viewModals : Model -> Html Msg
viewModals model =
    case model.showModal of
        NoModal ->
            text ""

        ContactChoiceModal ->
            viewContactChoiceModal

        AddModal ->
            viewAddModal model model.isSubmittingForm

        EditModal contact ->
            viewEditModal model model.isSubmittingForm

        CsvUploadModal state ->
            viewCsvUploadModal state model.isUploadingCsv model

        DeleteConfirmModal ->
            viewDeleteConfirmModal model

        ReassignAgentModal ->
            viewReassignAgentModal model


viewContactChoiceModal : Html Msg
viewContactChoiceModal =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-4" ]
        [ div [ class "bg-white rounded-md p-5 max-w-lg w-full mx-2 shadow-lg relative" ]
            [ button
                [ class "absolute top-2 right-2 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-xl font-semibold text-gray-900 mb-2" ]
                [ text "Add Contacts" ]
            , div [ class "text-xs text-gray-600 mb-4" ]
                [ text "Select how you want to add your new contacts." ]
            , div [ class "grid grid-cols-2 gap-3" ]
                [ div
                    [ class "p-3 border border-gray-200 rounded-md hover:border-[#03045E] hover:bg-[#03045E]/5 cursor-pointer transition-colors"
                    , onClick ChooseSingleContact
                    ]
                    [ div [ class "flex items-center mb-2" ]
                        [ div [ class "h-6 w-6 rounded-full bg-purple-100 flex items-center justify-center text-xs text-purple-700 font-medium" ]
                            [ viewIcon "M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" ]
                        ]
                    , h3 [ class "text-base font-medium text-gray-900 mb-1" ]
                        [ text "Single Contact" ]
                    , p [ class "text-xs text-gray-600" ]
                        [ text "Individual Form" ]
                    ]
                , div
                    [ class "p-3 border border-gray-200 rounded-md hover:border-[#03045E] hover:bg-[#03045E]/5 cursor-pointer transition-colors"
                    , onClick ChooseMultipleContacts
                    ]
                    [ div [ class "flex items-center mb-2" ]
                        [ div [ class "h-6 w-6 rounded-full bg-purple-100 flex items-center justify-center text-xs text-purple-700 font-medium" ]
                            [ viewIcon "M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" ]
                        ]
                    , h3 [ class "text-base font-medium text-gray-900 mb-1" ]
                        [ text "Multiple Contacts" ]
                    , p [ class "text-xs text-gray-600" ]
                        [ text "CSV Upload" ]
                    ]
                ]
            ]
        ]


viewAddModal : Model -> Bool -> Html Msg
viewAddModal model isSubmitting =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-4" ]
        [ div [ class "bg-white rounded-md p-5 max-w-lg w-full mx-2 shadow-lg relative max-h-[90vh] flex flex-col" ]
            [ button
                [ class "absolute top-2 right-2 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-xl font-semibold text-gray-900 mb-4" ]
                [ text "Add New Client" ]
            , div [ class "overflow-y-auto pr-1 flex-grow" ]
                [ viewContactForm model model.addForm UpdateAddForm SubmitAddForm "Add Client" isSubmitting ]
            ]
        ]


viewEditModal : Model -> Bool -> Html Msg
viewEditModal model isSubmitting =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-4" ]
        [ div [ class "bg-white rounded-md p-5 max-w-lg w-full mx-2 shadow-lg relative max-h-[90vh] flex flex-col" ]
            [ button
                [ class "absolute top-2 right-2 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-xl font-semibold text-gray-900 mb-4" ]
                [ text "Edit Client" ]
            , div [ class "overflow-y-auto pr-1 flex-grow" ]
                [ viewContactForm model model.editForm UpdateEditForm SubmitEditForm "Save Changes" isSubmitting ]
            ]
        ]


viewCsvUploadModal : UploadState -> Bool -> Model -> Html Msg
viewCsvUploadModal state isUploading model =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-2xl w-full mx-4 shadow-xl relative max-h-[90vh] overflow-y-auto" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Upload CSV" ]
            , div [ class "mb-6 text-sm text-gray-600" ]
                [ text "Need help formatting your CSV? "
                , Html.a
                    [ class "text-purple-600 hover:text-purple-800 hover:underline"
                    , Html.Attributes.href "/example.csv"
                    , Html.Attributes.download "example.csv"
                    ]
                    [ text "Download example CSV file" ]
                ]

            -- Error display
            , if state.error /= Nothing then
                div [ class "mb-6" ]
                    [ if state.stats /= Nothing then
                        let
                            stats =
                                Maybe.withDefault
                                    { totalRows = 0
                                    , errorRows = 0
                                    , validRows = 0
                                    , converted_carrier_rows = 0
                                    , supported_carriers = []
                                    }
                                    state.stats
                        in
                        div []
                            [ if stats.errorRows > 0 then
                                div [ class "p-4 mb-4 bg-red-50 border border-red-200 rounded-lg" ]
                                    [ div [ class "flex items-start" ]
                                        [ div [ class "flex-shrink-0" ]
                                            [ viewIcon "M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" ]
                                        , div [ class "ml-3" ]
                                            [ h3 [ class "text-sm font-medium text-red-800" ]
                                                [ text "Upload Errors" ]
                                            , div [ class "mt-2 text-sm text-red-700" ]
                                                [ text ("Found " ++ String.fromInt stats.errorRows ++ " rows with errors. Successfully imported " ++ String.fromInt stats.validRows ++ " rows.")
                                                , case state.errorCsv of
                                                    Just csvContent ->
                                                        div [ class "mt-2 font-medium" ]
                                                            [ button
                                                                [ class "text-purple-600 hover:text-purple-800 hover:underline"
                                                                , onClick (DownloadErrorCsv csvContent)
                                                                ]
                                                                [ text "Download and Fix Error Rows" ]
                                                            ]

                                                    Nothing ->
                                                        text ""
                                                ]
                                            ]
                                        ]
                                    ]

                              else
                                text ""
                            , if stats.converted_carrier_rows > 0 then
                                div [ class "p-4 bg-yellow-50 border border-yellow-200 rounded-lg" ]
                                    [ div [ class "flex items-start" ]
                                        [ div [ class "flex-shrink-0" ]
                                            [ viewIcon "M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" ]
                                        , div [ class "ml-3" ]
                                            [ h3 [ class "text-sm font-medium text-yellow-800 mb-3" ]
                                                [ text "Carrier Conversions" ]
                                            , div [ class "space-y-4 text-sm text-yellow-700" ]
                                                [ p []
                                                    [ text (String.fromInt stats.converted_carrier_rows ++ " rows had carriers we do not currently recognize or provide quotes for. This is normal and expected -- we will still email these contacts with quotes from supported Carriers.") ]
                                                , p []
                                                    [ text "However, this can also happen if there is a misspelling of a supported carrier. Please review to ensure the data is correct." ]
                                                , case state.converted_carriers_csv of
                                                    Just csvContent ->
                                                        div [ class "pt-1" ]
                                                            [ button
                                                                [ class "text-purple-600 hover:text-purple-800 hover:underline font-medium"
                                                                , onClick (DownloadCarrierConversionsCsv csvContent)
                                                                ]
                                                                [ text "Download Unrecognized Carrier Rows" ]
                                                            ]

                                                    Nothing ->
                                                        text ""
                                                , div [ class "pt-2 border-t border-yellow-200" ]
                                                    [ details [ class "text-sm" ]
                                                        [ summary [ class "cursor-pointer text-purple-600 hover:text-purple-800 font-medium" ]
                                                            [ text "Click to see supported carriers" ]
                                                        , div [ class "mt-3 pl-4 space-y-2" ]
                                                            (List.map
                                                                (\carrier ->
                                                                    div [ class "flex items-baseline" ]
                                                                        [ span [ class "font-medium" ] [ text carrier.name ]
                                                                        , if not (List.isEmpty carrier.aliases) then
                                                                            span [ class "ml-4 text-yellow-800" ]
                                                                                [ text ("Also accepts: " ++ String.join ", " carrier.aliases) ]

                                                                          else
                                                                            text ""
                                                                        ]
                                                                )
                                                                stats.supported_carriers
                                                            )
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]

                              else
                                text ""
                            ]

                      else
                        div [ class "p-4 bg-red-50 border border-red-200 rounded-lg" ]
                            [ div [ class "flex items-start" ]
                                [ div [ class "flex-shrink-0" ]
                                    [ viewIcon "M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" ]
                                , div [ class "ml-3" ]
                                    [ h3 [ class "text-sm font-medium text-red-800" ]
                                        [ text "Error uploading CSV" ]
                                    , div [ class "mt-2 text-sm text-red-700" ]
                                        [ text (Maybe.withDefault "" state.error) ]
                                    ]
                                ]
                            ]
                    ]

              else
                text ""

            -- Display invalid emails section (if any)
            , if not (List.isEmpty state.invalidEmails) then
                div [ class "mb-6 p-4 bg-yellow-50 border border-yellow-200 rounded-lg" ]
                    [ div [ class "flex items-start" ]
                        [ div [ class "flex-shrink-0" ]
                            [ viewIcon "M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" ]
                        , div [ class "ml-3 w-full" ]
                            [ h3 [ class "text-sm font-medium text-yellow-800 mb-2" ]
                                [ text ("Invalid Emails (" ++ String.fromInt (List.length state.invalidEmails) ++ ")") ]
                            , p [ class "text-sm text-yellow-700 mb-3" ]
                                [ text "The following records have invalid email addresses and will be skipped during import:" ]
                            , div [ class "max-h-80 overflow-y-auto bg-white border border-yellow-200 rounded p-2" ]
                                [ Html.ul [ class "text-xs text-yellow-800 space-y-1" ]
                                    (List.indexedMap
                                        (\index inv ->
                                            Html.li [ class "flex" ]
                                                [ span [ class "text-gray-500 w-16 flex-shrink-0" ] [ text ("Row " ++ String.fromInt (index + 1) ++ ":") ]
                                                , span [ class "flex-grow" ]
                                                    [ if String.isEmpty inv.email then
                                                        text "(empty string)"

                                                      else
                                                        text inv.email
                                                    ]
                                                ]
                                        )
                                        state.invalidEmails
                                    )
                                ]
                            ]
                        ]
                    ]

              else
                text ""

            -- Form fields (agent selection, etc.)
            , div [ class "mb-4 space-y-4" ]
                [ case model.currentUser of
                    Just user ->
                        if user.isAdmin then
                            div [ class "flex items-center space-x-2" ]
                                [ input
                                    [ type_ "checkbox"
                                    , checked state.overwriteDuplicates
                                    , onInput (\val -> ToggleOverwriteDuplicates (val == "true"))
                                    , class "rounded border-gray-300 text-purple-600 focus:ring-purple-500"
                                    ]
                                    []
                                , label [ class "text-sm text-gray-600" ]
                                    [ text "Overwrite existing contacts (matched on email address)" ]
                                ]

                        else
                            text ""

                    Nothing ->
                        text ""
                , div [ class "form-group" ]
                    [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                        [ text "Assign to Agent" ]
                    , div [ class "relative" ]
                        [ if List.isEmpty model.agents then
                            div [ class "p-2 text-gray-500 border rounded" ]
                                [ text "Loading agents..." ]

                          else
                            case model.currentUser of
                                Just user ->
                                    {--
                                    if user.isAgent && not user.isAdmin then
                                        -- For non-admin agents, show their name as fixed value
                                        let
                                            agentName =
                                                model.agents
                                                    |> List.filter (\agent -> agent.id == user.id)
                                                    |> List.head
                                                    |> Maybe.map (\agent -> agent.firstName ++ " " ++ agent.lastName)
                                                    |> Maybe.withDefault (user.firstName ++ " " ++ user.lastName)
                                        in
                                        div [ class "w-full px-4 py-3 bg-gray-100 border-[2.5px] border-gray-300 rounded-lg text-gray-700" ]
                                            [ text agentName ]

                                    else
                                    --}
                                    -- For admins, show dropdown with all agents
                                    let
                                        agentOptions =
                                            List.map
                                                (\agent ->
                                                    ( String.fromInt agent.id
                                                    , agent.firstName ++ " " ++ agent.lastName
                                                    )
                                                )
                                                model.agents

                                        -- Get default agent ID - either from state.selectedAgentId if valid, or from current user
                                        defaultAgentId =
                                            user.id
                                    in
                                    Html.select
                                        [ class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200 appearance-none"
                                        , value (state.selectedAgentId |> Maybe.map String.fromInt |> Maybe.withDefault "") -- Correctly bind to state.selectedAgentId
                                        , onInput (\val -> SelectUploadAgent (String.toInt val |> Maybe.withDefault 0))
                                        ]
                                        (List.map
                                            (\( val, label ) ->
                                                option [ value val ] [ text label ]
                                            )
                                            agentOptions
                                        )

                                Nothing ->
                                    text ""
                        ]
                    ]
                ]

            -- File upload area
            , if state.processingHeaders then
                -- Show loading indicator while processing CSV headers
                div [ class "w-full h-64 border-2 border-gray-200 rounded-lg flex flex-col items-center justify-center p-8" ]
                    [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-purple-500 border-t-transparent mb-4" ] []
                    , div [ class "text-gray-600 text-center" ]
                        [ span [ class "block font-medium mb-1" ]
                            [ text "Processing your CSV file" ]
                        , span [ class "text-sm" ]
                            [ text "We're analyzing your file to map the columns automatically..." ]
                        ]
                    ]

              else
                div
                    [ class
                        ("w-full h-64 border-2 border-dashed rounded-lg flex flex-col items-center justify-center p-8 transition-colors "
                            ++ (if state.dragOver then
                                    "border-purple-500 bg-purple-50"

                                else
                                    "border-gray-300 hover:border-purple-400"
                               )
                        )
                    , preventDefaultOn "dragenter" (Decode.succeed ( DragEnter, True ))
                    , preventDefaultOn "dragover" (Decode.succeed ( NoOp, True ))
                    , preventDefaultOn "dragleave" (Decode.succeed ( DragLeave, True ))
                    , preventDefaultOn "drop" (dropDecoder FileDrop)
                    ]
                    [ div [ class "text-gray-500 text-center" ]
                        [ span [ class "block text-lg font-medium mb-2" ]
                            [ text "Drag and drop your CSV file here, or " ]
                        , button
                            [ class "text-purple-600 font-semibold hover:text-purple-700 focus:outline-none focus:underline"
                            , onClick ClickedSelectFile
                            ]
                            [ text "browse" ]
                        , if state.file /= Nothing then
                            div [ class "mt-4 text-sm bg-green-50 text-green-800 px-3 py-2 rounded-lg" ]
                                [ text ("File selected: " ++ (Maybe.map File.name state.file |> Maybe.withDefault "")) ]

                          else
                            text ""
                        ]
                    ]

            -- Column mapping section
            , if state.file /= Nothing && not state.processingHeaders && not (List.isEmpty state.csvHeaders) then
                div [ class "mt-8 space-y-6" ]
                    [ viewColumnMapping state
                    , viewCarrierMapping state model.carriers
                    , div [ class "mt-4" ]
                        [ h3 [ class "text-sm font-medium text-gray-700 mb-2" ]
                            [ text "Available CSV Columns" ]
                        , div [ class "bg-gray-50 p-3 rounded-md text-sm" ]
                            [ span [ class "text-gray-400 mr-2" ]
                                [ text "Headers:" ]
                            , span [ class "text-gray-600" ]
                                [ text (String.join ", " state.csvHeaders) ]
                            ]
                        ]
                    ]

              else
                text ""

            -- Buttons
            , div [ class "mt-8 flex justify-end space-x-4" ]
                [ button
                    [ class "px-6 py-3 bg-gray-100 text-gray-600 text-sm font-medium rounded-lg hover:bg-gray-200 transition-colors duration-200 focus:ring-4 focus:ring-gray-200"
                    , onClick CloseModal
                    ]
                    [ text "Cancel" ]
                , if isUploading then
                    div [ class "px-6 py-3 flex items-center space-x-2" ]
                        [ viewSpinner ]

                  else
                    let
                        isDisabled =
                            state.file
                                == Nothing
                                || state.processingHeaders
                                || state.extractingCarriers
                                || not (hasCarrierMappings state)
                                || List.isEmpty state.detectedCarriers
                    in
                    button
                        [ type_ "submit"
                        , class
                            (if isDisabled then
                                "px-6 py-3 bg-purple-400 text-white text-sm font-medium rounded-lg transition-colors duration-200 focus:ring-4 focus:ring-purple-200 cursor-not-allowed"

                             else
                                "px-6 py-3 bg-purple-500 text-white text-sm font-medium rounded-lg hover:bg-purple-600 transition-colors duration-200 focus:ring-4 focus:ring-purple-200"
                            )
                        , onClick UploadCsv
                        , Html.Attributes.disabled isDisabled
                        ]
                        [ text "Upload" ]
                ]
            ]
        ]


viewColumnMapping : UploadState -> Html Msg
viewColumnMapping state =
    div [ class "space-y-4" ]
        [ h3 [ class "text-sm font-medium text-gray-700 flex justify-between" ]
            [ text "Column Mapping"
            , span [ class "text-xs text-purple-600" ]
                [ text ("Detected " ++ String.fromInt (List.length state.csvHeaders) ++ " columns in your CSV") ]
            ]
        , p [ class "text-xs text-gray-500" ]
            [ text "Map your CSV columns to our required fields." ]
        , div [ class "grid grid-cols-2 gap-3" ]
            [ viewColumnMapField "First Name" "firstName" state
            , viewColumnMapField "Last Name" "lastName" state
            , viewColumnMapField "Email" "email" state
            , viewColumnMapField "Phone Number" "phoneNumber" state
            , viewColumnMapField "Current Carrier" "currentCarrier" state
            , viewColumnMapField "Effective Date" "effectiveDate" state
            , viewColumnMapField "Birth Date" "birthDate" state
            , viewColumnMapField "Tobacco User" "tobaccoUser" state
            , viewColumnMapField "Gender" "gender" state
            , viewColumnMapField "ZIP Code" "zipCode" state
            , viewColumnMapField "Plan Type" "planType" state
            ]
        ]


viewCarrierMapping : UploadState -> List String -> Html Msg
viewCarrierMapping state supportedCarriers =
    let
        carrierColumnName =
            case state.columnMapping of
                Just mapping ->
                    mapping.currentCarrier

                Nothing ->
                    ""

        hasCarrierColumn =
            not (String.isEmpty carrierColumnName)

        hasExtractedCarriers =
            not (List.isEmpty state.detectedCarriers)
    in
    div [ class "mt-6 space-y-4 border-t pt-6" ]
        [ h3 [ class "text-sm font-medium text-gray-700" ]
            [ text "Carrier Mapping" ]
        , p [ class "text-xs text-gray-500" ]
            [ text "Map carrier names in your CSV to standard carrier names in our system." ]

        -- Show which column is used for carrier data
        , div [ class "mb-4" ]
            [ if hasCarrierColumn then
                div [ class "flex space-x-2" ]
                    [ div [ class "text-xs font-medium text-gray-600 py-1" ]
                        [ text ("Using column \"" ++ carrierColumnName ++ "\" for carrier data") ]

                    {--
                    , button
                        [ class "px-3 py-1 bg-purple-500 text-white text-xs font-medium rounded hover:bg-purple-600 transition-colors"
                        , onClick ExtractCarrierValues
                        , disabled state.extractingCarriers
                        ]
                        [ if state.extractingCarriers then
                            div [ class "flex items-center" ]
                                [ viewSpinner
                                , span [ class "ml-2" ] [ text "Extracting..." ]
                                ]

                          else
                            text "Extract Carriers"
                        ]
                    --}
                    ]

              else
                div [ class "text-xs text-gray-500 italic" ]
                    [ text "Select a column for \"Current Carrier\" in the mapping above to continue." ]
            ]

        -- Show carrier mappings once extracted
        , if hasExtractedCarriers then
            div [ class "border rounded-md p-4 bg-gray-50" ]
                [ p [ class "text-xs text-gray-600 mb-3" ]
                    [ text ("Found " ++ String.fromInt (List.length state.detectedCarriers) ++ " unique carrier names in your CSV.") ]
                , if List.isEmpty state.detectedCarriers then
                    div [ class "text-sm text-gray-500 italic" ]
                        [ text "No carrier names found in the selected column." ]

                  else
                    div [ class "space-y-2 max-h-64 overflow-y-auto pr-2" ]
                        (List.map
                            (\carrier -> viewCarrierMappingRow carrier state supportedCarriers)
                            state.detectedCarriers
                        )
                ]

          else if hasCarrierColumn then
            div [ class "text-sm text-gray-500 italic" ]
                [ text "Click 'Extract Carriers' to analyze carrier names in your CSV." ]

          else
            div [ class "text-sm text-gray-500 italic" ]
                [ text "Select the \"Current Carrier\" column in the mapping above to begin." ]
        ]


viewCarrierMappingRow : String -> UploadState -> List String -> Html Msg
viewCarrierMappingRow carrier state supportedCarriers =
    let
        currentMapping =
            case state.carrierMapping of
                Just mapping ->
                    Dict.get carrier mapping.mappings
                        |> Maybe.withDefault "Other"

                Nothing ->
                    "Other"
    in
    div [ class "flex justify-between items-center py-1 border-b border-gray-200" ]
        [ div [ class "text-sm text-gray-700" ]
            [ text carrier ]
        , div [ class "w-1/2" ]
            [ select
                [ class "w-full px-2 py-1 text-sm border rounded"
                , value currentMapping
                , onInput (UpdateCarrierMapping carrier)
                ]
                (option [ value "Other" ] [ text "Other/Unsupported" ]
                    :: List.map
                        (\c ->
                            option
                                [ value c
                                , selected (currentMapping == c)
                                ]
                                [ text c ]
                        )
                        supportedCarriers
                )
            ]
        ]


viewColumnMapField : String -> String -> UploadState -> Html Msg
viewColumnMapField labelText field state =
    div [ class "flex flex-col gap-1" ]
        [ label [ class "text-xs font-medium text-gray-600" ]
            [ text labelText ]
        , select
            [ class "w-full px-2 py-1 text-sm border rounded"
            , value (getColumnMapping field state)
            , onInput (UpdateColumnMapping field)
            ]
            (option [ value "" ] [ text "Select a column" ]
                :: List.map
                    (\header ->
                        option
                            [ value header
                            , selected (getColumnMapping field state == header)
                            ]
                            [ text header ]
                    )
                    state.csvHeaders
            )
        ]


getColumnMapping : String -> UploadState -> String
getColumnMapping field state =
    case state.columnMapping of
        Just mapping ->
            case field of
                "firstName" ->
                    mapping.firstName

                "lastName" ->
                    mapping.lastName

                "email" ->
                    mapping.email

                "phoneNumber" ->
                    mapping.phoneNumber

                "currentCarrier" ->
                    mapping.currentCarrier

                "effectiveDate" ->
                    mapping.effectiveDate

                "birthDate" ->
                    mapping.birthDate

                "tobaccoUser" ->
                    mapping.tobaccoUser

                "gender" ->
                    mapping.gender

                "zipCode" ->
                    mapping.zipCode

                "planType" ->
                    mapping.planType

                _ ->
                    ""

        Nothing ->
            ""


dropDecoder : (File -> msg) -> Decoder ( msg, Bool )
dropDecoder toMsg =
    Decode.at [ "dataTransfer", "files" ] (Decode.index 0 File.decoder)
        |> Decode.map (\file -> ( toMsg file, True ))


uploadCsv : File -> Bool -> Maybe Int -> Model -> Cmd Msg
uploadCsv file overwriteDuplicates maybeAgentId model =
    let
        actualOverwriteValue =
            case model.currentUser of
                Just user ->
                    if user.isAgent && not user.isAdmin then
                        False

                    else
                        overwriteDuplicates

                Nothing ->
                    overwriteDuplicates
    in
    File.toString file
        |> Task.andThen
            (\csvContent ->
                case model.showModal of
                    CsvUploadModal state ->
                        case ( state.columnMapping, state.carrierMapping ) of
                            ( Just colMapping, Just carrierMapping ) ->
                                case CsvProcessor.processCsvToContacts csvContent colMapping carrierMapping of
                                    Ok { valid, invalid } ->
                                        if List.isEmpty valid then
                                            -- If there are no valid contacts, return an error
                                            Task.succeed
                                                (Err
                                                    (Http.BadBody
                                                        "No valid contacts found in CSV. Please check email formats."
                                                    )
                                                )

                                        else
                                            Http.task
                                                { method = "POST"
                                                , headers = []
                                                , url = "/api/contacts/bulk-import"
                                                , body =
                                                    Http.jsonBody
                                                        (Encode.object
                                                            [ ( "contacts", Encode.list encodeProcessedContact valid )
                                                            , ( "overwriteExisting", Encode.bool actualOverwriteValue )
                                                            , ( "agentId"
                                                              , case maybeAgentId of
                                                                    Just id ->
                                                                        Encode.int id

                                                                    Nothing ->
                                                                        Encode.null
                                                              )
                                                            ]
                                                        )
                                                , resolver =
                                                    Http.stringResolver <|
                                                        \response ->
                                                            case response of
                                                                Http.GoodStatus_ _ body ->
                                                                    case Decode.decodeString uploadResponseDecoder body of
                                                                        Ok value ->
                                                                            Ok value

                                                                        Err err ->
                                                                            Err (Http.BadBody (Decode.errorToString err))

                                                                Http.BadUrl_ url ->
                                                                    Err (Http.BadUrl url)

                                                                Http.Timeout_ ->
                                                                    Err Http.Timeout

                                                                Http.NetworkError_ ->
                                                                    Err Http.NetworkError

                                                                Http.BadStatus_ metadata _ ->
                                                                    Err (Http.BadStatus metadata.statusCode)
                                                , timeout = Nothing
                                                }
                                                |> Task.map Ok
                                                |> Task.onError (\err -> Task.succeed (Err err))

                                    Err err ->
                                        Task.succeed
                                            (Err
                                                (Http.BadBody
                                                    (case err of
                                                        CsvProcessor.ParseError msg ->
                                                            "Failed to parse CSV: " ++ msg

                                                        CsvProcessor.EmptyFile ->
                                                            "CSV file is empty"

                                                        CsvProcessor.NoHeaders ->
                                                            "CSV file has no headers"
                                                    )
                                                )
                                            )

                            _ ->
                                Task.succeed
                                    (Err
                                        (Http.BadBody "Column mapping or carrier mapping not configured")
                                    )

                    _ ->
                        Task.succeed
                            (Err
                                (Http.BadBody "Invalid modal state")
                            )
            )
        |> Task.perform CsvUploaded



-- Add encoder for processed contacts


encodeProcessedContact : CsvProcessor.ProcessedContact -> Encode.Value
encodeProcessedContact contact =
    Encode.object
        [ ( "first_name", Encode.string contact.firstName )
        , ( "last_name", Encode.string contact.lastName )
        , ( "email", Encode.string contact.email )
        , ( "phone_number", Encode.string contact.phoneNumber )
        , ( "current_carrier", Encode.string contact.currentCarrier )
        , ( "effective_date", Encode.string contact.effectiveDate )
        , ( "birth_date", Encode.string contact.birthDate )
        , ( "tobacco_user", Encode.bool contact.tobaccoUser )
        , ( "gender", Encode.string contact.gender )
        , ( "zip_code", Encode.string contact.zipCode )
        , ( "plan_type", Encode.string contact.planType )
        ]



-- Helper function to encode column mapping to JSON


encodeColumnMapping : ColumnMapping -> String
encodeColumnMapping mapping =
    Encode.encode 0
        (Encode.object
            [ ( "firstName", Encode.string mapping.firstName )
            , ( "lastName", Encode.string mapping.lastName )
            , ( "email", Encode.string mapping.email )
            , ( "phoneNumber", Encode.string mapping.phoneNumber )
            , ( "currentCarrier", Encode.string mapping.currentCarrier )
            , ( "effectiveDate", Encode.string mapping.effectiveDate )
            , ( "birthDate", Encode.string mapping.birthDate )
            , ( "tobaccoUser", Encode.string mapping.tobaccoUser )
            , ( "gender", Encode.string mapping.gender )
            , ( "zipCode", Encode.string mapping.zipCode )
            , ( "planType", Encode.string mapping.planType )
            ]
        )



-- Helper function to encode carrier mapping to JSON


encodeCarrierMapping : CarrierMapping -> String
encodeCarrierMapping mapping =
    Encode.encode 0
        (Encode.object
            [ ( "detectedCarriers", Encode.list Encode.string mapping.detectedCarriers )
            , ( "mappings", Encode.dict identity Encode.string mapping.mappings )
            ]
        )


uploadResponseDecoder : Decode.Decoder UploadResponse
uploadResponseDecoder =
    Decode.succeed UploadResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "message" Decode.string
        |> Pipeline.required "totalRows" Decode.int


type alias UploadResponse =
    { success : Bool
    , message : String
    , totalRows : Int
    }


formatUploadError : String -> String
formatUploadError message =
    if String.startsWith "Missing required columns:" message then
        let
            missingColumns =
                String.dropLeft (String.length "Missing required columns:") message
                    |> String.trim
                    |> String.split ","
                    |> List.map String.trim
                    |> String.join ", "
        in
        "Your CSV is missing the following required columns: " ++ missingColumns ++ ". Please add these columns and try again."

    else
        message



-- Add this new subscription function


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.showModal of
            NoModal ->
                Sub.none

            ContactChoiceModal ->
                Browser.Events.onKeyDown (Decode.map HandleKeyDown (Decode.field "key" Decode.string))

            AddModal ->
                Browser.Events.onKeyDown (Decode.map HandleKeyDown (Decode.field "key" Decode.string))

            EditModal _ ->
                Browser.Events.onKeyDown (Decode.map HandleKeyDown (Decode.field "key" Decode.string))

            CsvUploadModal _ ->
                Browser.Events.onKeyDown (Decode.map HandleKeyDown (Decode.field "key" Decode.string))

            DeleteConfirmModal ->
                Browser.Events.onKeyDown (Decode.map HandleKeyDown (Decode.field "key" Decode.string))

            ReassignAgentModal ->
                Browser.Events.onKeyDown (Decode.map HandleKeyDown (Decode.field "key" Decode.string))
        , if model.openFilter /= Nothing then
            -- Only listen for clicks outside when a dropdown is open
            Browser.Events.onMouseDown (Decode.succeed CloseFilterDropdown)

          else
            Sub.none
        ]


sortContacts : Maybe SortColumn -> SortDirection -> List Contact -> List Contact
sortContacts maybeColumn direction contacts =
    case maybeColumn of
        Nothing ->
            contacts

        Just column ->
            let
                comparator =
                    case column of
                        NameCol ->
                            \a b ->
                                compare
                                    (Maybe.withDefault "" a.firstName ++ " " ++ Maybe.withDefault "" a.lastName)
                                    (Maybe.withDefault "" b.firstName ++ " " ++ Maybe.withDefault "" b.lastName)

                        StatusCol ->
                            \a b -> compare a.status b.status

                        EmailCol ->
                            \a b -> compare a.email b.email

                        PhoneNumberCol ->
                            \a b -> compare (Maybe.withDefault "" a.phoneNumber) (Maybe.withDefault "" b.phoneNumber)

                        StateCol ->
                            \a b -> compare (Maybe.withDefault "" a.state) (Maybe.withDefault "" b.state)

                        ContactOwnerCol ->
                            \a b ->
                                let
                                    aName =
                                        getUserName a.contactOwner

                                    bName =
                                        getUserName b.contactOwner
                                in
                                compare aName bName

                        CurrentCarrierCol ->
                            \a b -> compare (Maybe.withDefault "" a.currentCarrier) (Maybe.withDefault "" b.currentCarrier)

                        EffectiveDateCol ->
                            \a b -> compare (Maybe.withDefault "" a.effectiveDate) (Maybe.withDefault "" b.effectiveDate)
            in
            List.sortWith
                (if direction == Ascending then
                    comparator

                 else
                    \a b -> comparator b a
                )
                contacts


filterContacts : Filters -> String -> Time.Posix -> Maybe Int -> List Contact -> List Contact
filterContacts filters searchQuery currentTime defaultAgentId contacts =
    contacts
        |> filterBySearch searchQuery
        |> filterByCarriers filters.carriers
        |> filterByList (\c -> Maybe.withDefault "" c.state) filters.states
        |> filterByAge filters.ageRange
        |> filterByAgents filters.agents defaultAgentId


filterByCarriers : List String -> List Contact -> List Contact
filterByCarriers carriers contacts =
    if List.isEmpty carriers then
        contacts

    else
        List.filter
            (\contact ->
                case contact.currentCarrier of
                    Just carrier ->
                        List.member carrier carriers

                    Nothing ->
                        False
            )
            contacts


filterBySearch : String -> List Contact -> List Contact
filterBySearch query contacts =
    if String.isEmpty query then
        contacts

    else
        let
            loweredQuery =
                String.toLower query
        in
        List.filter
            (\contact ->
                String.contains loweredQuery (String.toLower (Maybe.withDefault "" contact.firstName))
                    || String.contains loweredQuery (String.toLower (Maybe.withDefault "" contact.lastName))
                    || (case contact.currentCarrier of
                            Just carrier ->
                                String.contains loweredQuery (String.toLower carrier)

                            Nothing ->
                                False
                       )
            )
            contacts


filterByAge : Maybe ( Int, Int ) -> List Contact -> List Contact
filterByAge range contacts =
    case range of
        Nothing ->
            contacts

        Just ( min, max ) ->
            List.filter
                (\contact ->
                    let
                        age =
                            calculateAge (Maybe.withDefault "" contact.birthDate)
                    in
                    age >= min && age <= max
                )
                contacts


calculateAge : String -> Int
calculateAge birthDate =
    -- This is a simplified version. You might want to use a proper date library
    2024 - (String.left 4 birthDate |> String.toInt |> Maybe.withDefault 0)


toggleFilter : Filters -> FilterType -> String -> Filters
toggleFilter filters filterType value =
    case filterType of
        CarrierFilter ->
            { filters | carriers = toggleList filters.carriers value }

        StateFilter ->
            { filters | states = toggleList filters.states value }

        AgeFilter ->
            { filters | ageRange = toggleAgeRange filters.ageRange value }

        AgentFilter ->
            { filters | agents = toggleAgentList filters.agents (String.toInt value |> Maybe.withDefault 0) }


toggleList : List String -> String -> List String
toggleList list value =
    if List.member value list then
        List.filter (\v -> v /= value) list

    else
        value :: list


toggleAgeRange : Maybe ( Int, Int ) -> String -> Maybe ( Int, Int )
toggleAgeRange maybeRange value =
    case maybeRange of
        Nothing ->
            Just ( String.toInt value |> Maybe.withDefault 0, String.toInt value |> Maybe.withDefault 0 )

        Just ( min, max ) ->
            if min == (String.toInt value |> Maybe.withDefault 0) then
                Just ( String.toInt value |> Maybe.withDefault 0, max )

            else if max == (String.toInt value |> Maybe.withDefault 0) then
                Just ( min, String.toInt value |> Maybe.withDefault 0 )

            else
                Just ( min, max )


setAgeFilter : Int -> Int -> Filters -> Filters
setAgeFilter min max filters =
    if max < 1 then
        { filters | ageRange = Nothing }
        -- Don't apply filter if max is 0 or negative

    else
        { filters | ageRange = Just ( min, max ) }



-- Helper function to get unique values from contacts


getUniqueValues : (Contact -> String) -> List Contact -> List String
getUniqueValues getter contacts =
    contacts
        |> List.map getter
        |> List.sort
        |> List.Extra.unique


zipInfoDecoder : Decode.Decoder ZipInfo
zipInfoDecoder =
    Decode.succeed ZipInfo
        |> Pipeline.required "state" Decode.string
        |> Pipeline.required "counties" (Decode.list Decode.string)
        |> Pipeline.required "cities" (Decode.list Decode.string)


filterByList : (Contact -> String) -> List String -> List Contact -> List Contact
filterByList getter selectedValues contacts =
    if List.isEmpty selectedValues then
        contacts

    else
        List.filter
            (\contact ->
                List.member (getter contact) selectedValues
            )
            contacts


viewContactForm : Model -> ContactForm -> (ContactFormField -> String -> Msg) -> Msg -> String -> Bool -> Html Msg
viewContactForm model form updateMsg submitMsg buttonText isSubmitting =
    let
        carrierOptions =
            ( "", "Select a carrier" ) :: List.map (\c -> ( c, c )) model.carriers ++ [ ( "Other", "Other" ) ]

        planTypeOptions =
            [ ( "", "Select a plan type" ), ( "Plan N", "Plan N" ), ( "Plan G", "Plan G" ), ( "Other", "Other" ) ]

        -- Simple agent dropdown options - show all available agents
        agentOptions =
            if List.isEmpty model.agents then
                -- If no agents loaded, use current user as fallback if they're an agent
                case model.currentUser of
                    Just user ->
                        [ ( String.fromInt user.id, user.firstName ++ " " ++ user.lastName ) ]

                    Nothing ->
                        []

            else
                -- Just show all available agents
                List.map
                    (\agent ->
                        ( String.fromInt agent.id, agent.firstName ++ " " ++ agent.lastName )
                    )
                    model.agents

        -- Get the selected agent ID or default to first agent
        defaultAgentId =
            case List.head model.agents of
                Just agent ->
                    String.fromInt agent.id

                Nothing ->
                    -- Try current user as fallback
                    case model.currentUser of
                        Just user ->
                            String.fromInt user.id

                        Nothing ->
                            ""

        selectedAgentId =
            case form.contactOwnerId of
                Just id ->
                    String.fromInt id

                Nothing ->
                    defaultAgentId

        selectedCarrier =
            Maybe.withDefault "" form.currentCarrier

        emailField =
            div [ class "form-group mb-3 relative" ]
                [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ]
                    [ text "Email" ]
                , div [ class "relative" ]
                    [ Html.input
                        [ type_ "email"
                        , class
                            ("w-full px-2 py-1.5 bg-white border-[1.5px] rounded-md text-sm text-gray-700 placeholder-gray-400 shadow-sm transition-all duration-200 "
                                ++ (if model.emailExists then
                                        "border-red-300 hover:border-red-400 focus:border-red-500 focus:ring-1 focus:ring-red-200"

                                    else
                                        "border-purple-300 hover:border-purple-400 focus:border-purple-500 focus:ring-1 focus:ring-purple-200"
                                   )
                            )
                        , value form.email
                        , onInput (updateMsg Email)
                        , on "blur" (Decode.succeed (EmailBlur form.email)) -- Add onBlur handler
                        , required True
                        ]
                        []
                    , if model.isCheckingEmail then
                        div [ class "absolute right-2 top-1.5" ]
                            [ viewSpinner ]

                      else if model.emailExists then
                        div
                            [ class "absolute right-2 top-1.5 text-red-500" ]
                            [ viewIcon "M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z" ]

                      else if String.length form.email > 0 then
                        div
                            [ class "absolute right-2 top-1.5 text-green-500" ]
                            [ viewIcon "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" ]

                      else
                        text ""
                    ]
                , if model.emailExists then
                    div [ class "mt-1 text-xs text-red-600" ]
                        [ text "A contact with this email already exists" ]

                  else
                    text ""
                ]
    in
    Html.form [ onSubmit submitMsg ]
        [ div [ class "grid grid-cols-1 sm:grid-cols-2 gap-x-4 gap-y-3" ]
            [ viewFormInput "First Name" "text" form.firstName FirstName updateMsg True
            , viewFormInput "Last Name" "text" form.lastName LastName updateMsg True
            , emailField
            , viewFormInput "Phone Number" "text" form.phoneNumber PhoneNumber updateMsg False
            , viewFormSelect "Current Carrier" selectedCarrier CurrentCarrier updateMsg carrierOptions
            , viewFormSelect "Plan Type" (Maybe.withDefault "" form.planType) PlanType updateMsg planTypeOptions
            , viewFormSelectWithValue "Assigned Agent" selectedAgentId ContactOwnerId updateMsg agentOptions
            , viewFormInput "Effective Date" "date" form.effectiveDate EffectiveDate updateMsg False
            , viewFormInput "Birth Date" "date" form.birthDate BirthDate updateMsg False
            , viewFormRadioGroup "Tobacco User"
                (if form.tobaccoUser then
                    "true"

                 else
                    "false"
                )
                TobaccoUser
                updateMsg
                [ ( "true", "Yes" ), ( "false", "No" ) ]
            , viewFormRadioGroup "Gender" form.gender Gender updateMsg [ ( "M", "Male" ), ( "F", "Female" ) ]
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
                        isContactFormValid form && not model.emailExists && not model.isCheckingEmail
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
                    [ text buttonText ]
            ]
        ]


viewFormInput : String -> String -> String -> ContactFormField -> (ContactFormField -> String -> Msg) -> Bool -> Html Msg
viewFormInput labelText inputType inputValue field updateMsg isRequired =
    let
        displayValue =
            if field == PhoneNumber then
                formatPhoneNumber inputValue

            else
                inputValue

        inputHandler =
            if field == PhoneNumber then
                \val ->
                    let
                        digits =
                            String.filter Char.isDigit val |> String.left 10
                    in
                    updateMsg field digits

            else
                updateMsg field

        placeholderText =
            if field == PhoneNumber then
                "(555) 555-5555"

            else
                ""
    in
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ]
            [ text labelText ]
        , Html.input
            [ type_ inputType
            , class "w-full px-2 py-1.5 bg-white border-[1.5px] border-purple-300 rounded-md text-sm text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-1 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , value displayValue
            , onInput inputHandler
            , required isRequired
            , placeholder placeholderText
            ]
            []
        ]


viewFormSelect : String -> String -> ContactFormField -> (ContactFormField -> String -> Msg) -> List ( String, String ) -> Html Msg
viewFormSelect labelText selectedValue field updateMsg options =
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ]
            [ text labelText ]
        , Html.select
            [ class "w-full px-2 py-1.5 bg-white border-[1.5px] border-purple-300 rounded-md text-sm text-gray-700 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-1 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , value selectedValue
            , onInput (updateMsg field)
            , required False
            ]
            (List.map
                (\( val, label ) ->
                    Html.option
                        [ value val, selected (val == selectedValue) ]
                        [ text label ]
                )
                options
            )
        ]


viewFormRadioGroup : String -> String -> ContactFormField -> (ContactFormField -> String -> Msg) -> List ( String, String ) -> Html Msg
viewFormRadioGroup labelText selectedValue field updateMsg options =
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ]
            [ text labelText ]
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
                            , onInput (\_ -> updateMsg field val)
                            , class "sr-only" -- Hide the actual radio button
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
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ]
            [ text "ZIP Code" ]
        , Html.input
            [ type_ "text"
            , class "w-full px-2 py-1.5 bg-white border-[1.5px] border-purple-300 rounded-md text-sm text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-1 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , value form.zipCode
            , onInput
                (\zip ->
                    Batch
                        [ case model.showModal of
                            AddModal ->
                                UpdateAddForm ZipCode zip

                            EditModal _ ->
                                UpdateEditForm ZipCode zip

                            _ ->
                                NoOp
                        , if String.length zip == 5 then
                            LookupZipCode zip

                          else
                            NoOp
                        ]
                )
            ]
            []
        ]


viewStateField : ContactForm -> Html Msg
viewStateField form =
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ]
            [ text "State" ]
        , Html.input
            [ type_ "text"
            , class "w-full px-2 py-1.5 bg-white border-[1.5px] border-gray-200 rounded-md text-sm text-gray-700 placeholder-gray-400 shadow-sm focus:ring-1 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , value form.state
            , Html.Attributes.disabled True
            ]
            []
        ]


viewSpinner : Html msg
viewSpinner =
    div [ class "animate-spin rounded-full h-5 w-5 border-2 border-purple-500 border-t-transparent" ] []


onClickOutside : msg -> Html.Attribute msg
onClickOutside msg =
    on "click" (Decode.succeed msg)


userDecoder : Decode.Decoder User
userDecoder =
    Decode.succeed User
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "first_name" Decode.string
        |> Pipeline.required "last_name" Decode.string
        |> Pipeline.required "is_admin" Decode.bool
        |> Pipeline.required "is_agent" Decode.bool
        |> Pipeline.required "is_default" Decode.bool
        |> Pipeline.required "organization_id" Decode.int
        |> Pipeline.required "is_active" Decode.bool
        |> Pipeline.required "phone" Decode.string
        |> Pipeline.optional "carriers" (Decode.list Decode.string) []
        |> Pipeline.optional "stateLicenses" (Decode.list Decode.string) []


deleteContacts : List Int -> Cmd Msg
deleteContacts contactIds =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/contacts"
        , body = Http.jsonBody (encodeContactIds contactIds)
        , expect = Http.expectJson ContactsDeleted deleteResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


encodeContactIds : List Int -> Encode.Value
encodeContactIds ids =
    Encode.list Encode.int ids


deleteResponseDecoder : Decode.Decoder DeleteResponse
deleteResponseDecoder =
    Decode.map3 DeleteResponse
        (Decode.field "success" Decode.bool)
        (Decode.field "deleted_ids" (Decode.list Decode.int))
        (Decode.field "message" Decode.string)



-- HELPER FUNCTIONS


isAdminOrAdminAgent : Maybe User -> Bool
isAdminOrAdminAgent maybeUser =
    case maybeUser of
        Just user ->
            let
                isAdmin =
                    user.isAdmin
            in
            isAdmin

        Nothing ->
            False


viewExpandedContent : Contact -> Html Msg
viewExpandedContent contact =
    div
        [ class "grid grid-cols-5 gap-4 py-2 px-4 transition-all duration-200 ease-in-out" ]
        [ viewExpandedField "Birth Date" (Maybe.withDefault "" contact.birthDate)
        , viewExpandedField "Tobacco User"
            (case contact.tobaccoUser of
                Just True ->
                    "Yes"

                Just False ->
                    "No"

                Nothing ->
                    ""
            )
        , viewExpandedField "Gender" (Maybe.withDefault "" contact.gender)
        , viewExpandedField "ZIP Code" (Maybe.withDefault "" contact.zipCode)
        , viewExpandedField "Plan Type" (Maybe.withDefault "" contact.planType)
        ]


viewExpandedField : String -> String -> Html Msg
viewExpandedField label value =
    div [ class "text-sm" ]
        [ span [ class "font-medium text-gray-500" ] [ text label ]
        , div [ class "mt-1 text-gray-900" ] [ text value ]
        ]


viewIcon : String -> Html Msg
viewIcon path =
    svg
        [ Svg.Attributes.class "w-4 h-4"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.viewBox "0 0 24 24"
        ]
        [ Svg.path [ Svg.Attributes.d path ] [] ]



-- HTTP FUNCTIONS


fetchContacts : Model -> Cmd Msg
fetchContacts model =
    let
        queryParams =
            [ ( "search", model.searchQuery )
            , ( "states", String.join "," model.activeFilters.states )
            , ( "carriers", String.join "," model.activeFilters.carriers )
            , ( "agents"
              , model.activeFilters.agents
                    |> List.map String.fromInt
                    |> String.join ","
              )
            , ( "page", String.fromInt model.pagination.currentPage )
            , ( "limit", String.fromInt model.pagination.itemsPerPage )
            ]
                |> List.filter (\( _, value ) -> not (String.isEmpty value))
                |> List.map (\( key, value ) -> Url.string key value)
    in
    Http.get
        { url = Url.absolute [ "api", "contacts" ] queryParams
        , expect = Http.expectJson GotContacts contactsDecoder
        }


fetchCurrentUser : Cmd Msg
fetchCurrentUser =
    Http.get
        { url = "/api/me"
        , expect = Http.expectJson GotCurrentUser userDecoder
        }


viewFilterDropdown : Model -> FilterType -> Html Msg
viewFilterDropdown model filterType =
    let
        options =
            case filterType of
                CarrierFilter ->
                    model.availableFilters.carriers

                StateFilter ->
                    model.availableFilters.states

                AgentFilter ->
                    -- Add "Default" option for unassigned contacts
                    model.agents
                        --|> List.filter (\agent -> agent.isAgent)
                        |> List.map
                            (\agent ->
                                agent.firstName ++ " " ++ agent.lastName
                            )

                _ ->
                    []

        activeFilters =
            case filterType of
                CarrierFilter ->
                    model.activeFilters.carriers

                StateFilter ->
                    model.activeFilters.states

                AgentFilter ->
                    model.activeFilters.agents
                        |> List.map String.fromInt

                _ ->
                    []

        hasActiveFilters =
            not (List.isEmpty activeFilters)
    in
    div
        [ class "absolute left-0 mt-2 w-60 sm:w-48 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5 z-10"

        -- IMPORTANT: This mousedown handler prevents the dropdown from closing when clicking inside it
        , Html.Events.stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
        ]
        [ div [ class "py-1" ]
            [ div [ class "p-2 border-b border-gray-200" ]
                [ button
                    [ class
                        ("w-full text-left text-sm font-medium "
                            ++ (if hasActiveFilters then
                                    "text-purple-600 hover:text-purple-800 cursor-pointer"

                                else
                                    "text-gray-400 cursor-not-allowed"
                               )
                        )
                    , onClick (SelectAllFilter filterType False)
                    , Html.Attributes.disabled (not hasActiveFilters)
                    ]
                    [ text "Clear Filters" ]
                ]
            , div [ class "max-h-56 sm:max-h-48 overflow-y-auto p-2" ]
                (case filterType of
                    AgentFilter ->
                        -- Special handling for agent filter since it's using IDs
                        -- The "Default" option (agentId = 0) is removed.
                        model.agents
                            --|> List.filter (\agent -> agent.isAgent)
                            |> List.map
                                (\agent ->
                                    label
                                        [ class "flex items-center space-x-2 py-1" ]
                                        [ input
                                            [ type_ "checkbox"
                                            , checked (List.member agent.id model.activeFilters.agents)
                                            , onClick (ToggleFilter filterType (String.fromInt agent.id))
                                            , class "rounded border-gray-300 text-purple-600 focus:ring-purple-500"
                                            ]
                                            []
                                        , span [ class "text-sm text-gray-600" ]
                                            [ text (agent.firstName ++ " " ++ agent.lastName) ]
                                        ]
                                )

                    _ ->
                        -- Original handling for other filters
                        List.map
                            (\option ->
                                label
                                    [ class "flex items-center space-x-2 py-1" ]
                                    [ input
                                        [ type_ "checkbox"
                                        , checked (List.member option activeFilters)
                                        , onClick (ToggleFilter filterType option)
                                        , class "rounded border-gray-300 text-purple-600 focus:ring-purple-500"
                                        ]
                                        []
                                    , span [ class "text-sm text-gray-600" ]
                                        [ text option ]
                                    ]
                            )
                            options
                )
            ]
        ]


isContactFormValid : ContactForm -> Bool
isContactFormValid form =
    String.length form.firstName
        > 0
        && String.length form.lastName
        > 0
        && String.length form.email
        > 0


isJust : Maybe a -> Bool
isJust maybeValue =
    case maybeValue of
        Just _ ->
            True

        Nothing ->
            False



-- Add the isValidEmail function here


isValidEmail : String -> Bool
isValidEmail email =
    String.contains "@" email
        && String.contains "." email
        && String.length email
        > 5


fetchCarriers : Cmd Msg
fetchCarriers =
    Http.get
        { url = "/api/settings/carriers"
        , expect = Http.expectJson GotCarriers (Decode.list (Decode.field "name" Decode.string))
        }


fetchAgents : Cmd Msg
fetchAgents =
    Http.get
        { url = "/api/agents"
        , expect = Http.expectJson GotAgents agentsResponseDecoder
        }



-- Add a decoder for the new response format


type alias AgentsResponseWithDefault =
    { agents : List User
    , defaultAgentId : Maybe String
    }


agentsResponseDecoder : Decoder AgentsResponseWithDefault
agentsResponseDecoder =
    Decode.succeed AgentsResponseWithDefault
        |> Pipeline.required "agents" (Decode.list agentDecoder)
        |> Pipeline.optional "defaultAgentId" (Decode.nullable Decode.string) Nothing


checkEmail : String -> Cmd Msg
checkEmail email =
    Http.request
        { method = "GET"
        , headers = []
        , url = "/api/contacts/check-email/" ++ email
        , body = Http.emptyBody
        , expect = Http.expectJson EmailChecked emailCheckDecoder
        , timeout = Just 5000 -- 5 second timeout
        , tracker = Nothing
        }


emailCheckDecoder : Decode.Decoder { exists : Bool }
emailCheckDecoder =
    Decode.oneOf
        [ -- Try to decode the standard response
          Decode.map (\exists -> { exists = exists })
            (Decode.field "exists" Decode.bool)
        , -- Try to decode success response wrapper with exists field
          Decode.field "success" Decode.bool
            |> Decode.andThen
                (\_ ->
                    Decode.map (\exists -> { exists = exists })
                        (Decode.field "exists" Decode.bool)
                )
        , -- Fallback for any other structure
          Decode.succeed { exists = False }
        ]


carrierDecoder : Decode.Decoder { name : String, aliases : List String }
carrierDecoder =
    Decode.succeed (\name aliases -> { name = name, aliases = aliases })
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "aliases" (Decode.list Decode.string)


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
                [ text "Delete Contacts" ]
            , p [ class "text-sm text-gray-600 mb-6" ]
                [ text ("Are you sure you want to delete " ++ String.fromInt (List.length model.selectedContacts) ++ " contacts? This action cannot be undone.") ]
            , div [ class "flex justify-end space-x-4" ]
                [ button
                    [ class "px-4 py-2 text-gray-700 text-sm font-medium rounded-lg border-2 border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200"
                    , onClick CloseModal
                    ]
                    [ text "Cancel" ]
                , if model.isDeletingContacts then
                    div [ class "px-4 py-2 flex items-center" ]
                        [ viewSpinner ]

                  else
                    button
                        [ class "px-4 py-2 bg-red-600 text-white text-sm font-medium rounded-lg hover:bg-red-700 transition-colors duration-200"
                        , onClick DeleteSelectedContacts
                        ]
                        [ text "Delete" ]
                ]
            ]
        ]



-- Add a new function to allow setting a custom selected value


viewFormSelectWithValue : String -> String -> ContactFormField -> (ContactFormField -> String -> Msg) -> List ( String, String ) -> Html Msg
viewFormSelectWithValue labelText selectedValue field updateMsg options =
    div [ class "form-group mb-3" ]
        [ Html.label [ class "block text-xs font-medium text-gray-700 mb-1" ]
            [ text labelText ]
        , div [ class "relative" ]
            [ Html.select
                [ class "w-full px-2 py-1.5 bg-white border-[1.5px] border-purple-300 rounded-md text-sm text-gray-700 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-1 focus:ring-purple-200 focus:bg-white transition-all duration-200 appearance-none"
                , value selectedValue
                , onInput (updateMsg field)
                , required False
                ]
                (option [ value "", disabled True, selected (selectedValue == "") ] [ text "Select an Agent" ]
                    :: List.map (\( val, txt ) -> option [ value val, selected (val == selectedValue) ] [ text txt ]) options
                )
            , div [ class "absolute inset-y-0 right-0 flex items-center px-1 pointer-events-none text-gray-500" ]
                [ viewIcon "M19 9l-7 7-7-7" ]
            ]
        ]


agentDecoder : Decode.Decoder User
agentDecoder =
    Decode.succeed User
        |> Pipeline.required "id"
            (Decode.oneOf
                [ -- Try to decode as an integer directly
                  Decode.int
                , -- If that fails, try to decode as a string and convert to int
                  Decode.string
                    |> Decode.andThen
                        (\str ->
                            case String.toInt str of
                                Just intVal ->
                                    Decode.succeed intVal

                                Nothing ->
                                    Decode.fail ("Could not convert agent ID string to integer: " ++ str)
                        )
                ]
            )
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "firstName" Decode.string
        |> Pipeline.required "lastName" Decode.string
        |> Pipeline.required "isAdmin" Decode.bool
        |> Pipeline.required "isAgent" Decode.bool
        |> Pipeline.required "isDefault" Decode.bool
        |> Pipeline.optional "organizationId" Decode.int 0
        -- Add default value for isActive since it's not in the API response
        |> Pipeline.hardcoded True
        -- Assume agents are active
        |> Pipeline.optional "phone" Decode.string ""
        |> Pipeline.optional "carriers" (Decode.list Decode.string) []
        |> Pipeline.optional "stateLicenses" (Decode.list Decode.string) []


viewReassignAgentModal : Model -> Html Msg
viewReassignAgentModal model =
    let
        -- Use all users in the agents list
        agentList =
            model.agents

        -- Create list of agent options
        agentOptions =
            List.map
                (\agent ->
                    ( agent.id
                    , agent.firstName ++ " " ++ agent.lastName
                    )
                )
                agentList

        -- Get the first agent ID as the default value if no agent is currently selected
        defaultAgentId =
            case List.head agentList of
                Just agent ->
                    String.fromInt agent.id

                Nothing ->
                    "0"
    in
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-4 sm:p-8" ]
        [ div [ class "bg-white rounded-xl p-5 sm:p-8 md:p-10 max-w-2xl w-full mx-4 shadow-xl relative overflow-y-auto max-h-[90vh]" ]
            [ button
                [ class "absolute top-2 sm:top-4 right-2 sm:right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Reassign Contacts" ]
            , div [ class "mb-6 text-sm text-gray-600" ]
                [ text "Select an agent to reassign the selected contacts to." ]
            , div [ class "form-group" ]
                [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                    [ text "Select Agent" ]
                , div [ class "relative" ]
                    [ if List.isEmpty model.agents then
                        div [ class "p-2 text-gray-500 border rounded" ]
                            [ text "Loading agents..." ]

                      else
                        Html.select
                            [ class "w-full px-3 py-2 sm:px-4 sm:py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200 appearance-none"
                            , value defaultAgentId
                            , onInput (\val -> SelectReassignAgent (String.toInt val |> Maybe.withDefault 0))
                            ]
                            (List.map
                                (\( val, label ) ->
                                    option [ value (String.fromInt val) ] [ text label ]
                                )
                                agentOptions
                            )
                    , div [ class "absolute inset-y-0 right-0 flex items-center px-2 pointer-events-none" ]
                        [ viewIcon "M19 9l-7 7-7-7" ]
                    ]
                ]
            , div [ class "mt-8 flex justify-end space-x-4" ]
                [ button
                    [ class "px-4 py-2 sm:px-6 sm:py-3 bg-white text-gray-700 text-sm font-medium rounded-lg border-2 border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200 focus:ring-4 focus:ring-purple-100"
                    , onClick CloseModal
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "px-4 py-2 sm:px-6 sm:py-3 bg-purple-500 text-white text-sm font-medium rounded-lg hover:bg-purple-600 transition-colors duration-200 focus:ring-4 focus:ring-purple-200"
                    , onClick ReassignSelectedContacts
                    ]
                    [ text "Reassign" ]
                ]
            ]
        ]


reassignContacts : List Int -> Int -> Cmd Msg
reassignContacts contactIds agentId =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/contacts/reassign"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "contact_ids", Encode.list Encode.int contactIds )
                    , ( "agent_id"
                      , if agentId == 0 then
                            -- Send null for Default option
                            Encode.null

                        else
                            Encode.int agentId
                      )
                    ]
                )
        , expect = Http.expectJson ContactsReassigned reassignResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


reassignResponseDecoder : Decode.Decoder ReassignResponse
reassignResponseDecoder =
    Decode.succeed ReassignResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "updated_ids" (Decode.list Decode.int)
        |> Pipeline.required "message" Decode.string


toggleAgentList : List Int -> Int -> List Int
toggleAgentList list value =
    if List.member value list then
        List.filter (\v -> v /= value) list

    else
        value :: list



-- Add the filterByAgents function


filterByAgents : List Int -> Maybe Int -> List Contact -> List Contact
filterByAgents agentIds defaultAgentId contacts =
    if List.isEmpty agentIds then
        -- No specific agents selected, filter by defaultAgentId or unassigned
        List.filter
            (\contact ->
                case contact.agentId of
                    Just id ->
                        id == Maybe.withDefault -1 defaultAgentId

                    -- Using -1 as a sentinel for Nothing that won't match any real ID
                    Nothing ->
                        True
             -- Include unassigned contacts
            )
            contacts

    else
        -- Specific agents selected
        List.filter
            (\contact ->
                case contact.agentId of
                    Just agentId ->
                        List.member agentId agentIds

                    Nothing ->
                        False
            )
            contacts



-- Replace the old contact limit banner functions with this more streamlined one


viewPaginationControls : Model -> Html Msg
viewPaginationControls model =
    let
        totalPages =
            model.pagination.totalPages

        currentPage =
            model.pagination.currentPage

        itemsPerPage =
            model.pagination.itemsPerPage

        totalItems =
            model.pagination.totalItems

        -- Calculate range of items being displayed
        startItem =
            (currentPage - 1) * itemsPerPage + 1

        endItem =
            min (currentPage * itemsPerPage) totalItems

        -- Create a list of page numbers to show
        pageNumbers =
            if totalPages <= 7 then
                List.range 1 totalPages

            else if currentPage <= 4 then
                List.range 1 5 ++ [ -1, totalPages ]

            else if currentPage >= totalPages - 3 then
                1 :: -1 :: List.range (totalPages - 4) totalPages

            else
                1 :: -1 :: List.range (currentPage - 1) (currentPage + 1) ++ [ -1, totalPages ]

        -- Helper function to render a page button
        pageButton page =
            if page == -1 then
                -- Render ellipsis for skipped pages
                span [ class "px-3 py-2 text-gray-400" ] [ text "..." ]

            else
                button
                    [ class
                        ("px-3 py-2 text-sm font-medium rounded-md transition-colors "
                            ++ (if page == currentPage then
                                    "bg-purple-50 text-purple-600 border-purple-500"

                                else
                                    "text-gray-500 hover:bg-gray-50 hover:text-gray-700"
                               )
                        )
                    , onClick (ChangePage page)
                    ]
                    [ text (String.fromInt page) ]
    in
    div [ class "max-w-7xl mx-auto pb-24" ]
        -- Added padding at bottom to prevent overlap with bulk action bar
        [ div [ class "mt-4 flex items-center justify-between border-t border-gray-200 bg-white px-4 py-3 sm:px-6" ]
            [ div [ class "flex flex-1 justify-between sm:hidden" ]
                [ button
                    [ class "relative inline-flex items-center rounded-md border border-gray-300 bg-white px-4 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50"
                    , onClick (ChangePage (currentPage - 1))
                    , Html.Attributes.disabled (currentPage == 1)
                    ]
                    [ text "Previous" ]
                , button
                    [ class "relative ml-3 inline-flex items-center rounded-md border border-gray-300 bg-white px-4 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50"
                    , onClick (ChangePage (currentPage + 1))
                    , Html.Attributes.disabled (currentPage == totalPages)
                    ]
                    [ text "Next" ]
                ]
            , div [ class "hidden sm:flex sm:flex-1 sm:items-center sm:justify-between" ]
                [ div [ class "text-sm text-gray-700" ]
                    [ span [] [ text "Showing " ]
                    , span [ class "font-medium" ] [ text (String.fromInt startItem) ]
                    , span [] [ text " to " ]
                    , span [ class "font-medium" ] [ text (String.fromInt endItem) ]
                    , span [] [ text " of " ]
                    , span [ class "font-medium" ] [ text (String.fromInt totalItems) ]
                    , span [] [ text " results" ]
                    ]
                , div [ class "flex items-center space-x-4" ]
                    [ div [ class "relative" ]
                        [ select
                            [ class "block w-full rounded-md border-gray-300 py-1.5 pl-3 pr-10 text-base focus:border-purple-500 focus:outline-none focus:ring-purple-500 sm:text-sm"
                            , onInput (\val -> ChangeItemsPerPage (Maybe.withDefault 100 (String.toInt val)))
                            , value (String.fromInt itemsPerPage)
                            ]
                            [ option [ value "50" ] [ text "50 per page" ]
                            , option [ value "100" ] [ text "100 per page" ]
                            , option [ value "250" ] [ text "250 per page" ]
                            ]
                        ]
                    , nav
                        [ class "isolate inline-flex -space-x-px rounded-md shadow-sm"
                        , Html.Attributes.attribute "aria-label" "Pagination"
                        ]
                        (List.map pageButton pageNumbers)
                    ]
                ]
            ]
        ]


extractCsvHeaders : File -> Cmd Msg
extractCsvHeaders file =
    Task.perform
        (\content -> CsvHeadersExtracted (extractHeadersFromCsv content))
        (File.toString file)


extractHeadersFromCsv : String -> Result String (List String)
extractHeadersFromCsv csvContent =
    case CsvProcessor.extractHeaders csvContent of
        Ok headers ->
            Ok headers

        Err CsvProcessor.EmptyFile ->
            Err "CSV file appears to be empty"

        Err CsvProcessor.NoHeaders ->
            Err "CSV file has no headers"

        Err (CsvProcessor.ParseError msg) ->
            Err ("Failed to parse CSV file: " ++ msg)


suggestMappings : List String -> Cmd Msg
suggestMappings headers =
    let
        -- Use the CsvProcessor to find best matches for column mappings locally
        columnMappings =
            CsvProcessor.suggestColumnMappings headers

        -- Create an empty carrier mapping for now (will be populated later)
        carrierMappings =
            Dict.empty
    in
    -- Return the suggested mappings directly instead of making an HTTP request
    Task.perform
        (\_ ->
            SuggestedMappingsReceived
                (Ok
                    { columnMappings = columnMappings
                    , carrierMappings = carrierMappings
                    }
                )
        )
        (Task.succeed ())


extractUniqueCarriers : String -> String -> List String
extractUniqueCarriers csvContent carrierColumn =
    case CsvProcessor.extractUniqueValues csvContent carrierColumn of
        Ok values ->
            values

        Err _ ->
            []



-- Helper function to get an element at a specific index


getAt : Int -> List a -> Maybe a
getAt index list =
    if index < 0 then
        Nothing

    else
        List.head (List.drop index list)



-- Add a function to fetch carriers and create CsvProcessor.CarrierMapping compatible format


fetchAndProcessCarriers : List String -> Cmd Msg
fetchAndProcessCarriers detectedCarriers =
    -- Use a model function instead of an API call
    -- We'll use this from the GotCarriers handler
    Cmd.none



-- Add this helper function near other helper functions


hasCarrierMappings : UploadState -> Bool
hasCarrierMappings state =
    case state.carrierMapping of
        Just mapping ->
            not (List.isEmpty mapping.detectedCarriers)

        Nothing ->
            False


getUserName : Maybe User -> String
getUserName maybeUser =
    case maybeUser of
        Just user ->
            user.firstName ++ " " ++ user.lastName

        Nothing ->
            "Default"
