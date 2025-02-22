module Dashboard exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser
import Browser.Events
import Browser.Navigation as Nav
import File exposing (File)
import File.Download
import File.Select as Select
import Html exposing (Html, button, col, colgroup, div, h1, h2, h3, input, label, nav, option, p, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, placeholder, required, title, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit, preventDefaultOn, stopPropagationOn)
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, nullable, string)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, stroke, viewBox)
import Task
import Time
import Url exposing (Url)
import Url.Builder as Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = \flags url key -> init key
        , view = \model -> { title = "Dashboard", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \_ -> NoOp
        }



-- MODEL


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


type Modal
    = NoModal
    | ContactChoiceModal
    | AddModal
    | EditModal Contact
    | CsvUploadModal UploadState


type alias Model =
    { contacts : List Contact
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
    , key : Nav.Key
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
    , stats : Maybe UploadStats
    , overwriteDuplicates : Bool
    }


type alias UploadStats =
    { totalRows : Int
    , errorRows : Int
    , validRows : Int
    }


type alias DeleteResponse =
    { success : Bool
    , deletedIds : List Int
    , message : String
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


type alias AvailableFilters =
    { carriers : List String
    , states : List String
    }


type alias ContactsResponse =
    { contacts : List Contact
    , filterOptions : AvailableFilters
    }


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    let
        initialModel =
            { contacts = []
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
            , currentUser = Nothing
            , showProfileMenu = False
            , error = Nothing
            , saveOnUpdate = False
            , expandedContactId = Nothing
            , availableFilters = { carriers = [], states = [] }
            , carriers = []
            , agents = []
            , key = key
            }
    in
    ( initialModel
    , Cmd.batch
        [ fetchContacts initialModel
        , fetchCurrentUser
        , Task.perform GotCurrentTime Time.now
        , fetchCarriers
        , fetchAgents
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
    , currentCarrier = ""
    , effectiveDate = ""
    , birthDate = ""
    , tobaccoUser = False
    , gender = "M"
    , zipCode = ""
    , planType = ""
    }


emptyFilters : Filters
emptyFilters =
    { carriers = []
    , states = []
    , ageRange = Nothing
    }


emptyUploadState : UploadState
emptyUploadState =
    { dragOver = False
    , file = Nothing
    , error = Nothing
    , errorCsv = Nothing
    , stats = Nothing
    , overwriteDuplicates = True
    }



-- UPDATE


type Msg
    = NoOp
    | ShowContactChoiceModal
    | ChooseSingleContact
    | ChooseMultipleContacts
    | ShowAddModal
    | ShowEditModal Contact
    | CloseModal
    | UpdateSearchQuery String
    | UpdateAddForm ContactFormField String
    | UpdateEditForm ContactFormField String
    | SubmitAddForm
    | SubmitEditForm
    | CheckEmail String
    | EmailChecked (Result Http.Error { exists : Bool })
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
    | DeleteSelectedContacts
    | ContactsDeleted (Result Http.Error DeleteResponse)
    | ToggleOverwriteDuplicates Bool
    | GotCurrentUser (Result Http.Error User)
    | NavigateToContact Int
    | GotCarriers (Result Http.Error (List String))
    | GotAgents (Result Http.Error (List User))


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShowContactChoiceModal ->
            ( { model | showModal = ContactChoiceModal }, Cmd.none )

        ChooseSingleContact ->
            ( { model | showModal = AddModal }, Cmd.none )

        ChooseMultipleContacts ->
            ( { model | showModal = CsvUploadModal emptyUploadState }, Cmd.none )

        ShowAddModal ->
            ( { model | showModal = AddModal }, Cmd.none )

        ShowEditModal contact ->
            ( { model
                | showModal = EditModal contact
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
                        submitEditFormWithFlag updatedForm True

                    else if field == Email && String.length value > 0 then
                        checkEmail value

                    else
                        Cmd.none
            in
            ( { model
                | addForm = updatedForm
                , isCheckingEmail = field == Email && String.length value > 0
                , emailExists = False
                , error = Nothing
              }
            , cmd
            )

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
                        submitEditFormWithFlag updatedForm True

                    else
                        Cmd.none
            in
            ( { model | editForm = updatedForm }, cmd )

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

        EmailChecked (Err _) ->
            ( { model
                | isCheckingEmail = False
                , error = Just "Failed to check email. Please try again."
              }
            , Cmd.none
            )

        GotContacts (Ok response) ->
            ( { model
                | contacts = response.contacts
                , isLoadingContacts = False
                , availableFilters = response.filterOptions
              }
            , Cmd.none
            )

        GotContacts (Err error) ->
            let
                _ =
                    Debug.log "Error fetching contacts" error
            in
            ( model, Cmd.none )

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
                                    |> (\form -> { form | state = contact.state })
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
                            }

                        StateFilter ->
                            { carriers = model.activeFilters.carriers
                            , states =
                                if select then
                                    options

                                else
                                    []
                            , ageRange = model.activeFilters.ageRange
                            }

                        _ ->
                            model.activeFilters

                updatedModel =
                    { model | activeFilters = updatedFilters }
            in
            ( updatedModel, fetchContacts updatedModel )

        CloseFilterDropdown ->
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
                        |> filterContacts model.activeFilters model.searchQuery model.currentTime
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
            ( { model | showModal = CsvUploadModal emptyUploadState }, Cmd.none )

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
            let
                _ =
                    Debug.log "File dropped"
                        { fileName = File.name file
                        , fileSize = File.size file
                        }
            in
            case model.showModal of
                CsvUploadModal state ->
                    ( { model | showModal = CsvUploadModal { state | file = Just file, dragOver = False } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FileSelected file ->
            case model.showModal of
                CsvUploadModal state ->
                    ( { model | showModal = CsvUploadModal { state | file = Just file } }
                    , Cmd.none
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
                            , uploadCsv file state.overwriteDuplicates
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CsvUploaded (Ok response) ->
            let
                _ =
                    Debug.log "CSV Upload Response" response

                errorMessage =
                    if String.startsWith "Missing required columns:" response.message then
                        let
                            missingColumns =
                                String.dropLeft (String.length "Missing required columns:") response.message
                                    |> String.trim
                                    |> String.split ","
                                    |> List.map String.trim
                                    |> String.join ", "
                        in
                        "Your CSV is missing the following required columns: " ++ missingColumns ++ ". Please add these columns and try again."

                    else
                        response.message

                currentModal =
                    case model.showModal of
                        CsvUploadModal state ->
                            if response.success && response.errorRows == 0 then
                                NoModal

                            else
                                CsvUploadModal
                                    { state
                                        | error = Just errorMessage
                                        , errorCsv = response.errorCsv
                                        , stats =
                                            Just
                                                { totalRows = response.totalRows
                                                , errorRows = response.errorRows
                                                , validRows = response.validRows
                                                }
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
                _ =
                    Debug.log "CSV Upload Error" httpError

                errorMessage =
                    case httpError of
                        Http.BadStatus 400 ->
                            "The CSV format is invalid. Please check that all required columns are present and data is in the correct format."

                        Http.NetworkError ->
                            "Network error. Please check your connection and try again."

                        Http.Timeout ->
                            "The upload timed out. Please try again."

                        _ ->
                            "An unexpected error occurred while uploading the CSV. Please try again."
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

        DeleteSelectedContacts ->
            ( { model | isDeletingContacts = True }
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

        GotAgents (Ok agents) ->
            ( { model | agents = agents }, Cmd.none )

        GotAgents (Err _) ->
            ( model, Cmd.none )



-- TODO: Handle error
-- Add other update cases here...
-- VIEW


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-white" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
            [ -- Stats Section
              div [ class "grid grid-cols-1 gap-5 sm:grid-cols-2 lg:grid-cols-4 mb-8" ]
                [ statsCard "Total Contacts" (String.fromInt (List.length model.contacts))
                , statsCard "Emails Sent" "1824"
                , statsCard "Emails Clicked" "425"
                , statsCard "Quotes Created" "385"
                ]
            , -- Filters and Actions
              div [ class "flex justify-between items-center mb-6" ]
                [ div [ class "flex items-center gap-4" ]
                    [ h1 [ class "text-lg font-semibold" ] [ text "Contacts " ]
                    , span [ class "text-sm text-gray-500" ]
                        [ text ("(" ++ String.fromInt (List.length model.contacts) ++ ")") ]
                    ]
                , div [ class "flex items-center gap-3" ]
                    [ div [ class "relative" ]
                        [ button
                            [ class "inline-flex items-center gap-2 px-3 py-2 border rounded-lg text-sm text-gray-700 hover:bg-gray-50"
                            , onClick (ToggleFilterDropdown CarrierFilter)
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
                            [ class "inline-flex items-center gap-2 px-3 py-2 border rounded-lg text-sm text-gray-700 hover:bg-gray-50"
                            , onClick (ToggleFilterDropdown StateFilter)
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
                            [ class "w-64 px-4 py-2 border rounded-lg text-sm placeholder-gray-500 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
                            , placeholder "Search contacts..."
                            , value model.searchQuery
                            , onInput UpdateSearchQuery
                            ]
                            []
                        ]
                    , button
                        [ class "px-4 py-2 bg-black text-white rounded-lg text-sm hover:bg-gray-800 transition-colors"
                        , onClick ShowContactChoiceModal
                        ]
                        [ text "+ Add Contact" ]
                    ]
                ]
            , -- Table Container with overflow handling
              div [ class "overflow-x-auto" ]
                [ table [ class "min-w-full border-separate border-spacing-0" ]
                    [ colgroup []
                        [ col [ class "w-12" ] [] -- Checkbox
                        , col [ class "w-48" ] [] -- Name
                        , col [ class "w-32" ] [] -- Contact Status
                        , col [ class "w-48" ] [] -- Email
                        , col [ class "w-32" ] [] -- Phone Number
                        , col [ class "w-16" ] [] -- State
                        , col [ class "w-32" ] [] -- Contact Owner (optional)
                        , col [ class "w-32" ] [] -- Current Carrier
                        , col [ class "w-28" ] [] -- Effective Date
                        , col [ class "w-20" ] [] -- Actions
                        ]
                    , thead [ class "bg-gray-50" ]
                        [ tr []
                            [ th [ class "sticky top-0 px-3 py-2 border-b border-gray-200 bg-gray-50" ]
                                [ input
                                    [ type_ "checkbox"
                                    , class "rounded border-gray-300 text-purple-600 focus:ring-purple-500"
                                    , checked (not (List.isEmpty model.contacts) && List.all (\c -> List.member c.id model.selectedContacts) model.contacts)
                                    , onClick
                                        (if not (List.isEmpty model.contacts) && List.all (\c -> List.member c.id model.selectedContacts) model.contacts then
                                            DeselectAllContacts

                                         else
                                            SelectAllContacts
                                        )
                                    ]
                                    []
                                ]
                            , tableHeader "Name"
                            , tableHeader "Contact Status"
                            , tableHeader "Email"
                            , tableHeader "Phone Number"
                            , tableHeader "State"
                            , if isAdminOrAdminAgent model.currentUser then
                                tableHeader "Contact Owner"

                              else
                                text ""
                            , tableHeader "Current Carrier"
                            , tableHeader "Effective Date"
                            , tableHeader "Actions"
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
        , viewModals model
        ]


statsCard : String -> String -> Html Msg
statsCard title value =
    div [ class "bg-white rounded-lg border p-6 hover:shadow-lg transition-shadow" ]
        [ div [ class "text-sm text-gray-600 mb-2" ] [ text title ]
        , div [ class "text-3xl font-semibold" ] [ text value ]
        ]


tableHeader : String -> Html Msg
tableHeader headerText =
    th [ class "px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase tracking-wider border-b border-gray-200 bg-gray-50" ]
        [ text headerText ]


viewTableRow : Model -> Contact -> List (Html Msg)
viewTableRow model contact =
    let
        cellClass =
            "px-3 py-2 text-sm border-t border-gray-200"

        initials =
            String.left 1 contact.firstName ++ String.left 1 contact.lastName
    in
    [ tr [ class "hover:bg-gray-50 transition-colors duration-200" ]
        [ td
            [ class (cellClass ++ " text-center")
            , stopPropagationOn "click" (Decode.succeed ( ToggleSelectContact contact.id, True ))
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
                    [ text (contact.firstName ++ " " ++ contact.lastName) ]
                ]
            ]
        , td [ class cellClass ]
            [ viewStatus contact.status ]
        , td [ class cellClass ]
            [ text contact.email ]
        , td [ class cellClass ]
            [ text (formatPhoneNumber contact.phoneNumber) ]
        , td [ class cellClass ]
            [ text contact.state ]
        , if isAdminOrAdminAgent model.currentUser then
            td [ class cellClass ]
                [ text (Maybe.map .firstName contact.contactOwner |> Maybe.withDefault "Default") ]

          else
            text ""
        , td [ class cellClass ]
            [ text contact.currentCarrier ]
        , td [ class cellClass ]
            [ text contact.effectiveDate ]
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
    Http.post
        { url = "/api/contacts"
        , body = Http.jsonBody (encodeContactForm form)
        , expect = Http.expectJson ContactAdded contactDecoder
        }


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
    let
        debugLog label value =
            let
                _ =
                    Debug.log ("Decoding " ++ label) value
            in
            value
    in
    Decode.succeed Contact
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "first_name" Decode.string
        |> Pipeline.required "last_name" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.optional "phone_number"
            (Decode.string
                |> Decode.andThen
                    (\val ->
                        let
                            _ =
                                Debug.log "Raw phone_number" val
                        in
                        Decode.succeed val
                    )
            )
            ""
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
        |> Pipeline.required "last_emailed" (Decode.nullable Decode.string)


contactsDecoder : Decode.Decoder ContactsResponse
contactsDecoder =
    Decode.succeed ContactsResponse
        |> Pipeline.required "contacts" (Decode.list contactDecoder)
        |> Pipeline.required "filterOptions" filterOptionsDecoder


filterOptionsDecoder : Decode.Decoder AvailableFilters
filterOptionsDecoder =
    Decode.succeed AvailableFilters
        |> Pipeline.required "carriers" (Decode.list Decode.string)
        |> Pipeline.required "states" (Decode.list Decode.string)


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
            viewCsvUploadModal state model.isUploadingCsv


viewContactChoiceModal : Html Msg
viewContactChoiceModal =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-2xl w-full mx-4 shadow-xl relative" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Add Contacts" ]
            , div [ class "text-sm text-gray-600 mb-8" ]
                [ text "Select how you want to add your new contacts." ]
            , div [ class "grid grid-cols-2 gap-6" ]
                [ div
                    [ class "p-6 border-2 border-gray-200 rounded-lg hover:border-purple-400 cursor-pointer transition-colors"
                    , onClick ChooseSingleContact
                    ]
                    [ div [ class "flex items-center mb-4" ]
                        [ div [ class "h-8 w-8 rounded-full bg-purple-100 flex items-center justify-center text-sm text-purple-700 font-medium" ]
                            [ viewIcon "M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" ]
                        ]
                    , h3 [ class "text-lg font-medium text-gray-900 mb-2" ]
                        [ text "Single Contact" ]
                    , p [ class "text-sm text-gray-600" ]
                        [ text "Individual Form" ]
                    ]
                , div
                    [ class "p-6 border-2 border-gray-200 rounded-lg hover:border-purple-400 cursor-pointer transition-colors"
                    , onClick ChooseMultipleContacts
                    ]
                    [ div [ class "flex items-center mb-4" ]
                        [ div [ class "h-8 w-8 rounded-full bg-purple-100 flex items-center justify-center text-sm text-purple-700 font-medium" ]
                            [ viewIcon "M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" ]
                        ]
                    , h3 [ class "text-lg font-medium text-gray-900 mb-2" ]
                        [ text "Multiple Contacts" ]
                    , p [ class "text-sm text-gray-600" ]
                        [ text "CSV Upload" ]
                    ]
                ]
            ]
        ]


viewAddModal : Model -> Bool -> Html Msg
viewAddModal model isSubmitting =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-5xl w-full mx-4 shadow-xl relative" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Add New Client" ]
            , viewContactForm model model.addForm UpdateAddForm SubmitAddForm "Add Client" isSubmitting
            ]
        ]


viewEditModal : Model -> Bool -> Html Msg
viewEditModal model isSubmitting =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-5xl w-full mx-4 shadow-xl relative" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Edit Client" ]
            , viewContactForm model model.editForm UpdateEditForm SubmitEditForm "Save Changes" isSubmitting
            ]
        ]


viewCsvUploadModal : UploadState -> Bool -> Html Msg
viewCsvUploadModal state isUploading =
    div [ class "fixed inset-0 bg-gray-500/75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-2xl w-full mx-4 shadow-xl relative" ]
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
            , if state.error /= Nothing then
                div [ class "mb-6 p-4 bg-red-50 border border-red-200 rounded-lg" ]
                    [ div [ class "flex items-start" ]
                        [ div [ class "flex-shrink-0" ]
                            [ viewIcon "M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" ]
                        , div [ class "ml-3" ]
                            [ h3 [ class "text-sm font-medium text-red-800" ]
                                [ text "Error uploading CSV" ]
                            , div [ class "mt-2 text-sm text-red-700" ]
                                [ text (Maybe.withDefault "" state.error)
                                , case state.stats of
                                    Just stats ->
                                        if stats.errorRows > 0 then
                                            div [ class "mt-2" ]
                                                [ case state.errorCsv of
                                                    Just csvContent ->
                                                        button
                                                            [ class "text-purple-600 hover:text-purple-800 hover:underline"
                                                            , onClick (DownloadErrorCsv csvContent)
                                                            ]
                                                            [ text "Download and Fix Errors" ]

                                                    Nothing ->
                                                        text ""
                                                ]

                                        else
                                            text ""

                                    Nothing ->
                                        text ""
                                ]
                            ]
                        ]
                    ]

              else
                text ""
            , div [ class "mb-4 flex items-center space-x-2" ]
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
            , div
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
                [ viewIcon "M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12"
                , div [ class "mt-4 text-center" ]
                    [ text "Drag and drop your CSV file here, or "
                    , button
                        [ class "text-purple-500 hover:text-purple-700 hover:underline"
                        , onClick ClickedSelectFile
                        ]
                        [ text "browse" ]
                    ]
                , case state.file of
                    Just file ->
                        div [ class "mt-4 text-sm text-gray-600" ]
                            [ text ("Selected: " ++ File.name file) ]

                    Nothing ->
                        text ""
                ]
            , div [ class "mt-8 flex justify-end space-x-4" ]
                [ button
                    [ class "px-6 py-3 bg-white text-gray-700 text-sm font-medium rounded-lg border-2 border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200 focus:ring-4 focus:ring-purple-100"
                    , onClick CloseModal
                    ]
                    [ text "Cancel" ]
                , if isUploading then
                    div [ class "px-6 py-3 flex items-center space-x-2" ]
                        [ viewSpinner ]

                  else
                    button
                        [ type_ "submit"
                        , class "px-6 py-3 bg-purple-500 text-white text-sm font-medium rounded-lg hover:bg-purple-600 transition-colors duration-200 focus:ring-4 focus:ring-purple-200"
                        , onClick UploadCsv
                        , Html.Attributes.disabled (state.file == Nothing)
                        ]
                        [ text "Upload" ]
                ]
            ]
        ]


dropDecoder : (File -> msg) -> Decoder ( msg, Bool )
dropDecoder toMsg =
    Decode.at [ "dataTransfer", "files" ] (Decode.index 0 File.decoder)
        |> Decode.map (\file -> ( toMsg file, True ))


uploadCsv : File -> Bool -> Cmd Msg
uploadCsv file overwriteDuplicates =
    let
        _ =
            Debug.log "Uploading CSV"
                { fileName = File.name file
                , fileSize = File.size file
                , overwriteDuplicates = overwriteDuplicates
                }

        body =
            Http.multipartBody
                [ Http.filePart "file" file
                , Http.stringPart "overwrite_duplicates"
                    (if overwriteDuplicates then
                        "true"

                     else
                        "false"
                    )
                ]
    in
    Http.post
        { url = "/api/contacts/upload"
        , body = body
        , expect = Http.expectJson CsvUploaded uploadResponseDecoder
        }


uploadResponseDecoder : Decode.Decoder UploadResponse
uploadResponseDecoder =
    let
        errorCsvDecoder =
            Decode.oneOf
                [ Decode.string |> Decode.map Just
                , Decode.null Nothing
                ]
    in
    Decode.succeed UploadResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "message" Decode.string
        |> Pipeline.required "error_csv" errorCsvDecoder
        |> Pipeline.required "total_rows" Decode.int
        |> Pipeline.required "error_rows" Decode.int
        |> Pipeline.required "valid_rows" Decode.int


type alias UploadResponse =
    { success : Bool
    , message : String
    , errorCsv : Maybe String
    , totalRows : Int
    , errorRows : Int
    , validRows : Int
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

            _ ->
                Browser.Events.onKeyDown (Decode.map HandleKeyDown (Decode.field "key" Decode.string))
        , case model.openFilter of
            Just _ ->
                Browser.Events.onMouseDown (Decode.succeed CloseFilterDropdown)

            Nothing ->
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
                                    (a.firstName ++ " " ++ a.lastName)
                                    (b.firstName ++ " " ++ b.lastName)

                        StatusCol ->
                            \a b -> compare a.status b.status

                        EmailCol ->
                            \a b -> compare a.email b.email

                        PhoneNumberCol ->
                            \a b -> compare a.phoneNumber b.phoneNumber

                        StateCol ->
                            \a b -> compare a.state b.state

                        ContactOwnerCol ->
                            \a b ->
                                compare
                                    (Maybe.map .firstName a.contactOwner |> Maybe.withDefault "Default")
                                    (Maybe.map .firstName b.contactOwner |> Maybe.withDefault "Default")

                        CurrentCarrierCol ->
                            \a b -> compare a.currentCarrier b.currentCarrier

                        EffectiveDateCol ->
                            \a b -> compare a.effectiveDate b.effectiveDate
            in
            List.sortWith
                (if direction == Ascending then
                    comparator

                 else
                    \a b -> comparator b a
                )
                contacts


filterContacts : Filters -> String -> Time.Posix -> List Contact -> List Contact
filterContacts filters searchQuery currentTime contacts =
    contacts
        |> filterBySearch searchQuery
        |> filterByList .currentCarrier filters.carriers
        |> filterByList .state filters.states
        |> filterByAge filters.ageRange


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
                String.contains loweredQuery (String.toLower contact.firstName)
                    || String.contains loweredQuery (String.toLower contact.lastName)
                    || String.contains loweredQuery (String.toLower contact.currentCarrier)
            )
            contacts


filterByAge : Maybe ( Int, Int ) -> List Contact -> List Contact
filterByAge maybeRange contacts =
    case maybeRange of
        Nothing ->
            contacts

        Just ( min, max ) ->
            List.filter
                (\contact ->
                    let
                        age =
                            calculateAge contact.birthDate
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
            ( "", "Select a carrier" ) :: List.map (\c -> ( c, c )) (model.carriers ++ [ "Other" ])

        planTypeOptions =
            [ ( "", "Select a plan type" ), ( "Plan N", "Plan N" ), ( "Plan G", "Plan G" ), ( "Other", "Other" ) ]

        agentOptions =
            ( "", "Default" ) :: List.map (\agent -> ( String.fromInt agent.id, agent.firstName ++ " " ++ agent.lastName )) model.agents

        emailField =
            div [ class "form-group relative" ]
                [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                    [ text "Email" ]
                , div [ class "relative" ]
                    [ Html.input
                        [ type_ "email"
                        , class
                            ("w-full px-4 py-3 bg-white border-[2.5px] rounded-lg text-gray-700 placeholder-gray-400 shadow-sm transition-all duration-200 "
                                ++ (if model.emailExists then
                                        "border-red-300 hover:border-red-400 focus:border-red-500 focus:ring-2 focus:ring-red-200"

                                    else
                                        "border-purple-300 hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200"
                                   )
                            )
                        , value form.email
                        , onInput (updateMsg Email)
                        , required True
                        ]
                        []
                    , if model.isCheckingEmail then
                        div [ class "absolute right-3 top-3" ]
                            [ viewSpinner ]

                      else if model.emailExists then
                        div
                            [ class "absolute right-3 top-3 text-red-500" ]
                            [ viewIcon "M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z" ]

                      else if String.length form.email > 0 then
                        div
                            [ class "absolute right-3 top-3 text-green-500" ]
                            [ viewIcon "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" ]

                      else
                        text ""
                    ]
                , if model.emailExists then
                    div [ class "mt-2 text-sm text-red-600" ]
                        [ text "A contact with this email already exists" ]

                  else
                    text ""
                ]
    in
    Html.form [ onSubmit submitMsg ]
        [ div [ class "grid grid-cols-2 gap-x-8 gap-y-6" ]
            [ viewFormInput "First Name" "text" form.firstName FirstName updateMsg True
            , viewFormInput "Last Name" "text" form.lastName LastName updateMsg True
            , emailField
            , viewFormInput "Phone Number" "text" form.phoneNumber PhoneNumber updateMsg True
            , viewFormSelect "Current Carrier" form.currentCarrier CurrentCarrier updateMsg carrierOptions
            , viewFormSelect "Plan Type" form.planType PlanType updateMsg planTypeOptions
            , viewFormSelect "Contact Owner" (Maybe.map String.fromInt form.contactOwnerId |> Maybe.withDefault "") ContactOwnerId updateMsg agentOptions
            , viewFormInput "Effective Date" "date" form.effectiveDate EffectiveDate updateMsg True
            , viewFormInput "Birth Date" "date" form.birthDate BirthDate updateMsg True
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
            , div [ class "col-span-2 grid grid-cols-2 gap-x-8" ]
                [ viewZipCodeField model form
                , viewStateField form
                ]
            ]
        , if model.error /= Nothing && not model.emailExists then
            div [ class "mt-4 text-red-600 text-sm" ] [ text (Maybe.withDefault "" model.error) ]

          else
            text ""
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
                let
                    isValid =
                        isContactFormValid form && not model.emailExists && not model.isCheckingEmail
                in
                button
                    [ type_ "submit"
                    , class
                        ("px-6 py-3 text-white text-sm font-medium rounded-lg transition-colors duration-200 focus:ring-4 focus:ring-purple-200 "
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
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , Html.input
            [ type_ inputType
            , class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , value displayValue
            , onInput inputHandler
            , required isRequired
            , placeholder placeholderText
            ]
            []
        ]


viewFormSelect : String -> String -> ContactFormField -> (ContactFormField -> String -> Msg) -> List ( String, String ) -> Html Msg
viewFormSelect labelText selectedValue field updateMsg options =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , div [ class "relative" ]
            [ Html.select
                [ class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200 appearance-none"
                , value selectedValue
                , onInput (updateMsg field)
                ]
                (List.map (\( val, txt ) -> option [ value val ] [ text txt ]) options)
            , div [ class "absolute inset-y-0 right-0 flex items-center px-2 pointer-events-none" ]
                [ viewIcon "M19 9l-7 7-7-7" ]
            ]
        ]


viewFormRadioGroup : String -> String -> ContactFormField -> (ContactFormField -> String -> Msg) -> List ( String, String ) -> Html Msg
viewFormRadioGroup labelText selectedValue field updateMsg options =
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
    div []
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text "ZIP Code" ]
        , Html.input
            [ type_ "text"
            , class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
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
    div []
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text "State" ]
        , Html.input
            [ type_ "text"
            , class "w-full px-4 py-3 bg-white border-[2.5px] border-gray-200 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
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
        |> Pipeline.required "organization_id" Decode.int
        |> Pipeline.required "is_active" Decode.bool
        |> Pipeline.required "phone" Decode.string


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
            user.isAdmin || (user.isAgent && user.isAdmin)

        Nothing ->
            False


viewExpandedContent : Contact -> Html Msg
viewExpandedContent contact =
    div
        [ class "grid grid-cols-5 gap-4 py-2 px-4 transition-all duration-200 ease-in-out" ]
        [ viewExpandedField "Birth Date" contact.birthDate
        , viewExpandedField "Tobacco User"
            (if contact.tobaccoUser then
                "Yes"

             else
                "No"
            )
        , viewExpandedField "Gender" contact.gender
        , viewExpandedField "ZIP Code" contact.zipCode
        , viewExpandedField "Plan Type" contact.planType
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


viewFilterDropdown : Model -> FilterType -> Html Msg
viewFilterDropdown model filterType =
    let
        options =
            case filterType of
                CarrierFilter ->
                    model.availableFilters.carriers

                StateFilter ->
                    model.availableFilters.states

                _ ->
                    []

        activeFilters =
            case filterType of
                CarrierFilter ->
                    model.activeFilters.carriers

                StateFilter ->
                    model.activeFilters.states

                _ ->
                    []

        hasActiveFilters =
            not (List.isEmpty activeFilters)
    in
    div
        [ class "absolute left-0 mt-2 w-48 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5 z-10"
        , stopPropagationOn "mousedown" (Decode.succeed ( CloseFilterDropdown, True ))
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
            , div [ class "max-h-48 overflow-y-auto p-2" ]
                (List.map
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
        && String.length form.phoneNumber
        > 0
        && String.length form.state
        > 0
        && String.length form.currentCarrier
        > 0
        && String.length form.effectiveDate
        > 0
        && String.length form.birthDate
        > 0
        && String.length form.zipCode
        > 0
        && String.length form.planType
        > 0


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
        , expect = Http.expectJson GotAgents (Decode.list userDecoder)
        }


checkEmail : String -> Cmd Msg
checkEmail email =
    Http.get
        { url = "/api/contacts/check-email/" ++ email
        , expect = Http.expectJson EmailChecked emailCheckDecoder
        }


emailCheckDecoder : Decode.Decoder { exists : Bool }
emailCheckDecoder =
    Decode.map (\exists -> { exists = exists })
        (Decode.field "exists" Decode.bool)
