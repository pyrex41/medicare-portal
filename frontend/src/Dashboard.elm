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
import Html exposing (Html, button, col, colgroup, div, h1, h2, h3, input, label, nav, option, select, span, table, tbody, td, text, th, thead, tr)
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
import Url.Builder as Url



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
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
    , currentUser : Maybe User
    , showProfileMenu : Bool
    , error : Maybe String
    , saveOnUpdate : Bool
    , expandedContactId : Maybe Int
    , availableFilters : AvailableFilters
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


init : ( Model, Cmd Msg )
init =
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
            , currentUser = Nothing
            , showProfileMenu = False
            , error = Nothing
            , saveOnUpdate = False
            , expandedContactId = Nothing
            , availableFilters = { carriers = [], states = [] }
            }
    in
    ( initialModel
    , Cmd.batch
        [ fetchContacts initialModel
        , fetchCurrentUser
        , Task.perform GotCurrentTime Time.now
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
    | ShowAddModal
    | ShowEditModal Contact
    | CloseModal
    | UpdateSearchQuery String
    | UpdateAddForm ContactFormField String
    | UpdateEditForm ContactFormField String
    | SubmitAddForm
    | SubmitEditForm
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
    | ToggleExpansion Int


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


type FilterType
    = CarrierFilter
    | StateFilter
    | AgeFilter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
            ( { model | showModal = NoModal }, Cmd.none )

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
                            { form | phoneNumber = value }

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

                cmd =
                    if field == ZipCode && String.length value == 5 then
                        submitEditFormWithFlag updatedForm True

                    else
                        Cmd.none
            in
            ( { model | addForm = updatedForm }, cmd )

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
                            { form | phoneNumber = value }

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
                currentModal =
                    case model.showModal of
                        CsvUploadModal state ->
                            if response.success && response.errorRows == 0 then
                                NoModal

                            else
                                CsvUploadModal
                                    { state
                                        | error = Just response.message
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
            case model.showModal of
                CsvUploadModal state ->
                    let
                        errorMessage =
                            case httpError |> Debug.log "HTTP Error" of
                                Http.BadStatus 400 ->
                                    "Invalid CSV format. Please check the required columns and data."

                                _ ->
                                    "Failed to upload CSV. Please try again."
                    in
                    ( { model | showModal = CsvUploadModal { state | error = Just errorMessage } }
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

        ToggleExpansion id ->
            ( { model
                | expandedContactId =
                    if model.expandedContactId == Just id then
                        Nothing

                    else
                        Just id
              }
            , Cmd.none
            )



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
                        , onClick ShowAddModal
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
        isExpanded =
            model.expandedContactId == Just contact.id

        rowClass =
            "hover:bg-gray-50 cursor-pointer transition-colors duration-200"
                ++ (if isExpanded then
                        " bg-gray-50"

                    else
                        ""
                   )

        initials =
            String.left 1 contact.firstName ++ String.left 1 contact.lastName

        cellClass =
            "px-3 py-2 text-sm border-t border-gray-200"
    in
    [ tr [ class rowClass ]
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
        ]
    ]
        ++ (if isExpanded then
                [ tr []
                    [ td
                        [ class "px-3 py-2 bg-gray-50 border-t border-gray-200"
                        , attribute "colspan" "9"
                        ]
                        [ viewExpandedContent contact ]
                    ]
                ]

            else
                []
           )


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
        , ( "phone_number", Encode.string form.phoneNumber )
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

        AddModal ->
            viewAddModal model model.isSubmittingForm

        EditModal contact ->
            viewEditModal model model.isSubmittingForm

        CsvUploadModal state ->
            viewCsvUploadModal state model.isUploadingCsv


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
            , case state.stats of
                Just stats ->
                    div [ class "mt-4 space-y-2" ]
                        [ div [ class "text-sm text-gray-600" ]
                            [ text <| "Total rows: " ++ String.fromInt stats.totalRows ]
                        , div [ class "text-sm text-gray-600" ]
                            [ text <| "Valid rows: " ++ String.fromInt stats.validRows ]
                        , if stats.errorRows > 0 then
                            div [ class "text-sm text-red-600" ]
                                [ text <| "Error rows: " ++ String.fromInt stats.errorRows
                                , case state.errorCsv of
                                    Just csvContent ->
                                        button
                                            [ class "ml-2 text-purple-600 hover:text-purple-800 hover:underline"
                                            , onClick (DownloadErrorCsv csvContent)
                                            ]
                                            [ text "Download and Fix Errors" ]

                                    Nothing ->
                                        text ""
                                ]

                          else
                            text ""
                        ]

                Nothing ->
                    text ""
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
    Html.form [ onSubmit submitMsg ]
        [ div [ class "grid grid-cols-2 gap-x-8 gap-y-6" ]
            [ viewFormInput "First Name" "text" form.firstName FirstName updateMsg True
            , viewFormInput "Last Name" "text" form.lastName LastName updateMsg True
            , viewFormInput "Email" "email" form.email Email updateMsg True
            , viewFormInput "Phone Number" "text" form.phoneNumber PhoneNumber updateMsg True
            , viewFormInput "State" "text" form.state State updateMsg True
            , viewFormInput "Current Carrier" "text" form.currentCarrier CurrentCarrier updateMsg True
            , viewFormInput "Effective Date" "date" form.effectiveDate EffectiveDate updateMsg True
            , viewFormInput "Birth Date" "date" form.birthDate BirthDate updateMsg True
            , viewFormInput "Tobacco User"
                "text"
                (if form.tobaccoUser then
                    "true"

                 else
                    "false"
                )
                TobaccoUser
                updateMsg
                True
            , viewFormSelect "Gender"
                form.gender
                Gender
                updateMsg
                [ ( "M", "Male" )
                , ( "F", "Female" )
                ]
            , div [ class "col-span-2 grid grid-cols-2 gap-x-8" ]
                [ viewZipCodeField model form
                , viewStateField form
                ]
            ]
        , if model.error /= Nothing then
            div [ class "mt-4 text-red-600 text-sm" ]
                [ text (Maybe.withDefault "" model.error) ]

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
                div [ class "px-6 py-3 flex items-center space-x-2" ]
                    [ viewSpinner ]

              else
                button
                    [ type_ "submit"
                    , class "px-6 py-3 bg-purple-500 text-white text-sm font-medium rounded-lg hover:bg-purple-600 transition-colors duration-200 focus:ring-4 focus:ring-purple-200"
                    ]
                    [ text buttonText ]
            ]
        ]


viewFormInput : String -> String -> String -> ContactFormField -> (ContactFormField -> String -> Msg) -> Bool -> Html Msg
viewFormInput labelText inputType inputValue field updateMsg isRequired =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , Html.input
            [ type_ inputType
            , class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
            , value inputValue
            , onInput (updateMsg field)
            , required isRequired
            ]
            []
        ]


viewFormSelect : String -> String -> ContactFormField -> (ContactFormField -> String -> Msg) -> List ( String, String ) -> Html Msg
viewFormSelect labelText selectedValue field updateMsg options =
    div [ class "form-group" ]
        [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , Html.select
            [ class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200 appearance-none"
            , value selectedValue
            , onInput (updateMsg field)
            ]
            (List.map
                (\( val, txt ) ->
                    option [ value val ] [ text txt ]
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
                    case model.showModal of
                        AddModal ->
                            UpdateAddForm ZipCode zip

                        EditModal _ ->
                            UpdateEditForm ZipCode zip

                        _ ->
                            NoOp
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

            len =
                String.length digits
        in
        if len >= 10 then
            let
                area =
                    String.slice 0 3 digits

                prefix =
                    String.slice 3 6 digits

                line =
                    String.slice 6 10 digits
            in
            "(" ++ area ++ ") " ++ prefix ++ "-" ++ line

        else
            phone


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
        , stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
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
