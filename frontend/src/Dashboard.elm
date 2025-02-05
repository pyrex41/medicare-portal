module Dashboard exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Browser.Events
import File exposing (File)
import File.Download
import File.Select as Select
import Html exposing (Html, button, div, h1, h2, h3, input, label, nav, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, placeholder, required, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit, preventDefaultOn, stopPropagationOn)
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, nullable, string)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra
import Svg exposing (svg)
import Svg.Attributes exposing (d, fill, stroke, viewBox)
import Task
import Time
import Url.Builder as Url



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
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
    , currentCarrier : String
    , planType : String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : Bool
    , gender : String
    , state : String
    , zipCode : String
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
    }


type alias ContactForm =
    { id : Maybe Int
    , firstName : String
    , lastName : String
    , email : String
    , currentCarrier : String
    , planType : String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : Bool
    , gender : String
    , state : String
    , zipCode : String
    , agentId : Maybe Int
    }


type SortColumn
    = FirstNameCol
    | LastNameCol
    | EmailCol
    | CarrierCol
    | PlanTypeCol
    | EffectiveDateCol
    | BirthDateCol
    | TobaccoCol
    | GenderCol
    | StateCol
    | ZipCodeCol


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { contacts = []
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
      }
    , Cmd.batch
        [ fetchContacts
        , Task.perform GotCurrentTime Time.now
        ]
    )


emptyForm : ContactForm
emptyForm =
    { id = Nothing
    , firstName = ""
    , lastName = ""
    , email = ""
    , currentCarrier = ""
    , planType = ""
    , effectiveDate = ""
    , birthDate = ""
    , tobaccoUser = False
    , gender = "M"
    , state = ""
    , zipCode = ""
    , agentId = Nothing
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
    | GotContacts (Result Http.Error (List Contact))
    | ContactAdded (Result Http.Error Contact)
    | ContactUpdated (Result Http.Error Contact)
    | HandleKeyDown String
    | SetSort SortColumn
    | ToggleFilter FilterType String
    | SetAgeFilter Int Int -- (min, max)
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


type ContactFormField
    = FirstName
    | LastName
    | Email
    | CurrentCarrier
    | PlanType
    | EffectiveDate
    | BirthDate
    | TobaccoUser
    | Gender
    | State
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
                    , currentCarrier = contact.currentCarrier
                    , planType = contact.planType
                    , effectiveDate = contact.effectiveDate
                    , birthDate = contact.birthDate
                    , tobaccoUser = contact.tobaccoUser
                    , gender = contact.gender
                    , state = contact.state
                    , zipCode = contact.zipCode
                    , agentId = contact.agentId
                    }
              }
            , Cmd.none
            )

        CloseModal ->
            ( { model | showModal = NoModal }, Cmd.none )

        UpdateSearchQuery query ->
            ( { model | searchQuery = query }, Cmd.none )

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

                        CurrentCarrier ->
                            { form | currentCarrier = value }

                        PlanType ->
                            { form | planType = value }

                        EffectiveDate ->
                            { form | effectiveDate = value }

                        BirthDate ->
                            { form | birthDate = value }

                        TobaccoUser ->
                            { form | tobaccoUser = value == "true" }

                        Gender ->
                            { form | gender = value }

                        State ->
                            { form | state = value }

                        ZipCode ->
                            { form | zipCode = value }
            in
            ( { model | addForm = updatedForm }, Cmd.none )

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

                        CurrentCarrier ->
                            { form | currentCarrier = value }

                        PlanType ->
                            { form | planType = value }

                        EffectiveDate ->
                            { form | effectiveDate = value }

                        BirthDate ->
                            { form | birthDate = value }

                        TobaccoUser ->
                            { form | tobaccoUser = value == "true" }

                        Gender ->
                            { form | gender = value }

                        State ->
                            { form | state = value }

                        ZipCode ->
                            { form | zipCode = value }
            in
            ( { model | editForm = updatedForm }, Cmd.none )

        SubmitAddForm ->
            ( { model | isSubmittingForm = True }
            , submitAddForm model.addForm
            )

        SubmitEditForm ->
            ( { model | isSubmittingForm = True }
            , submitEditForm model.editForm
            )

        GotContacts (Ok contacts) ->
            ( { model | contacts = contacts, isLoadingContacts = False }, Cmd.none )

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
            ( { model
                | contacts = updateContact contact model.contacts
                , showModal = NoModal
                , editForm = emptyForm
                , isSubmittingForm = False
              }
            , Cmd.none
            )

        ContactUpdated (Err _) ->
            ( { model | isSubmittingForm = False }
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
            ( { model | activeFilters = toggleFilter model.activeFilters filterType value }, Cmd.none )

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
                            getUniqueValues .currentCarrier model.contacts

                        StateFilter ->
                            getUniqueValues .state model.contacts

                        _ ->
                            []

                updatedFilters =
                    case filterType of
                        CarrierFilter ->
                            { activeFilters
                                | carriers =
                                    if select then
                                        options

                                    else
                                        []
                            }

                        StateFilter ->
                            { activeFilters
                                | states =
                                    if select then
                                        options

                                    else
                                        []
                            }

                        _ ->
                            model.activeFilters

                activeFilters =
                    model.activeFilters
            in
            ( { model | activeFilters = updatedFilters }, Cmd.none )

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
            in
            ( { model
                | showModal = currentModal
                , isUploadingCsv = False
              }
            , if response.success then
                fetchContacts

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
                ( { model
                    | contacts = List.filter (\c -> not (List.member c.id response.deletedIds)) model.contacts
                    , selectedContacts = []
                    , isDeletingContacts = False
                  }
                , fetchContacts
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



-- TODO: Handle error
-- Add other update cases here...
-- VIEW


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-50" ]
        [ viewNavHeader
        , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
            [ viewActionBar model
            , viewContactsTable model
            , viewModals model
            ]
        ]


viewNavHeader : Html Msg
viewNavHeader =
    nav [ class "bg-white border-b border-gray-200" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex justify-between h-16" ]
                [ div [ class "flex" ]
                    [ div [ class "flex-shrink-0 flex items-center" ]
                        [ h1 [ class "text-xl font-semibold text-purple-600" ]
                            [ text "Medicare Max" ]
                        ]
                    ]
                , div [ class "flex items-center" ]
                    [ button
                        [ class "px-3 py-1.5 bg-purple-500 text-white text-sm font-medium rounded-md hover:bg-purple-600 transition-colors duration-200" ]
                        [ text "Profile" ]
                    ]
                ]
            ]
        ]



-- Add other view helper functions here...
-- HTTP


fetchContacts : Cmd Msg
fetchContacts =
    Http.get
        { url = "/api/contacts"
        , expect = Http.expectJson GotContacts contactsDecoder
        }



-- Add other HTTP functions and JSON decoders/encoders here...
-- VIEW HELPERS


viewActionBar : Model -> Html Msg
viewActionBar model =
    div [ class "space-y-4 mb-6" ]
        [ div [ class "flex justify-between items-center" ]
            [ div [ class "flex space-x-3" ]
                [ button
                    [ class "px-3 py-1.5 bg-purple-500 text-white text-sm font-medium rounded-md hover:bg-purple-600 transition-colors duration-200"
                    , onClick ShowAddModal
                    ]
                    [ viewIcon "M12 4v16m8-8H4"
                    , text "Add Client"
                    ]
                , button
                    [ class "px-3 py-1.5 bg-white text-gray-700 text-sm font-medium rounded-md border border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200"
                    , onClick ShowCsvUploadModal
                    ]
                    [ viewIcon "M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4-4m0 0L8 8m4-4v12"
                    , text "Upload CSV"
                    ]
                , button
                    [ class "px-3 py-1.5 bg-white text-gray-700 text-sm font-medium rounded-md border border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200" ]
                    [ viewIcon "M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"
                    , text "Export"
                    ]
                ]
            , div [ class "flex space-x-3" ]
                [ input
                    [ class "px-3 py-1.5 w-64 bg-white border border-gray-200 rounded-md text-sm placeholder-gray-400 focus:border-purple-400 focus:ring-2 focus:ring-purple-400/20 transition-colors duration-200"
                    , placeholder "Search contacts..."
                    , onInput UpdateSearchQuery
                    , value model.searchQuery
                    ]
                    []
                ]
            ]
        , viewFilters model
        ]


viewFilters : Model -> Html Msg
viewFilters model =
    div [ class "space-y-4" ]
        [ h3 [ class "text-lg font-medium text-gray-700" ] [ text "Filters" ]
        , div [ class "flex space-x-4" ]
            [ viewFilterDropdown model
                "Carriers"
                CarrierFilter
                (getUniqueValues .currentCarrier model.contacts)
                model.activeFilters.carriers
            , viewFilterDropdown model
                "States"
                StateFilter
                (getUniqueValues .state model.contacts)
                model.activeFilters.states
            , viewAgeFilter model.activeFilters.ageRange
            ]
        ]


viewFilterDropdown : Model -> String -> FilterType -> List String -> List String -> Html Msg
viewFilterDropdown model label_ filterType options selectedValues =
    let
        isOpen =
            model.openFilter == Just filterType

        allSelected =
            List.length selectedValues == List.length options

        chevronIcon =
            if isOpen then
                "M19 9l-7 7-7-7"

            else
                "M9 5l7 7-7 7"

        selectionDisplay =
            if List.isEmpty selectedValues then
                [ span [ class "text-sm text-gray-600" ] [ text "Select..." ] ]

            else if List.length selectedValues == 1 then
                [ span [ class "text-sm text-gray-600" ] [ text (List.head selectedValues |> Maybe.withDefault "") ] ]

            else
                [ span [ class "text-sm text-gray-600" ]
                    [ text (String.fromInt (List.length selectedValues) ++ " selected") ]
                , if filterType == CarrierFilter && not (List.isEmpty selectedValues) then
                    button
                        [ class "ml-2 px-2 py-1 text-xs text-purple-600 hover:text-purple-800 hover:underline"
                        , onClick EmailSelectedCarriers
                        ]
                        [ text "Email" ]

                  else
                    text ""
                ]
    in
    div [ class "relative w-48" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text label_ ]
        , div
            [ class "flex items-center justify-between w-full px-4 py-2 bg-white border border-gray-200 rounded-md cursor-pointer hover:bg-gray-50"
            , onClick (ToggleFilterDropdown filterType)
            ]
            [ div [ class "flex items-center space-x-2" ] selectionDisplay
            , svg
                [ Svg.Attributes.class "w-5 h-5 text-gray-400"
                , viewBox "0 0 24 24"
                , fill "none"
                , stroke "currentColor"
                ]
                [ Svg.path
                    [ d chevronIcon
                    , Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.strokeWidth "2"
                    ]
                    []
                ]
            ]
        , if isOpen then
            div
                [ class "absolute z-10 w-full mt-1 bg-white border border-gray-200 rounded-md shadow-lg"
                , stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
                ]
                [ div [ class "p-2 border-b border-gray-200" ]
                    [ label [ class "flex items-center space-x-2" ]
                        [ input
                            [ type_ "checkbox"
                            , checked allSelected
                            , onClick (SelectAllFilter filterType (not allSelected))
                            , class "rounded border-gray-300 text-purple-600 focus:ring-purple-500"
                            ]
                            []
                        , span [ class "text-sm text-gray-600" ]
                            [ text "Select All" ]
                        ]
                    ]
                , div [ class "max-h-48 overflow-y-auto p-2" ]
                    (List.map
                        (\option ->
                            label
                                [ class "flex items-center space-x-2 py-1" ]
                                [ input
                                    [ type_ "checkbox"
                                    , checked (List.member option selectedValues)
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

          else
            text ""
        ]


viewAgeFilter : Maybe ( Int, Int ) -> Html Msg
viewAgeFilter maybeRange =
    div [ class "w-48" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text "Age Range (years)" ]
        , div [ class "flex items-center space-x-2" ]
            [ div [ class "flex items-center space-x-1" ]
                [ span [ class "text-sm text-gray-600" ] [ text "Min:" ]
                , input
                    [ type_ "number"
                    , class "w-16 px-4 py-2 border border-gray-200 rounded-md text-sm"
                    , value (Maybe.map (Tuple.first >> String.fromInt) maybeRange |> Maybe.withDefault "")
                    , onInput
                        (\val ->
                            if String.isEmpty val then
                                SetAgeFilter 0 0

                            else
                                SetAgeFilter (String.toInt val |> Maybe.withDefault 0) (Maybe.map Tuple.second maybeRange |> Maybe.withDefault 0)
                        )
                    ]
                    []
                ]
            , div [ class "flex items-center space-x-1" ]
                [ span [ class "text-sm text-gray-600" ] [ text "Max:" ]
                , input
                    [ type_ "number"
                    , class "w-16 px-4 py-2 border border-gray-200 rounded-md text-sm"
                    , value (Maybe.map (Tuple.second >> String.fromInt) maybeRange |> Maybe.withDefault "")
                    , onInput
                        (\val ->
                            if String.isEmpty val then
                                SetAgeFilter (Maybe.map Tuple.first maybeRange |> Maybe.withDefault 0) 0

                            else
                                SetAgeFilter (Maybe.map Tuple.first maybeRange |> Maybe.withDefault 0) (String.toInt val |> Maybe.withDefault 0)
                        )
                    ]
                    []
                ]
            , case maybeRange of
                Just _ ->
                    button
                        [ class "px-2 py-1 text-xs text-gray-600 hover:text-gray-800 border border-gray-200 rounded-md hover:border-gray-300 bg-white hover:bg-gray-50 transition-colors duration-200"
                        , onClick (SetAgeFilter 0 0)
                        ]
                        [ text "Reset" ]

                Nothing ->
                    text ""
            ]
        ]


viewContactsTable : Model -> Html Msg
viewContactsTable model =
    if model.isLoadingContacts then
        div [ class "flex justify-center items-center h-64" ]
            [ viewSpinner
            , span [ class "ml-2 text-gray-600" ] [ text "Loading contacts..." ]
            ]

    else
        let
            filteredAndSortedContacts =
                model.contacts
                    |> filterContacts model.activeFilters model.searchQuery model.currentTime
                    |> sortContacts model.sortColumn model.sortDirection

            selectionCount =
                List.length model.selectedContacts

            selectionText =
                case selectionCount of
                    0 ->
                        "No contacts selected"

                    1 ->
                        "1 contact selected"

                    n ->
                        String.fromInt n ++ " contacts selected"
        in
        div []
            [ div [ class "mb-4 flex items-center space-x-2" ]
                [ span [ class "text-sm text-gray-600" ]
                    [ text selectionText ]
                , if selectionCount > 0 then
                    div [ class "flex space-x-2" ]
                        [ button
                            [ class "px-2 py-1 text-sm text-purple-600 hover:text-purple-800 hover:bg-purple-50 rounded-md transition-colors duration-200 flex items-center space-x-1"
                            , onClick EmailSelectedContacts
                            ]
                            [ viewIcon "M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"
                            , span [] [ text "Email" ]
                            ]
                        , if model.isDeletingContacts then
                            div [ class "px-2 py-1 flex items-center space-x-1" ]
                                [ viewSpinner ]

                          else
                            button
                                [ class "px-2 py-1 text-sm text-red-600 hover:text-red-800 hover:bg-red-50 rounded-md transition-colors duration-200 flex items-center space-x-1"
                                , onClick DeleteSelectedContacts
                                ]
                                [ viewIcon "M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"
                                , span [] [ text "Delete" ]
                                ]
                        ]

                  else
                    text ""
                ]
            , div [ class "bg-white shadow-sm rounded-lg border border-gray-200" ]
                [ div [ class "overflow-x-auto" ]
                    [ table [ class "min-w-full table-fixed" ]
                        [ viewTableHeader model filteredAndSortedContacts
                        , tbody [ class "divide-y divide-gray-200" ]
                            (List.map (viewContactRow model) filteredAndSortedContacts)
                        ]
                    ]
                ]
            ]


viewTableHeader : Model -> List Contact -> Html Msg
viewTableHeader model visibleContacts =
    let
        allSelected =
            not (List.isEmpty model.selectedContacts)
                && List.length model.selectedContacts
                == List.length visibleContacts
    in
    thead []
        [ tr [ class "bg-gray-50 border-b border-gray-200" ]
            (th
                [ class "w-12 px-4 py-3 border-r border-gray-200" ]
                [ div [ class "flex items-center justify-between" ]
                    [ input
                        [ type_ "checkbox"
                        , class "rounded border-gray-300 text-purple-600 focus:ring-purple-500"
                        , checked allSelected
                        , onClick
                            (if allSelected then
                                DeselectAllContacts

                             else
                                SelectAllContacts
                            )
                        ]
                        []
                    ]
                ]
                :: List.map
                    (\( col, label ) ->
                        viewSortableHeaderCell model col label
                    )
                    [ ( FirstNameCol, "First Name" )
                    , ( LastNameCol, "Last Name" )
                    , ( EmailCol, "Email" )
                    , ( CarrierCol, "Current Carrier" )
                    , ( PlanTypeCol, "Plan Type" )
                    , ( EffectiveDateCol, "Effective Date" )
                    , ( BirthDateCol, "Birth Date" )
                    , ( TobaccoCol, "Tobacco" )
                    , ( GenderCol, "Gender" )
                    , ( StateCol, "State" )
                    , ( ZipCodeCol, "ZIP Code" )
                    ]
            )
        ]


viewSortableHeaderCell : Model -> SortColumn -> String -> Html Msg
viewSortableHeaderCell model column label =
    th
        [ class "px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider border-r border-gray-200 cursor-pointer hover:bg-gray-100"
        , onClick (SetSort column)
        ]
        [ div [ class "flex items-center space-x-1" ]
            [ text label
            , viewSortIcon model column
            ]
        ]


viewSortIcon : Model -> SortColumn -> Html Msg
viewSortIcon model column =
    if model.sortColumn == Just column then
        case model.sortDirection of
            Ascending ->
                viewIcon "M5 15l7-7 7 7"

            Descending ->
                viewIcon "M19 9l-7 7-7-7"

    else
        text ""


viewContactRow : Model -> Contact -> Html Msg
viewContactRow model contact =
    let
        mainClass =
            [ class "px-4 py-3 text-sm text-gray-900 border-r border-gray-200" ]
    in
    tr [ class "hover:bg-gray-50" ]
        (td
            [ class "w-12 px-4 py-3 border-r border-gray-200" ]
            [ input
                [ type_ "checkbox"
                , class "rounded border-gray-300 text-purple-600 focus:ring-purple-500"
                , checked (List.member contact.id model.selectedContacts)
                , onClick (ToggleSelectContact contact.id)
                ]
                []
            ]
            :: [ td mainClass [ text contact.firstName ]
               , td mainClass [ text contact.lastName ]
               , td mainClass [ text contact.email ]
               , td mainClass [ text contact.currentCarrier ]
               , td mainClass [ text contact.planType ]
               , td mainClass [ text contact.effectiveDate ]
               , td mainClass [ text contact.birthDate ]
               , td mainClass
                    [ text
                        (if contact.tobaccoUser then
                            "Yes"

                         else
                            "No"
                        )
                    ]
               , td mainClass [ text contact.gender ]
               , td mainClass [ text contact.state ]
               , td mainClass [ text contact.zipCode ]
               , td [ class "px-4 py-3 text-sm text-gray-900" ] [ viewContactActions contact model.isDeletingContacts ]
               ]
        )


viewContactActions : Contact -> Bool -> Html Msg
viewContactActions contact isDeleting =
    div [ class "flex space-x-2 justify-center" ]
        [ button
            [ class "text-gray-400 hover:text-purple-500 transition-colors duration-200"
            , onClick (ShowEditModal contact)
            , title "Edit"
            ]
            [ viewIcon "M15.232 5.232l3.536 3.536m-2.036-5.036a2.5 2.5 0 113.536 3.536L6.5 21.036H3v-3.572L16.732 3.732z"
            ]
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


submitEditForm : ContactForm -> Cmd Msg
submitEditForm form =
    case form.id of
        Just id ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "/api/contacts/" ++ String.fromInt id
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
        |> Pipeline.required "first_name" Decode.string
        |> Pipeline.required "last_name" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "current_carrier" Decode.string
        |> Pipeline.required "plan_type" Decode.string
        |> Pipeline.required "effective_date" Decode.string
        |> Pipeline.required "birth_date" Decode.string
        |> Pipeline.required "tobacco_user" Decode.bool
        |> Pipeline.required "gender" Decode.string
        |> Pipeline.required "state" Decode.string
        |> Pipeline.required "zip_code" Decode.string
        |> Pipeline.optional "agent_id" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "last_emailed_date" (Decode.nullable Decode.string) Nothing


contactsDecoder : Decode.Decoder (List Contact)
contactsDecoder =
    Decode.list contactDecoder


encodeContactForm : ContactForm -> Encode.Value
encodeContactForm form =
    Encode.object
        [ ( "first_name", Encode.string form.firstName )
        , ( "last_name", Encode.string form.lastName )
        , ( "email", Encode.string form.email )
        , ( "current_carrier", Encode.string form.currentCarrier )
        , ( "plan_type", Encode.string form.planType )
        , ( "effective_date", Encode.string form.effectiveDate )
        , ( "birth_date", Encode.string form.birthDate )
        , ( "tobacco_user", Encode.bool form.tobaccoUser )
        , ( "gender", Encode.string form.gender )
        , ( "state", Encode.string form.state )
        , ( "zip_code", Encode.string form.zipCode )
        , ( "agent_id", Maybe.map Encode.int form.agentId |> Maybe.withDefault Encode.null )
        ]


viewModals : Model -> Html Msg
viewModals model =
    case model.showModal of
        NoModal ->
            text ""

        AddModal ->
            viewAddModal model.addForm model.isSubmittingForm

        EditModal contact ->
            viewEditModal model.editForm model.isSubmittingForm

        CsvUploadModal state ->
            viewCsvUploadModal state model.isUploadingCsv


viewAddModal : ContactForm -> Bool -> Html Msg
viewAddModal form isSubmitting =
    div [ class "fixed inset-0 bg-gray-500 bg-opacity-75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-5xl w-full mx-4 shadow-xl relative" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Add New Client" ]
            , viewContactForm form UpdateAddForm SubmitAddForm "Add Client" isSubmitting
            ]
        ]


viewEditModal : ContactForm -> Bool -> Html Msg
viewEditModal form isSubmitting =
    div [ class "fixed inset-0 bg-gray-500 bg-opacity-75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-5xl w-full mx-4 shadow-xl relative" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Edit Client" ]
            , viewContactForm form UpdateEditForm SubmitEditForm "Save Changes" isSubmitting
            ]
        ]


viewCsvUploadModal : UploadState -> Bool -> Html Msg
viewCsvUploadModal state isUploading =
    div [ class "fixed inset-0 bg-gray-500 bg-opacity-75 flex items-center justify-center p-8" ]
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
                    [ text "Overwrite existing contacts with matching email addresses" ]
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
                , case state.error of
                    Just error ->
                        div [ class "mt-4 text-sm text-red-600" ]
                            [ text error ]

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
                                            [ text "Download Errors" ]

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
                    [ class "px-6 py-3 bg-white text-gray-700 text-sm font-medium rounded-lg border-2 border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200"
                    , onClick CloseModal
                    ]
                    [ text "Cancel" ]
                , if isUploading then
                    div [ class "px-6 py-3 flex items-center space-x-2" ]
                        [ viewSpinner ]

                  else
                    button
                        [ class "px-6 py-3 bg-purple-500 text-white text-sm font-medium rounded-lg hover:bg-purple-600 transition-colors duration-200"
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
                        FirstNameCol ->
                            \a b -> compare a.firstName b.firstName

                        LastNameCol ->
                            \a b -> compare a.lastName b.lastName

                        EmailCol ->
                            \a b -> compare a.email b.email

                        CarrierCol ->
                            \a b -> compare a.currentCarrier b.currentCarrier

                        PlanTypeCol ->
                            \a b -> compare a.planType b.planType

                        EffectiveDateCol ->
                            \a b -> compare a.effectiveDate b.effectiveDate

                        BirthDateCol ->
                            \a b -> compare a.birthDate b.birthDate

                        TobaccoCol ->
                            \a b ->
                                compare
                                    (if a.tobaccoUser then
                                        1

                                     else
                                        0
                                    )
                                    (if b.tobaccoUser then
                                        1

                                     else
                                        0
                                    )

                        GenderCol ->
                            \a b -> compare a.gender b.gender

                        StateCol ->
                            \a b -> compare a.state b.state

                        ZipCodeCol ->
                            \a b -> compare a.zipCode b.zipCode
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


viewContactForm : ContactForm -> (ContactFormField -> String -> Msg) -> Msg -> String -> Bool -> Html Msg
viewContactForm form updateMsg submitMsg buttonText isSubmitting =
    Html.form [ onSubmit submitMsg ]
        [ div [ class "grid grid-cols-2 gap-x-8 gap-y-6" ]
            [ viewFormInput "First Name" "text" form.firstName FirstName updateMsg True
            , viewFormInput "Last Name" "text" form.lastName LastName updateMsg True
            , viewFormInput "Email" "email" form.email Email updateMsg True
            , viewFormInput "Current Carrier" "text" form.currentCarrier CurrentCarrier updateMsg True
            , viewFormInput "Plan Type" "text" form.planType PlanType updateMsg True
            , viewFormInput "Effective Date" "date" form.effectiveDate EffectiveDate updateMsg True
            , viewFormInput "Birth Date" "date" form.birthDate BirthDate updateMsg True
            , viewFormSelect "Tobacco User"
                (if form.tobaccoUser then
                    "true"

                 else
                    "false"
                )
                TobaccoUser
                updateMsg
                [ ( "false", "No" )
                , ( "true", "Yes" )
                ]
            , viewFormSelect "Gender"
                form.gender
                Gender
                updateMsg
                [ ( "M", "Male" )
                , ( "F", "Female" )
                ]
            , div [ class "form-group" ]
                [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                    [ text "ZIP Code" ]
                , Html.input
                    [ type_ "text"
                    , class "w-full px-4 py-3 bg-white border-[2.5px] border-purple-300 rounded-lg text-gray-700 placeholder-gray-400 shadow-sm hover:border-purple-400 focus:border-purple-500 focus:ring-2 focus:ring-purple-200 focus:bg-white transition-all duration-200"
                    , value form.zipCode
                    , onInput
                        (\zip ->
                            if String.all Char.isDigit zip && String.length zip <= 5 then
                                Batch
                                    [ updateMsg ZipCode zip
                                    , if String.length zip == 5 then
                                        LookupZipCode zip

                                      else
                                        NoOp
                                    ]

                            else
                                NoOp
                        )
                    , required True
                    , Html.Attributes.maxlength 5
                    , Html.Attributes.pattern "[0-9]*"
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ Html.label [ class "block text-sm font-medium text-gray-700 mb-2" ]
                    [ text "State" ]
                , Html.input
                    [ type_ "text"
                    , class "w-full px-4 py-3 bg-gray-100 border-[2.5px] border-gray-200 rounded-lg text-gray-700 cursor-not-allowed"
                    , value form.state
                    , Html.Attributes.disabled True
                    ]
                    []
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
    Decode.succeed DeleteResponse
        |> Pipeline.required "success" Decode.bool
        |> Pipeline.required "deleted_ids" (Decode.list Decode.int)
        |> Pipeline.required "message" Decode.string


viewSpinner : Html Msg
viewSpinner =
    div [ class "animate-spin rounded-full h-5 w-5 border-2 border-purple-500 border-t-transparent" ] []
