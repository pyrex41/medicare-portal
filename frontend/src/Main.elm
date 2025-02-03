module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, h1, h2, h3, input, label, nav, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, placeholder, required, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
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
    , currentCarrier : String
    , planType : String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : Bool
    , gender : String
    , state : String
    , zipCode : String
    , lastEmailed : Maybe String
    }


type Modal
    = NoModal
    | AddModal
    | EditModal Contact


type alias Model =
    { contacts : List Contact
    , showModal : Modal
    , searchQuery : String
    , addForm : ContactForm
    , editForm : ContactForm
    , sortColumn : Maybe SortColumn
    , sortDirection : SortDirection
    , activeFilters : Filters
    }


type alias ContactForm =
    { id : Maybe Int
    , firstName : String
    , lastName : String
    , currentCarrier : String
    , planType : String
    , effectiveDate : String
    , birthDate : String
    , tobaccoUser : Bool
    , gender : String
    , state : String
    , zipCode : String
    }


type SortColumn
    = FirstNameCol
    | LastNameCol
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
    { birthDateRange : Maybe ( String, String ) -- (start, end)
    , effectiveDateRange : Maybe ( String, String )
    , carriers : List String
    , states : List String
    , ageRange : Maybe ( Int, Int ) -- (min, max)
    }


type alias ZipInfo =
    { state : String
    , counties : List String
    , cities : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { contacts = []
      , showModal = NoModal
      , searchQuery = ""
      , addForm = emptyForm
      , editForm = emptyForm
      , sortColumn = Nothing
      , sortDirection = Ascending
      , activeFilters = emptyFilters
      }
    , fetchContacts
    )


emptyForm : ContactForm
emptyForm =
    { id = Nothing
    , firstName = ""
    , lastName = ""
    , currentCarrier = ""
    , planType = ""
    , effectiveDate = ""
    , birthDate = ""
    , tobaccoUser = False
    , gender = "M"
    , state = ""
    , zipCode = ""
    }


emptyFilters : Filters
emptyFilters =
    { birthDateRange = Nothing
    , effectiveDateRange = Nothing
    , carriers = []
    , states = []
    , ageRange = Nothing
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
    | SetDateFilter FilterType String String -- (filterType, start, end)
    | SetAgeFilter Int Int -- (min, max)
    | ClearFilters
    | LookupZipCode String
    | GotZipLookup (Result Http.Error ZipInfo)
    | Batch (List Msg)


type ContactFormField
    = FirstName
    | LastName
    | CurrentCarrier
    | PlanType
    | EffectiveDate
    | BirthDate
    | TobaccoUser
    | Gender
    | State
    | ZipCode


type FilterType
    = BirthDateFilter
    | EffectiveDateFilter
    | CarrierFilter
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
                    , currentCarrier = contact.currentCarrier
                    , planType = contact.planType
                    , effectiveDate = contact.effectiveDate
                    , birthDate = contact.birthDate
                    , tobaccoUser = contact.tobaccoUser
                    , gender = contact.gender
                    , state = contact.state
                    , zipCode = contact.zipCode
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
            ( model, submitAddForm model.addForm )

        SubmitEditForm ->
            ( model, submitEditForm model.editForm )

        GotContacts (Ok contacts) ->
            ( { model | contacts = contacts }, Cmd.none )

        GotContacts (Err error) ->
            let
                _ =
                    Debug.log "Error fetching contacts" error
            in
            ( model, Cmd.none )

        -- TODO: Handle error
        ContactAdded (Ok contact) ->
            ( { model
                | contacts = contact :: model.contacts
                , showModal = NoModal
                , addForm = emptyForm
              }
            , Cmd.none
            )

        ContactAdded (Err _) ->
            ( model, Cmd.none )

        -- TODO: Handle error
        ContactUpdated (Ok contact) ->
            ( { model
                | contacts = updateContact contact model.contacts
                , showModal = NoModal
                , editForm = emptyForm
              }
            , Cmd.none
            )

        ContactUpdated (Err _) ->
            ( model, Cmd.none )

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

        SetDateFilter filterType start end ->
            ( { model | activeFilters = setDateFilter model.activeFilters filterType ( start, end ) }, Cmd.none )

        SetAgeFilter min max ->
            ( { model | activeFilters = setAgeFilter model.activeFilters min max }, Cmd.none )

        ClearFilters ->
            ( { model | activeFilters = emptyFilters }, Cmd.none )

        LookupZipCode zipCode ->
            ( model
            , Http.get
                { url = "http://localhost:8000/api/zip-lookup/" ++ zipCode
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
        { url = "http://localhost:8000/api/contacts"
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
                    [ class "px-3 py-1.5 bg-white text-gray-700 text-sm font-medium rounded-md border border-gray-200 hover:border-gray-300 hover:bg-gray-50 transition-colors duration-200" ]
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
        , viewFilterPanel model
        ]


viewFilterPanel : Model -> Html Msg
viewFilterPanel model =
    div [ class "bg-white p-4 rounded-lg border border-gray-200 space-y-4" ]
        [ div [ class "flex justify-between items-center" ]
            [ h3 [ class "text-sm font-medium text-gray-700" ]
                [ text "Filters" ]
            , button
                [ class "text-sm text-purple-600 hover:text-purple-700"
                , onClick ClearFilters
                ]
                [ text "Clear all" ]
            ]
        , div [ class "grid grid-cols-4 gap-4" ]
            [ viewDateRangeFilter "Birth Date" BirthDateFilter model.activeFilters.birthDateRange
            , viewDateRangeFilter "Effective Date" EffectiveDateFilter model.activeFilters.effectiveDateRange
            , viewCheckboxFilter "Carriers" CarrierFilter (getUniqueValues .currentCarrier model.contacts) model.activeFilters.carriers
            , viewCheckboxFilter "States" StateFilter (getUniqueValues .state model.contacts) model.activeFilters.states
            ]
        ]


viewDateRangeFilter : String -> FilterType -> Maybe ( String, String ) -> Html Msg
viewDateRangeFilter label_ filterType maybeRange =
    div [ class "space-y-2" ]
        [ label [ class "block text-sm font-medium text-gray-700" ]
            [ text label_ ]
        , div [ class "flex space-x-2" ]
            [ input
                [ type_ "date"
                , class "flex-1 px-2 py-1 border border-gray-200 rounded-md text-sm"
                , value (Maybe.map Tuple.first maybeRange |> Maybe.withDefault "")
                , onInput (\val -> SetDateFilter filterType val (Maybe.map Tuple.second maybeRange |> Maybe.withDefault ""))
                ]
                []
            , input
                [ type_ "date"
                , class "flex-1 px-2 py-1 border border-gray-200 rounded-md text-sm"
                , value (Maybe.map Tuple.second maybeRange |> Maybe.withDefault "")
                , onInput (\val -> SetDateFilter filterType (Maybe.map Tuple.first maybeRange |> Maybe.withDefault "") val)
                ]
                []
            ]
        ]


viewCheckboxFilter : String -> FilterType -> List String -> List String -> Html Msg
viewCheckboxFilter label_ filterType options selectedValues =
    div [ class "space-y-2" ]
        [ label [ class "block text-sm font-medium text-gray-700" ]
            [ text label_ ]
        , div [ class "space-y-1 max-h-32 overflow-y-auto" ]
            (List.map
                (\option ->
                    label [ class "flex items-center space-x-2" ]
                        [ input
                            [ type_ "checkbox"
                            , checked (List.member option selectedValues)
                            , onClick (ToggleFilter filterType option)
                            , class "text-purple-600 rounded focus:ring-purple-500"
                            ]
                            []
                        , span [ class "text-sm text-gray-600" ]
                            [ text option ]
                        ]
                )
                options
            )
        ]


viewContactsTable : Model -> Html Msg
viewContactsTable model =
    let
        filteredAndSortedContacts =
            model.contacts
                |> filterContacts model.activeFilters model.searchQuery
                |> sortContacts model.sortColumn model.sortDirection
    in
    div [ class "bg-white shadow-sm rounded-lg border border-gray-200" ]
        [ div [ class "overflow-x-auto" ]
            [ table [ class "min-w-full table-fixed" ]
                [ viewTableHeader model
                , tbody [ class "divide-y divide-gray-200" ]
                    (List.map viewContactRow filteredAndSortedContacts)
                ]
            ]
        ]


viewTableHeader : Model -> Html Msg
viewTableHeader model =
    thead []
        [ tr [ class "bg-gray-50 border-b border-gray-200" ]
            (List.map
                (\( col, label ) ->
                    viewSortableHeaderCell model col label
                )
                [ ( FirstNameCol, "First Name" )
                , ( LastNameCol, "Last Name" )
                , ( CarrierCol, "Current Carrier" )
                , ( PlanTypeCol, "Plan Type" )
                , ( EffectiveDateCol, "Effective Date" )
                , ( BirthDateCol, "Birth Date" )
                , ( TobaccoCol, "Tobacco" )
                , ( GenderCol, "Gender" )
                , ( StateCol, "State" )
                , ( ZipCodeCol, "ZIP Code" )
                ]
                ++ [ th [ class "px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider border-r border-gray-200" ]
                        [ text "Actions" ]
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


viewContactRow : Contact -> Html Msg
viewContactRow contact =
    let
        mainClass =
            [ class "px-4 py-3 text-sm text-gray-900 border-r border-gray-200" ]
    in
    tr [ class "hover:bg-gray-50" ]
        [ td mainClass [ text contact.firstName ]
        , td mainClass [ text contact.lastName ]
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
        , td [ class "px-4 py-3 text-sm text-gray-900" ] [ viewContactActions contact ]
        ]


viewContactActions : Contact -> Html Msg
viewContactActions contact =
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
        { url = "http://localhost:8000/api/contacts"
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
                , url = "http://localhost:8000/api/contacts/" ++ String.fromInt id
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
        |> Pipeline.required "current_carrier" Decode.string
        |> Pipeline.required "plan_type" Decode.string
        |> Pipeline.required "effective_date" Decode.string
        |> Pipeline.required "birth_date" Decode.string
        |> Pipeline.required "tobacco_user" Decode.bool
        |> Pipeline.required "gender" Decode.string
        |> Pipeline.required "state" Decode.string
        |> Pipeline.required "zip_code" Decode.string
        |> Pipeline.optional "last_emailed" (Decode.nullable Decode.string) Nothing


contactsDecoder : Decode.Decoder (List Contact)
contactsDecoder =
    Decode.list contactDecoder


encodeContactForm : ContactForm -> Encode.Value
encodeContactForm form =
    Encode.object
        [ ( "first_name", Encode.string form.firstName )
        , ( "last_name", Encode.string form.lastName )
        , ( "current_carrier", Encode.string form.currentCarrier )
        , ( "plan_type", Encode.string form.planType )
        , ( "effective_date", Encode.string form.effectiveDate )
        , ( "birth_date", Encode.string form.birthDate )
        , ( "tobacco_user", Encode.bool form.tobaccoUser )
        , ( "gender", Encode.string form.gender )
        , ( "state", Encode.string form.state )
        , ( "zip_code", Encode.string form.zipCode )
        ]


viewModals : Model -> Html Msg
viewModals model =
    case model.showModal of
        NoModal ->
            text ""

        AddModal ->
            viewAddModal model.addForm

        EditModal contact ->
            viewEditModal model.editForm


viewAddModal : ContactForm -> Html Msg
viewAddModal form =
    div [ class "fixed inset-0 bg-gray-500 bg-opacity-75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-5xl w-full mx-4 shadow-xl relative" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Add New Client" ]
            , viewContactForm form UpdateAddForm SubmitAddForm "Add Client"
            ]
        ]


viewEditModal : ContactForm -> Html Msg
viewEditModal form =
    div [ class "fixed inset-0 bg-gray-500 bg-opacity-75 flex items-center justify-center p-8" ]
        [ div [ class "bg-white rounded-xl p-10 max-w-5xl w-full mx-4 shadow-xl relative" ]
            [ button
                [ class "absolute top-4 right-4 text-gray-400 hover:text-gray-600 transition-colors duration-200"
                , onClick CloseModal
                ]
                [ viewIcon "M6 18L18 6M6 6l12 12" ]
            , h2 [ class "text-2xl font-semibold text-gray-900 mb-8" ]
                [ text "Edit Client" ]
            , viewContactForm form UpdateEditForm SubmitEditForm "Save Changes"
            ]
        ]


viewContactForm : ContactForm -> (ContactFormField -> String -> Msg) -> Msg -> String -> Html Msg
viewContactForm form updateMsg submitMsg buttonText =
    Html.form [ onSubmit submitMsg ]
        [ div [ class "grid grid-cols-2 gap-x-8 gap-y-6" ]
            [ viewFormInput "First Name" "text" form.firstName FirstName updateMsg True
            , viewFormInput "Last Name" "text" form.lastName LastName updateMsg True
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
            , button
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



-- Add this new subscription function


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.showModal of
        NoModal ->
            Sub.none

        _ ->
            Browser.Events.onKeyDown (Decode.map HandleKeyDown (Decode.field "key" Decode.string))


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


filterContacts : Filters -> String -> List Contact -> List Contact
filterContacts filters searchQuery contacts =
    contacts
        |> filterBySearch searchQuery
        |> filterByDateRange .birthDate filters.birthDateRange
        |> filterByDateRange .effectiveDate filters.effectiveDateRange
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


filterByDateRange : (Contact -> String) -> Maybe ( String, String ) -> List Contact -> List Contact
filterByDateRange getter maybeRange contacts =
    case maybeRange of
        Nothing ->
            contacts

        Just ( start, end ) ->
            List.filter
                (\contact ->
                    let
                        date =
                            getter contact
                    in
                    date >= start && date <= end
                )
                contacts


filterByList : (Contact -> String) -> List String -> List Contact -> List Contact
filterByList getter values contacts =
    if List.isEmpty values then
        contacts

    else
        List.filter
            (\contact ->
                List.member (getter contact) values
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
        BirthDateFilter ->
            { filters | birthDateRange = toggleRange filters.birthDateRange value }

        EffectiveDateFilter ->
            { filters | effectiveDateRange = toggleRange filters.effectiveDateRange value }

        CarrierFilter ->
            { filters | carriers = toggleList filters.carriers value }

        StateFilter ->
            { filters | states = toggleList filters.states value }

        AgeFilter ->
            { filters | ageRange = toggleAgeRange filters.ageRange value }


toggleRange : Maybe ( String, String ) -> String -> Maybe ( String, String )
toggleRange maybeRange value =
    case maybeRange of
        Nothing ->
            Just ( value, value )

        Just ( start, end ) ->
            if start == value then
                Just ( value, end )

            else if end == value then
                Just ( start, value )

            else
                Just ( start, end )


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


setDateFilter : Filters -> FilterType -> ( String, String ) -> Filters
setDateFilter filters filterType ( start, end ) =
    case filterType of
        BirthDateFilter ->
            { filters | birthDateRange = Just ( start, end ) }

        EffectiveDateFilter ->
            { filters | effectiveDateRange = Just ( start, end ) }

        _ ->
            filters


setAgeFilter : Filters -> Int -> Int -> Filters
setAgeFilter filters min max =
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
