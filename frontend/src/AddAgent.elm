module AddAgent exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser, chompIf, chompWhile, end, succeed, symbol)
import StateRegions exposing (Region(..), getRegionStates, regionToString)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, viewBox)



-- Constants


allStates : List String
allStates =
    [ "AL"
    , "AK"
    , "AZ"
    , "AR"
    , "CA"
    , "CO"
    , "CT"
    , "DE"
    , "FL"
    , "GA"
    , "HI"
    , "ID"
    , "IL"
    , "IN"
    , "IA"
    , "KS"
    , "KY"
    , "LA"
    , "ME"
    , "MD"
    , "MA"
    , "MI"
    , "MN"
    , "MS"
    , "MO"
    , "MT"
    , "NE"
    , "NV"
    , "NH"
    , "NJ"
    , "NM"
    , "NY"
    , "NC"
    , "ND"
    , "OH"
    , "OK"
    , "OR"
    , "PA"
    , "RI"
    , "SC"
    , "SD"
    , "TN"
    , "TX"
    , "UT"
    , "VT"
    , "VA"
    , "WA"
    , "WV"
    , "WI"
    , "WY"
    , "DC"
    ]


allCarriers : List String
allCarriers =
    [ "Aetna"
    , "Humana"
    , "UnitedHealthcare"
    , "Cigna"
    , "Aflac"
    , "Allstate"
    , "Mutual of Omaha"
    , "Ace Chubb"
    ]


type alias Model =
    { email : String
    , firstName : String
    , lastName : String
    , rawPhone : String
    , displayPhone : String
    , isAdmin : Bool
    , carriers : List String
    , stateLicenses : List String
    , error : Maybe String
    , isSetup : Bool
    , key : Nav.Key
    , isInheriting : Bool
    }


type Msg
    = UpdateEmail String
    | UpdateFirstName String
    | UpdateLastName String
    | UpdatePhone String
    | ToggleAdmin
    | SelectCarrier String Bool
    | SelectState String Bool
    | SelectAllStates Bool
    | SaveAgent
    | AgentSaved (Result Http.Error ())
    | FinishSetup
    | SelectCommonStates Region
    | InheritFromOrg
    | GotOrgSettings (Result Http.Error Settings)


init : { isSetup : Bool, key : Nav.Key } -> ( Model, Cmd Msg )
init flags =
    ( { email = ""
      , firstName = ""
      , lastName = ""
      , rawPhone = ""
      , displayPhone = ""
      , isAdmin = False
      , carriers = []
      , stateLicenses = []
      , error = Nothing
      , isSetup = flags.isSetup
      , key = flags.key
      , isInheriting = False
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title =
        if model.isSetup then
            "Add Your First Agent"

        else
            "Add Agent"
    , body =
        [ div [ class "min-h-screen bg-gray-50 py-12" ]
            [ div [ class "max-w-3xl mx-auto" ]
                [ if model.isSetup then
                    viewSetupHeader

                  else
                    viewNormalHeader
                , div [ class "bg-white shadow rounded-lg p-6 space-y-6" ]
                    [ viewBasicInfo model
                    , viewWritingNumbers model
                    , viewStateLicenses model
                    , viewBottomBar model
                    ]
                ]
            ]
        ]
    }


viewSetupHeader : Html Msg
viewSetupHeader =
    div [ class "text-center mb-8" ]
        [ h1 [ class "text-3xl font-bold text-gray-900" ]
            [ text "Add Your First Agent" ]
        , p [ class "mt-2 text-gray-600" ]
            [ text "Set up your first agent to get started" ]
        ]


viewNormalHeader : Html Msg
viewNormalHeader =
    h1 [ class "text-2xl font-semibold text-gray-900 mb-6" ]
        [ text "Add Agent" ]


viewBasicInfo : Model -> Html Msg
viewBasicInfo model =
    div [ class "space-y-4" ]
        [ div []
            [ label [ class "block text-sm font-medium text-gray-700" ]
                [ text "First Name" ]
            , input
                [ type_ "text"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                , value model.firstName
                , onInput UpdateFirstName
                , placeholder "Enter first name"
                ]
                []
            ]
        , div []
            [ label [ class "block text-sm font-medium text-gray-700" ]
                [ text "Last Name" ]
            , input
                [ type_ "text"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                , value model.lastName
                , onInput UpdateLastName
                , placeholder "Enter last name"
                ]
                []
            ]
        , div []
            [ label [ class "block text-sm font-medium text-gray-700" ]
                [ text "Email" ]
            , input
                [ type_ "email"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                , value model.email
                , onInput UpdateEmail
                , placeholder "name@example.com"
                ]
                []
            ]
        , div []
            [ label [ class "block text-sm font-medium text-gray-700" ]
                [ text "Phone" ]
            , input
                [ type_ "tel"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
                , value model.displayPhone
                , onInput UpdatePhone
                , placeholder "(555) 555-5555"
                ]
                []
            ]
        , div [ class "mt-4" ]
            [ label [ class "inline-flex items-center" ]
                [ input
                    [ type_ "checkbox"
                    , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                    , checked model.isAdmin
                    , onClick ToggleAdmin
                    ]
                    []
                , span [ class "ml-2 text-sm text-gray-700" ]
                    [ text "Make this agent an admin" ]
                ]
            ]
        , div [ class "mt-6" ]
            [ button
                [ class "w-full inline-flex justify-center items-center px-4 py-2 border border-gray-300 shadow-sm text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                , onClick InheritFromOrg
                , disabled model.isInheriting
                ]
                [ if model.isInheriting then
                    div [ class "flex items-center" ]
                        [ div [ class "animate-spin -ml-1 mr-3 h-5 w-5 text-gray-700" ]
                            [ svg
                                [ Svg.Attributes.class "h-5 w-5"
                                , viewBox "0 0 24 24"
                                ]
                                [ path
                                    [ d "M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                                    , fill "currentColor"
                                    ]
                                    []
                                ]
                            ]
                        , text "Inheriting..."
                        ]

                  else
                    text "Inherit from Organization Settings"
                ]
            ]
        ]


viewWritingNumbers : Model -> Html Msg
viewWritingNumbers model =
    div [ class "space-y-4" ]
        [ h3 [ class "text-lg font-medium text-gray-900" ]
            [ text "Carriers" ]
        , div [ class "grid grid-cols-3 gap-4" ]
            (List.map
                (\carrier ->
                    label [ class "inline-flex items-center" ]
                        [ input
                            [ type_ "checkbox"
                            , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
                            , checked (List.member carrier model.carriers)
                            , onCheck (\isChecked -> SelectCarrier carrier isChecked)
                            ]
                            []
                        , span [ class "ml-2 text-sm text-gray-700" ]
                            [ text carrier ]
                        ]
                )
                allCarriers
            )
        ]


viewStateLicenses : Model -> Html Msg
viewStateLicenses model =
    div [ class "space-y-4" ]
        [ h3 [ class "text-lg font-medium text-gray-900" ]
            [ text "State Licenses" ]
        , div [ class "mb-4 space-y-2" ]
            [ div [ class "flex gap-4" ]
                [ div []
                    [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                        [ text "Batch Select" ]
                    , div [ class "flex gap-2" ]
                        [ button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                            , onClick (SelectAllStates True)
                            ]
                            [ text "Select All" ]
                        , button
                            [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50 min-w-[70px]"
                            , onClick (SelectAllStates False)
                            ]
                            [ text "Clear All" ]
                        ]
                    ]
                , div []
                    [ div [ class "text-sm font-medium text-gray-700 mb-2" ]
                        [ text "By Region:" ]
                    , div [ class "flex gap-2" ]
                        (List.map
                            (\region ->
                                button
                                    [ class "px-3 py-1 text-sm border rounded-md hover:bg-gray-50"
                                    , onClick (SelectCommonStates region)
                                    ]
                                    [ text (regionToString region) ]
                            )
                            StateRegions.allRegions
                        )
                    ]
                ]
            ]
        , div [ class "grid grid-cols-6 gap-4" ]
            (List.map (viewStateCheckbox model) allStates)
        ]


viewStateCheckbox : Model -> String -> Html Msg
viewStateCheckbox model state =
    label [ class "inline-flex items-center" ]
        [ input
            [ type_ "checkbox"
            , class "rounded border-gray-300 text-blue-600 focus:ring-blue-500"
            , checked (List.member state model.stateLicenses)
            , onCheck (\isChecked -> SelectState state isChecked)
            ]
            []
        , span [ class "ml-2 text-sm text-gray-700" ]
            [ text state ]
        ]


viewBottomBar : Model -> Html Msg
viewBottomBar model =
    div [ class "fixed bottom-0 left-0 right-0 bg-white border-t border-gray-200 px-4 py-4 sm:px-6" ]
        [ div [ class "flex justify-between max-w-3xl mx-auto" ]
            [ if model.error /= Nothing then
                p [ class "text-red-600" ]
                    [ text (Maybe.withDefault "" model.error) ]

              else
                text ""
            , div [ class "flex space-x-4" ]
                [ button
                    [ class "px-4 py-2 text-sm font-medium text-white bg-blue-600 rounded-md hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
                    , onClick SaveAgent
                    , disabled (not (canSave model))
                    ]
                    [ text "Save Agent" ]
                , if model.isSetup then
                    button
                        [ class "px-4 py-2 text-sm font-medium text-white bg-green-600 rounded-md hover:bg-green-700"
                        , onClick FinishSetup
                        ]
                        [ text "Complete Setup" ]

                  else
                    text ""
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        UpdateFirstName name ->
            ( { model | firstName = name }, Cmd.none )

        UpdateLastName name ->
            ( { model | lastName = name }, Cmd.none )

        UpdatePhone input ->
            let
                rawDigits =
                    String.filter Char.isDigit input
                        |> String.left 10

                formattedPhone =
                    formatPhoneNumber rawDigits
            in
            ( { model
                | rawPhone = rawDigits
                , displayPhone = formattedPhone
              }
            , Cmd.none
            )

        ToggleAdmin ->
            ( { model | isAdmin = not model.isAdmin }, Cmd.none )

        SelectCarrier carrier isSelected ->
            ( { model
                | carriers =
                    if isSelected then
                        carrier :: model.carriers

                    else
                        List.filter ((/=) carrier) model.carriers
              }
            , Cmd.none
            )

        SelectState state isSelected ->
            ( { model
                | stateLicenses =
                    if isSelected then
                        state :: model.stateLicenses

                    else
                        List.filter ((/=) state) model.stateLicenses
              }
            , Cmd.none
            )

        SelectAllStates isSelected ->
            ( { model
                | stateLicenses =
                    if isSelected then
                        allStates

                    else
                        []
              }
            , Cmd.none
            )

        SelectCommonStates region ->
            ( { model | stateLicenses = model.stateLicenses ++ getRegionStates region }
            , Cmd.none
            )

        SaveAgent ->
            ( model
            , saveAgent model
            )

        AgentSaved (Ok ()) ->
            if model.isSetup then
                ( model
                , Nav.pushUrl model.key "/dashboard"
                )

            else
                ( { model | error = Nothing }
                , Nav.pushUrl model.key "/agents"
                )

        AgentSaved (Err _) ->
            ( { model | error = Just "Failed to save agent" }
            , Cmd.none
            )

        FinishSetup ->
            ( model
            , Nav.pushUrl model.key "/dashboard"
            )

        InheritFromOrg ->
            ( { model | isInheriting = True }
            , Http.get
                { url = "/api/settings"
                , expect = Http.expectJson GotOrgSettings (Decode.field "orgSettings" settingsObjectDecoder)
                }
            )

        GotOrgSettings (Ok settings) ->
            ( { model
                | carriers = settings.carrierContracts
                , stateLicenses = settings.stateLicenses
                , isInheriting = False
              }
            , Cmd.none
            )

        GotOrgSettings (Err _) ->
            ( { model
                | error = Just "Failed to load organization settings"
                , isInheriting = False
              }
            , Cmd.none
            )



-- Helper functions


formatPhoneNumber : String -> String
formatPhoneNumber rawPhone =
    let
        digits =
            String.filter Char.isDigit rawPhone
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
        "("
            ++ String.left 3 digits
            ++ ") "
            ++ String.slice 3 6 digits
            ++ "-"
            ++ String.dropLeft 6 digits


saveAgent : Model -> Cmd Msg
saveAgent model =
    Http.post
        { url = "/api/agents"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "firstName", Encode.string model.firstName )
                    , ( "lastName", Encode.string model.lastName )
                    , ( "email", Encode.string model.email )
                    , ( "phone", Encode.string model.rawPhone )
                    , ( "isAdmin", Encode.bool model.isAdmin )
                    , ( "carriers", Encode.list Encode.string model.carriers )
                    , ( "stateLicenses", Encode.list Encode.string model.stateLicenses )
                    ]
                )
        , expect = Http.expectWhatever AgentSaved
        }


settingsDecoder : Decoder SettingsResponse
settingsDecoder =
    Decode.map2 SettingsResponse
        (Decode.field "orgSettings" settingsObjectDecoder)
        (Decode.field "canEditOrgSettings" Decode.bool)


type alias SettingsResponse =
    { orgSettings : Settings
    , canEditOrgSettings : Bool
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


isValidEmail : String -> Bool
isValidEmail email =
    case Parser.run emailParser email of
        Ok _ ->
            True

        Err _ ->
            False


emailParser : Parser ()
emailParser =
    let
        usernameChar c =
            Char.isAlphaNum c || List.member c [ '.', '_', '-', '+', '%' ]

        domainChar c =
            Char.isAlphaNum c || c == '-'
    in
    succeed ()
        |. chompIf usernameChar
        |. chompWhile usernameChar
        |. symbol "@"
        |. chompIf domainChar
        |. chompWhile domainChar
        |. symbol "."
        |. chompIf Char.isAlpha
        |. chompWhile Char.isAlpha
        |. end


isValidPhone : String -> Bool
isValidPhone phone =
    String.length (String.filter Char.isDigit phone) == 10


canSave : Model -> Bool
canSave model =
    let
        hasValidName =
            not (String.isEmpty (String.trim model.firstName))
                && not (String.isEmpty (String.trim model.lastName))

        hasValidEmail =
            isValidEmail model.email

        hasValidPhone =
            isValidPhone model.displayPhone
    in
    hasValidName && hasValidEmail && hasValidPhone
