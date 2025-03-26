module Waitlist exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (Html, a, button, div, form, h1, img, input, label, p, span, text)
import Html.Attributes exposing (class, disabled, for, href, id, min, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Encode


type alias Model =
    { name : String
    , email : String
    , phone : String
    , displayPhone : String
    , numAgents : String
    , bookSize : String
    , formState : FormState
    , errorMessage : Maybe String
    }


type FormState
    = Editing
    | Submitting
    | Success
    | Error


type Msg
    = UpdateName String
    | UpdateEmail String
    | UpdatePhone String
    | UpdateNumAgents String
    | UpdateBookSize String
    | SubmitForm
    | GotSubmitResponse (Result Http.Error ())


init : ( Model, Cmd Msg )
init =
    ( { name = ""
      , email = ""
      , phone = ""
      , displayPhone = ""
      , numAgents = ""
      , bookSize = ""
      , formState = Editing
      , errorMessage = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName value ->
            ( { model | name = value }, Cmd.none )

        UpdateEmail value ->
            ( { model | email = value }, Cmd.none )

        UpdatePhone value ->
            let
                digitsOnly =
                    String.filter Char.isDigit value
                        |> String.left 10
            in
            ( { model
                | phone = digitsOnly
                , displayPhone = formatPhoneNumber digitsOnly
              }
            , Cmd.none
            )

        UpdateNumAgents value ->
            if String.all Char.isDigit value || value == "" then
                ( { model | numAgents = value }, Cmd.none )

            else
                ( model, Cmd.none )

        UpdateBookSize value ->
            if String.all Char.isDigit value || value == "" then
                ( { model | bookSize = value }, Cmd.none )

            else
                ( model, Cmd.none )

        SubmitForm ->
            if isValidForm model then
                ( { model | formState = Submitting }
                , submitForm model
                )

            else
                ( { model | errorMessage = Just "Please fill out all fields correctly." }
                , Cmd.none
                )

        GotSubmitResponse result ->
            case result of
                Ok _ ->
                    ( { model | formState = Success }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | formState = Error
                        , errorMessage = Just "Something went wrong. Please try again."
                      }
                    , Cmd.none
                    )


isValidForm : Model -> Bool
isValidForm model =
    not (String.isEmpty model.name)
        && isValidEmail model.email
        && not (String.isEmpty model.phone)
        && isValidPositiveInt model.numAgents
        && isValidPositiveInt model.bookSize


isValidEmail : String -> Bool
isValidEmail email =
    let
        trimmedEmail =
            String.trim email
    in
    not (String.isEmpty trimmedEmail)
        && String.contains "@" trimmedEmail
        && String.contains "." trimmedEmail
        && not (String.startsWith "@" trimmedEmail)
        && not (String.endsWith "@" trimmedEmail)
        && not (String.endsWith "." trimmedEmail)
        && (String.indexes "@" trimmedEmail |> List.length)
        == 1
        && String.length (String.dropLeft (String.length (String.left (String.indexes "." trimmedEmail |> List.head |> Maybe.withDefault 0) trimmedEmail)) trimmedEmail)
        > 1


isValidPositiveInt : String -> Bool
isValidPositiveInt str =
    case String.toInt str of
        Just n ->
            n > 0

        Nothing ->
            False


submitForm : Model -> Cmd Msg
submitForm model =
    Http.post
        { url = "/api/waitlist"
        , body =
            Http.jsonBody
                (encodeFormData model)
        , expect = Http.expectWhatever GotSubmitResponse
        }


encodeFormData : Model -> Json.Encode.Value
encodeFormData model =
    Json.Encode.object
        [ ( "name", Json.Encode.string model.name )
        , ( "email", Json.Encode.string model.email )
        , ( "phone", Json.Encode.string model.phone )
        , ( "numAgents", Json.Encode.string model.numAgents )
        , ( "bookSize", Json.Encode.string model.bookSize )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Join the Waitlist - Medicare Max"
    , body =
        [ div [ class "min-h-screen bg-white py-16 px-4 sm:px-6 lg:px-8" ]
            [ div [ class "max-w-md mx-auto" ]
                [ div [ class "text-center mb-8" ]
                    [ a [ href "/", class "block" ]
                        [ img
                            [ Html.Attributes.src "/images/medicare-max-logo.png"
                            , Html.Attributes.alt "Medicare Max Logo"
                            , class "mx-auto mb-6 h-12"
                            ]
                            []
                        ]
                    , h1 [ class "text-3xl font-bold text-gray-900" ]
                        [ text "Join the Waitlist" ]
                    , p [ class "mt-2 text-sm text-gray-600" ]
                        [ text "Be the first to know when we launch." ]
                    ]
                , case model.formState of
                    Success ->
                        div [ class "bg-green-50 p-4 rounded-md" ]
                            [ p [ class "text-green-800 text-center" ]
                                [ text "Thanks for joining! We'll be in touch soon." ]
                            ]

                    _ ->
                        formView model
                ]
            ]
        ]
    }


formView : Model -> Html Msg
formView model =
    form [ onSubmit SubmitForm, class "space-y-8" ]
        [ div [ class "space-y-2" ]
            [ label [ for "name", class "block text-sm font-medium text-gray-700" ]
                [ text "Name" ]
            , div [ class "mt-1" ]
                [ input
                    [ type_ "text"
                    , id "name"
                    , value model.name
                    , onInput UpdateName
                    , class "shadow-sm focus:ring-[#03045E] focus:border-[#03045E] block w-full sm:text-sm border-gray-300 rounded-md px-3.5 py-2.5"
                    , placeholder "John Smith"
                    ]
                    []
                ]
            ]
        , div [ class "space-y-2" ]
            [ label [ for "email", class "block text-sm font-medium text-gray-700" ]
                [ text "Email" ]
            , div [ class "mt-1" ]
                [ input
                    [ type_ "email"
                    , id "email"
                    , value model.email
                    , onInput UpdateEmail
                    , class "shadow-sm focus:ring-[#03045E] focus:border-[#03045E] block w-full sm:text-sm border-gray-300 rounded-md px-3.5 py-2.5"
                    , placeholder "you@example.com"
                    ]
                    []
                ]
            ]
        , div [ class "space-y-2" ]
            [ label [ for "phone", class "block text-sm font-medium text-gray-700" ]
                [ text "Phone Number" ]
            , div [ class "mt-1" ]
                [ input
                    [ type_ "tel"
                    , id "phone"
                    , value model.displayPhone
                    , onInput UpdatePhone
                    , class "shadow-sm focus:ring-[#03045E] focus:border-[#03045E] block w-full sm:text-sm border-gray-300 rounded-md px-3.5 py-2.5"
                    , placeholder "(555) 555-5555"
                    ]
                    []
                ]
            ]
        , div [ class "space-y-2" ]
            [ label [ for "numAgents", class "block text-sm font-medium text-gray-700" ]
                [ text "Number of Agents" ]
            , div [ class "mt-1" ]
                [ input
                    [ type_ "number"
                    , id "numAgents"
                    , value model.numAgents
                    , onInput UpdateNumAgents
                    , class "shadow-sm focus:ring-[#03045E] focus:border-[#03045E] block w-full sm:text-sm border-gray-300 rounded-md px-3.5 py-2.5"
                    , Html.Attributes.min "1"
                    , placeholder "1"
                    ]
                    []
                ]
            ]
        , div [ class "space-y-2" ]
            [ label [ for "bookSize", class "block text-sm font-medium text-gray-700" ]
                [ text "Current Book Size" ]
            , div [ class "mt-1" ]
                [ input
                    [ type_ "number"
                    , id "bookSize"
                    , value model.bookSize
                    , onInput UpdateBookSize
                    , class "shadow-sm focus:ring-[#03045E] focus:border-[#03045E] block w-full sm:text-sm border-gray-300 rounded-md px-3.5 py-2.5"
                    , Html.Attributes.min "1"
                    , placeholder "100"
                    ]
                    []
                ]
            ]
        , if model.errorMessage /= Nothing then
            div [ class "text-red-600 text-sm" ]
                [ text (Maybe.withDefault "" model.errorMessage) ]

          else
            text ""
        , div []
            [ button
                [ type_ "submit"
                , class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-[#03045E] hover:bg-[#1a1f5f] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#03045E]"
                , disabled (model.formState == Submitting)
                ]
                [ text
                    (if model.formState == Submitting then
                        "Submitting..."

                     else
                        "Join Waitlist"
                    )
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


formatPhoneNumber : String -> String
formatPhoneNumber phone =
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
