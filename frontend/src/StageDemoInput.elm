module StageDemoInput exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, type_, id, placeholder, value, disabled, href, target, for, src, alt)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import Json.Decode as Decode
import Svg exposing (svg, path)
import Svg.Attributes exposing (d, fill, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)

type alias Model =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , submitting : Bool
    , error : Maybe String
    , success : Bool
    }

type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateEmail String
    | UpdatePhone String
    | Submit
    | SubmitResponse (Result Http.Error SubmitResult)

type alias SubmitResult =
    { success : Bool
    , message : String
    }

init : Model
init =
    { firstName = ""
    , lastName = ""
    , email = ""
    , phone = ""
    , submitting = False
    , error = Nothing
    , success = False
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFirstName value ->
            ( { model | firstName = value }, Cmd.none )

        UpdateLastName value ->
            ( { model | lastName = value }, Cmd.none )

        UpdateEmail value ->
            ( { model | email = value }, Cmd.none )

        UpdatePhone value ->
            ( { model | phone = value }, Cmd.none )

        Submit ->
            if String.isEmpty model.firstName || String.isEmpty model.lastName || String.isEmpty model.email || String.isEmpty model.phone then
                ( { model | error = Just "All fields are required" }, Cmd.none )
            else
                ( { model | submitting = True, error = Nothing }
                , submitForm model
                )

        SubmitResponse result ->
            case result of
                Ok submitResult ->
                    ( { model | submitting = False, success = True, error = Nothing }, Cmd.none )

                Err _ ->
                    ( { model | submitting = False, error = Just "Failed to submit. Please try again." }, Cmd.none )

submitForm : Model -> Cmd Msg
submitForm model =
    Http.post
        { url = "/api/stage-demo/submit"
        , body = Http.jsonBody (encodeFormData model)
        , expect = Http.expectJson SubmitResponse decodeSubmitResult
        }

encodeFormData : Model -> Encode.Value
encodeFormData model =
    Encode.object
        [ ( "firstName", Encode.string model.firstName )
        , ( "lastName", Encode.string model.lastName )
        , ( "email", Encode.string model.email )
        , ( "phone", Encode.string model.phone )
        ]

decodeSubmitResult : Decode.Decoder SubmitResult
decodeSubmitResult =
    Decode.map2 SubmitResult
        (Decode.field "success" Decode.bool)
        (Decode.field "message" Decode.string)

view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-white py-12 px-4 sm:px-6 lg:px-8" ]
        [ div [ class "max-w-md mx-auto" ]
            [ -- Logo
              div [ class "text-center mb-8" ]
                [ img 
                    [ src "/images/medicare-max-logo.png"
                    , alt "Medicare Max"
                    , class "h-12 mx-auto"
                    ]
                    []
                ]
            , div [ class "bg-white shadow-lg rounded-lg p-8 border border-gray-100" ]
                [ if model.success then
                    successView
                  else
                    formView model
                ]
            ]
        ]

successView : Html Msg
successView =
    div [ class "text-center" ]
        [ div [ class "mx-auto flex items-center justify-center h-12 w-12 rounded-full bg-green-100 mb-4" ]
            [ svg [ class "h-6 w-6 text-green-600", fill "none", stroke "currentColor", viewBox "0 0 24 24" ]
                [ path [ strokeLinecap "round", strokeLinejoin "round", strokeWidth "2", d "M5 13l4 4L19 7" ] []
                ]
            ]
        , h3 [ class "text-lg font-medium text-gray-900 mb-2" ] [ text "Success!" ]
        , p [ class "text-gray-600" ] [ text "Check your email and texts for your personalized Medicare quote!" ]
        ]

formView : Model -> Html Msg
formView model =
    div []
        [ h2 [ class "text-2xl font-bold text-[#141B29] mb-6 text-center" ] [ text "Get Your Personalized Medicare Quote" ]
        , p [ class "text-[#475467] text-center mb-8" ] [ text "See how much you could save on your Medicare Supplement plan" ]
        , Html.form [ class "space-y-4" ]
            [ div []
                [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "firstName" ] [ text "First Name" ]
                , input
                    [ type_ "text"
                    , id "firstName"
                    , class "block w-full border-gray-300 rounded-md shadow-sm focus:ring-[#03045E] focus:border-[#03045E]"
                    , placeholder "John"
                    , value model.firstName
                    , onInput UpdateFirstName
                    , disabled model.submitting
                    ]
                    []
                ]
            , div []
                [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "lastName" ] [ text "Last Name" ]
                , input
                    [ type_ "text"
                    , id "lastName"
                    , class "block w-full border-gray-300 rounded-md shadow-sm focus:ring-[#03045E] focus:border-[#03045E]"
                    , placeholder "Doe"
                    , value model.lastName
                    , onInput UpdateLastName
                    , disabled model.submitting
                    ]
                    []
                ]
            , div []
                [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "email" ] [ text "Email" ]
                , input
                    [ type_ "email"
                    , id "email"
                    , class "block w-full border-gray-300 rounded-md shadow-sm focus:ring-[#03045E] focus:border-[#03045E]"
                    , placeholder "john@example.com"
                    , value model.email
                    , onInput UpdateEmail
                    , disabled model.submitting
                    ]
                    []
                ]
            , div []
                [ label [ class "block text-sm font-medium text-gray-700 mb-1", for "phone" ] [ text "Phone Number" ]
                , input
                    [ type_ "tel"
                    , id "phone"
                    , class "block w-full border-gray-300 rounded-md shadow-sm focus:ring-[#03045E] focus:border-[#03045E]"
                    , placeholder "(555) 123-4567"
                    , value model.phone
                    , onInput UpdatePhone
                    , disabled model.submitting
                    ]
                    []
                ]
            , case model.error of
                Just err ->
                    div [ class "text-red-600 text-sm" ] [ text err ]

                Nothing ->
                    text ""
            , button
                [ type_ "button"
                , class "w-full flex justify-center py-3 px-4 border border-transparent rounded-md shadow-sm text-base font-semibold text-white bg-[#03045E] hover:bg-[#1a1f5f] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#03045E] disabled:opacity-50 transition-colors duration-200"
                , onClick Submit
                , disabled model.submitting
                ]
                [ if model.submitting then
                    text "Submitting..."
                  else
                    text "Get My Personalized Quote"
                ]
            ]
        ]