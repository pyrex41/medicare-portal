module StageDemoInput exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (path, svg)
import Svg.Attributes as SA


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
    div [ Attr.class "min-h-screen bg-white py-12 px-4 sm:px-6 lg:px-8" ]
        [ div [ Attr.class "max-w-md mx-auto" ]
            [ -- Logo
              div [ Attr.class "text-center mb-8" ]
                [ img
                    [ Attr.src "/images/medicare-max-logo.png"
                    , Attr.alt "Medicare Max"
                    , Attr.class "h-12 mx-auto"
                    ]
                    []
                ]
            , div [ Attr.class "bg-white shadow-lg rounded-lg p-8 border border-gray-100" ]
                [ if model.success then
                    successView

                  else
                    formView model
                ]
            ]
        ]


successView : Html Msg
successView =
    div [ Attr.class "text-center" ]
        [ div [ Attr.class "mx-auto flex items-center justify-center h-12 w-12 rounded-full bg-green-100 mb-4" ]
            [ svg [ SA.class "h-6 w-6 text-green-600", SA.fill "none", SA.stroke "currentColor", SA.viewBox "0 0 24 24" ]
                [ path [ SA.strokeLinecap "round", SA.strokeLinejoin "round", SA.strokeWidth "2", SA.d "M5 13l4 4L19 7" ] []
                ]
            ]
        , h3 [ Attr.class "text-lg font-medium text-gray-900 mb-2" ] [ text "Success!" ]
        , p [ Attr.class "text-gray-600" ] [ text "Check your email and texts for your personalized Medicare quote!" ]
        ]


formView : Model -> Html Msg
formView model =
    div []
        [ h2 [ Attr.class "text-2xl font-bold text-[#141B29] mb-6 text-center" ] [ text "Get Your Personalized Medicare Quote" ]
        , p [ Attr.class "text-[#475467] text-center mb-8" ] [ text "See how much you could save on your Medicare Supplement plan" ]
        , Html.form [ Attr.class "space-y-4" ]
            [ div []
                [ label [ Attr.class "block text-sm font-medium text-gray-700 mb-1", Attr.for "firstName" ] [ text "First Name" ]
                , input
                    [ Attr.type_ "text"
                    , Attr.id "firstName"
                    , Attr.class "block w-full border-gray-300 rounded-md shadow-sm focus:ring-[#03045E] focus:border-[#03045E]"
                    , Attr.placeholder "John"
                    , Attr.value model.firstName
                    , onInput UpdateFirstName
                    , Attr.disabled model.submitting
                    ]
                    []
                ]
            , div []
                [ label [ Attr.class "block text-sm font-medium text-gray-700 mb-1", Attr.for "lastName" ] [ text "Last Name" ]
                , input
                    [ Attr.type_ "text"
                    , Attr.id "lastName"
                    , Attr.class "block w-full border-gray-300 rounded-md shadow-sm focus:ring-[#03045E] focus:border-[#03045E]"
                    , Attr.placeholder "Doe"
                    , Attr.value model.lastName
                    , onInput UpdateLastName
                    , Attr.disabled model.submitting
                    ]
                    []
                ]
            , div []
                [ label [ Attr.class "block text-sm font-medium text-gray-700 mb-1", Attr.for "email" ] [ text "Email" ]
                , input
                    [ Attr.type_ "email"
                    , Attr.id "email"
                    , Attr.class "block w-full border-gray-300 rounded-md shadow-sm focus:ring-[#03045E] focus:border-[#03045E]"
                    , Attr.placeholder "john@example.com"
                    , Attr.value model.email
                    , onInput UpdateEmail
                    , Attr.disabled model.submitting
                    ]
                    []
                ]
            , div []
                [ label [ Attr.class "block text-sm font-medium text-gray-700 mb-1", Attr.for "phone" ] [ text "Phone Number" ]
                , input
                    [ Attr.type_ "tel"
                    , Attr.id "phone"
                    , Attr.class "block w-full border-gray-300 rounded-md shadow-sm focus:ring-[#03045E] focus:border-[#03045E]"
                    , Attr.placeholder "(555) 123-4567"
                    , Attr.value model.phone
                    , onInput UpdatePhone
                    , Attr.disabled model.submitting
                    ]
                    []
                ]
            , case model.error of
                Just err ->
                    div [ Attr.class "text-red-600 text-sm" ] [ text err ]

                Nothing ->
                    text ""
            , button
                [ Attr.type_ "button"
                , Attr.class "w-full flex justify-center py-3 px-4 border border-transparent rounded-md shadow-sm text-base font-semibold text-white bg-[#03045E] hover:bg-[#1a1f5f] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#03045E] disabled:opacity-50 transition-colors duration-200"
                , onClick Submit
                , Attr.disabled model.submitting
                ]
                [ if model.submitting then
                    text "Submitting..."

                  else
                    text "Get My Personalized Quote"
                ]
            ]
        ]
