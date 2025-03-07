module Onboarding.Steps.EnterpriseForm exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, viewBox)



-- MODEL


type alias Model =
    { companyName : String
    , contactName : String
    , email : String
    , phone : String
    , displayPhone : String
    , message : String
    , isSubmitting : Bool
    , isSubmitted : Bool
    , error : Maybe String
    , key : Nav.Key
    }


type Msg
    = UpdateCompanyName String
    | UpdateContactName String
    | UpdateEmail String
    | UpdatePhone String
    | UpdateMessage String
    | SubmitForm
    | SubmitSuccess
    | SubmitError String
    | BackToPlans


type OutMsg
    = NoOutMsg
    | BackToPlanSelection
    | ShowError String


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { companyName = ""
      , contactName = ""
      , email = ""
      , phone = ""
      , displayPhone = ""
      , message = ""
      , isSubmitting = False
      , isSubmitted = False
      , error = Nothing
      , key = key
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        UpdateCompanyName value ->
            ( { model | companyName = value }
            , Cmd.none
            , NoOutMsg
            )

        UpdateContactName value ->
            ( { model | contactName = value }
            , Cmd.none
            , NoOutMsg
            )

        UpdateEmail value ->
            ( { model | email = value }
            , Cmd.none
            , NoOutMsg
            )

        UpdatePhone value ->
            let
                digitsOnly =
                    String.filter Char.isDigit value

                updatedPhone =
                    String.left 10 digitsOnly
            in
            ( { model
                | phone = updatedPhone
                , displayPhone = formatPhoneNumber updatedPhone
              }
            , Cmd.none
            , NoOutMsg
            )

        UpdateMessage value ->
            ( { model | message = value }
            , Cmd.none
            , NoOutMsg
            )

        SubmitForm ->
            if isFormValid model then
                ( { model | isSubmitting = True, error = Nothing }
                , submitEnterpriseForm model
                , NoOutMsg
                )

            else
                ( { model | error = Just "Please fill out all required fields" }
                , Cmd.none
                , NoOutMsg
                )

        SubmitSuccess ->
            ( { model | isSubmitted = True, isSubmitting = False }
            , Cmd.none
            , NoOutMsg
            )

        SubmitError errorMsg ->
            ( { model | error = Just errorMsg, isSubmitting = False }
            , Cmd.none
            , NoOutMsg
            )

        BackToPlans ->
            ( model
            , Cmd.none
            , BackToPlanSelection
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Enterprise Plan Inquiry" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Please provide your information and we'll contact you about our Enterprise plan options." ]
            ]
        , if model.isSubmitted then
            viewSuccess

          else
            viewForm model
        ]


viewSuccess : Html Msg
viewSuccess =
    div [ class "bg-green-50 border border-green-200 rounded-lg p-8 text-center" ]
        [ div [ class "mx-auto flex items-center justify-center h-12 w-12 rounded-full bg-green-100 mb-4" ]
            [ -- Checkmark icon
              svg
                [ Svg.Attributes.class "h-6 w-6 text-green-600"
                , Svg.Attributes.viewBox "0 0 20 20"
                , Svg.Attributes.fill "currentColor"
                ]
                [ path
                    [ fillRule "evenodd"
                    , d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
                    , clipRule "evenodd"
                    ]
                    []
                ]
            ]
        , h3 [ class "text-lg font-medium text-gray-900 mb-2" ]
            [ text "Thank you for your interest!" ]
        , p [ class "text-gray-600" ]
            [ text "We've received your inquiry and will contact you soon to discuss our Enterprise plan options." ]
        , div [ class "mt-6" ]
            [ button
                [ class "px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 transition-colors"
                , onClick BackToPlans
                ]
                [ text "Back to Plans" ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    let
        isValid =
            isFormValid model
    in
    Html.form [ onSubmit SubmitForm, class "bg-white rounded-lg shadow-sm p-6 space-y-6" ]
        [ -- Form error message
          case model.error of
            Just errorMsg ->
                div [ class "bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4" ]
                    [ text errorMsg ]

            Nothing ->
                text ""
        , -- Company Name
          div []
            [ label [ for "company-name", class "block text-sm font-medium text-gray-700 mb-1" ]
                [ text "Company Name *" ]
            , input
                [ type_ "text"
                , id "company-name"
                , class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                , value model.companyName
                , onInput UpdateCompanyName
                , placeholder "Enter your company name"
                , required True
                ]
                []
            ]
        , -- Contact Name
          div []
            [ label [ for "contact-name", class "block text-sm font-medium text-gray-700 mb-1" ]
                [ text "Contact Name *" ]
            , input
                [ type_ "text"
                , id "contact-name"
                , class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                , value model.contactName
                , onInput UpdateContactName
                , placeholder "Enter your full name"
                , required True
                ]
                []
            ]
        , -- Email
          div []
            [ label [ for "email", class "block text-sm font-medium text-gray-700 mb-1" ]
                [ text "Email *" ]
            , input
                [ type_ "email"
                , id "email"
                , class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                , value model.email
                , onInput UpdateEmail
                , placeholder "Enter your email address"
                , required True
                ]
                []
            , if not (String.isEmpty model.email) && not (isValidEmail model.email) then
                div [ class "text-red-500 text-sm mt-1" ]
                    [ text "Please enter a valid email address" ]

              else
                text ""
            ]
        , -- Phone
          div []
            [ label [ for "phone", class "block text-sm font-medium text-gray-700 mb-1" ]
                [ text "Phone *" ]
            , input
                [ type_ "tel"
                , id "phone"
                , class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                , value model.displayPhone
                , onInput UpdatePhone
                , placeholder "Enter your phone number"
                , required True
                ]
                []
            , if not (String.isEmpty model.phone) && not (isValidPhone model.phone) then
                div [ class "text-red-500 text-sm mt-1" ]
                    [ text "Please enter a valid 10-digit phone number" ]

              else
                text ""
            ]
        , -- Message
          div []
            [ label [ for "message", class "block text-sm font-medium text-gray-700 mb-1" ]
                [ text "Message" ]
            , textarea
                [ id "message"
                , class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                , value model.message
                , onInput UpdateMessage
                , placeholder "Tell us about your needs and requirements"
                , rows 4
                ]
                []
            ]
        , -- Submit button
          div [ class "flex items-center justify-between pt-4" ]
            [ button
                [ type_ "button"
                , class "px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md shadow-sm hover:bg-gray-50"
                , onClick BackToPlans
                ]
                [ text "Back to Plans" ]
            , button
                [ type_ "submit"
                , class
                    ("px-4 py-2 text-sm font-medium text-white rounded-md shadow-sm focus:outline-none focus:ring-2 focus:ring-offset-2 "
                        ++ (if isValid then
                                "bg-blue-600 border border-transparent hover:bg-blue-700 focus:ring-blue-500"

                            else
                                "bg-gray-400 border border-transparent cursor-not-allowed"
                           )
                    )
                , disabled (not isValid || model.isSubmitting)
                ]
                [ if model.isSubmitting then
                    text "Submitting..."

                  else
                    text "Submit Inquiry"
                ]
            ]
        ]



-- HELPERS


isFormValid : Model -> Bool
isFormValid model =
    not (String.isEmpty (String.trim model.companyName))
        && not (String.isEmpty (String.trim model.contactName))
        && not (String.isEmpty (String.trim model.email))
        && isValidEmail model.email
        && not (String.isEmpty model.phone)
        && isValidPhone model.phone


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


isValidPhone : String -> Bool
isValidPhone phone =
    let
        -- Remove all non-digit characters
        digits =
            String.filter Char.isDigit phone
    in
    String.length digits == 10


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
        "("
            ++ String.left 3 digits
            ++ ") "
            ++ String.slice 3 6 digits
            ++ "-"
            ++ String.dropLeft 6 digits



-- API


submitEnterpriseForm : Model -> Cmd Msg
submitEnterpriseForm model =
    let
        body =
            Encode.object
                [ ( "companyName", Encode.string model.companyName )
                , ( "contactName", Encode.string model.contactName )
                , ( "email", Encode.string model.email )
                , ( "phone", Encode.string model.phone )
                , ( "message", Encode.string model.message )
                ]
    in
    Http.post
        { url = "/api/enterprise-inquiry"
        , body = Http.jsonBody body
        , expect = Http.expectJson handleSubmitResponse (Decode.field "success" Decode.bool)
        }


handleSubmitResponse : Result Http.Error Bool -> Msg
handleSubmitResponse result =
    case result of
        Ok True ->
            SubmitSuccess

        Ok False ->
            SubmitError "The server couldn't process your request. Please try again."

        Err httpError ->
            case httpError of
                Http.BadUrl _ ->
                    SubmitError "Invalid URL. Please contact support."

                Http.Timeout ->
                    SubmitError "Request timed out. Please check your connection and try again."

                Http.NetworkError ->
                    SubmitError "Network error. Please check your connection and try again."

                Http.BadStatus statusCode ->
                    SubmitError ("Server error: " ++ String.fromInt statusCode)

                Http.BadBody _ ->
                    SubmitError "Invalid response from server. Please try again."



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
