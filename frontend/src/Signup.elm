module Signup exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)
import Url



-- MODEL


type alias Model =
    { organizationName : String
    , adminFirstName : String
    , adminLastName : String
    , adminEmail : String
    , error : Maybe String
    , isSubmitting : Bool
    , submitted : Bool
    , orgNameStatus : OrgNameStatus
    , emailStatus : EmailStatus
    }


type OrgNameStatus
    = NotChecked
    | Checking
    | Valid
    | Invalid String


type EmailStatus
    = EmailNotChecked
    | EmailChecking
    | EmailValid
    | EmailInvalid String


type Msg
    = UpdateOrganizationName String
    | CheckOrganizationName
    | GotOrgNameResponse (Result Http.Error OrgNameResponse)
    | UpdateAdminFirstName String
    | UpdateAdminLastName String
    | UpdateAdminEmail String
    | CheckAdminEmail
    | GotEmailResponse (Result Http.Error EmailResponse)
    | SubmitForm
    | GotSignupResponse (Result Http.Error SignupResponse)


type alias SignupResponse =
    { success : Bool
    , message : String
    }


type alias OrgNameResponse =
    { available : Bool
    , message : String
    }


type alias EmailResponse =
    { available : Bool
    , message : String
    }


init : ( Model, Cmd Msg )
init =
    ( { organizationName = ""
      , adminFirstName = ""
      , adminLastName = ""
      , adminEmail = ""
      , error = Nothing
      , isSubmitting = False
      , submitted = False
      , orgNameStatus = NotChecked
      , emailStatus = EmailNotChecked
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateOrganizationName value ->
            ( { model
                | organizationName = value
                , orgNameStatus = NotChecked -- Clear status when typing
              }
            , Cmd.none
            )

        CheckOrganizationName ->
            if String.length model.organizationName >= 1 then
                ( { model | orgNameStatus = Checking }
                  -- Show loading state
                , checkOrgName model.organizationName
                )

            else
                ( { model | orgNameStatus = NotChecked }
                , Cmd.none
                )

        GotOrgNameResponse (Ok response) ->
            ( { model
                | orgNameStatus =
                    if response.available then
                        Valid

                    else
                        Invalid response.message
              }
            , Cmd.none
            )

        GotOrgNameResponse (Err _) ->
            ( { model
                | orgNameStatus = Invalid "Failed to check organization name"
              }
            , Cmd.none
            )

        UpdateAdminFirstName value ->
            ( { model | adminFirstName = value }, Cmd.none )

        UpdateAdminLastName value ->
            ( { model | adminLastName = value }, Cmd.none )

        UpdateAdminEmail value ->
            ( { model
                | adminEmail = value
                , emailStatus = EmailNotChecked
              }
            , Cmd.none
            )

        CheckAdminEmail ->
            if String.length model.adminEmail >= 1 then
                ( { model | emailStatus = EmailChecking }
                , checkEmail model.adminEmail
                )

            else
                ( { model | emailStatus = EmailNotChecked }
                , Cmd.none
                )

        GotEmailResponse result ->
            case result of
                Ok response ->
                    ( { model
                        | emailStatus =
                            if response.available then
                                EmailValid

                            else
                                EmailInvalid response.message
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | emailStatus = EmailInvalid "Failed to check email availability"
                      }
                    , Cmd.none
                    )

        SubmitForm ->
            if isFormValid model then
                ( { model | isSubmitting = True }
                , submitForm model
                )

            else
                ( { model | error = Just "Please fill out all fields and ensure email and organization name are valid" }
                , Cmd.none
                )

        GotSignupResponse (Ok response) ->
            if response.success then
                ( { model
                    | isSubmitting = False
                    , submitted = True
                    , error = Nothing
                  }
                , Cmd.none
                )

            else
                ( { model
                    | error = Just response.message
                    , isSubmitting = False
                  }
                , Cmd.none
                )

        GotSignupResponse (Err _) ->
            ( { model
                | error = Just "Failed to create organization. Please try again."
                , isSubmitting = False
              }
            , Cmd.none
            )


validateForm : Model -> Bool
validateForm model =
    not (String.isEmpty model.organizationName)
        && not (String.isEmpty model.adminFirstName)
        && not (String.isEmpty model.adminLastName)
        && not (String.isEmpty model.adminEmail)


submitForm : Model -> Cmd Msg
submitForm model =
    Http.post
        { url = "/api/organizations/signup"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "organizationName", Encode.string model.organizationName )
                    , ( "adminFirstName", Encode.string model.adminFirstName )
                    , ( "adminLastName", Encode.string model.adminLastName )
                    , ( "adminEmail", Encode.string model.adminEmail )
                    ]
                )
        , expect =
            Http.expectJson GotSignupResponse
                (Decode.map2 SignupResponse
                    (Decode.field "success" Decode.bool)
                    (Decode.field "message" Decode.string)
                )
        }



-- Add this function to check organization name


checkOrgName : String -> Cmd Msg
checkOrgName name =
    Http.get
        { url = "/api/organizations/check-name/" ++ Url.percentEncode name
        , expect =
            Http.expectJson GotOrgNameResponse
                (Decode.map2 OrgNameResponse
                    (Decode.field "available" Decode.bool)
                    (Decode.field "message" Decode.string)
                )
        }



-- Add this function to check email


checkEmail : String -> Cmd Msg
checkEmail email =
    Http.get
        { url = "/api/organizations/check-email/" ++ Url.percentEncode email
        , expect = Http.expectJson GotEmailResponse emailResponseDecoder
        }


emailResponseDecoder : Decode.Decoder EmailResponse
emailResponseDecoder =
    Decode.map2 EmailResponse
        (Decode.field "available" Decode.bool)
        (Decode.field "message" Decode.string)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Create Organization"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center py-12 sm:px-6 lg:px-8" ]
            [ div [ class "sm:mx-auto sm:w-full sm:max-w-md" ]
                [ h2 [ class "mt-6 text-center text-3xl font-extrabold text-gray-900" ]
                    [ text "Create your organization" ]
                ]
            , div [ class "mt-8 sm:mx-auto sm:w-full sm:max-w-md" ]
                [ div [ class "bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10" ]
                    [ if model.submitted then
                        viewSuccess

                      else
                        viewForm model
                    ]
                ]
            ]
        ]
    }


viewSuccess : Html Msg
viewSuccess =
    div [ class "text-center" ]
        [ div [ class "mx-auto flex items-center justify-center h-12 w-12 rounded-full bg-green-100" ]
            [ -- Checkmark icon
              svg
                [ Svg.Attributes.class "h-6 w-6 text-green-600"
                , fill "none"
                , viewBox "0 0 24 24"
                , stroke "currentColor"
                ]
                [ path
                    [ strokeLinecap "round"
                    , strokeLinejoin "round"
                    , strokeWidth "2"
                    , d "M5 13l4 4L19 7"
                    ]
                    []
                ]
            ]
        , h3 [ class "mt-3 text-lg font-medium text-gray-900" ]
            [ text "Check your email" ]
        , p [ class "mt-2 text-sm text-gray-500" ]
            [ text "We've sent you a magic link to verify your account and complete the setup." ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ onSubmit SubmitForm, class "space-y-6" ]
        [ -- Admin Email with validation
          div []
            [ label [ for "admin-email", class "block text-sm font-medium text-gray-700" ]
                [ text "Admin Email" ]
            , div [ class "mt-1" ]
                [ input
                    [ type_ "email"
                    , id "admin-email"
                    , value model.adminEmail
                    , onInput UpdateAdminEmail
                    , onBlur CheckAdminEmail
                    , class "appearance-none block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-purple-500 focus:border-purple-500 sm:text-sm"
                    ]
                    []
                , viewEmailStatus model.emailStatus
                ]
            ]
        , -- Organization Name with validation
          div []
            [ label [ for "organization-name", class "block text-sm font-medium text-gray-700" ]
                [ text "Organization Name" ]
            , div [ class "mt-1" ]
                [ input
                    [ type_ "text"
                    , id "organization-name"
                    , value model.organizationName
                    , onInput UpdateOrganizationName
                    , onBlur CheckOrganizationName
                    , class "appearance-none block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-purple-500 focus:border-purple-500 sm:text-sm"
                    ]
                    []
                , viewOrgNameStatus model.orgNameStatus
                ]
            ]
        , -- Admin First Name
          div []
            [ label [ for "admin-first-name", class "block text-sm font-medium text-gray-700" ]
                [ text "Admin First Name" ]
            , div [ class "mt-1" ]
                [ input
                    [ type_ "text"
                    , id "admin-first-name"
                    , value model.adminFirstName
                    , onInput UpdateAdminFirstName
                    , class "appearance-none block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-purple-500 focus:border-purple-500 sm:text-sm"
                    ]
                    []
                ]
            ]
        , -- Admin Last Name
          div []
            [ label [ for "admin-last-name", class "block text-sm font-medium text-gray-700" ]
                [ text "Admin Last Name" ]
            , div [ class "mt-1" ]
                [ input
                    [ type_ "text"
                    , id "admin-last-name"
                    , value model.adminLastName
                    , onInput UpdateAdminLastName
                    , class "appearance-none block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-purple-500 focus:border-purple-500 sm:text-sm"
                    ]
                    []
                ]
            ]
        , -- Error Message
          case model.error of
            Just error ->
                div [ class "text-red-600 text-sm" ]
                    [ text error ]

            Nothing ->
                text ""
        , -- Submit Button
          div []
            [ button
                [ type_ "submit"
                , class (submitButtonClass model)
                , disabled (not (isFormValid model) || model.isSubmitting)
                ]
                [ if model.isSubmitting then
                    text "Creating Organization..."

                  else
                    text "Create Organization"
                ]
            ]
        ]


viewOrgNameStatus : OrgNameStatus -> Html Msg
viewOrgNameStatus status =
    div [ class "mt-1 transition-all duration-200" ]
        [ case status of
            NotChecked ->
                text ""

            Checking ->
                div [ class "text-blue-600 text-sm flex items-center" ]
                    [ -- Loading spinner
                      div [ class "animate-spin h-4 w-4 mr-2 border-2 border-blue-600 border-t-transparent rounded-full" ] []
                    , text "Checking availability..."
                    ]

            Valid ->
                div [ class "text-green-600 text-sm flex items-center" ]
                    [ -- Checkmark icon
                      svg
                        [ Svg.Attributes.class "h-4 w-4 mr-1"
                        , fill "none"
                        , viewBox "0 0 24 24"
                        , stroke "currentColor"
                        ]
                        [ path
                            [ strokeLinecap "round"
                            , strokeLinejoin "round"
                            , strokeWidth "2"
                            , d "M5 13l4 4L19 7"
                            ]
                            []
                        ]
                    , text "Organization name is available"
                    ]

            Invalid message ->
                div [ class "text-red-600 text-sm flex items-center" ]
                    [ -- X icon
                      svg
                        [ Svg.Attributes.class "h-4 w-4 mr-1"
                        , fill "none"
                        , viewBox "0 0 24 24"
                        , stroke "currentColor"
                        ]
                        [ path
                            [ strokeLinecap "round"
                            , strokeLinejoin "round"
                            , strokeWidth "2"
                            , d "M6 18L18 6M6 6l12 12"
                            ]
                            []
                        ]
                    , text message
                    ]
        ]


viewEmailStatus : EmailStatus -> Html Msg
viewEmailStatus status =
    div [ class "mt-1 transition-all duration-200" ]
        [ case status of
            EmailNotChecked ->
                text ""

            EmailChecking ->
                div [ class "text-blue-600 text-sm flex items-center" ]
                    [ div [ class "animate-spin h-4 w-4 mr-2 border-2 border-blue-600 border-t-transparent rounded-full" ] []
                    , text "Checking availability..."
                    ]

            EmailValid ->
                div [ class "text-green-600 text-sm flex items-center" ]
                    [ -- Checkmark icon
                      svg
                        [ Svg.Attributes.class "h-4 w-4 mr-1"
                        , fill "none"
                        , viewBox "0 0 24 24"
                        , stroke "currentColor"
                        ]
                        [ path
                            [ strokeLinecap "round"
                            , strokeLinejoin "round"
                            , strokeWidth "2"
                            , d "M5 13l4 4L19 7"
                            ]
                            []
                        ]
                    , text "Email is available"
                    ]

            EmailInvalid message ->
                div [ class "text-red-600 text-sm flex items-center" ]
                    [ -- X icon
                      svg
                        [ Svg.Attributes.class "h-4 w-4 mr-1"
                        , fill "none"
                        , viewBox "0 0 24 24"
                        , stroke "currentColor"
                        ]
                        [ path
                            [ strokeLinecap "round"
                            , strokeLinejoin "round"
                            , strokeWidth "2"
                            , d "M6 18L18 6M6 6l12 12"
                            ]
                            []
                        ]
                    , text message
                    ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Add this helper function


isFormValid : Model -> Bool
isFormValid model =
    let
        isEmailValid =
            String.contains "@" model.adminEmail
                && String.contains "." model.adminEmail
                && model.emailStatus
                == EmailValid

        isOrgValid =
            String.length model.organizationName
                > 0
                && model.orgNameStatus
                == Valid

        areNamesValid =
            String.length model.adminFirstName
                > 0
                && String.length model.adminLastName
                > 0
    in
    isEmailValid && isOrgValid && areNamesValid



-- Add helper function for submit button classes


submitButtonClass : Model -> String
submitButtonClass model =
    "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium "
        ++ (if isFormValid model then
                "text-white bg-purple-600 hover:bg-purple-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-purple-500"

            else
                "text-white bg-purple-300 cursor-not-allowed"
           )
