module Signup exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Components.ProgressIndicator as ProgressIndicator exposing (Step)
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
    , currentStep : SignupStep
    , key : Nav.Key
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


type SignupStep
    = AccountSetup
    | CompanyDetails
    | CompanyStyle
    | SetupPayment


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { organizationName = ""
      , adminFirstName = ""
      , adminLastName = ""
      , adminEmail = ""
      , error = Nothing
      , isSubmitting = False
      , submitted = False
      , orgNameStatus = NotChecked
      , emailStatus = EmailNotChecked
      , currentStep = AccountSetup
      , key = key
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
            if String.isEmpty (String.trim model.adminEmail) then
                ( { model | emailStatus = EmailNotChecked }
                , Cmd.none
                )

            else if model.emailStatus == EmailChecking then
                ( model, Cmd.none )

            else
                ( { model | emailStatus = EmailChecking }
                , checkEmail model.adminEmail
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
                , Nav.pushUrl model.key "/onboarding"
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
    if String.isEmpty (String.trim name) then
        Cmd.none

    else
        Http.get
            { url = "/api/organizations/check-name/" ++ Url.percentEncode (String.trim name)
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
        [ div [ class "min-h-screen bg-gray-50 flex" ]
            [ viewProgress model
            , div [ class "flex-1 ml-80" ]
                [ div [ class "max-w-2xl mx-auto py-12 px-8" ]
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
                , Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.d "M5 13l4 4L19 7"
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
    Html.form [ onSubmit SubmitForm, class "space-y-6 max-w-md" ]
        [ h1 [ class "text-2xl font-semibold text-[#101828] mb-2" ]
            [ text "Agent Details" ]
        , p [ class "text-[#667085] text-base mb-8" ]
            [ text "Let's get to know you" ]
        , -- Form fields
          div [ class "space-y-6" ]
            [ -- Email field first
              div []
                [ label [ for "admin-email", class "block text-sm font-medium text-[#344054] mb-1.5" ]
                    [ text "Email" ]
                , div [ class "mt-1" ]
                    [ input
                        [ type_ "email"
                        , id "admin-email"
                        , value model.adminEmail
                        , onInput UpdateAdminEmail
                        , onBlur CheckAdminEmail
                        , class "block w-full px-3.5 py-2.5 bg-white border border-[#d0d5dd] rounded-lg shadow-sm text-[#101828] focus:outline-none focus:ring-2 focus:ring-[#03045e] focus:border-[#03045e] sm:text-sm"
                        , placeholder "Enter your email"
                        ]
                        []
                    , viewEmailStatus model.emailStatus
                    ]
                ]

            -- Organization name field second
            , div []
                [ label [ for "organization-name", class "block text-sm font-medium text-[#344054] mb-1.5" ]
                    [ text "Organization name" ]
                , div [ class "mt-1" ]
                    [ input
                        [ type_ "text"
                        , id "organization-name"
                        , value model.organizationName
                        , onInput UpdateOrganizationName
                        , onBlur CheckOrganizationName
                        , class "block w-full px-3.5 py-2.5 bg-white border border-[#d0d5dd] rounded-lg shadow-sm text-[#101828] focus:outline-none focus:ring-2 focus:ring-[#03045e] focus:border-[#03045e] sm:text-sm"
                        , placeholder "Enter organization name"
                        ]
                        []
                    , viewOrgNameStatus model.orgNameStatus
                    ]
                ]

            -- First Name field third
            , div []
                [ label [ for "admin-first-name", class "block text-sm font-medium text-[#344054] mb-1.5" ]
                    [ text "First Name" ]
                , div [ class "mt-1" ]
                    [ input
                        [ type_ "text"
                        , id "admin-first-name"
                        , value model.adminFirstName
                        , onInput UpdateAdminFirstName
                        , class "block w-full px-3.5 py-2.5 bg-white border border-[#d0d5dd] rounded-lg shadow-sm text-[#101828] focus:outline-none focus:ring-2 focus:ring-[#03045e] focus:border-[#03045e] sm:text-sm"
                        , placeholder "Enter your first name"
                        ]
                        []
                    ]
                ]

            -- Last Name field fourth
            , div []
                [ label [ for "admin-last-name", class "block text-sm font-medium text-[#344054] mb-1.5" ]
                    [ text "Last Name" ]
                , div [ class "mt-1" ]
                    [ input
                        [ type_ "text"
                        , id "admin-last-name"
                        , value model.adminLastName
                        , onInput UpdateAdminLastName
                        , class "block w-full px-3.5 py-2.5 bg-white border border-[#d0d5dd] rounded-lg shadow-sm text-[#101828] focus:outline-none focus:ring-2 focus:ring-[#03045e] focus:border-[#03045e] sm:text-sm"
                        , placeholder "Enter your last name"
                        ]
                        []
                    ]
                ]
            ]
        , -- Submit button
          button
            [ type_ "submit"
            , class (submitButtonClass model)
            , disabled (not (isFormValid model) || model.isSubmitting)
            ]
            [ if model.isSubmitting then
                text "Creating Organization..."

              else
                text "Continue"
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
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.viewBox "0 0 24 24"
                        , Svg.Attributes.stroke "currentColor"
                        ]
                        [ path
                            [ Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.d "M5 13l4 4L19 7"
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
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.viewBox "0 0 24 24"
                        , Svg.Attributes.stroke "currentColor"
                        ]
                        [ path
                            [ Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.d "M6 18L18 6M6 6l12 12"
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
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.viewBox "0 0 24 24"
                        , Svg.Attributes.stroke "currentColor"
                        ]
                        [ path
                            [ Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.d "M5 13l4 4L19 7"
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
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.viewBox "0 0 24 24"
                        , Svg.Attributes.stroke "currentColor"
                        ]
                        [ path
                            [ Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeLinejoin "round"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.d "M6 18L18 6M6 6l12 12"
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
            not (String.isEmpty (String.trim model.adminEmail))
                && String.contains "@" model.adminEmail
                && String.contains "." model.adminEmail
                && model.emailStatus
                == EmailValid

        isOrgValid =
            not (String.isEmpty (String.trim model.organizationName))
                && model.orgNameStatus
                == Valid

        areNamesValid =
            not (String.isEmpty (String.trim model.adminFirstName))
                && not (String.isEmpty (String.trim model.adminLastName))
    in
    isEmailValid && isOrgValid && areNamesValid



-- Add helper function for submit button classes


submitButtonClass : Model -> String
submitButtonClass model =
    "w-full flex justify-center py-2.5 px-4 border border-transparent rounded-lg shadow-sm text-sm font-medium transition-colors "
        ++ (if isFormValid model then
                "text-white bg-[#03045e] hover:bg-[#03045e]/90 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#03045e]"

            else
                "text-white bg-[#03045e]/60 cursor-not-allowed"
           )


viewProgress : Model -> Html Msg
viewProgress model =
    let
        currentStep =
            case model.currentStep of
                AccountSetup ->
                    1

                CompanyDetails ->
                    2

                CompanyStyle ->
                    3

                SetupPayment ->
                    4

        makeStep : Int -> String -> String -> String -> Step
        makeStep stepNum icon title description =
            { icon = icon
            , title = title
            , description = description
            , isCompleted = stepNum < currentStep
            , isActive = stepNum == currentStep
            }
    in
    ProgressIndicator.view
        [ makeStep 1 "👤" "Your Details" "Please provide your name and email"
        , makeStep 2 "🏢" "Company Details" "General info for your Company"
        , makeStep 3 "⚙️" "Company Style" "Style your platform"
        , makeStep 4 "💳" "Setup Payment" "The final step to get started"
        ]
