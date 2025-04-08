module Onboarding.Steps.CompanyDetails exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task



-- MODEL


type alias Model =
    { agencyName : String
    , website : String
    , phone : String
    , primaryColor : String
    , secondaryColor : String
    , logo : Maybe String
    , isLoading : Bool
    , error : Maybe String
    , key : Nav.Key
    , orgSlug : String
    , uploadingLogo : Bool
    , sessionToken : String
    , loadedFromSession : Bool
    }


init : Nav.Key -> String -> String -> ( Model, Cmd Msg )
init key orgSlug sessionToken =
    ( { agencyName = ""
      , website = ""
      , phone = ""
      , primaryColor = "#6B46C1"
      , secondaryColor = "#9F7AEA"
      , logo = Nothing
      , isLoading = True
      , error = Nothing
      , key = key
      , orgSlug = orgSlug
      , uploadingLogo = False
      , sessionToken = sessionToken
      , loadedFromSession = False
      }
    , fetchCompanyDetails orgSlug
    )



-- UPDATE


type Msg
    = UpdateAgencyName String
    | UpdateWebsite String
    | UpdatePhone String
    | UpdatePrimaryColor String
    | UpdateSecondaryColor String
    | UploadLogo
    | GotLogo File
    | GotLogoUrl String
    | LogoUploaded (Result Http.Error String)
    | NextStepClicked
    | SkipStepClicked
    | GotCompanyDetails (Result Http.Error CompanyDetailsResponse)
    | CompanyDetailsSaved (Result Http.Error ())
    | NoOp


type OutMsg
    = NoOutMsg
    | NextStep
    | ShowError String


type alias CompanyDetailsResponse =
    { agencyName : String
    , website : String
    , phone : String
    , primaryColor : String
    , secondaryColor : String
    , logo : Maybe String
    }


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        UpdateAgencyName value ->
            ( { model | agencyName = value }, Cmd.none, NoOutMsg )

        UpdateWebsite value ->
            ( { model | website = value }, Cmd.none, NoOutMsg )

        UpdatePhone value ->
            ( { model | phone = value }, Cmd.none, NoOutMsg )

        UpdatePrimaryColor value ->
            ( { model | primaryColor = value }, Cmd.none, NoOutMsg )

        UpdateSecondaryColor value ->
            ( { model | secondaryColor = value }, Cmd.none, NoOutMsg )

        UploadLogo ->
            ( model, Select.file [ "image/png", "image/jpeg" ] GotLogo, NoOutMsg )

        GotLogo file ->
            ( { model | uploadingLogo = True }, Task.perform GotLogoUrl (File.toUrl file), NoOutMsg )

        GotLogoUrl url ->
            ( { model | logo = Just url, uploadingLogo = False }, Cmd.none, NoOutMsg )

        LogoUploaded result ->
            case result of
                Ok url ->
                    ( { model | logo = Just url, uploadingLogo = False }, Cmd.none, NoOutMsg )

                Err _ ->
                    ( { model | error = Just "Failed to upload logo", uploadingLogo = False }
                    , Cmd.none
                    , ShowError "Failed to upload logo"
                    )

        NextStepClicked ->
            ( { model | isLoading = True }
            , saveCompanyDetails model
            , NoOutMsg
            )

        SkipStepClicked ->
            ( model, Cmd.none, NextStep )

        GotCompanyDetails result ->
            case result of
                Ok response ->
                    ( { model
                        | agencyName = response.agencyName
                        , website = response.website
                        , phone = response.phone
                        , primaryColor = response.primaryColor
                        , secondaryColor = response.secondaryColor
                        , logo = response.logo
                        , isLoading = False
                        , loadedFromSession = True
                      }
                    , Cmd.none
                    , NoOutMsg
                    )

                Err _ ->
                    ( { model | isLoading = False }, Cmd.none, NoOutMsg )

        CompanyDetailsSaved result ->
            case result of
                Ok _ ->
                    ( { model | isLoading = False }
                    , Cmd.none
                    , NextStep
                    )

                Err _ ->
                    ( { model
                        | error = Just "Failed to save company details"
                        , isLoading = False
                      }
                    , Cmd.none
                    , ShowError "Failed to save company details"
                    )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "space-y-8" ]
        [ div [ class "mb-8" ]
            [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                [ text "Company Details" ]
            , p [ class "text-gray-600 mt-2" ]
                [ text "Tell us about your agency (all fields are optional)" ]
            , if model.loadedFromSession then
                p [ class "text-blue-600 mt-2 italic" ]
                    [ text "Your previously entered information has been loaded." ]

              else
                text ""
            ]
        , if model.isLoading then
            viewLoading

          else
            viewCompanyDetailsForm model
        ]


viewCompanyDetailsForm : Model -> Html Msg
viewCompanyDetailsForm model =
    div [ class "space-y-8" ]
        [ div [ class "bg-white shadow rounded-lg p-6" ]
            [ h2 [ class "text-lg font-medium mb-4" ] [ text "Agency Settings" ]
            , div [ class "space-y-6" ]
                [ div [ class "space-y-4" ]
                    [ viewFormGroup "Agency Name"
                        (input
                            [ type_ "text"
                            , class "w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                            , value model.agencyName
                            , onInput UpdateAgencyName
                            , placeholder "Enter your agency name"
                            ]
                            []
                        )
                    , viewFormGroup "Primary Color"
                        (div [ class "flex items-center space-x-4" ]
                            [ input
                                [ type_ "color"
                                , class "w-16 h-10 p-1 border border-gray-300 rounded"
                                , value model.primaryColor
                                , onInput UpdatePrimaryColor
                                ]
                                []
                            , input
                                [ type_ "text"
                                , class "flex-1 px-4 py-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                                , value model.primaryColor
                                , onInput UpdatePrimaryColor
                                ]
                                []
                            ]
                        )
                    , viewFormGroup "Secondary Color"
                        (div [ class "flex items-center space-x-4" ]
                            [ input
                                [ type_ "color"
                                , class "w-16 h-10 p-1 border border-gray-300 rounded"
                                , value model.secondaryColor
                                , onInput UpdateSecondaryColor
                                ]
                                []
                            , input
                                [ type_ "text"
                                , class "flex-1 px-4 py-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                                , value model.secondaryColor
                                , onInput UpdateSecondaryColor
                                ]
                                []
                            ]
                        )
                    , viewFormGroup "Logo"
                        (div [ class "flex items-center space-x-4" ]
                            [ case model.logo of
                                Just logoUrl ->
                                    div [ class "flex items-center space-x-4" ]
                                        [ img
                                            [ src logoUrl
                                            , class "h-16 w-16 object-contain border border-gray-200 rounded"
                                            ]
                                            []
                                        , button
                                            [ class "px-4 py-2 text-sm text-blue-600 hover:text-blue-800 border border-blue-200 rounded"
                                            , onClick UploadLogo
                                            , disabled model.uploadingLogo
                                            ]
                                            [ text "Change Logo" ]
                                        ]

                                Nothing ->
                                    if model.uploadingLogo then
                                        div [ class "flex items-center space-x-2" ]
                                            [ div [ class "animate-spin rounded-full h-5 w-5 border-t-2 border-b-2 border-blue-500" ] []
                                            , text "Uploading..."
                                            ]

                                    else
                                        button
                                            [ class "px-4 py-2 text-sm text-blue-600 hover:text-blue-800 border border-blue-200 rounded"
                                            , onClick UploadLogo
                                            ]
                                            [ text "Upload Logo" ]
                            ]
                        )
                    ]
                ]
            ]
        , if model.error /= Nothing then
            div [ class "bg-red-100 border border-red-400 text-red-700 px-6 py-4 rounded" ]
                [ text (Maybe.withDefault "" model.error) ]

          else
            text ""
        , div [ class "flex justify-center mt-8" ]
            [ button
                [ class "px-6 py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                , onClick NextStepClicked
                , disabled model.isLoading
                ]
                [ if model.isLoading then
                    div [ class "flex items-center justify-center" ]
                        [ div [ class "animate-spin mr-2 h-4 w-4 border-t-2 border-b-2 border-white rounded-full" ] []
                        , text "Saving..."
                        ]

                  else
                    text "Continue"
                ]
            ]
        ]


viewFormGroup : String -> Html Msg -> Html Msg
viewFormGroup labelText content =
    div [ class "mb-4" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-2" ]
            [ text labelText ]
        , content
        ]


viewLoading : Html msg
viewLoading =
    div [ class "flex justify-center items-center py-12" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-t-4 border-b-4 border-blue-500 mx-auto" ] []
        , p [ class "mt-4 text-gray-500" ]
            [ text "Loading..." ]
        ]



-- HELPERS


isFormValid : Model -> Bool
isFormValid model =
    -- Agency name is now optional as it will be auto-generated
    True



-- API CALLS


fetchCompanyDetails : String -> Cmd Msg
fetchCompanyDetails _ =
    Http.get
        { url = "/api/onboarding/settings"
        , expect = Http.expectJson GotCompanyDetails companyDetailsDecoder
        }


saveCompanyDetails : Model -> Cmd Msg
saveCompanyDetails model =
    let
        url =
            "/api/onboarding/company-details"
    in
    Http.request
        { method = "POST"
        , headers = [] -- The session token is in the cookies, no need to pass it
        , url = url
        , body = Http.jsonBody (encodeCompanyDetails model)
        , expect = Http.expectWhatever CompanyDetailsSaved
        , timeout = Nothing
        , tracker = Nothing
        }



-- DECODERS & ENCODERS


companyDetailsDecoder : Decode.Decoder CompanyDetailsResponse
companyDetailsDecoder =
    Decode.field "companyDetailsModel"
        (Decode.map6 CompanyDetailsResponse
            (Decode.field "agencyName" Decode.string)
            (Decode.field "website" Decode.string)
            (Decode.field "phone" Decode.string)
            (Decode.field "primaryColor" Decode.string)
            (Decode.field "secondaryColor" Decode.string)
            (Decode.field "logo" (Decode.nullable Decode.string))
        )


encodeCompanyDetails : Model -> Encode.Value
encodeCompanyDetails model =
    Encode.object
        [ ( "agencyName", Encode.string model.agencyName )
        , ( "website", Encode.string model.website )
        , ( "phone", Encode.string model.phone )
        , ( "primaryColor", Encode.string model.primaryColor )
        , ( "secondaryColor", Encode.string model.secondaryColor )
        , ( "logo", Maybe.withDefault Encode.null (Maybe.map Encode.string model.logo) )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
