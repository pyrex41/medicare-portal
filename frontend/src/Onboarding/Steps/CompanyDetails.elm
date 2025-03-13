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
    }


init : Nav.Key -> String -> String -> ( Model, Cmd Msg )
init key orgSlug sessionToken =
    ( { agencyName = ""
      , website = ""
      , phone = ""
      , primaryColor = "#6B46C1"
      , secondaryColor = "#9F7AEA"
      , logo = Nothing
      , isLoading = False
      , error = Nothing
      , key = key
      , orgSlug = orgSlug
      , uploadingLogo = False
      , sessionToken = sessionToken
      }
    , Cmd.none
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
            ]
        , if model.isLoading then
            viewLoading

          else
            div [ class "space-y-8" ]
                [ div [ class "bg-white shadow rounded-lg p-8" ]
                    [ div [ class "space-y-8 max-w-3xl mx-auto" ]
                        [ div [ class "mb-6" ]
                            [ label [ class "block text-sm font-medium text-gray-700 mb-3" ]
                                [ text "Agency Name" ]
                            , input
                                [ type_ "text"
                                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 px-4 py-3"
                                , value model.agencyName
                                , onInput UpdateAgencyName
                                , placeholder "Enter your agency name"
                                ]
                                []
                            ]
                        , div [ class "grid grid-cols-1 md:grid-cols-2 gap-8 mb-6" ]
                            [ div []
                                [ label [ class "block text-sm font-medium text-gray-700 mb-3" ]
                                    [ text "Primary Color (Optional)" ]
                                , div [ class "mt-1 flex items-center space-x-4" ]
                                    [ input
                                        [ type_ "color"
                                        , class "h-12 w-12 border border-gray-300 rounded"
                                        , value model.primaryColor
                                        , onInput UpdatePrimaryColor
                                        ]
                                        []
                                    , input
                                        [ type_ "text"
                                        , class "flex-1 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 px-4 py-3"
                                        , value model.primaryColor
                                        , onInput UpdatePrimaryColor
                                        ]
                                        []
                                    ]
                                ]
                            , div []
                                [ label [ class "block text-sm font-medium text-gray-700 mb-3" ]
                                    [ text "Secondary Color (Optional)" ]
                                , div [ class "mt-1 flex items-center space-x-4" ]
                                    [ input
                                        [ type_ "color"
                                        , class "h-12 w-12 border border-gray-300 rounded"
                                        , value model.secondaryColor
                                        , onInput UpdateSecondaryColor
                                        ]
                                        []
                                    , input
                                        [ type_ "text"
                                        , class "flex-1 rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 px-4 py-3"
                                        , value model.secondaryColor
                                        , onInput UpdateSecondaryColor
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , div [ class "mb-6" ]
                            [ label [ class "block text-sm font-medium text-gray-700 mb-3" ]
                                [ text "Logo (Optional)" ]
                            , div [ class "mt-1 flex items-center space-x-4" ]
                                [ case model.logo of
                                    Just logoUrl ->
                                        div [ class "flex items-center space-x-6" ]
                                            [ img
                                                [ src logoUrl
                                                , class "h-20 w-20 object-contain border border-gray-200 rounded p-1"
                                                ]
                                                []
                                            , button
                                                [ class "px-5 py-2.5 text-sm text-blue-600 hover:text-blue-800 border border-blue-200 rounded-md"
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
                                                [ class "px-5 py-2.5 text-sm text-blue-600 hover:text-blue-800 border border-blue-200 rounded-md"
                                                , onClick UploadLogo
                                                ]
                                                [ text "Upload Logo" ]
                                ]
                            ]
                        ]
                    ]
                , if model.error /= Nothing then
                    div [ class "bg-red-100 border border-red-400 text-red-700 px-6 py-4 rounded" ]
                        [ text (Maybe.withDefault "" model.error) ]

                  else
                    text ""
                , div [ class "flex justify-between mt-8" ]
                    [ button
                        [ class "px-6 py-3 bg-gray-200 text-gray-700 rounded-md hover:bg-gray-300 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-gray-500"
                        , onClick SkipStepClicked
                        ]
                        [ text "Skip" ]
                    , button
                        [ class "px-6 py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                        , onClick NextStepClicked
                        ]
                        [ text "Continue" ]
                    ]
                ]
        ]


viewLoading : Html msg
viewLoading =
    div [ class "text-center py-12" ]
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
fetchCompanyDetails orgSlug =
    Http.get
        { url = "/api/onboarding/settings"
        , expect = Http.expectJson GotCompanyDetails companyDetailsDecoder
        }


saveCompanyDetails : Model -> Cmd Msg
saveCompanyDetails model =
    let
        url =
            "/api/onboarding/company-details"

        _ =
            Debug.log "Saving company details"
                { url = url
                , agencyName = model.agencyName
                , website = model.website
                , phone = model.phone
                }
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
    Decode.map6 CompanyDetailsResponse
        (Decode.field "agencyName" Decode.string)
        (Decode.field "website" Decode.string)
        (Decode.field "phone" Decode.string)
        (Decode.field "primaryColor" Decode.string)
        (Decode.field "secondaryColor" Decode.string)
        (Decode.field "logo" (Decode.nullable Decode.string))


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
