module BrandSettings exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser
import Browser.Navigation as Nav
import Components.SetupLayout as SetupLayout
import Debug
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Task



-- MODEL


type alias Model =
    { key : Nav.Key
    , session : String
    , orgSlug : String
    , isSetup : Bool
    , brandName : String
    , primaryColor : String
    , secondaryColor : String
    , logo : Maybe String
    , isLoading : Bool
    , error : Maybe String
    , isSaving : Bool
    }


type alias BrandSettings =
    { brandName : String
    , primaryColor : String
    , secondaryColor : String
    , logo : Maybe String
    }


init : { key : Nav.Key, session : String, orgSlug : String, isSetup : Bool } -> ( Model, Cmd Msg )
init flags =
    ( { key = flags.key
      , session = flags.session
      , orgSlug = flags.orgSlug
      , isSetup = flags.isSetup
      , brandName = ""
      , primaryColor = "#6B46C1" -- Default purple color
      , secondaryColor = "#9F7AEA"
      , logo = Nothing
      , isLoading = True
      , error = Nothing
      , isSaving = False
      }
    , fetchBrandSettings flags.orgSlug flags.session
    )



-- UPDATE


type Msg
    = GotBrandSettings (Result Http.Error BrandSettings)
    | UpdateBrandName String
    | UpdatePrimaryColor String
    | UpdateSecondaryColor String
    | UploadLogo
    | GotLogo File
    | GotLogoUrl String
    | LogoUploaded (Result Http.Error String)
    | SaveSettings
    | SettingsSaved (Result Http.Error ())
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBrandSettings (Ok settings) ->
            ( { model
                | brandName = settings.brandName
                , primaryColor = settings.primaryColor
                , secondaryColor = settings.secondaryColor
                , logo = settings.logo
                , isLoading = False
              }
            , Cmd.none
            )

        GotBrandSettings (Err _) ->
            ( { model
                | error = Just "Failed to load brand settings"
                , isLoading = False
              }
            , Cmd.none
            )

        UpdateBrandName name ->
            ( { model | brandName = name }
            , Cmd.none
            )

        UpdatePrimaryColor color ->
            ( { model | primaryColor = color }
            , Cmd.none
            )

        UpdateSecondaryColor color ->
            ( { model | secondaryColor = color }
            , Cmd.none
            )

        UploadLogo ->
            ( model
            , Select.file [ "image/png", "image/jpeg" ] GotLogo
            )

        GotLogo file ->
            ( model
            , Task.perform GotLogoUrl (File.toUrl file)
            )

        GotLogoUrl url ->
            ( { model | logo = Just url }
            , Cmd.none
            )

        LogoUploaded (Ok url) ->
            ( { model | logo = Just url }
            , Cmd.none
            )

        LogoUploaded (Err _) ->
            ( { model | error = Just "Failed to upload logo" }
            , Cmd.none
            )

        SaveSettings ->
            ( { model | isSaving = True }
            , saveBrandSettings model
            )

        SettingsSaved (Ok ()) ->
            ( { model | isSaving = False, error = Nothing }
            , Cmd.none
            )

        SettingsSaved (Err _) ->
            ( { model
                | isSaving = False
                , error = Just "Failed to save settings"
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Brand Settings"
    , body =
        [ if model.isSetup then
            SetupLayout.view SetupLayout.BrandSetup
                [ if model.isLoading then
                    viewLoading

                  else
                    viewSettings model
                ]

          else
            div [ class "min-h-screen bg-gray-50" ]
                [ viewHeader
                , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8" ]
                    [ if model.isLoading then
                        viewLoading

                      else
                        viewSettings model
                    ]
                ]
        ]
    }


viewHeader : Html Msg
viewHeader =
    nav [ class "bg-white border-b border-gray-200" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex justify-between h-16" ]
                [ div [ class "flex" ]
                    [ div [ class "flex-shrink-0 flex items-center" ]
                        [ h1 [ class "text-xl font-semibold text-purple-600" ]
                            [ text "Brand Settings" ]
                        ]
                    ]
                ]
            ]
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "flex justify-center items-center h-64" ]
        [ div [ class "animate-spin rounded-full h-12 w-12 border-4 border-purple-500 border-t-transparent" ] []
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    div [ class "bg-white shadow-sm rounded-lg p-6" ]
        [ div [ class "space-y-6" ]
            [ viewErrorMessage model.error
            , viewFormGroup "Brand Name"
                (input
                    [ type_ "text"
                    , class "w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-purple-500 focus:border-purple-500"
                    , value model.brandName
                    , onInput UpdateBrandName
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
                        , class "flex-1 px-4 py-2 border border-gray-300 rounded-md focus:ring-purple-500 focus:border-purple-500"
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
                        , class "flex-1 px-4 py-2 border border-gray-300 rounded-md focus:ring-purple-500 focus:border-purple-500"
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
                                    [ class "px-4 py-2 text-sm text-purple-600 hover:text-purple-800"
                                    , onClick UploadLogo
                                    ]
                                    [ text "Change Logo" ]
                                ]

                        Nothing ->
                            button
                                [ class "px-4 py-2 text-sm text-purple-600 hover:text-purple-800 border border-purple-200 rounded"
                                , onClick UploadLogo
                                ]
                                [ text "Upload Logo" ]
                    ]
                )
            , div [ class "flex justify-center mt-8" ]
                [ button
                    [ class "px-6 py-2 bg-purple-600 text-white rounded-md hover:bg-purple-700 focus:outline-none focus:ring-2 focus:ring-purple-500 focus:ring-offset-2 disabled:opacity-50"
                    , onClick SaveSettings
                    , disabled model.isSaving
                    ]
                    [ if model.isSaving then
                        text "Saving..."

                      else
                        text "Save Changes"
                    ]
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


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage maybeError =
    case maybeError of
        Just error ->
            div [ class "bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded relative" ]
                [ text error ]

        Nothing ->
            text ""



-- HTTP


fetchBrandSettings : String -> String -> Cmd Msg
fetchBrandSettings orgSlug session =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ session) ]
        , url = "/api/organizations/" ++ orgSlug ++ "/brand"
        , body = Http.emptyBody
        , expect = Http.expectJson GotBrandSettings brandSettingsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveBrandSettings : Model -> Cmd Msg
saveBrandSettings model =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ model.session) ]
        , url = "/api/organizations/" ++ model.orgSlug ++ "/brand"
        , body = Http.jsonBody (encodeBrandSettings model)
        , expect = Http.expectWhatever SettingsSaved
        , timeout = Nothing
        , tracker = Nothing
        }



-- JSON


brandSettingsDecoder : Decoder BrandSettings
brandSettingsDecoder =
    Decode.succeed BrandSettings
        |> Pipeline.required "brand_name" Decode.string
        |> Pipeline.required "primary_color" Decode.string
        |> Pipeline.required "secondary_color" Decode.string
        |> Pipeline.optional "logo" (Decode.nullable Decode.string) Nothing


encodeBrandSettings : Model -> Encode.Value
encodeBrandSettings model =
    Encode.object
        [ ( "brand_name", Encode.string model.brandName )
        , ( "primary_color", Encode.string model.primaryColor )
        , ( "secondary_color", Encode.string model.secondaryColor )
        , ( "logo", Maybe.map Encode.string model.logo |> Maybe.withDefault Encode.null )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
