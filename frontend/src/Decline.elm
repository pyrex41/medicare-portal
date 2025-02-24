module Decline exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import Url.Parser.Query as Query


type alias Model =
    { name : String
    , email : String
    , isSubmitting : Bool
    , error : Maybe String
    , success : Bool
    , quoteId : Maybe String
    , key : Nav.Key
    }


type Msg
    = UpdateName String
    | UpdateEmail String
    | SubmitForm
    | GotSubmitResponse (Result Http.Error ())
    | GotContactInfo (Result Http.Error ContactInfo)


type alias ContactInfo =
    { email : String
    , firstName : String
    , lastName : String
    }


init : Nav.Key -> Maybe String -> ( Model, Cmd Msg )
init key maybeQuoteId =
    ( { name = ""
      , email = ""
      , isSubmitting = False
      , error = Nothing
      , success = False
      , quoteId = maybeQuoteId
      , key = key
      }
    , case maybeQuoteId of
        Just quoteId ->
            Http.get
                { url = "/api/quotes/decode/" ++ quoteId
                , expect = Http.expectJson GotContactInfo contactInfoDecoder
                }

        Nothing ->
            Cmd.none
    )


contactInfoDecoder : D.Decoder ContactInfo
contactInfoDecoder =
    D.field "contact"
        (D.map3 ContactInfo
            (D.field "email" D.string)
            (D.field "firstName" D.string)
            (D.field "lastName" D.string)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        SubmitForm ->
            ( { model | isSubmitting = True }
            , Http.post
                { url = "/api/contact-request"
                , body = Http.jsonBody (encodeForm model)
                , expect = Http.expectWhatever GotSubmitResponse
                }
            )

        GotSubmitResponse result ->
            case result of
                Ok _ ->
                    ( { model | isSubmitting = False, success = True }, Cmd.none )

                Err _ ->
                    ( { model | isSubmitting = False, error = Just "Failed to submit form. Please try again." }, Cmd.none )

        GotContactInfo result ->
            case result of
                Ok info ->
                    ( { model
                        | email = info.email
                        , name = info.firstName ++ " " ++ info.lastName
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


encodeForm : Model -> E.Value
encodeForm model =
    E.object
        [ ( "name", E.string model.name )
        , ( "email", E.string model.email )
        , ( "type", E.string "decline" )
        , ( "quoteId", Maybe.map E.string model.quoteId |> Maybe.withDefault E.null )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Not Eligible - Medicare Max"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ nav [ class "bg-white border-b border-gray-200" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "flex justify-between h-16 items-center" ]
                        [ div [ class "flex-shrink-0" ]
                            [ img [ src "/images/medicare-max-logo.png", class "h-8 w-auto", alt "Medicare Max" ] [] ]
                        ]
                    ]
                ]
            , div [ class "max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-12" ]
                [ if model.success then
                    div [ class "text-center" ]
                        [ h1 [ class "text-3xl font-bold text-gray-900 mb-4" ]
                            [ text "Thank You" ]
                        , p [ class "text-gray-600" ]
                            [ text "We'll be in touch soon to discuss your options." ]
                        ]

                  else
                    div []
                        [ h1 [ class "text-3xl font-bold text-center text-gray-900 mb-4" ]
                            [ text "We Need to Talk" ]
                        , p [ class "text-gray-600 text-center mb-8" ]
                            [ text "Based on your answers, you may not qualify for this plan. However, we'd love to help you find a different plan that's a perfect fit for your needs." ]
                        , case model.error of
                            Just error ->
                                div [ class "bg-red-50 border border-red-400 text-red-700 px-4 py-3 rounded mb-4" ]
                                    [ text error ]

                            Nothing ->
                                text ""
                        , Html.form [ onSubmit SubmitForm, class "space-y-6 max-w-lg mx-auto" ]
                            [ div []
                                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                                    [ text "Name" ]
                                , input
                                    [ type_ "text"
                                    , class "w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-purple-500 focus:border-purple-500"
                                    , value model.name
                                    , onInput UpdateName
                                    , required True
                                    ]
                                    []
                                ]
                            , div []
                                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                                    [ text "Email" ]
                                , input
                                    [ type_ "email"
                                    , class "w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-purple-500 focus:border-purple-500"
                                    , value model.email
                                    , onInput UpdateEmail
                                    , required True
                                    ]
                                    []
                                ]
                            , button
                                [ class "w-full bg-purple-600 text-white py-3 px-4 rounded-lg hover:bg-purple-700 transition-colors duration-200 disabled:opacity-50"
                                , type_ "submit"
                                , disabled model.isSubmitting
                                ]
                                [ if model.isSubmitting then
                                    text "Submitting..."

                                  else
                                    text "Request Follow-up"
                                ]
                            ]
                        ]
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
