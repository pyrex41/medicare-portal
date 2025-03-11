module Schedule exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Url


type EligibilityStatus
    = Accept
    | Decline
    | Generic


type alias OrgInfo =
    { redirectUrl : Maybe String
    , agentName : String
    }


type alias Model =
    { name : Maybe String
    , email : Maybe String
    , phoneNumber : Maybe String
    , isSubmitting : Bool
    , error : Maybe String
    , success : Bool
    , quoteId : Maybe String
    , key : Nav.Key
    , status : EligibilityStatus
    , redirectUrl : Maybe String
    }


type Msg
    = UpdateName String
    | UpdateEmail String
    | UpdatePhoneNumber String
    | SubmitForm
    | GotSubmitResponse (Result Http.Error SubmitResponse)
    | GotContactInfo (Result Http.Error ContactInfo)


type alias ContactInfo =
    { email : String
    , firstName : String
    , lastName : String
    , phoneNumber : String
    }


type alias SubmitResponse =
    { success : Bool
    , message : String
    }


init : Nav.Key -> Maybe String -> Maybe String -> ( Model, Cmd Msg )
init key maybeQuoteId maybeStatus =
    let
        status =
            case maybeStatus of
                Just "accept" ->
                    Accept

                Just "decline" ->
                    Decline

                _ ->
                    Generic

        commands =
            case maybeQuoteId of
                Just quoteId ->
                    [ Http.get
                        { url = "/api/quotes/decode/" ++ quoteId
                        , expect = Http.expectJson GotContactInfo contactInfoDecoder
                        }
                    ]

                Nothing ->
                    []
    in
    ( { name = Nothing
      , email = Nothing
      , phoneNumber = Nothing
      , isSubmitting = False
      , error = Nothing
      , success = False
      , quoteId = maybeQuoteId
      , key = key
      , status = status
      , redirectUrl = Just "https://calendly.com/medicareschool-max/30min"
      }
    , Cmd.batch commands
    )


contactInfoDecoder : D.Decoder ContactInfo
contactInfoDecoder =
    D.field "contact"
        (D.map4 ContactInfo
            (D.field "email" D.string)
            (D.field "firstName" D.string)
            (D.field "lastName" D.string)
            (D.field "phoneNumber" D.string)
        )


submitResponseDecoder : D.Decoder SubmitResponse
submitResponseDecoder =
    D.map2 SubmitResponse
        (D.field "success" D.bool)
        (D.field "message" D.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = Just name }, Cmd.none )

        UpdateEmail email ->
            ( { model | email = Just email }, Cmd.none )

        UpdatePhoneNumber phoneNumber ->
            ( { model | phoneNumber = Just (stripPhoneNumber phoneNumber) }, Cmd.none )

        SubmitForm ->
            ( { model | isSubmitting = True }
            , Http.post
                { url = "/api/contact-request"
                , body = Http.jsonBody (encodeForm model)
                , expect = Http.expectJson GotSubmitResponse submitResponseDecoder
                }
            )

        GotSubmitResponse result ->
            case result of
                Ok response ->
                    if response.success then
                        ( { model | isSubmitting = False, success = True }
                        , Cmd.none
                        )

                    else
                        ( { model | isSubmitting = False, error = Just response.message }
                        , Cmd.none
                        )

                Err _ ->
                    ( { model | isSubmitting = False, error = Just "Failed to submit form. Please try again." }
                    , Cmd.none
                    )

        GotContactInfo result ->
            case result of
                Ok info ->
                    ( { model
                        | email = Just info.email
                        , name = Just (info.firstName ++ " " ++ info.lastName)
                        , phoneNumber = Just info.phoneNumber
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


stripPhoneNumber : String -> String
stripPhoneNumber phoneNumber =
    String.filter Char.isDigit phoneNumber


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    if String.isEmpty phone then
        ""

    else
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


encodeForm : Model -> E.Value
encodeForm model =
    E.object
        [ ( "name", model.name |> Maybe.map Url.percentEncode |> Maybe.map E.string |> Maybe.withDefault E.null )
        , ( "email", model.email |> Maybe.map Url.percentEncode |> Maybe.map E.string |> Maybe.withDefault E.null )
        , ( "phoneNumber", model.phoneNumber |> Maybe.map stripPhoneNumber |> Maybe.map E.string |> Maybe.withDefault E.null )
        , ( "type"
          , E.string
                (case model.status of
                    Accept ->
                        "accept"

                    Decline ->
                        "decline"

                    Generic ->
                        "generic"
                )
          )
        , ( "quoteId", Maybe.map E.string model.quoteId |> Maybe.withDefault E.null )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = getTitle model.status
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ div [ class "max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-12" ]
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
                            [ text (getHeading model.status) ]
                        , p [ class "text-gray-600 text-center mb-8" ]
                            [ text (getMessage model.status) ]
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
                                    , value (model.name |> Maybe.withDefault "")
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
                                    , value (model.email |> Maybe.withDefault "")
                                    , onInput UpdateEmail
                                    , required True
                                    ]
                                    []
                                ]
                            , div []
                                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                                    [ text "Phone Number" ]
                                , input
                                    [ type_ "tel"
                                    , class "w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-purple-500 focus:border-purple-500"
                                    , value (model.phoneNumber |> Maybe.map formatPhoneNumber |> Maybe.withDefault "")
                                    , onInput UpdatePhoneNumber
                                    , required True
                                    ]
                                    []
                                ]
                            , case model.redirectUrl of
                                Just url ->
                                    let
                                        fullUrl =
                                            List.Extra.zip
                                                [ "email", "name", "location" ]
                                                [ model.email
                                                , model.name
                                                , model.phoneNumber
                                                ]
                                                |> List.filterMap
                                                    (\( key, value ) ->
                                                        case value of
                                                            Just s ->
                                                                case key of
                                                                    "location" ->
                                                                        Just (key ++ "=1" ++ Url.percentEncode s)

                                                                    _ ->
                                                                        Just (key ++ "=" ++ Url.percentEncode s)

                                                            Nothing ->
                                                                Nothing
                                                    )
                                                |> String.join "&"
                                                |> (\s -> url ++ "?" ++ s)
                                    in
                                    a
                                        [ class "w-full bg-purple-600 text-white py-3 px-4 rounded-lg hover:bg-purple-700 transition-colors duration-200 disabled:opacity-50 text-center block"
                                        , href fullUrl
                                        , target "_blank"
                                        ]
                                        [ text "Schedule Follow-up" ]

                                Nothing ->
                                    button
                                        [ class "w-full bg-purple-600 text-white py-3 px-4 rounded-lg hover:bg-purple-700 transition-colors duration-200 disabled:opacity-50"
                                        , type_ "submit"
                                        , disabled model.isSubmitting
                                        ]
                                        [ if model.isSubmitting then
                                            text "Submitting..."

                                          else
                                            text "Schedule Follow-up"
                                        ]
                            ]
                        ]
                ]
            ]
        ]
    }


getTitle : EligibilityStatus -> String
getTitle status =
    case status of
        Accept ->
            "Good News! - Medicare Max"

        Decline ->
            "Not Eligible - Medicare Max"

        Generic ->
            "Schedule Follow-up - Medicare Max"


getHeading : EligibilityStatus -> String
getHeading status =
    case status of
        Accept ->
            "Great News!"

        Decline ->
            "We Need to Talk"

        Generic ->
            "Let's Connect"


getMessage : EligibilityStatus -> String
getMessage status =
    case status of
        Accept ->
            "Based on your answers, you look like a good candidate to switch plans. Let's schedule a follow-up to discuss your options."

        Decline ->
            "Based on your answers, you may not qualify for this plan. However, we'd love to help you find a different plan that's a perfect fit for your needs."

        Generic ->
            "Let's schedule a follow-up call to discuss your Medicare options and find the best plan for your needs."


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
