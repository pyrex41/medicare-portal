module SelfServiceOnboarding exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, string)



-- MODEL


type alias Model =
    { orgId : Maybe String
    , orgSlug : Maybe String
    , email : String
    , firstName : String
    , lastName : String
    , optInQuarterlyUpdates : Bool
    , emailReadOnly : Bool
    , isSubmitting : Bool
    , error : Maybe String
    , success : Bool
    , key : Nav.Key
    }



-- INIT


type Route
    = SlugRoute String
    | QueryRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map SlugRoute (Parser.s "self-onboarding" </> string)
        , Parser.map QueryRoute Parser.top
        ]


parseUrl : Url -> Route
parseUrl url =
    Parser.parse routeParser url |> Maybe.withDefault QueryRoute


type alias UrlParams =
    { orgId : Maybe String
    , email : Maybe String
    , hash : Maybe String
    }


init : Nav.Key -> Url -> ( Model, Cmd Msg )
init key url =
    let
        route =
            parseUrl url

        initialModel =
            { orgId = Nothing
            , orgSlug = Nothing
            , email = ""
            , firstName = ""
            , lastName = ""
            , optInQuarterlyUpdates = False
            , emailReadOnly = False
            , isSubmitting = False
            , error = Nothing
            , success = False
            , key = key
            }
    in
    case route of
        SlugRoute slug ->
            ( { initialModel | orgSlug = Just slug }
            , fetchOrgDetails slug
            )

        QueryRoute ->
            -- Handle query parameters for backward compatibility
            let
                params =
                    parseUrlParams url

                orgId =
                    params.orgId

                email =
                    params.email

                hash =
                    params.hash

                initCmd =
                    case orgId of
                        Just oid ->
                            let
                                queryParams =
                                    List.filterMap identity
                                        [ Just ( "orgId", oid )
                                        , Maybe.map (\e -> ( "email", e )) email
                                        , Maybe.map (\h -> ( "hash", h )) hash
                                        ]
                                        |> List.map (\( k, v ) -> k ++ "=" ++ Url.percentEncode v)
                                        |> String.join "&"
                            in
                            Http.get
                                { url = "/api/self-service/init?" ++ queryParams
                                , expect = Http.expectJson GotInitResponse initResponseDecoder
                                }

                        Nothing ->
                            Cmd.none
            in
            ( { initialModel | orgId = orgId }
            , initCmd
            )


parseUrlParams : Url -> UrlParams
parseUrlParams url =
    -- This function parses query parameters for backward compatibility
    let
        queryParams =
            url.query
                |> Maybe.map (String.split "&")
                |> Maybe.withDefault []
                |> List.map (String.split "=")
                |> List.filterMap
                    (\parts ->
                        case parts of
                            [ key, value ] ->
                                Just ( key, Url.percentDecode value |> Maybe.withDefault value )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList

        getParam name =
            Dict.get name queryParams
    in
    { orgId = getParam "orgId"
    , email = getParam "email"
    , hash = getParam "hash"
    }


fetchOrgDetails : String -> Cmd Msg
fetchOrgDetails slug =
    Http.get
        { url = "/api/self-service/" ++ slug
        , expect = Http.expectJson GotOrgDetails orgDetailsDecoder
        }



-- UPDATE


type Msg
    = GotInitResponse (Result Http.Error InitResponse)
    | GotOrgDetails (Result Http.Error OrgDetails)
    | UpdateEmail String
    | UpdateFirstName String
    | UpdateLastName String
    | ToggleOptIn Bool
    | SubmitForm
    | GotSignupResponse (Result Http.Error ())


type alias InitResponse =
    { contact : Maybe Contact
    , email : Maybe String
    , emailReadOnly : Bool
    }


type alias OrgDetails =
    { orgId : String
    , orgSlug : String
    }


type alias Contact =
    { email : String
    , firstName : String
    , lastName : String
    , optInQuarterlyUpdates : Bool
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotOrgDetails result ->
            case result of
                Ok details ->
                    ( { model | orgId = Just details.orgId, error = Nothing }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Organization not found or invalid link." }
                    , Cmd.none
                    )

        GotInitResponse result ->
            case result of
                Ok response ->
                    let
                        email =
                            response.email |> Maybe.withDefault ""

                        contact =
                            response.contact

                        firstName =
                            contact |> Maybe.map .firstName |> Maybe.withDefault ""

                        lastName =
                            contact |> Maybe.map .lastName |> Maybe.withDefault ""

                        optIn =
                            contact |> Maybe.map .optInQuarterlyUpdates |> Maybe.withDefault False
                    in
                    ( { model
                        | email = email
                        , firstName = firstName
                        , lastName = lastName
                        , optInQuarterlyUpdates = optIn
                        , emailReadOnly = response.emailReadOnly
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to load existing contact details. Please try again." }
                    , Cmd.none
                    )

        UpdateEmail newEmail ->
            ( { model
                | email =
                    if model.emailReadOnly then
                        model.email

                    else
                        newEmail
              }
            , Cmd.none
            )

        UpdateFirstName newFirstName ->
            ( { model | firstName = newFirstName }, Cmd.none )

        UpdateLastName newLastName ->
            ( { model | lastName = newLastName }, Cmd.none )

        ToggleOptIn newValue ->
            ( { model | optInQuarterlyUpdates = newValue }, Cmd.none )

        SubmitForm ->
            if isFormValid model then
                ( { model | isSubmitting = True, error = Nothing }
                , submitForm model
                )

            else
                ( { model | error = Just "Please fill out all required fields" }
                , Cmd.none
                )

        GotSignupResponse result ->
            case result of
                Ok _ ->
                    ( { model | isSubmitting = False, success = True, error = Nothing }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | isSubmitting = False, error = Just "Signup failed. Please try again." }
                    , Cmd.none
                    )


isFormValid : Model -> Bool
isFormValid model =
    not (String.isEmpty model.email)
        && not (String.isEmpty model.firstName)
        && not (String.isEmpty model.lastName)
        && model.orgId
        /= Nothing



-- FORM SUBMISSION


submitForm : Model -> Cmd Msg
submitForm model =
    Http.post
        { url = "/api/self-service/signup"
        , body = Http.jsonBody (encodeForm model)
        , expect = Http.expectWhatever GotSignupResponse
        }


encodeForm : Model -> Encode.Value
encodeForm model =
    Encode.object
        [ ( "orgId", Encode.string (Maybe.withDefault "" model.orgId) )
        , ( "email", Encode.string model.email )
        , ( "firstName", Encode.string model.firstName )
        , ( "lastName", Encode.string model.lastName )
        , ( "optInQuarterlyUpdates", Encode.bool model.optInQuarterlyUpdates )
        ]



-- DECODERS


initResponseDecoder : Decoder InitResponse
initResponseDecoder =
    Decode.map3 InitResponse
        (Decode.maybe (Decode.field "contact" contactDecoder))
        (Decode.maybe (Decode.field "email" Decode.string))
        (Decode.field "emailReadOnly" Decode.bool)


contactDecoder : Decoder Contact
contactDecoder =
    Decode.map4 Contact
        (Decode.field "email" Decode.string)
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.field "optInQuarterlyUpdates" Decode.bool)


orgDetailsDecoder : Decoder OrgDetails
orgDetailsDecoder =
    Decode.map2 OrgDetails
        (Decode.field "orgId" Decode.string)
        (Decode.field "orgSlug" Decode.string)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Self-Service Onboarding"
    , body =
        [ div [ class "container mx-auto px-4 py-8" ]
            [ h1 [ class "text-2xl font-bold mb-6" ] [ text "Complete Your Profile" ]
            , viewForm model
            ]
        ]
    }


viewForm : Model -> Html Msg
viewForm model =
    div [ class "max-w-md mx-auto bg-white rounded-lg shadow-md p-6" ]
        [ if model.success then
            div [ class "p-4 mb-4 bg-green-100 border border-green-400 text-green-700 rounded" ]
                [ text "Profile updated successfully!" ]

          else if model.orgId == Nothing && model.error == Nothing then
            div [ class "p-4 mb-4 bg-yellow-100 border border-yellow-400 text-yellow-700 rounded" ]
                [ text "Loading organization details..." ]

          else
            div []
                [ inputField "Email" model.email UpdateEmail model.emailReadOnly
                , inputField "First Name" model.firstName UpdateFirstName False
                , inputField "Last Name" model.lastName UpdateLastName False
                , checkboxField "Receive quarterly Medicare updates" model.optInQuarterlyUpdates ToggleOptIn
                , viewError model.error
                , button
                    [ type_ "button"
                    , class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    , onClick SubmitForm
                    , disabled (model.isSubmitting || not (isFormValid model))
                    ]
                    [ text
                        (if model.isSubmitting then
                            "Submitting..."

                         else
                            "Submit"
                        )
                    ]
                ]
        ]


inputField : String -> String -> (String -> Msg) -> Bool -> Html Msg
inputField labelText value toMsg isDisabled =
    div [ class "mb-4" ]
        [ label [ class "block text-sm font-medium text-gray-700 mb-1" ] [ text labelText ]
        , input
            [ type_ "text"
            , class "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
            , Html.Attributes.value value
            , onInput toMsg
            , disabled isDisabled
            ]
            []
        ]


checkboxField : String -> Bool -> (Bool -> Msg) -> Html Msg
checkboxField labelText isChecked toMsg =
    div [ class "mb-6" ]
        [ label [ class "flex items-center" ]
            [ input
                [ type_ "checkbox"
                , class "h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
                , checked isChecked
                , onCheck toMsg
                ]
                []
            , span [ class "ml-2 text-sm text-gray-600" ] [ text labelText ]
            ]
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ class "mb-4 p-3 bg-red-100 border border-red-400 text-red-700 rounded" ]
                [ text error ]

        Nothing ->
            text ""



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = \_ url key -> init key url
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = \_ -> GotInitResponse (Err (Http.BadUrl "URL changed"))
        , onUrlRequest = \_ -> GotInitResponse (Err (Http.BadUrl "URL requested"))
        }
