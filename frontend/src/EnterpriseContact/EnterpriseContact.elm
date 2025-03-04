module EnterpriseContact.EnterpriseContact exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (Html, a, button, div, h1, h2, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (class, href, id, placeholder, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Encode as Encode


type alias Model =
    { name : String
    , email : String
    , phone : String
    , company : String
    , employees : String
    , message : String
    , isSubmitting : Bool
    , submitResult : Maybe (Result Http.Error ())
    }


type Msg
    = UpdateName String
    | UpdateEmail String
    | UpdatePhone String
    | UpdateCompany String
    | UpdateEmployees String
    | UpdateMessage String
    | SubmitForm
    | GotSubmitResponse (Result Http.Error ())


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , email = ""
      , phone = ""
      , company = ""
      , employees = ""
      , message = ""
      , isSubmitting = False
      , submitResult = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName value ->
            ( { model | name = value }, Cmd.none )

        UpdateEmail value ->
            ( { model | email = value }, Cmd.none )

        UpdatePhone value ->
            ( { model | phone = value }, Cmd.none )

        UpdateCompany value ->
            ( { model | company = value }, Cmd.none )

        UpdateEmployees value ->
            ( { model | employees = value }, Cmd.none )

        UpdateMessage value ->
            ( { model | message = value }, Cmd.none )

        SubmitForm ->
            ( { model | isSubmitting = True }
            , submitEnterpriseRequest model
            )

        GotSubmitResponse result ->
            ( { model | isSubmitting = False, submitResult = Just result }
            , Cmd.none
            )


submitEnterpriseRequest : Model -> Cmd Msg
submitEnterpriseRequest model =
    let
        encodedFormData =
            Encode.object
                [ ( "name", Encode.string model.name )
                , ( "email", Encode.string model.email )
                , ( "phone", Encode.string model.phone )
                , ( "company", Encode.string model.company )
                , ( "companySize", Encode.string model.employees )
                , ( "message", Encode.string model.message )
                ]
    in
    Http.post
        { url = "/api/organizations/enterprise-contact"
        , body = Http.jsonBody encodedFormData
        , expect = Http.expectWhatever GotSubmitResponse
        }


view : Model -> Browser.Document Msg
view model =
    { title = "Enterprise Contact - Medicare Max"
    , body =
        [ div [ class "container mx-auto py-8 px-4 max-w-3xl" ]
            [ div [ class "space-y-8" ]
                [ div [ class "mb-8 text-center" ]
                    [ h1 [ class "text-3xl font-semibold text-gray-900" ]
                        [ text "Enterprise Plan Inquiry" ]
                    , p [ class "text-gray-600 mt-2" ]
                        [ text "Complete the form below to get in touch with our sales team about the Enterprise plan." ]
                    ]
                , if isSuccess model then
                    viewSuccess

                  else
                    viewForm model
                ]
            ]
        ]
    }


isSuccess : Model -> Bool
isSuccess model =
    case model.submitResult of
        Just (Ok _) ->
            True

        _ ->
            False


viewSuccess : Html Msg
viewSuccess =
    div [ class "p-8 bg-green-50 rounded-lg border border-green-200 text-center" ]
        [ div [ class "text-green-600 text-5xl mb-4" ]
            [ text "✓" ]
        , h2 [ class "text-2xl font-semibold text-gray-900 mb-4" ]
            [ text "Thank You!" ]
        , p [ class "text-gray-600 mb-6" ]
            [ text "We've received your Enterprise plan inquiry. Our sales team will contact you within 1 business day to discuss your needs." ]
        , a
            [ href "/dashboard"
            , class "inline-block px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            ]
            [ text "Return to Dashboard" ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ onSubmit SubmitForm, class "space-y-6" ]
        [ div [ class "grid grid-cols-1 md:grid-cols-2 gap-6" ]
            [ formField "Full Name" "name" "text" model.name UpdateName True
            , formField "Email" "email" "email" model.email UpdateEmail True
            , formField "Phone" "phone" "tel" model.phone UpdatePhone True
            , formField "Company" "company" "text" model.company UpdateCompany True
            , div [ class "col-span-1" ]
                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                    [ text "Company Size" ]
                , select
                    [ class "w-full p-2 border border-gray-300 rounded-md"
                    , value model.employees
                    , onInput UpdateEmployees
                    ]
                    [ option [ value "" ] [ text "Select company size" ]
                    , option [ value "1-10" ] [ text "1-10 employees" ]
                    , option [ value "11-50" ] [ text "11-50 employees" ]
                    , option [ value "51-200" ] [ text "51-200 employees" ]
                    , option [ value "201-500" ] [ text "201-500 employees" ]
                    , option [ value "501+" ] [ text "501+ employees" ]
                    ]
                ]
            ]
        , div [ class "col-span-2" ]
            [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                [ text "Message" ]
            , textarea
                [ class "w-full p-2 border border-gray-300 rounded-md h-32"
                , value model.message
                , onInput UpdateMessage
                , placeholder "Tell us about your specific requirements and how we can help you"
                ]
                []
            ]
        , if model.isSubmitting then
            div [ class "flex justify-center my-6" ]
                [ div [ class "animate-spin rounded-full h-8 w-8 border-t-2 border-b-2 border-blue-500" ] [] ]

          else
            div [ class "flex justify-center mt-8" ]
                [ button
                    [ type_ "submit"
                    , class "px-8 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
                    ]
                    [ text "Submit Request" ]
                ]
        , case model.submitResult of
            Just (Err _) ->
                div [ class "text-red-500 text-center" ]
                    [ text "There was an error submitting your request. Please try again." ]

            _ ->
                text ""
        ]


formField : String -> String -> String -> String -> (String -> Msg) -> Bool -> Html Msg
formField labelText idValue inputType value toMsg required =
    div []
        [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
            [ text labelText
            , if required then
                span [ class "text-red-500 ml-1" ] [ text "*" ]

              else
                text ""
            ]
        , input
            [ type_ inputType
            , id idValue
            , class "w-full p-2 border border-gray-300 rounded-md"
            , Html.Attributes.value value
            , onInput toMsg
            , Html.Attributes.required required
            ]
            []
        ]
