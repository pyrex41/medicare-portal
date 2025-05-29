module ContactUs exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, form, h1, h2, h3, input, label, p, section, span, text, textarea)
import Html.Attributes exposing (class, for, href, id, name, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Encode as E
import MyIcon



-- MODEL


type alias Model =
    { name : String
    , email : String
    , phone : String
    , message : String
    , loading : Bool
    , submitStatus : SubmitStatus
    }


type SubmitStatus
    = NotSubmitted
    | Submitting
    | Success
    | Error String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , email = ""
      , phone = ""
      , message = ""
      , loading = False
      , submitStatus = NotSubmitted
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateName String
    | UpdateEmail String
    | UpdatePhone String
    | UpdateMessage String
    | SubmitForm
    | FormSubmitted (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        UpdatePhone phone ->
            ( { model | phone = phone }, Cmd.none )

        UpdateMessage message ->
            ( { model | message = message }, Cmd.none )

        SubmitForm ->
            if String.isEmpty model.name || String.isEmpty model.email || String.isEmpty model.message then
                ( { model | submitStatus = Error "Please fill in all required fields" }, Cmd.none )

            else
                ( { model | submitStatus = Submitting }
                , submitContactForm model
                )

        FormSubmitted result ->
            case result of
                Ok _ ->
                    ( { model
                        | submitStatus = Success
                        , name = ""
                        , email = ""
                        , phone = ""
                        , message = ""
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | submitStatus = Error "Failed to submit form. Please try again." }
                    , Cmd.none
                    )


submitContactForm : Model -> Cmd Msg
submitContactForm model =
    Http.post
        { url = "/api/contact-us"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "name", E.string model.name )
                    , ( "email", E.string model.email )
                    , ( "phone", E.string model.phone )
                    , ( "message", E.string model.message )
                    ]
                )
        , expect = Http.expectWhatever FormSubmitted
        }



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Contact Us - Medicare Max"
    , body = [ viewContent model ]
    }


viewContent : Model -> Html Msg
viewContent model =
    div [ class "min-h-screen bg-white" ]
        [ div [ class "max-w-7xl mx-auto py-12 px-4 sm:px-6 lg:px-8" ]
            [ div [ class "text-center mb-12" ]
                [ h1 [ class "text-4xl font-bold text-gray-900 mb-4" ]
                    [ text "Contact Us" ]
                , p [ class "text-lg text-gray-600" ]
                    [ text "We're here to help with all your Medicare needs" ]
                ]
            , div [ class "grid grid-cols-1 lg:grid-cols-2 gap-12" ]
                [ viewContactInfo
                , viewContactForm model
                ]
            ]
        ]


viewContactInfo : Html Msg
viewContactInfo =
    div [ class "space-y-8" ]
        [ div [ class "bg-white rounded-lg shadow-md p-8" ]
            [ h2 [ class "text-2xl font-semibold text-gray-900 mb-6" ]
                [ text "Get in Touch" ]
            , div [ class "space-y-6" ]
                [ viewContactItem (MyIcon.phone 20 "#6B7280") "Phone" "1-800-MEDICARE"
                , viewContactItem (MyIcon.mail 20 "#6B7280") "Email" "information@medicaremax.ai"
                , viewContactItem (MyIcon.mapPin 20 "#6B7280") "Address" "123 Medicare Lane, Suite 100\nHealthcare City, HC 12345"
                , viewContactItem (MyIcon.clock 20 "#6B7280") "Business Hours" "Monday - Friday: 8AM - 8PM\nSaturday: 9AM - 5PM\nSunday: Closed"
                ]
            ]
        , div [ class "bg-blue-50 rounded-lg p-6" ]
            [ h3 [ class "text-lg font-semibold text-blue-900 mb-3" ]
                [ text "Quick Help" ]
            , div [ class "space-y-2" ]
                [ a
                    [ href "/schedule-main"
                    , class "block text-blue-600 hover:text-blue-800 underline"
                    ]
                    [ text "Schedule a Demo" ]
                , a
                    [ href "/pricing"
                    , class "block text-blue-600 hover:text-blue-800 underline"
                    ]
                    [ text "View Pricing Plans" ]
                , a
                    [ href "/signup"
                    , class "block text-blue-600 hover:text-blue-800 underline"
                    ]
                    [ text "Start Free Trial" ]
                ]
            ]
        ]


viewContactItem : Html msg -> String -> String -> Html msg
viewContactItem icon title content =
    div [ class "flex items-start space-x-3" ]
        [ div [ class "flex-shrink-0 mt-1" ]
            [ icon
            ]
        , div []
            [ p [ class "font-medium text-gray-900" ]
                [ text title ]
            , p [ class "text-gray-600 whitespace-pre-line" ]
                [ text content ]
            ]
        ]


viewContactForm : Model -> Html Msg
viewContactForm model =
    div [ class "bg-white rounded-lg shadow-md p-8" ]
        [ h2 [ class "text-2xl font-semibold text-gray-900 mb-6" ]
            [ text "Send us a Message" ]
        , case model.submitStatus of
            Success ->
                div [ class "bg-green-50 border border-green-200 rounded-md p-4 mb-6" ]
                    [ p [ class "text-green-800" ]
                        [ text "Thank you for contacting us! We'll get back to you soon." ]
                    ]

            Error errorMsg ->
                div [ class "bg-red-50 border border-red-200 rounded-md p-4 mb-6" ]
                    [ p [ class "text-red-800" ]
                        [ text errorMsg ]
                    ]

            _ ->
                text ""
        , form [ onSubmit SubmitForm ]
            [ div [ class "space-y-6" ]
                [ div []
                    [ label
                        [ for "name"
                        , class "block text-sm font-medium text-gray-700 mb-1"
                        ]
                        [ text "Name *" ]
                    , input
                        [ type_ "text"
                        , id "name"
                        , name "name"
                        , value model.name
                        , onInput UpdateName
                        , placeholder "Your name"
                        , class "w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                        ]
                        []
                    ]
                , div []
                    [ label
                        [ for "email"
                        , class "block text-sm font-medium text-gray-700 mb-1"
                        ]
                        [ text "Email *" ]
                    , input
                        [ type_ "email"
                        , id "email"
                        , name "email"
                        , value model.email
                        , onInput UpdateEmail
                        , placeholder "your@email.com"
                        , class "w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                        ]
                        []
                    ]
                , div []
                    [ label
                        [ for "phone"
                        , class "block text-sm font-medium text-gray-700 mb-1"
                        ]
                        [ text "Phone (optional)" ]
                    , input
                        [ type_ "tel"
                        , id "phone"
                        , name "phone"
                        , value model.phone
                        , onInput UpdatePhone
                        , placeholder "(123) 456-7890"
                        , class "w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                        ]
                        []
                    ]
                , div []
                    [ label
                        [ for "message"
                        , class "block text-sm font-medium text-gray-700 mb-1"
                        ]
                        [ text "Message *" ]
                    , textarea
                        [ id "message"
                        , name "message"
                        , rows 5
                        , value model.message
                        , onInput UpdateMessage
                        , placeholder "How can we help you?"
                        , class "w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                        ]
                        []
                    ]
                , button
                    [ type_ "submit"
                    , class "w-full bg-blue-600 text-white py-3 px-6 rounded-md font-medium hover:bg-blue-700 transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed"
                    , Html.Attributes.disabled (model.submitStatus == Submitting)
                    ]
                    [ text
                        (if model.submitStatus == Submitting then
                            "Sending..."

                         else
                            "Send Message"
                        )
                    ]
                ]
            ]
        ]
