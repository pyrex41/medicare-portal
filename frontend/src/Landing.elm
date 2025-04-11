module Landing exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Url.Builder as Builder


type alias Model =
    { quoteId : Maybe String
    }


type Msg
    = NavigateTo String


init : { quoteId : Maybe String } -> ( Model, Cmd Msg )
init flags =
    ( { quoteId = flags.quoteId }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo path ->
            ( model, Cmd.none )



-- Navigation will be handled by parent


view : Model -> Browser.Document Msg
view model =
    { title = "Thank You - Medicare Max"
    , body =
        [ div [ class "min-h-screen bg-gray-50 flex flex-col items-center justify-center px-4 sm:px-6 lg:px-8" ]
            [ div [ class "max-w-md w-full space-y-8 text-center" ]
                [ h1 [ class "text-3xl font-bold text-gray-900 mb-8" ]
                    [ text "Thank you for entering your information!" ]
                , p [ class "text-lg text-gray-600 mb-8" ]
                    [ text "You'll receive an email with a copy of your quote that you can click through, just like your clients will." ]
                , div [ class "flex items-center justify-center my-12" ]
                    [ div [ class "w-full max-w-[100px] h-[1px] bg-gray-300" ] [] ]
                , div [ class "mt-6" ]
                    [ case model.quoteId of
                        Just id ->
                            a
                                [ href (Builder.absolute [ "compare" ] [ Builder.string "id" id ])
                                , class "inline-flex items-center px-6 py-3 border border-transparent text-base font-medium rounded-md text-white bg-purple-600 hover:bg-purple-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-purple-500"
                                , onClick (NavigateTo (Builder.absolute [ "compare" ] [ Builder.string "id" id ]))
                                ]
                                [ text "View Quote on This Device" ]

                        Nothing ->
                            text ""
                    ]
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
