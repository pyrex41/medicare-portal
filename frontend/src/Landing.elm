module Landing exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, a, br, div, h1, img, p, span, text)
import Html.Attributes exposing (class, href, src)
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
    { title = "Thanks for checking it out - Medicare Max"
    , body =
        [ div [ class "min-h-screen bg-white" ]
            [ div [ class "max-w-md mx-auto px-4 sm:px-6 lg:px-8 pt-12" ]
                [ div [ class "mb-16" ]
                    [ img
                        [ src "/images/medicare-max-logo.png"
                        , class "h-12"
                        ]
                        []
                    ]
                , div [ class "text-center" ]
                    [ div [ class "mb-8" ]
                        [ img
                            [ src "/images/party-popper.png"
                            , class "h-16 mx-auto"
                            ]
                            []
                        ]
                    , h1 [ class "text-[40px] leading-[48px] font-bold text-[#111111] mb-4" ]
                        [ text "Thanks for"
                        , br [] []
                        , text "checking it out."
                        ]
                    , p [ class "text-lg text-gray-600 mb-8" ]
                        [ text "Head to "
                        , span [ class "text-[#0A0F51] font-bold" ] [ text "your email" ]
                        , text " to see what"
                        , br [] []
                        , text "your clients will experience"
                        ]
                    , a
                        [ href "/"
                        , class "inline-flex items-center px-6 py-3 text-base font-medium rounded-md text-white bg-[#0A0F51] hover:bg-[#0A0F51]/90 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#0A0F51] mb-8"
                        , onClick (NavigateTo "/")
                        ]
                        [ text "Back to the Homepage" ]
                    , div [ class "text-gray-600 text-sm max-w-[200px] mx-auto" ]
                        [ text "Didn't receive an email yet - "
                        , case model.quoteId of
                            Just id ->
                                a
                                    [ href (Builder.absolute [ "compare" ] [ Builder.string "id" id ])
                                    , class "text-[#0A0F51] underline hover:text-[#0A0F51]/90"
                                    , onClick (NavigateTo (Builder.absolute [ "compare" ] [ Builder.string "id" id ]))
                                    ]
                                    [ text "click here" ]

                            Nothing ->
                                text ""
                        , text " for your quote"
                        ]
                    ]
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
