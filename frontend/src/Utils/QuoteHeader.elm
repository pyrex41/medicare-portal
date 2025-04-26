module Utils.QuoteHeader exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


viewHeader : Maybe String -> Maybe String -> Html msg
viewHeader maybeImage maybeName =
    div [ class "flex justify-center items-center mt-4 mb-6" ]
        [ case ( maybeImage, maybeName ) of
            ( Just logo, _ ) ->
                img [ src logo, alt "Organization Logo", class "h-16 max-w-[240px] md:max-w-[300px] object-contain px-4" ] []

            ( _, Just name ) ->
                div [ class "text-4xl font-bold text-[#101828] leading-[1.2]" ] [ text name ]

            _ ->
                text ""
        ]
