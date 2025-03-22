module Utils.QuoteHeader exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


viewHeader : Maybe String -> Maybe String -> Html msg
viewHeader maybeImage maybeName =
    div [ class "flex justify-center items-center mt-4 mb-2" ]
        [ case ( maybeImage, maybeName ) of
            ( Just logo, _ ) ->
                img [ src logo, alt "Organization Logo", class "h-24 max-w-[400px] object-contain" ] []

            ( _, Just name ) ->
                div [ class "text-2xl font-bold text-[#101828] leading-[1.2]" ] [ text name ]

            _ ->
                text ""
        ]
