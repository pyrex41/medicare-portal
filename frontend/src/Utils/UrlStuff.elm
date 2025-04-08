module Utils.UrlStuff exposing (getQueryParams)

import Dict exposing (Dict)
import Url exposing (Url)


getQueryParams : Url -> Dict String String
getQueryParams url =
    url.query
        |> Maybe.map (\q -> String.split "&" q)
        |> Maybe.withDefault []
        |> List.filterMap
            (\param ->
                case String.split "=" param of
                    key :: value :: [] ->
                        Just ( key, value )

                    _ ->
                        Nothing
            )
        |> Dict.fromList
