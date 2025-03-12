module Utils.RandomOrgName exposing (generateOrgName)

import Random


chars : List Char
chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890"
        |> String.toList


generateOrgName : Random.Generator String
generateOrgName =
    Random.list 10 (Random.int 0 (List.length chars - 1))
        |> Random.map
            (\indices ->
                indices
                    |> List.map
                        (\i ->
                            chars
                                |> List.drop i
                                |> List.head
                                |> Maybe.withDefault ' '
                        )
                    |> String.fromList
            )
