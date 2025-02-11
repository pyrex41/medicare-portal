module StateRegions exposing (Region(..), allRegions, getRegionStates, regionToString, stringToRegion)


type Region
    = WestCoast
    | EastCoast
    | South
    | Midwest


allRegions : List Region
allRegions =
    [ WestCoast, EastCoast, South, Midwest ]


regionToString : Region -> String
regionToString region =
    case region of
        WestCoast ->
            "West Coast"

        EastCoast ->
            "East Coast"

        South ->
            "South"

        Midwest ->
            "Midwest"


stringToRegion : String -> Maybe Region
stringToRegion str =
    case str of
        "west" ->
            Just WestCoast

        "east" ->
            Just EastCoast

        "south" ->
            Just South

        "midwest" ->
            Just Midwest

        _ ->
            Nothing


getRegionStates : Region -> List String
getRegionStates region =
    case region of
        WestCoast ->
            [ "CA", "OR", "WA", "AK", "HI" ]

        EastCoast ->
            [ "ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "DE", "MD", "DC" ]

        South ->
            [ "VA", "NC", "SC", "GA", "FL", "AL", "MS", "LA", "AR", "TN", "KY", "WV", "TX", "OK" ]

        Midwest ->
            [ "OH", "MI", "IN", "IL", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS", "MT", "WY", "CO", "ID", "NV", "NM", "AZ", "UT" ]
