module Lucide exposing (alertCircle, brain, building2, droplets, heart, icon, lungs, stethoscope)

import Svg exposing (Svg, svg, use)
import Svg.Attributes exposing (class, viewBox, xlinkHref)


icon : String -> String -> Svg msg
icon name className =
    svg
        [ class className
        , viewBox "0 0 24 24"
        ]
        [ use [ xlinkHref ("#lucide-" ++ name) ] [] ]


heart : String -> Svg msg
heart =
    icon "heart"


lungs : String -> Svg msg
lungs =
    icon "lungs"



-- Using trees icon as a substitute for lungs


alertCircle : String -> Svg msg
alertCircle =
    icon "alert-circle"


droplets : String -> Svg msg
droplets =
    icon "droplets"


brain : String -> Svg msg
brain =
    icon "brain"


building2 : String -> Svg msg
building2 =
    icon "building-2"


stethoscope : String -> Svg msg
stethoscope =
    icon "stethoscope"
