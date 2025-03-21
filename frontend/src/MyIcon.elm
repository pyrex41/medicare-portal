module MyIcon exposing (..)

import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (..)


heartPulse : Int -> String -> Svg msg
heartPulse size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M19 14c1.49-1.46 3-3.21 3-5.5A5.5 5.5 0 0 0 16.5 3c-1.76 0-3 .5-4.5 2-1.5-1.5-2.74-2-4.5-2A5.5 5.5 0 0 0 2 8.5c0 2.3 1.5 4.05 3 5.5l7 7Z" ] []
        , Svg.path [ d "M3.22 12H9.5l.5-1 2 4.5 2-7 1.5 3.5h5.27" ] []
        ]


stethoscope : Int -> String -> Svg msg
stethoscope size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M11 2v2" ] []
        , Svg.path [ d "M5 2v2" ] []
        , Svg.path [ d "M5 3H4a2 2 0 0 0-2 2v4a6 6 0 0 0 12 0V5a2 2 0 0 0-2-2h-1" ] []
        , Svg.path [ d "M8 15a6 6 0 0 0 12 0v-3" ] []
        , Svg.circle [ cx "20", cy "10", r "2" ] []
        ]


syringe : Int -> String -> Svg msg
syringe size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "m18 2 4 4" ] []
        , Svg.path [ d "m17 7 3-3" ] []
        , Svg.path [ d "M19 9 8.7 19.3c-1 1-2.5 1-3.4 0l-.6-.6c-1-1-1-2.5 0-3.4L15 5" ] []
        , Svg.path [ d "m9 11 4 4" ] []
        , Svg.path [ d "m5 19-3 3" ] []
        , Svg.path [ d "m14 4 6 6" ] []
        ]


lungs : Int -> String -> Svg msg
lungs size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M10 10v.2A3 3 0 0 1 8.9 16H5a3 3 0 0 1-1-5.8V10a3 3 0 0 1 6 0Z" ] []
        , Svg.path [ d "M7 16v6" ] []
        , Svg.path [ d "M13 19v3" ] []
        , Svg.path [ d "M12 19h8.3a1 1 0 0 0 .7-1.7L18 14h.3a1 1 0 0 0 .7-1.7L16 9h.2a1 1 0 0 0 .8-1.7L13 3l-1.4 1.5" ] []
        ]


droplets : Int -> String -> Svg msg
droplets size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M7 16.3c2.2 0 4-1.83 4-4.05 0-1.16-.57-2.26-1.71-3.19S7.29 6.75 7 5.3c-.29 1.45-1.14 2.84-2.29 3.76S3 11.1 3 12.25c0 2.22 1.8 4.05 4 4.05z" ] []
        , Svg.path [ d "M12.56 6.6A10.97 10.97 0 0 0 14 3.02c.5 2.5 2 4.9 4 6.5s3 3.5 3 5.5a6.98 6.98 0 0 1-11.91 4.97" ] []
        ]


brain : Int -> String -> Svg msg
brain size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M12 5a3 3 0 1 0-5.997.125 4 4 0 0 0-2.526 5.77 4 4 0 0 0 .556 6.588A4 4 0 1 0 12 18Z" ] []
        , Svg.path [ d "M12 5a3 3 0 1 1 5.997.125 4 4 0 0 1 2.526 5.77 4 4 0 0 1-.556 6.588A4 4 0 1 1 12 18Z" ] []
        , Svg.path [ d "M15 13a4.5 4.5 0 0 1-3-4 4.5 4.5 0 0 1-3 4" ] []
        , Svg.path [ d "M17.599 6.5a3 3 0 0 0 .399-1.375" ] []
        , Svg.path [ d "M6.003 5.125A3 3 0 0 0 6.401 6.5" ] []
        , Svg.path [ d "M3.477 10.896a4 4 0 0 1 .585-.396" ] []
        , Svg.path [ d "M19.938 10.5a4 4 0 0 1 .585.396" ] []
        , Svg.path [ d "M6 18a4 4 0 0 1-1.967-.516" ] []
        , Svg.path [ d "M19.967 17.484A4 4 0 0 1 18 18" ] []
        ]


building2 : Int -> String -> Svg msg
building2 size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M6 22V4a2 2 0 0 1 2-2h8a2 2 0 0 1 2 2v18Z" ] []
        , Svg.path [ d "M6 12H4a2 2 0 0 0-2 2v6a2 2 0 0 0 2 2h2" ] []
        , Svg.path [ d "M18 9h2a2 2 0 0 1 2 2v9a2 2 0 0 1-2 2h-2" ] []
        , Svg.path [ d "M10 6h4" ] []
        , Svg.path [ d "M10 10h4" ] []
        , Svg.path [ d "M10 14h4" ] []
        , Svg.path [ d "M10 18h4" ] []
        ]


activity : Int -> String -> Svg msg
activity size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M22 12h-2.48a2 2 0 0 0-1.93 1.46l-2.35 8.36a.25.25 0 0 1-.48 0L9.24 2.18a.25.25 0 0 0-.48 0l-2.35 8.36A2 2 0 0 1 4.49 12H2" ] []
        ]


heartScan : Int -> String -> Svg msg
heartScan size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M11.246 16.657a1 1 0 0 0 1.508 0l3.57-4.101A2.75 2.75 0 1 0 12 9.168a2.75 2.75 0 1 0-4.324 3.388z" ] []
        , Svg.path [ d "M17 3h2a2 2 0 0 1 2 2v2" ] []
        , Svg.path [ d "M21 17v2a2 2 0 0 1-2 2h-2" ] []
        , Svg.path [ d "M3 7V5a2 2 0 0 1 2-2h2" ] []
        , Svg.path [ d "M7 21H5a2 2 0 0 1-2-2v-2" ] []
        ]


hospital : Int -> String -> Svg msg
hospital size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M12 6v4" ] []
        , Svg.path [ d "M14 14h-4" ] []
        , Svg.path [ d "M14 18h-4" ] []
        , Svg.path [ d "M14 8h-4" ] []
        , Svg.path [ d "M18 12h2a2 2 0 0 1 2 2v6a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2v-9a2 2 0 0 1 2-2h2" ] []
        , Svg.path [ d "M18 22V4a2 2 0 0 0-2-2H8a2 2 0 0 0-2 2v18" ] []
        ]


squareActivity : Int -> String -> Svg msg
squareActivity size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ rect [ width "18", height "18", x "3", y "3", rx "2" ] []
        , Svg.path [ d "M17 12h-2l-2 5-2-10-2 5H7" ] []
        ]
