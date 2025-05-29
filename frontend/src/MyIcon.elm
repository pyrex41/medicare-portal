module MyIcon exposing (..)

import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (..)


zapOff : Int -> String -> Svg msg
zapOff size color =
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
        [ Svg.path [ d "M10.513 4.856 13.12 2.17a.5.5 0 0 1 .86.46l-1.377 4.317" ] []
        , Svg.path [ d "M15.656 10H20a1 1 0 0 1 .78 1.63l-1.72 1.773" ] []
        , Svg.path [ d "M16.273 16.273 10.88 21.83a.5.5 0 0 1-.86-.46l1.92-6.02A1 1 0 0 0 11 14H4a1 1 0 0 1-.78-1.63l4.507-4.643" ] []
        , Svg.path [ d "m2 2 20 20" ] []
        ]


zap : Int -> String -> Svg msg
zap size color =
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
        [ Svg.path [ d "M4 14a1 1 0 0 1-.78-1.63l9.9-10.2a.5.5 0 0 1 .86.46l-1.92 6.02A1 1 0 0 0 13 10h7a1 1 0 0 1 .78 1.63l-9.9 10.2a.5.5 0 0 1-.86-.46l1.92-6.02A1 1 0 0 0 11 14z" ] []
        ]


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


clipboardPlus : Int -> String -> Svg msg
clipboardPlus size color =
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
        [ Svg.rect [ width "8", height "4", x "8", y "2", rx "1", ry "1" ] []
        , Svg.path [ d "M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2" ] []
        , Svg.path [ d "M9 14h6" ] []
        , Svg.path [ d "M12 17v-6" ] []
        ]


clipboardList : Int -> String -> Svg msg
clipboardList size color =
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
        [ Svg.rect [ width "8", height "4", x "8", y "2", rx "1", ry "1" ] []
        , Svg.path [ d "M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2" ] []
        , Svg.path [ d "M12 11h4" ] []
        , Svg.path [ d "M12 16h4" ] []
        , Svg.path [ d "M8 11h.01" ] []
        , Svg.path [ d "M8 16h.01" ] []
        ]


users : Int -> String -> Svg msg
users size color =
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
        [ Svg.path [ d "M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "9", cy "7", r "4" ] []
        , Svg.path [ d "M22 21v-2a4 4 0 0 0-3-3.87" ] []
        , Svg.path [ d "M16 3.13a4 4 0 0 1 0 7.75" ] []
        ]


brand : Int -> String -> Svg msg
brand size color =
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
        [ Svg.rect [ width "3", height "8", x "13", y "2", rx "1.5" ] []
        , Svg.path [ d "M19 8.5V10h1.5A1.5 1.5 0 1 0 19 8.5" ] []
        , Svg.rect [ width "3", height "8", x "8", y "14", rx "1.5" ] []
        , Svg.path [ d "M5 15.5V14H3.5A1.5 1.5 0 1 0 5 15.5" ] []
        , Svg.rect [ width "8", height "3", x "14", y "13", rx "1.5" ] []
        , Svg.path [ d "M15.5 19H14v1.5a1.5 1.5 0 1 0 1.5-1.5" ] []
        , Svg.rect [ width "8", height "3", x "2", y "8", rx "1.5" ] []
        , Svg.path [ d "M8.5 5H10V3.5A1.5 1.5 0 1 0 8.5 5" ] []
        ]


undo2 size color =
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
        [ Svg.path [ d "M9 14 4 9l5-5" ] []
        , Svg.path [ d "M4 9h10.5a5.5 5.5 0 0 1 5.5 5.5a5.5 5.5 0 0 1-5.5 5.5H11" ] []
        ]


calendarDays : Int -> String -> Svg msg
calendarDays size color =
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
        [ Svg.path [ d "M8 2v4" ] []
        , Svg.path [ d "M16 2v4" ] []
        , Svg.rect [ width "18", height "18", x "3", y "4", rx "2" ] []
        , Svg.path [ d "M3 10h18" ] []
        , Svg.path [ d "M8 14h.01" ] []
        , Svg.path [ d "M12 14h.01" ] []
        , Svg.path [ d "M16 14h.01" ] []
        , Svg.path [ d "M8 18h.01" ] []
        , Svg.path [ d "M12 18h.01" ] []
        , Svg.path [ d "M16 18h.01" ] []
        ]


phoneOutgoing : Int -> String -> Svg msg
phoneOutgoing size color =
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
        [ Svg.polyline [ points "22 8 22 2 16 2" ] []
        , Svg.line [ x1 "16", x2 "22", y1 "8", y2 "2" ] []
        , Svg.path [ d "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
        ]


phoneIncoming : Int -> String -> Svg msg
phoneIncoming size color =
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
        [ Svg.polyline [ points "16 2 16 8 22 8" ] []
        , Svg.line [ x1 "22", x2 "16", y1 "2", y2 "8" ] []
        , Svg.path [ d "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
        ]


clipboardPaste : Int -> String -> Svg msg
clipboardPaste size color =
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
        [ Svg.path [ d "M15 2H9a1 1 0 0 0-1 1v2c0 .6.4 1 1 1h6c.6 0 1-.4 1-1V3c0-.6-.4-1-1-1Z" ] []
        , Svg.path [ d "M8 4H6a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2M16 4h2a2 2 0 0 1 2 2v2M11 14h10" ] []
        , Svg.path [ d "m17 10 4 4-4 4" ] []
        ]



-- home page


lightning : Int -> String -> Svg msg
lightning size color =
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
        [ Svg.path [ d "M13 2L4.09347 12.6879C3.74466 13.1064 3.57026 13.3157 3.56759 13.4925C3.56528 13.6461 3.63375 13.7923 3.75327 13.8889C3.89076 14 4.16319 14 4.70805 14H12L11 22L19.9066 11.3121C20.2554 10.8936 20.4298 10.6843 20.4324 10.5075C20.4348 10.3539 20.3663 10.2077 20.2468 10.1111C20.1093 10 19.8368 10 19.292 10H12L13 2Z" ] []
        ]


envelope : Int -> String -> Svg msg
envelope size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 25 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M2.33334 7L10.4983 12.7154C11.1594 13.1783 11.49 13.4097 11.8496 13.4993C12.1672 13.5785 12.4994 13.5785 12.8171 13.4993C13.1767 13.4097 13.5073 13.1783 14.1684 12.7154L22.3333 7M7.13334 20H17.5333C19.2135 20 20.0536 20 20.6953 19.673C21.2598 19.3854 21.7187 18.9265 22.0064 18.362C22.3333 17.7202 22.3333 16.8802 22.3333 15.2V8.8C22.3333 7.11984 22.3333 6.27976 22.0064 5.63803C21.7187 5.07354 21.2598 4.6146 20.6953 4.32698C20.0536 4 19.2135 4 17.5333 4H7.13334C5.45319 4 4.61311 4 3.97137 4.32698C3.40689 4.6146 2.94794 5.07354 2.66032 5.63803C2.33334 6.27976 2.33334 7.11984 2.33334 8.8V15.2C2.33334 16.8802 2.33334 17.7202 2.66032 18.362C2.94794 18.9265 3.40689 19.3854 3.97137 19.673C4.61311 20 5.45319 20 7.13334 20Z" ] []
        ]


brightArrow : Int -> String -> Svg msg
brightArrow size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 25 24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M11.6666 3H8.46663C6.78647 3 5.94639 3 5.30465 3.32698C4.74017 3.6146 4.28123 4.07354 3.99361 4.63803C3.66663 5.27976 3.66663 6.11984 3.66663 7.8V16.2C3.66663 17.8802 3.66663 18.7202 3.99361 19.362C4.28123 19.9265 4.74017 20.3854 5.30465 20.673C5.94639 21 6.78647 21 8.46663 21H16.8666C18.5468 21 19.3869 21 20.0286 20.673C20.5931 20.3854 21.052 19.9265 21.3396 19.362C21.6666 18.7202 21.6666 17.8802 21.6666 16.2V13M12.6666 8H16.6666V12M16.1666 3.5V2M20.106 4.56066L21.1666 3.5M21.1769 8.5H22.6769M3.66663 13.3471C4.31857 13.4478 4.9865 13.5 5.66663 13.5C10.053 13.5 13.932 11.3276 16.2863 8" ] []
        ]


chatBubbles : Int -> String -> Svg msg
chatBubbles size color =
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
        [ Svg.path [ d "M6.09436 11.2288C6.03221 10.8282 5.99996 10.4179 5.99996 10C5.99996 5.58172 9.60525 2 14.0526 2C18.4999 2 22.1052 5.58172 22.1052 10C22.1052 10.9981 21.9213 11.9535 21.5852 12.8345C21.5154 13.0175 21.4804 13.109 21.4646 13.1804C21.4489 13.2512 21.4428 13.301 21.4411 13.3735C21.4394 13.4466 21.4493 13.5272 21.4692 13.6883L21.8717 16.9585C21.9153 17.3125 21.9371 17.4895 21.8782 17.6182C21.8266 17.731 21.735 17.8205 21.6211 17.8695C21.4911 17.9254 21.3146 17.8995 20.9617 17.8478L17.7765 17.3809C17.6101 17.3565 17.527 17.3443 17.4512 17.3448C17.3763 17.3452 17.3245 17.3507 17.2511 17.3661C17.177 17.3817 17.0823 17.4172 16.893 17.4881C16.0097 17.819 15.0524 18 14.0526 18C13.6344 18 13.2237 17.9683 12.8227 17.9073M7.63158 22C10.5965 22 13 19.5376 13 16.5C13 13.4624 10.5965 11 7.63158 11C4.66668 11 2.26316 13.4624 2.26316 16.5C2.26316 17.1106 2.36028 17.6979 2.53955 18.2467C2.61533 18.4787 2.65322 18.5947 2.66566 18.6739C2.67864 18.7567 2.68091 18.8031 2.67608 18.8867C2.67145 18.9668 2.65141 19.0573 2.61134 19.2383L2 22L4.9948 21.591C5.15827 21.5687 5.24 21.5575 5.31137 21.558C5.38652 21.5585 5.42641 21.5626 5.50011 21.5773C5.5701 21.5912 5.67416 21.6279 5.88227 21.7014C6.43059 21.8949 7.01911 22 7.63158 22Z" ] []
        ]


smilieyChat : Int -> String -> Svg msg
smilieyChat size color =
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
        [ Svg.path [ d "M8.99962 14C8.99962 14 10.3121 15.5 12.4996 15.5C14.6871 15.5 15.9996 14 15.9996 14M15.2496 9H15.2596M9.74962 9H9.75962M12.4996 20C17.194 20 20.9996 16.1944 20.9996 11.5C20.9996 6.80558 17.194 3 12.4996 3C7.8052 3 3.99962 6.80558 3.99962 11.5C3.99962 12.45 4.15547 13.3636 4.443 14.2166C4.55119 14.5376 4.60529 14.6981 4.61505 14.8214C4.62469 14.9432 4.6174 15.0286 4.58728 15.1469C4.55677 15.2668 4.48942 15.3915 4.35472 15.6408L2.71906 18.6684C2.48575 19.1002 2.36909 19.3161 2.3952 19.4828C2.41794 19.6279 2.50337 19.7557 2.6288 19.8322C2.7728 19.9201 3.01692 19.8948 3.50517 19.8444L8.62619 19.315C8.78127 19.299 8.85881 19.291 8.92949 19.2937C8.999 19.2963 9.04807 19.3029 9.11586 19.3185C9.18478 19.3344 9.27145 19.3678 9.44478 19.4345C10.3928 19.7998 11.4228 20 12.4996 20ZM15.7496 9C15.7496 9.27614 15.5258 9.5 15.2496 9.5C14.9735 9.5 14.7496 9.27614 14.7496 9C14.7496 8.72386 14.9735 8.5 15.2496 8.5C15.5258 8.5 15.7496 8.72386 15.7496 9ZM10.2496 9C10.2496 9.27614 10.0258 9.5 9.74962 9.5C9.47348 9.5 9.24962 9.27614 9.24962 9C9.24962 8.72386 9.47348 8.5 9.74962 8.5C10.0258 8.5 10.2496 8.72386 10.2496 9Z" ] []
        ]


commandKey : Int -> String -> Svg msg
commandKey size color =
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
        [ Svg.path [ d "M9 9V6C9 4.34315 7.65685 3 6 3C4.34315 3 3 4.34315 3 6C3 7.65685 4.34315 9 6 9H9ZM9 9V15M9 9H15M9 15V18C9 19.6569 7.65685 21 6 21C4.34315 21 3 19.6569 3 18C3 16.3431 4.34315 15 6 15H9ZM9 15H15M15 15H18C19.6569 15 21 16.3431 21 18C21 19.6569 19.6569 21 18 21C16.3431 21 15 19.6569 15 18V15ZM15 15V9M15 9V6C15 4.34315 16.3431 3 18 3C19.6569 3 21 4.34315 21 6C21 7.65685 19.6569 9 18 9H15Z" ] []
        ]


heartBubble : Int -> String -> Svg msg
heartBubble size color =
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
        [ Svg.path [ d "M20.9996 11.5C20.9996 16.1944 17.194 20 12.4996 20C11.4228 20 10.3928 19.7998 9.44478 19.4345C9.27145 19.3678 9.18478 19.3344 9.11586 19.3185C9.04807 19.3029 8.999 19.2963 8.92949 19.2937C8.85881 19.291 8.78127 19.299 8.62619 19.315L3.50517 19.8444C3.01692 19.8948 2.7728 19.9201 2.6288 19.8322C2.50337 19.7557 2.41794 19.6279 2.3952 19.4828C2.36909 19.3161 2.48575 19.1002 2.71906 18.6684L4.35472 15.6408C4.48942 15.3915 4.55677 15.2668 4.58728 15.1469C4.6174 15.0286 4.62469 14.9432 4.61505 14.8214C4.60529 14.6981 4.55119 14.5376 4.443 14.2166C4.15547 13.3636 3.99962 12.45 3.99962 11.5C3.99962 6.80558 7.8052 3 12.4996 3C17.194 3 20.9996 6.80558 20.9996 11.5Z" ] []
        , Svg.path [ d "M12.4965 8.94925C11.5968 7.9104 10.0965 7.63095 8.96924 8.58223C7.84196 9.5335 7.68326 11.124 8.56851 12.2491C9.11696 12.9461 10.4935 14.2191 11.4616 15.087C11.8172 15.4057 11.995 15.5651 12.2084 15.6293C12.3914 15.6844 12.6017 15.6844 12.7847 15.6293C12.9981 15.5651 13.1759 15.4057 13.5315 15.087C14.4996 14.2191 15.8761 12.9461 16.4246 12.2491C17.3098 11.124 17.1705 9.5235 16.0238 8.58223C14.8772 7.64096 13.3963 7.9104 12.4965 8.94925Z" ] []
        ]


circleMinus : Int -> String -> Svg msg
circleMinus size color =
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
        [ Svg.path [ d "M8 12H16M22 12C22 17.5228 17.5228 22 12 22C6.47715 22 2 17.5228 2 12C2 6.47715 6.47715 2 12 2C17.5228 2 22 6.47715 22 12Z" ] []
        ]


circlePlus : Int -> String -> Svg msg
circlePlus size color =
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
        [ Svg.path [ d "M12 10V18M8 14H16M22 14C22 19.5228 17.5228 24 12 24C6.47715 24 2 19.5228 2 14C2 8.47715 6.47715 4 12 4C17.5228 4 22 8.47715 22 14Z" ] []
        ]


maxLogo : Int -> String -> Svg msg
maxLogo size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 246 34"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M28.0608 2.05762C27.5512 1.90329 27.0107 1.82031 26.4509 1.82031H11.6131L21.1611 19.295L28.0608 2.05762Z", fill "white" ] []
        , Svg.path [ d "M5.27168 1.82713C2.33588 1.97181 0 4.3998 0 7.37378V18.0748C0.63094 18.0398 1.35488 18.0179 2.21965 18.0179C2.95641 18.0179 3.383 17.8859 3.63477 17.7535C3.86366 17.6332 4.0932 17.4384 4.3236 17.0378C4.86375 16.0985 5.27168 14.2768 5.27168 10.9835V1.82713Z", fill "white" ] []
        , Svg.path [ d "M0 23.6386V26.6258C0 29.6929 2.48443 32.1793 5.54913 32.1793H21.8764L10.8108 11.9272C10.7435 15.0062 10.3224 17.7398 9.13305 19.808C8.43859 21.0155 7.47738 22.0066 6.21494 22.6702C4.97538 23.3217 3.61007 23.5714 2.21965 23.5714C1.31899 23.5714 0.609185 23.5976 0 23.6386Z", fill "white" ] []
        , Svg.path [ d "M21.9817 32.1793H26.4509C29.5156 32.1793 32 29.6929 32 26.6258V7.37378C32 7.30239 31.9987 7.23132 31.996 7.16059L21.9817 32.1793Z", fill "white" ] []
        , Svg.path [ d "M42.24 29V9.008H48.652L53.104 22.364H53.188L57.64 9.008H64.08V29H59.376V17.296H59.236C59.068 17.968 58.844 18.808 58.48 19.816L55.288 29H51.06L47.868 19.816C47.504 18.808 47.252 17.968 47.112 17.296H46.972V29H42.24ZM75.46 29.42C70.616 29.42 66.892 25.724 66.892 21.132C66.892 16.568 70.616 12.984 75.46 12.984C80.332 12.984 84.028 16.792 84.028 21.524C84.028 21.86 84 22.448 83.972 22.756H71.932C72.184 24.352 73.668 24.94 75.516 24.94C76.832 24.94 77.924 24.632 78.428 23.596H83.72C82.712 27.068 79.52 29.42 75.46 29.42ZM72.1 19.536H78.736C78.54 18.668 77.308 17.464 75.376 17.464C73.444 17.464 72.24 18.5 72.1 19.536ZM92.6798 29.42C88.6198 29.42 85.5678 25.78 85.5678 21.216C85.5678 16.624 88.6198 12.984 92.6798 12.984C95.0598 12.984 97.1038 14.16 97.9718 15.812H98.1118V9.008H102.928V29H98.2518V26.76H98.1118C96.6838 28.832 94.7798 29.42 92.6798 29.42ZM94.0798 24.94C96.2638 24.94 97.5518 23.4 97.5518 21.216C97.5518 19.004 96.2638 17.464 94.0798 17.464C91.8678 17.464 90.6918 19.004 90.6918 21.216C90.6918 23.4 91.8678 24.94 94.0798 24.94ZM108.923 11.36C107.439 11.36 106.235 10.156 106.235 8.672C106.235 7.216 107.439 6.012 108.923 6.012C110.379 6.012 111.583 7.216 111.583 8.672C111.583 10.156 110.379 11.36 108.923 11.36ZM106.515 28.944V13.404H111.331V28.944H106.515ZM121.965 29.364C117.401 29.364 113.677 25.668 113.677 21.076C113.677 16.512 117.401 12.816 121.965 12.816C124.345 12.816 126.501 13.824 128.013 15.42L124.905 18.556C124.261 17.8 123.309 17.324 121.965 17.324C120.033 17.324 118.493 18.892 118.493 21.076C118.493 23.288 120.033 24.856 121.965 24.856C123.197 24.856 124.065 24.464 124.709 23.82L127.817 26.956C126.305 28.468 124.261 29.364 121.965 29.364ZM135.534 29.42C131.222 29.42 128.142 25.78 128.142 21.216C128.142 16.624 131.222 12.984 135.534 12.984C137.942 12.984 139.846 14.16 140.714 15.812H140.854V13.404H145.53V29H140.854V26.76H140.714C139.286 28.832 137.662 29.42 135.534 29.42ZM136.654 24.94C138.866 24.94 140.154 23.4 140.154 21.216C140.154 19.004 138.866 17.464 136.654 17.464C134.47 17.464 133.266 19.004 133.266 21.216C133.266 23.4 134.47 24.94 136.654 24.94ZM149.144 29V13.404H158.524V17.884H153.96V29H149.144ZM167.937 29.42C163.093 29.42 159.369 25.724 159.369 21.132C159.369 16.568 163.093 12.984 167.937 12.984C172.809 12.984 176.505 16.792 176.505 21.524C176.505 21.86 176.477 22.448 176.449 22.756H164.409C164.661 24.352 166.145 24.94 167.993 24.94C169.309 24.94 170.401 24.632 170.905 23.596H176.197C175.189 27.068 171.997 29.42 167.937 29.42ZM164.577 19.536H171.213C171.017 18.668 169.785 17.464 167.853 17.464C165.921 17.464 164.717 18.5 164.577 19.536ZM185.029 29V9.008H191.441L195.893 22.364H195.977L200.429 9.008H206.869V29H202.165V17.296H202.025C201.857 17.968 201.633 18.808 201.269 19.816L198.077 29H193.849L190.657 19.816C190.293 18.808 190.041 17.968 189.901 17.296H189.761V29H185.029ZM217.073 29.42C212.761 29.42 209.681 25.78 209.681 21.216C209.681 16.624 212.761 12.984 217.073 12.984C219.481 12.984 221.385 14.16 222.253 15.812H222.393V13.404H227.069V29H222.393V26.76H222.253C220.825 28.832 219.201 29.42 217.073 29.42ZM218.193 24.94C220.405 24.94 221.693 23.4 221.693 21.216C221.693 19.004 220.405 17.464 218.193 17.464C216.009 17.464 214.805 19.004 214.805 21.216C214.805 23.4 216.009 24.94 218.193 24.94ZM228.611 29L234.071 20.992L228.835 13.404H234.575L236.703 17.044L238.831 13.404H244.571L239.335 20.992L244.795 29H239.111L236.703 25.024L234.323 29H228.611Z", fill "white" ] []
        ]


chevronDown : Int -> String -> Svg msg
chevronDown size color =
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
        [ Svg.path [ d "m6 9 6 6 6-6" ] []
        ]


chevronLeft : Int -> String -> Svg msg
chevronLeft size color =
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
        [ Svg.path [ d "m15 18-6-6 6-6" ] []
        ]


chevronRight : Int -> String -> Svg msg
chevronRight size color =
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
        [ Svg.path [ d "m9 18 6-6-6-6" ] []
        ]


banknote : Int -> String -> Svg msg
banknote size color =
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
        [ Svg.rect [ x "2", y "6", width "20", height "12", rx "2" ] []
        , Svg.circle [ cx "12", cy "12", r "2" ] []
        , Svg.path [ d "M6 12h.01M18 12h.01" ] []
        ]


purpleIcon : Int -> String -> Svg msg
purpleIcon size color =
    svg
        [ width (String.fromInt size)
        , height (String.fromInt size)
        , viewBox "0 0 22 22"
        , fill "none"
        ]
        [ Svg.path
            [ d "M11 21C16.5228 21 21 16.5228 21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21Z"
            , fill color
            ]
            []
        , Svg.path
            [ d "M11 15V11M11 7H11.01M21 11C21 16.5228 16.5228 21 11 21C5.47715 21 1 16.5228 1 11C1 5.47715 5.47715 1 11 1C16.5228 1 21 5.47715 21 11Z"
            , stroke "#F9F5FF"
            , strokeWidth "2"
            , strokeLinecap "round"
            , strokeLinejoin "round"
            ]
            []
        ]


phone : Int -> String -> Svg msg
phone size color =
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
        [ Svg.path [ d "M13.832 16.568a1 1 0 0 0 1.213-.303l.355-.465A2 2 0 0 1 17 15h3a2 2 0 0 1 2 2v3a2 2 0 0 1-2 2A18 18 0 0 1 2 4a2 2 0 0 1 2-2h3a2 2 0 0 1 2 2v3a2 2 0 0 1-.8 1.6l-.468.351a1 1 0 0 0-.292 1.233 14 14 0 0 0 6.392 6.384" ] []
        ]


mapPin : Int -> String -> Svg msg
mapPin size color =
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
        [ Svg.path [ d "M18 8c0 3.613-3.869 7.429-5.393 8.795a1 1 0 0 1-1.214 0C9.87 15.429 6 11.613 6 8a6 6 0 0 1 12 0" ] []
        , Svg.circle [ cx "12", cy "8", r "2" ] []
        , Svg.path [ d "M8.714 14h-3.71a1 1 0 0 0-.948.683l-2.004 6A1 1 0 0 0 3 22h18a1 1 0 0 0 .948-1.316l-2-6a1 1 0 0 0-.949-.684h-3.712" ] []
        ]


clock : Int -> String -> Svg msg
clock size color =
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
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polyline [ points "12 6 12 12 16 14" ] []
        ]


mail size color =
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
        [ Svg.path [ d "m22 7-8.991 5.727a2 2 0 0 1-2.009 0L2 7" ] []
        , Svg.rect [ x "2", y "4", width "20", height "16", rx "2" ] []
        ]
