module TutorialModal exposing (viewTutorialModal)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


viewTutorialModal : msg -> Html msg
viewTutorialModal closeTutorialModal =
    div [ class "fixed inset-0 z-50 bg-slate-100 bg-opacity-20 backdrop-blur-sm flex items-center justify-center p-4" ]
        [ div [ class "bg-white p-4 sm:p-6 rounded-lg shadow-lg max-w-2xl w-full" ]
            [ div [ class "flex justify-between items-center mb-4" ]
                [ h2 [ class "text-lg sm:text-xl font-semibold text-[#03045E]" ] [ text "Welcome to MedicareMax!" ]
                , button
                    [ class "text-gray-400 hover:text-gray-600 text-xl p-1", onClick closeTutorialModal ]
                    [ text "×" ]
                ]
            , div [ class "mb-4 sm:mb-6" ]
                [ img
                    [ src "images/tutorial.gif"
                    , class "w-full aspect-video max-h-[50vh] sm:h-96 object-contain rounded-md"
                    , alt "Tutorial demonstration showing how to add contacts"
                    ]
                    []
                ]
            , div [ class "mb-4 text-gray-600 text-sm sm:text-base" ]
                [ h3 [ class "text-lg font-semibold text-[#03045E] mb-3" ] [ text "Quick How-To" ]
                , h4 [ class "text-base font-medium text-gray-800 mb-2" ] [ text "How to Add Contacts" ]
                , p [ class "leading-relaxed py-2" ]
                    [ text "To add new contacts, simply click the \"Add Contacts\" button. You'll then be prompted to choose between two options: Individual Upload or Bulk Upload."
                    ]
                , p [ class "leading-relaxed py-2" ]
                    [ text "Select Individual Upload to enter contact details one at a time, or choose Bulk Upload to import a list of contacts using a CSV or Excel file. Follow the on-screen instructions for each method to complete the upload process."
                    ]
                ]
            , div [ class "flex justify-end" ]
                [ button
                    [ class "px-4 py-2 bg-[#03045E] text-white rounded-md hover:bg-opacity-90 w-full sm:w-auto"
                    , onClick closeTutorialModal
                    ]
                    [ text "Close" ]
                ]
            ]
        ]
