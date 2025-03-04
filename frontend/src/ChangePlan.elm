module ChangePlan exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import ChoosePlan
import Html exposing (Html, div, h1, nav)
import Html.Attributes exposing (class)
import Json.Decode as Decode


type alias Model =
    { choosePlanModel : ChoosePlan.Model
    }


type Msg
    = ChoosePlanMsg ChoosePlan.Msg


init : { key : Nav.Key, session : String, orgSlug : String } -> ( Model, Cmd Msg )
init { key, session, orgSlug } =
    let
        ( choosePlanModel, choosePlanCmd ) =
            ChoosePlan.init orgSlug session key True
    in
    ( { choosePlanModel = choosePlanModel }
    , Cmd.map ChoosePlanMsg choosePlanCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChoosePlanMsg choosePlanMsg ->
            let
                ( updatedChoosePlanModel, choosePlanCmd ) =
                    ChoosePlan.update choosePlanMsg model.choosePlanModel
            in
            ( { model | choosePlanModel = updatedChoosePlanModel }
            , Cmd.map ChoosePlanMsg choosePlanCmd
            )


view : Model -> Document Msg
view model =
    { title = "Change Plan - Medicare Max"
    , body =
        [ simpleHeader
        , div [ class "container mx-auto pt-16 pb-8" ]
            [ ChoosePlan.viewChangePlan model.choosePlanModel
                |> Html.map ChoosePlanMsg
            ]
        ]
    }


simpleHeader : Html Msg
simpleHeader =
    nav [ class "bg-white shadow fixed w-full z-10" ]
        [ div [ class "container mx-auto px-4 py-3 flex justify-between items-center" ]
            [ h1 [ class "text-xl font-semibold text-gray-900" ]
                [ Html.text "Medicare Max" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ChoosePlanMsg (ChoosePlan.subscriptions model.choosePlanModel)
