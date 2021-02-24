module Main exposing (main)

import Browser
import Color
import Html
import Math.Vector3 exposing (vec3)
import View3d.Main as View3d
import View3d.RendererCommon


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    {}


type alias Model =
    View3d.Model


type alias Msg =
    View3d.Msg


defaultOptions : View3d.RendererCommon.Options
defaultOptions =
    { orthogonalView = False
    , drawWires = False
    , fadeToBackground = 0.0
    , fadeToBlue = 0.0
    , backgroundColor = vec3 0.0 0.0 0.0
    , addOutlines = False
    , outlineWidth = 0.0
    , outlineColor = vec3 0.0 0.0 0.0
    , drawShadows = False
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            View3d.init
                |> View3d.setSize { width = 400, height = 400 }
    in
    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div [] [ View3d.view identity model defaultOptions Color.black ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, _ ) =
            View3d.update msg model
    in
    ( newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    View3d.subscriptions identity model
