module Main exposing (main)

import Browser
import Color
import Html
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import View3d.Main as View3d
import View3d.Mesh as Mesh
import View3d.RendererCommon exposing (..)


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


square : Mesh.Mesh Vertex
square =
    Mesh.IndexedTriangles
        [ { position = vec3 0 0 0, normal = vec3 0 0 1 }
        , { position = vec3 1 0 0, normal = vec3 0 0 1 }
        , { position = vec3 1 1 0, normal = vec3 0 0 1 }
        , { position = vec3 0 1 0, normal = vec3 0 0 1 }
        ]
        [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]


meshes : List (Mesh.Mesh Vertex)
meshes =
    [ square ]


baseMaterial : Material
baseMaterial =
    { color = Color.red, roughness = 0.5, metallic = 0.1 }


instances : List Instance
instances =
    [ { material = { baseMaterial | color = Color.red }
      , transform = Mat4.identity
      , idxMesh = 0
      , idxInstance = 0
      }
    , { material = { baseMaterial | color = Color.green }
      , transform = Mat4.makeRotate (pi / 2) (vec3 1 0 0)
      , idxMesh = 0
      , idxInstance = 0
      }
    , { material = { baseMaterial | color = Color.blue }
      , transform = Mat4.makeRotate (pi / 2) (vec3 0 -1 0)
      , idxMesh = 0
      , idxInstance = 0
      }
    , { material = { baseMaterial | color = Color.rgb 0 1 1 }
      , transform = Mat4.makeTranslate (vec3 0 0 1)
      , idxMesh = 0
      , idxInstance = 0
      }
    , { material = { baseMaterial | color = Color.rgb 1 0 1 }
      , transform =
            Mat4.makeRotate (pi / 2) (vec3 1 0 0)
                |> Mat4.translate (vec3 0 0 -1)
      , idxMesh = 0
      , idxInstance = 0
      }
    , { material = { baseMaterial | color = Color.rgb 1 1 0 }
      , transform =
            Mat4.makeRotate (pi / 2) (vec3 0 -1 0)
                |> Mat4.translate (vec3 0 0 -1)
      , idxMesh = 0
      , idxInstance = 0
      }
    ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            View3d.init
                |> View3d.setSize { width = 400, height = 400 }
                |> View3d.setScene (Just meshes) instances
                |> View3d.encompass
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
