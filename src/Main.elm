module Main exposing (main)

import Browser
import Color
import Html
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
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


options : View3d.RendererCommon.Options
options =
    { orthogonalView = False
    , drawWires = False
    , fadeToBackground = 0.4
    , fadeToBlue = 0.1
    , backgroundColor = vec3 0 0 0
    , addOutlines = False
    , outlineWidth = 0.0
    , outlineColor = vec3 0 0 0
    , drawShadows = True
    }


triangulate : List vertex -> List ( vertex, vertex, vertex )
triangulate corners =
    case corners of
        u :: v :: w :: rest ->
            ( w, u, v ) :: List.map2 (\r s -> ( u, r, s )) (w :: rest) rest

        _ ->
            []


cylinder : Float -> Float -> Int -> Mesh.Mesh Vertex
cylinder radius length nrSegments =
    let
        n =
            nrSegments

        ring =
            List.range 0 (n - 1)
                |> List.map (\i -> toFloat i * 2 * pi / toFloat n)
                |> List.map (\alpha -> vec3 (cos alpha) (sin alpha) 0.0)

        bottom =
            List.map
                (\p -> { position = Vec3.scale radius p, normal = p })
                ring

        bottomInset =
            List.map
                (\v ->
                    { position = Vec3.scale 0.9 v.position
                    , normal = vec3 0 0 -1
                    }
                )
                bottom

        top =
            List.map
                (\v ->
                    { v
                        | position = Vec3.add v.position (vec3 0 0 length)
                    }
                )
                bottom

        topInset =
            List.map
                (\v ->
                    { position = Vec3.add v.position (vec3 0 0 length)
                    , normal = vec3 0 0 1
                    }
                )
                bottomInset

        vertices =
            List.concat [ bottomInset, bottom, top, topInset ]

        faces =
            List.concat
                [ [ List.range 0 (n - 1) |> List.reverse ]
                , [ List.range (3 * n) (4 * n - 1) ]
                , List.range 0 (n - 1)
                    |> List.concatMap
                        (\i ->
                            let
                                j =
                                    modBy n (i + 1)
                            in
                            [ 0, n, 2 * n ]
                                |> List.map
                                    (\k ->
                                        [ i + k, j + k, j + k + n, i + k + n ]
                                    )
                        )
                ]
    in
    Mesh.IndexedTriangles vertices (List.concatMap triangulate faces)


meshes : List (Mesh.Mesh Vertex)
meshes =
    [ cylinder 0.1 1.0 48 ]


stickMaterial : Material
stickMaterial =
    { color = Color.hsl 0.3 0.9 0.7
    , roughness = 0.5
    , metallic = 0.1
    }


instances : List Instance
instances =
    [ { material = stickMaterial
      , transform = Mat4.identity
      , idxMesh = 0
      , idxInstance = 0
      }
    , { material = stickMaterial
      , transform = Mat4.makeRotate (pi / 2) (vec3 1 0 0)
      , idxMesh = 0
      , idxInstance = 1
      }
    , { material = stickMaterial
      , transform = Mat4.makeRotate (pi / 2) (vec3 0 1 0)
      , idxMesh = 0
      , idxInstance = 2
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
    Html.div [] [ View3d.view identity model options Color.black ]


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
