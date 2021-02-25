module Main exposing (main)

import Array exposing (Array)
import Browser
import Color
import Html
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
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


sectorData : Array Vec3 -> List Int -> List { idx : Int, normal : Vec3 }
sectorData allVertices face =
    let
        getPos v =
            Array.get v allVertices |> Maybe.withDefault (vec3 0 0 0)

        verts =
            List.map (\v -> { idx = v, pos = getPos v }) face

        compute u v w =
            { idx = v.idx
            , normal = Vec3.cross (Vec3.sub w.pos v.pos) (Vec3.sub u.pos v.pos)
            }
    in
    case verts of
        a :: b :: rest ->
            List.map3 compute verts (b :: rest ++ [ a ]) (rest ++ [ a, b ])

        _ ->
            []


triangulate : List vertex -> List ( vertex, vertex, vertex )
triangulate corners =
    case corners of
        u :: v :: w :: rest ->
            ( w, u, v ) :: List.map2 (\r s -> ( u, r, s )) (w :: rest) rest

        _ ->
            []


makeMesh : { verts : List Vec3, faces : List (List Int) } -> Mesh.Mesh Vertex
makeMesh { verts, faces } =
    let
        vertices =
            Array.fromList verts

        sectors =
            List.concatMap (\f -> sectorData vertices f) faces

        vertexNormal idx =
            sectors
                |> List.filter (\e -> e.idx == idx)
                |> List.map (\e -> e.normal)
                |> List.foldl Vec3.add (vec3 0 0 0)

        verticesWithNormals =
            List.indexedMap
                (\idx pos -> { position = pos, normal = vertexNormal idx })
                verts

        triangles =
            List.concatMap triangulate faces
    in
    Mesh.IndexedTriangles verticesWithNormals triangles


cylinder : Float -> Int -> { verts : List Vec3, faces : List (List Int) }
cylinder radius nrSegments =
    let
        n =
            nrSegments

        r =
            radius

        a =
            2 * pi / toFloat n

        bottom =
            List.range 0 (n - 1)
                |> List.map toFloat
                |> List.map (\i -> ( r * cos (a * i), r * sin (a * i), 0 ))

        top =
            List.map (\( x, y, z ) -> ( x, y, 1 - z )) bottom

        bottomInset =
            List.map (\( x, y, z ) -> ( 0.9 * x, 0.9 * y, z )) bottom

        topInset =
            List.map (\( x, y, z ) -> ( 0.9 * x, 0.9 * y, z )) top

        vertices =
            List.concat [ bottomInset, bottom, top, topInset ]
                |> List.map (\( x, y, z ) -> vec3 x y z)

        ring s t =
            List.range 0 (n - 1)
                |> List.map (\i -> ( i, modBy n (i + 1) ))
                |> List.map (\( i, j ) -> [ i + s, j + s, j + t, i + t ])

        faces =
            List.concat
                [ [ List.range 0 (n - 1) |> List.reverse ]
                , ring 0 n
                , ring n (2 * n)
                , ring (2 * n) (3 * n)
                , [ List.range (3 * n) (4 * n - 1) ]
                ]
    in
    { verts = vertices, faces = faces }


meshes : List (Mesh.Mesh Vertex)
meshes =
    [ cylinder 0.1 48 |> makeMesh ]


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
