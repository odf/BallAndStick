module Main exposing (main)

import Array exposing (Array)
import Browser
import Color
import Dict
import Html
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import View3d
import View3d.Mesh as Mesh
import View3d.RendererCommon exposing (..)


type alias Flags =
    { points : List (List Float)
    , edges : List (List Float)
    }


type alias Model =
    View3d.Model


type alias Msg =
    View3d.Msg


type alias PreMesh =
    { verts : List Vec3, faces : List (List Int) }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( ballMeshes, ballInstances ) =
            List.map listToPoint flags.points
                |> makeBalls 0 ballMaterial 0.125

        ( cornerMeshes, cornerInstances ) =
            List.map listToPoint unitCell.points
                |> makeBalls 1 cellMaterial 0.01

        ( stickMeshes, stickInstances ) =
            List.map listToEdge flags.edges
                |> makeSticks 2 stickMaterial 0.05

        ( edgeMeshes, edgeInstances ) =
            List.map listToEdge unitCell.edges
                |> makeSticks (2 + List.length stickMeshes) cellMaterial 0.01

        meshes =
            ballMeshes
                ++ cornerMeshes
                ++ stickMeshes
                ++ edgeMeshes
                |> List.map makeMesh

        instances =
            ballInstances
                ++ cornerInstances
                ++ stickInstances
                ++ edgeInstances

        model =
            View3d.init
                |> View3d.setSize { width = 768, height = 768 }
                |> View3d.setScene (Just meshes) instances
                |> View3d.encompass
    in
    ( model, Cmd.none )


makeBalls :
    Int
    -> Material
    -> Float
    -> List Vec3
    -> ( List PreMesh, List Instance )
makeBalls offset material radius coordinates =
    ( [ ball radius ]
    , List.map
        (\pos ->
            { material = material
            , transform = Mat4.makeTranslate pos
            , idxMesh = offset
            , idxInstance = 0
            }
        )
        coordinates
    )


makeSticks :
    Int
    -> Material
    -> Float
    -> List ( Vec3, Vec3 )
    -> ( List PreMesh, List Instance )
makeSticks offset material radius coordinates =
    let
        params =
            List.map (\( u, v ) -> stickParameters u v) coordinates

        lengthKey x =
            round (x * 1000)

        makeStick k =
            cylinder radius (toFloat k / 1000) 48

        sticks =
            List.map .length params
                |> List.map lengthKey
                |> List.foldl
                    (\k d ->
                        if Dict.member k d then
                            d

                        else
                            Dict.insert k (makeStick k) d
                    )
                    Dict.empty

        stickIndex =
            Dict.keys sticks
                |> List.indexedMap (\i k -> ( k, i ))
                |> Dict.fromList

        makeInstance { origin, length, rotation } =
            let
                idxMesh =
                    Dict.get (lengthKey length) stickIndex
                        |> Maybe.withDefault -1
            in
            { material = material
            , transform = Mat4.mul (Mat4.makeTranslate origin) rotation
            , idxMesh = idxMesh + offset
            , idxInstance = 0
            }
    in
    ( Dict.values sticks
    , List.map makeInstance params
        |> List.filter (\inst -> inst.idxMesh >= 0)
    )


stickMaterial : Material
stickMaterial =
    { color = Color.hsl 0.13 0.9 0.7
    , roughness = 0.5
    , metallic = 0.1
    }


ballMaterial : Material
ballMaterial =
    { color = Color.hsl 0.0 0.6 0.5
    , roughness = 0.5
    , metallic = 0.1
    }


cellMaterial : Material
cellMaterial =
    { color = Color.hsl 0.67 0.5 0.5
    , roughness = 0.5
    , metallic = 0.1
    }


listToPoint : List Float -> Vec3
listToPoint xs =
    case xs of
        x :: y :: z :: _ ->
            vec3 x y z

        _ ->
            vec3 0 0 0


listToEdge : List Float -> ( Vec3, Vec3 )
listToEdge xs =
    case xs of
        a :: b :: c :: rest ->
            ( vec3 a b c, listToPoint rest )

        _ ->
            ( vec3 0 0 0, vec3 0 0 0 )


unitCell : { points : List (List number), edges : List (List number) }
unitCell =
    { points =
        [ [ 0, 0, 0 ]
        , [ 1, 0, 0 ]
        , [ 0, 1, 0 ]
        , [ 1, 1, 0 ]
        , [ 0, 0, 1 ]
        , [ 1, 0, 1 ]
        , [ 0, 1, 1 ]
        , [ 1, 1, 1 ]
        ]
    , edges =
        [ [ 0, 0, 0, 1, 0, 0 ]
        , [ 0, 1, 0, 1, 1, 0 ]
        , [ 0, 0, 1, 1, 0, 1 ]
        , [ 0, 1, 1, 1, 1, 1 ]
        , [ 0, 0, 0, 0, 1, 0 ]
        , [ 1, 0, 0, 1, 1, 0 ]
        , [ 0, 0, 1, 0, 1, 1 ]
        , [ 1, 0, 1, 1, 1, 1 ]
        , [ 0, 0, 0, 0, 0, 1 ]
        , [ 1, 0, 0, 1, 0, 1 ]
        , [ 0, 1, 0, 0, 1, 1 ]
        , [ 1, 1, 0, 1, 1, 1 ]
        ]
    }



-- View


view : Model -> Html.Html Msg
view model =
    Html.div [] [ View3d.view identity model options ]


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



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, _ ) =
            View3d.update msg model
    in
    ( newModel, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    View3d.subscriptions identity model



-- Geometry


stickParameters :
    Vec3
    -> Vec3
    -> { origin : Vec3, length : Float, rotation : Mat4 }
stickParameters from to =
    let
        w =
            Vec3.direction to from

        ex =
            vec3 1 0 0

        ey =
            vec3 0 1 0

        t =
            if abs (Vec3.dot w ex) < abs (Vec3.dot w ey) then
                ex

            else
                ey

        u =
            Vec3.normalize (Vec3.cross w t)

        v =
            Vec3.normalize (Vec3.cross w u)
    in
    { origin = from
    , length = Vec3.distance from to
    , rotation = Mat4.makeBasis u v w
    }


cylinder : Float -> Float -> Int -> PreMesh
cylinder radius length nrSegments =
    let
        n =
            nrSegments

        a =
            2 * pi / toFloat n

        d =
            0.1

        bottom =
            List.range 0 (n - 1)
                |> List.map toFloat
                |> List.map
                    (\i -> ( radius * cos (a * i), radius * sin (a * i), 0 ))

        bottomCapInset =
            List.map (\( x, y, z ) -> ( (1 - d) * x, (1 - d) * y, z )) bottom

        bottomCapBevel =
            List.map
                (\( x, y, z ) -> ( (1 - d / 2) * x, (1 - d / 2) * y, z ))
                bottom

        bottomShaftBevel =
            List.map (\( x, y, z ) -> ( x, y, z + (d / 2) * radius )) bottom

        bottomShaftInset =
            List.map (\( x, y, z ) -> ( x, y, z + d * radius )) bottom

        topCapInset =
            List.map (\( x, y, z ) -> ( x, y, length - z )) bottomCapInset

        topCapBevel =
            List.map (\( x, y, z ) -> ( x, y, length - z )) bottomCapBevel

        topShaftBevel =
            List.map (\( x, y, z ) -> ( x, y, length - z )) bottomShaftBevel

        topShaftInset =
            List.map (\( x, y, z ) -> ( x, y, length - z )) bottomShaftInset

        vertices =
            List.concat
                [ bottomCapInset
                , bottomCapBevel
                , bottomShaftBevel
                , bottomShaftInset
                , topShaftInset
                , topShaftBevel
                , topCapBevel
                , topCapInset
                ]
                |> List.map (\( x, y, z ) -> vec3 x y z)

        ring s t =
            List.range 0 (n - 1)
                |> List.map (\i -> ( i, modBy n (i + 1) ))
                |> List.map (\( i, j ) -> [ i + s, j + s, j + t, i + t ])

        faces =
            (List.range 0 (n - 1) |> List.reverse)
                :: List.range (7 * n) (8 * n - 1)
                :: (List.range 0 6
                        |> List.map (\k -> ring (k * n) ((k + 1) * n))
                        |> List.concat
                   )
    in
    { verts = vertices, faces = faces }


ball : Float -> PreMesh
ball radius =
    let
        refine =
            subdivide
                >> (\s -> { s | verts = List.map Vec3.normalize s.verts })

        tmp =
            { verts =
                [ vec3 1 0 0
                , vec3 0 1 0
                , vec3 0 0 1
                , vec3 -1 0 0
                , vec3 0 -1 0
                , vec3 0 0 -1
                ]
            , faces =
                [ [ 0, 1, 2 ]
                , [ 1, 0, 5 ]
                , [ 2, 1, 3 ]
                , [ 0, 2, 4 ]
                , [ 3, 5, 4 ]
                , [ 5, 3, 1 ]
                , [ 4, 5, 0 ]
                , [ 3, 4, 2 ]
                ]
            }
                |> refine
                |> refine
                |> refine
    in
    { tmp | verts = List.map (Vec3.scale radius) tmp.verts }


subdivide : PreMesh -> PreMesh
subdivide { verts, faces } =
    let
        vertexArray =
            Array.fromList verts

        centroid vs =
            List.map (\v -> Array.get v vertexArray) vs
                |> List.filterMap identity
                |> List.foldl Vec3.add (vec3 0 0 0)
                |> Vec3.scale (1 / toFloat (List.length vs))

        allEdges =
            List.concatMap edges faces
                |> List.filter (\( u, v ) -> u < v)

        n =
            List.length verts

        m =
            List.length allEdges

        midPointIndex =
            List.indexedMap (\i e -> ( e, i + n )) allEdges
                |> List.concatMap
                    (\( ( u, v ), i ) -> [ ( ( u, v ), i ), ( ( v, u ), i ) ])
                |> Dict.fromList

        vertices =
            List.concat
                [ verts
                , List.map (\( u, v ) -> centroid [ u, v ]) allEdges
                , List.map centroid faces
                ]

        makeSubFace i ( u, v, w ) =
            [ Dict.get ( u, v ) midPointIndex
            , Just v
            , Dict.get ( v, w ) midPointIndex
            , Just (n + m + i)
            ]
                |> List.filterMap identity

        facesOut =
            faces
                |> List.indexedMap
                    (\i f -> List.map (makeSubFace i) (sectors f))
                |> List.concat
    in
    { verts = vertices, faces = facesOut }


makeMesh : PreMesh -> Mesh.Mesh Vertex
makeMesh { verts, faces } =
    let
        vertices =
            Array.fromList verts

        allSectorData =
            List.concatMap (sectorData vertices) faces

        vertexNormal idx =
            List.filter (\e -> e.idx == idx) allSectorData
                |> List.map .normal
                |> List.foldl Vec3.add (vec3 0 0 0)

        verticesWithNormals =
            List.indexedMap
                (\idx pos -> { position = pos, normal = vertexNormal idx })
                verts

        triangles =
            List.concatMap triangulate faces
    in
    Mesh.IndexedTriangles verticesWithNormals triangles


edges : List Int -> List ( Int, Int )
edges face =
    case face of
        a :: rest ->
            List.map2 Tuple.pair face (rest ++ [ a ])

        _ ->
            []


sectors : List Int -> List ( Int, Int, Int )
sectors face =
    case face of
        a :: b :: rest ->
            List.map3 (\u v w -> ( u, v, w ))
                face
                (b :: rest ++ [ a ])
                (rest ++ [ a, b ])

        _ ->
            []


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
