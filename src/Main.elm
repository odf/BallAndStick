port module Main exposing (main)

import Array exposing (Array)
import Axis3d
import Browser
import Color
import Dict
import Direction3d
import Frame3d exposing (Frame3d)
import Html
import Length
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Mesh
import Plane3d
import Point3d exposing (Point3d)
import Vector3d
import View3d
import View3d.Instance as Instance exposing (Instance)


type alias Flags =
    { structures : Array Structure
    , options : OptionsJS
    }


type alias Structure =
    { points : List (List Float)
    , edges : List (List Float)
    , cell : List Float
    }


emptyStructure : Structure
emptyStructure =
    { points = [], edges = [], cell = [] }


type alias OptionsJS =
    { sizeFrame : Size2d
    , background : ColorHSL
    , perspective : Bool
    , shadows : Bool
    , depthCueing : Bool
    , outlines : Bool
    , drawCellFrame : Bool
    , colorSticks : ColorHSL
    , colorBalls : ColorHSL
    , colorCell : ColorHSL
    , radiusSticks : Float
    , radiusBalls : Float
    , radiusCellFrame : Float
    }


type alias Size2d =
    { width : Float
    , height : Float
    }


type alias ColorHSL =
    { hue : Float
    , saturation : Float
    , lightness : Float
    }


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { scene : View3d.Model WorldCoordinates
    , structures : Array Structure
    , options : OptionsJS
    , selectedStructure : Int
    , background : Color.Color
    , orthographic : Bool
    , shadows : Bool
    , depthCueing : Bool
    , outlines : Bool
    , drawCellFrame : Bool
    }


port selectStructure : (Int -> msg) -> Sub msg


type Msg
    = ViewMsg View3d.Msg
    | Selection Int


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
        structure =
            Array.get 0 flags.structures
                |> Maybe.withDefault emptyStructure

        scene =
            View3d.init
                |> View3d.setSize flags.options.sizeFrame
                |> View3d.setScene (geometry structure flags.options)
                |> View3d.encompass

        model =
            { scene = scene
            , structures = flags.structures
            , options = flags.options
            , selectedStructure = 0
            , background = makeColorHSL flags.options.background
            , orthographic = not flags.options.perspective
            , shadows = flags.options.shadows
            , depthCueing = flags.options.depthCueing
            , outlines = flags.options.outlines
            , drawCellFrame = flags.options.drawCellFrame
            }
    in
    ( model, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    let
        defaultOptions =
            View3d.defaultOptions

        fadeToBackground =
            if model.depthCueing then
                defaultOptions.fadeToBackground

            else
                0.0

        fadeToBlue =
            if model.depthCueing then
                defaultOptions.fadeToBlue

            else
                0.0

        options =
            { defaultOptions
                | backgroundColor = model.background
                , orthogonalView = model.orthographic
                , drawShadows = model.shadows
                , fadeToBackground = fadeToBackground
                , fadeToBlue = fadeToBlue
                , addOutlines = model.outlines
                , outlineWidth = 0.1
                , outlineColor = model.background
            }
    in
    Html.div [] [ View3d.view ViewMsg model.scene options ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewMsg vmsg ->
            let
                ( newScene, _ ) =
                    View3d.update vmsg model.scene

                newModel =
                    { model | scene = newScene }
            in
            ( newModel, Cmd.none )

        Selection index ->
            let
                structure =
                    Array.get index model.structures
                        |> Maybe.withDefault emptyStructure

                scene =
                    model.scene
                        |> View3d.setScene (geometry structure model.options)
                        |> View3d.encompass
            in
            ( { model | scene = scene }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ View3d.subscriptions ViewMsg model.scene
        , selectStructure Selection
        ]



-- Helpers


makeColorHSL : ColorHSL -> Color.Color
makeColorHSL { hue, saturation, lightness } =
    Color.hsl hue saturation lightness



-- Geometry


geometry : Structure -> OptionsJS -> List (Instance coords)
geometry structure options =
    let
        matSticks =
            options.colorSticks |> makeColorHSL |> baseMaterial

        matBalls =
            options.colorBalls |> makeColorHSL |> baseMaterial

        matCell =
            options.colorCell |> makeColorHSL |> baseMaterial

        toCartesian =
            listToCellTransform structure.cell

        transformPoint p =
            Mat4.transform toCartesian p

        transformEdge ( from, to ) =
            ( transformPoint from, transformPoint to )

        balls =
            structure.points
                |> List.map (listToPoint >> transformPoint)
                |> makeBalls matBalls options.radiusBalls

        sticks =
            structure.edges
                |> List.map (listToEdge >> transformEdge)
                |> makeSticks matSticks options.radiusSticks

        corners =
            unitCell.points
                |> List.map (listToPoint >> transformPoint)
                |> makeBalls matCell options.radiusCellFrame

        edges =
            unitCell.edges
                |> List.map (listToEdge >> transformEdge)
                |> makeSticks matCell options.radiusCellFrame
    in
    if options.drawCellFrame then
        balls ++ sticks ++ corners ++ edges

    else
        balls ++ sticks


positionVector : Vec3 -> Vector3d.Vector3d Length.Meters coords
positionVector pos =
    Vec3.toRecord pos |> Vector3d.fromMeters


makeBalls : View3d.Material -> Float -> List Vec3 -> List (Instance coords)
makeBalls material radius coordinates =
    let
        mesh =
            ball radius

        makeInstance pos =
            Instance.make material mesh
                |> Instance.translateBy (positionVector pos)
    in
    List.map makeInstance coordinates


makeSticks :
    View3d.Material
    -> Float
    -> List ( Vec3, Vec3 )
    -> List (Instance coords)
makeSticks material radius coordinates =
    let
        params =
            List.map (\( u, v ) -> stickParameters u v) coordinates

        lengthKey x =
            round (x * 1000)

        addStick k d =
            if Dict.member k d then
                d

            else
                cylinder radius (toFloat k / 1000) 48
                    |> flip (Dict.insert k) d

        sticks =
            List.map (.length >> lengthKey) params
                |> List.foldl addStick Dict.empty

        makeInstance { length, frame } =
            Dict.get (lengthKey length) sticks
                |> Maybe.map (Instance.make material >> Instance.placeIn frame)
    in
    List.filterMap makeInstance params


baseMaterial : Color.Color -> View3d.Material
baseMaterial color =
    { color = color
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


listToCellTransform : List Float -> Mat4
listToCellTransform xs =
    case xs of
        a :: b :: c :: alpha :: beta :: gamma :: _ ->
            let
                vx =
                    cos (degrees gamma)

                vy =
                    sin (degrees gamma)

                wx =
                    cos (degrees beta)

                wy =
                    (cos (degrees alpha) - vx * wx) / vy

                wz =
                    sqrt (1 - wx ^ 2 - wy ^ 2)
            in
            Mat4.makeBasis
                (Vec3.scale a (vec3 1 0 0))
                (Vec3.scale b (vec3 vx vy 0))
                (Vec3.scale c (vec3 wx wy wz))

        _ ->
            Mat4.identity


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


stickParameters :
    Vec3
    -> Vec3
    ->
        { length : Float
        , frame : Frame3d Length.Meters coords defines
        }
stickParameters from to =
    let
        origin =
            Vec3.toRecord from |> Point3d.fromMeters

        destination =
            Vec3.toRecord to |> Point3d.fromMeters

        direction =
            Direction3d.from origin destination
                |> Maybe.withDefault Direction3d.x
    in
    { length = Vec3.distance from to
    , frame = Frame3d.withZDirection direction origin
    }


cylinder : Float -> Float -> Int -> View3d.Mesh coords
cylinder radius length nrSegments =
    let
        d =
            radius * 0.1

        angle u =
            toFloat u * 2 * pi / toFloat nrSegments

        radial a r z =
            Point3d.meters (r * cos a) (r * sin a) z

        midplane =
            Plane3d.xy |> Plane3d.offsetBy (Length.meters (length / 2))

        position u v =
            if v > 4 then
                position u (9 - v) |> Point3d.mirrorAcross midplane

            else
                case v of
                    0 ->
                        radial (angle u) 0 0

                    1 ->
                        radial (angle u) (radius - d) 0

                    2 ->
                        radial (angle u) (radius - d / 2) 0

                    3 ->
                        radial (angle u) radius (d / 2)

                    _ ->
                        radial (angle u) radius d
    in
    Mesh.indexedBall nrSegments 9 position |> convertMesh


ball : Float -> View3d.Mesh coords
ball radius =
    let
        positions =
            [ ( ( 0, 0 ), Point3d.meters 0 0 -1 )
            , ( ( 0, 1 ), Point3d.meters 1 0 0 )
            , ( ( 1, 1 ), Point3d.meters 0 1 0 )
            , ( ( 2, 1 ), Point3d.meters -1 0 0 )
            , ( ( 3, 1 ), Point3d.meters 0 -1 0 )
            , ( ( 0, 2 ), Point3d.meters 0 0 1 )
            ]
                |> Dict.fromList

        getPosition =
            flip Dict.get positions
                >> Maybe.withDefault (Point3d.meters 0 0 0)

        pushOut p =
            let
                axis =
                    Axis3d.throughPoints Point3d.origin p
                        |> Maybe.withDefault Axis3d.x
            in
            Point3d.along axis (Length.meters radius)

        refine =
            Mesh.subdivideSmoothly (always False) identity (always identity)
                >> Mesh.mapVertices pushOut
    in
    Mesh.indexedBall 4 2 Tuple.pair
        |> Mesh.mapVertices getPosition
        |> refine
        |> refine
        |> refine
        |> convertMesh


convertMesh : Mesh.Mesh (Point3d Length.Meters coords) -> View3d.Mesh coords
convertMesh meshIn =
    let
        makeVertex position normal =
            { position = position, normal = normal }
    in
    Mesh.withNormals identity makeVertex meshIn
        |> Mesh.toTriangularMesh
        |> View3d.mesh


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a
