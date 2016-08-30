module Main exposing (main)

import Array
import String
import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Evt
import Html.App as App
import InlineHover as Hov
import WebGL exposing (Drawable, Shader, Renderable)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Collision exposing (Bounds)
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Frame exposing (Frame)
import Model exposing (Vertex)


main : Program Never
main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


type alias Model =
    { room : Room
    , redFrame : Frame
    , redBounds : Bounds
    , blueFrame : Frame
    , blueBounds : Bounds
    }


type Room
    = Entrance
    | PositionEditor PositionFields
    | OrientationEditor OrientationFields


type alias PositionFields =
    { xText : String
    , yText : String
    , zText : String
    , solid : Solid
    }


type alias OrientationFields =
    { angleText : String
    , axis : Vector
    , solid : Solid
    }


type Solid
    = Red
    | Blue


type Action
    = EditX String
    | EditY String
    | EditZ String
    | EditAngle String
    | SetAxis Vector
    | ChangeRoom Room
    | SetPosition
    | ExtrinsicNudge
    | IntrinsicNudge
    | ExtrinsicRotate
    | IntrinsicRotate
    | ResetOrientation


init : Model
init =
    { room = Entrance
    , redFrame = Frame.identity
    , blueFrame =
        { position = Vector.vector 0 0 -5
        , orientation = Quaternion.identity
        }
    }


update : Action -> Model -> Model
update action model =
    case ( action, model.room ) of
        ( ChangeRoom room, _ ) ->
            { model | room = room }

        ( EditX xText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | xText = xText } }

        ( EditY yText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | yText = yText } }

        ( EditZ zText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | zText = zText } }

        ( EditAngle angleText, OrientationEditor fields ) ->
            { model | room = OrientationEditor { fields | angleText = angleText } }

        ( SetAxis axis, OrientationEditor fields ) ->
            { model | room = OrientationEditor { fields | axis = axis } }

        ( SetPosition, PositionEditor fields ) ->
            updateSolid
                (parseVector >> Frame.setPosition)
                fields
                model

        ( ExtrinsicNudge, PositionEditor fields ) ->
            updateSolid
                (parseVector >> Frame.extrinsicNudge)
                fields
                model

        ( IntrinsicNudge, PositionEditor fields ) ->
            updateSolid
                (parseVector >> Frame.intrinsicNudge)
                fields
                model

        ( ExtrinsicRotate, OrientationEditor fields ) ->
            updateSolid
                (parseRotation >> Frame.extrinsicRotate)
                fields
                model

        ( IntrinsicRotate, OrientationEditor fields ) ->
            updateSolid
                (parseRotation >> Frame.intrinsicRotate)
                fields
                model

        ( ResetOrientation, OrientationEditor fields ) ->
            updateSolid
                (always (Frame.setOrientation Quaternion.identity))
                fields
                model

        _ ->
            model


type alias WithSolid a =
    { a | solid : Solid }


updateSolid : (WithSolid a -> Frame -> Frame) -> WithSolid a -> Model -> Model
updateSolid transform fields model =
    case fields.solid of
        Red ->
            { model | redFrame = transform fields model.redFrame }

        Blue ->
            { model | blueFrame = transform fields model.blueFrame }


parseVector : PositionFields -> Vector
parseVector fields =
    Vector.vector
        (toFloat fields.xText)
        (toFloat fields.yText)
        (toFloat fields.zText)


parseRotation : OrientationFields -> Quaternion
parseRotation fields =
    degrees (toFloat fields.angleText)
        |> Quaternion.fromAxisAngle fields.axis
        |> Maybe.withDefault Quaternion.identity


toFloat : String -> Float
toFloat text =
    String.toFloat text
        |> Result.withDefault 0


view : Model -> Html Action
view model =
    Html.div
        [ Attr.style
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            , ( "justify-content", "center" )
            ]
        ]
        [ controlPanel model
        , world model
        , statusPanel model
        ]


controlPanel : Model -> Html Action
controlPanel model =
    let
        titleStyle =
            Attr.style
                [ ( "font-size", "1.2rem" )
                , ( "font-weight", "normal" )
                , ( "text-align", "center" )
                ]

        ( title, controls ) =
            case model.room of
                Entrance ->
                    ( "Collision Test", entranceControls )

                PositionEditor _ ->
                    ( "Change Position", positionControls )

                OrientationEditor _ ->
                    ( "Change Orientation", orientationControls )
    in
        Html.div
            [ Attr.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "width", "200px" )
                ]
            ]
            [ Html.h1 [ titleStyle ] [ Html.text title ]
            , divider
            , controls
            ]


entranceControls : Html Action
entranceControls =
    let
        editPositionFor solid =
            { xText = ""
            , yText = ""
            , zText = ""
            , solid = solid
            }
                |> PositionEditor
                |> ChangeRoom

        editOrientationFor solid =
            { angleText = ""
            , axis = Vector.xAxis
            , solid = solid
            }
                |> OrientationEditor
                |> ChangeRoom
    in
        Html.div
            [ Attr.style
                [ ( "display", "flex" )
                , ( "flex-wrap", "wrap" )
                , ( "justify-content", "center" )
                ]
            ]
            [ button (editPositionFor Red) "Red Position"
            , button (editOrientationFor Red) "Red Orientation"
            , spacer
            , button (editPositionFor Blue) "Blue Position"
            , button (editOrientationFor Blue) "Blue Orientation"
            ]


positionControls : Html Action
positionControls =
    Html.div
        [ Attr.style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
        ]
        [ inputField EditX "X "
        , inputField EditY "Y "
        , inputField EditZ "Z "
        , spacer
        , button SetPosition "Set Position"
        , button ExtrinsicNudge "Extrinsic Nudge"
        , button IntrinsicNudge "Intrinsic Nudge"
        , spacer
        , backButton
        ]


orientationControls : Html Action
orientationControls =
    let
        toAxis value =
            if value == "y" then
                SetAxis Vector.yAxis
            else if value == "z" then
                SetAxis Vector.zAxis
            else
                SetAxis Vector.xAxis
    in
        Html.div
            [ Attr.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                ]
            ]
            [ inputField EditAngle "Angle (Degrees) "
            , Html.select [ Evt.on "change" (Json.map toAxis Evt.targetValue) ]
                [ Html.option [ Attr.value "x" ] [ Html.text "X Axis" ]
                , Html.option [ Attr.value "y" ] [ Html.text "Y Axis" ]
                , Html.option [ Attr.value "z" ] [ Html.text "Z Axis" ]
                ]
            , spacer
            , button ExtrinsicRotate "Extrinsic Rotate"
            , button IntrinsicRotate "Intrinsic Rotate"
            , button ResetOrientation "Reset"
            , spacer
            , backButton
            ]


inputField : (String -> msg) -> String -> Html msg
inputField sendMsg label =
    Html.div []
        [ Html.text label
        , Html.input [ Attr.size 3, Evt.onInput sendMsg ] []
        ]


backButton : Html Action
backButton =
    button (ChangeRoom Entrance) "Back"


button : msg -> String -> Html msg
button sendMsg label =
    Hov.hover
        [ ( "background-color", "#eeeeee" ) ]
        Html.button
        [ Evt.onClick sendMsg
        , Attr.style
            [ ( "background-color", "#ffffff" )
            , ( "border", "none" )
            , ( "width", "80%" )
            , ( "font-size", "1rem" )
            , ( "padding", "5px 10px" )
            , ( "margin", "5px 0px" )
            ]
        ]
        [ Html.text label ]


divider : Html a
divider =
    Html.hr [ Attr.style [ ( "width", "80%" ) ] ] []


spacer : Html a
spacer =
    Html.div [ Attr.style [ ( "width", "100%" ), ( "margin", "10px 0px" ) ] ] [ Html.text " " ]


statusPanel : Model -> Html a
statusPanel model =
    let
        titleStyle =
            Attr.style
                [ ( "font-size", "1.2rem" )
                , ( "font-weight", "normal" )
                , ( "text-align", "center" )
                ]
    in
        Html.div [ Attr.style [ ( "width", "250px" ) ] ]
            [ Html.h2 [ titleStyle ] [ Html.text "Red" ]
            , divider
            , displayFrame model.redFrame
            , Html.h2 [ titleStyle ] [ Html.text "Blue" ]
            , divider
            , displayFrame model.blueFrame
            ]


displayFrame : Frame -> Html a
displayFrame frame =
    Html.div
        [ Attr.style
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            , ( "justify-content", "space-around" )
            , ( "margin-bottom", "25px" )
            ]
        ]
        [ Html.div []
            [ Html.text ("X = " ++ float frame.position.x)
            , Html.br [] []
            , Html.text ("Y = " ++ float frame.position.y)
            , Html.br [] []
            , Html.text ("Z = " ++ float frame.position.z)
            ]
        , Html.div []
            [ Html.text ("Qw = " ++ float frame.orientation.scalar)
            , Html.br [] []
            , Html.text ("Qx = " ++ float frame.orientation.vector.x)
            , Html.br [] []
            , Html.text ("Qy = " ++ float frame.orientation.vector.y)
            , Html.br [] []
            , Html.text ("Qz = " ++ float frame.orientation.vector.z)
            ]
        ]


float : Float -> String
float x =
    let
        sign =
            if x > -5.0e-3 then
                "+"
            else
                "-"

        cents =
            round (abs x * 100)

        integerPart =
            cents // 100

        centRemainder =
            cents % 100

        decimal =
            if centRemainder < 10 then
                ".0"
            else
                "."
    in
        String.concat [ sign, toString integerPart, decimal, toString centRemainder ]


world : Model -> Html a
world model =
    WebGL.toHtml
        [ Attr.width 500
        , Attr.height 500
        , Attr.style [ ( "background-color", "#d0f0ff" ) ]
        ]
        [ drawSolid Red model.redFrame
        , drawSolid Blue model.blueFrame
        ]


drawSolid : Solid -> Frame -> Renderable
drawSolid solid frame =
    WebGL.render vertexShader fragmentShader cube (uniform solid frame)


cube : Drawable Vertex
cube =
    Model.drawable
        { vertexPositions =
            Array.fromList
                [ Vector.vector -1 1 1
                , Vector.vector 1 1 1
                , Vector.vector 1 -1 1
                , Vector.vector -1 -1 1
                , Vector.vector -1 1 -1
                , Vector.vector 1 1 -1
                , Vector.vector 1 -1 -1
                , Vector.vector -1 -1 -1
                ]
        , vertexIndexes =
            [ [ 3, 2, 1, 0 ]
            , [ 5, 4, 0, 1 ]
            , [ 6, 5, 1, 2 ]
            , [ 7, 6, 2, 3 ]
            , [ 7, 3, 0, 4 ]
            , [ 7, 4, 5, 6 ]
            ]
        }


uniform : Solid -> Frame -> Uniform
uniform solid frame =
    let
        color =
            case solid of
                Red ->
                    Vec3.vec3 1 0 0

                Blue ->
                    Vec3.vec3 0 0 1

        cameraPosition =
            Vector.vector 5 5 5

        cameraOrientation =
            Mat4.makeRotate (turns 0.125) (Vec3.vec3 1 0 0)
                |> Mat4.rotate (turns -0.125) (Vec3.vec3 0 1 0)

        placement =
            Frame.toMat4 frame
    in
        { cameraPosition = Vec3.fromRecord cameraPosition
        , cameraOrientation = cameraOrientation
        , perspective = Mat4.makeOrtho -10 10 -10 10 -100 100
        , placement = placement
        , inversePlacement = Mat4.inverseOrthonormal placement
        , diffuseColor = color
        }


type alias Uniform =
    { cameraPosition : Vec3
    , perspective : Mat4
    , cameraOrientation : Mat4
    , placement : Mat4
    , inversePlacement : Mat4
    , diffuseColor : Vec3
    }


type alias Varying =
    { nonspecularColor : Vec3
    , specularFactor : Float
    }


vertexShader : Shader Vertex Uniform Varying
vertexShader =
    [glsl|
         precision mediump float;

         attribute vec3 position;
         attribute vec3 normal;

         uniform vec3 cameraPosition;
         uniform mat4 cameraOrientation;
         uniform mat4 perspective;
         uniform mat4 placement;
         uniform mat4 inversePlacement;
         uniform vec3 diffuseColor;

         varying vec3 nonspecularColor;
         varying float specularFactor;

         void main() {
             // Vertex Positioning

             vec4 worldFrame = placement * vec4(position, 1);
             vec4 cameraOffset = worldFrame - vec4(cameraPosition, 0);
             gl_Position = perspective * cameraOrientation * cameraOffset;


             // Lighting
             vec3 ambientColor = vec3(0.1, 0.1, 0.1);
             vec3 lightDirection = normalize(vec3(1, 1.5, 2));

             vec3 surfaceNormal = vec3(vec4(normal, 0) * inversePlacement);
             float diffuseFactor = dot(lightDirection, surfaceNormal);
             nonspecularColor = ambientColor + diffuseColor * diffuseFactor;

             vec3 reflection = normalize(2.0 * diffuseFactor * surfaceNormal - lightDirection);
             vec3 cameraDirection = normalize(-cameraOffset.xyz);
             specularFactor = clamp(dot(reflection, cameraDirection), 0.0, 1.0);
         }
    |]


fragmentShader : Shader {} Uniform Varying
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 nonspecularColor;
        varying float specularFactor;

        void main() {
            float shininess = 3.0;
            vec3 baseSpecColor = vec3(1, 1, 1);

            vec3 specularColor = baseSpecColor * pow(specularFactor, shininess);

            gl_FragColor = vec4(nonspecularColor + specularColor, 1);
        }
    |]
