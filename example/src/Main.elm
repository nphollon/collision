module Main exposing (main)

import Array
import String
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Evt
import Html.App as App
import WebGL exposing (Drawable, Shader, Renderable)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Vector exposing (Vector)
import Quaternion exposing (Quaternion)
import Model exposing (Vertex)


main : Program Never
main =
    App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { room : Room
    , redPosition : Vector
    , bluePosition : Vector
    }


type Room
    = Entrance
    | PositionEditor PositionFields


type alias PositionFields =
    { xText : String, yText : String, zText : String, solid : Solid }


type Solid
    = Red
    | Blue


type Action
    = EditX String
    | EditY String
    | EditZ String
    | ChangeRoom Room
    | SetPosition
    | NudgePosition


init : ( Model, Cmd Action )
init =
    { room = Entrance
    , redPosition = Vector.vector 0 0 0
    , bluePosition = Vector.vector 0 0 -5
    }
        ! []


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.none


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case ( action, model.room ) of
        ( ChangeRoom room, _ ) ->
            { model | room = room } ! []

        ( SetPosition, PositionEditor fields ) ->
            setPosition model fields ! []

        ( NudgePosition, PositionEditor fields ) ->
            nudgePosition model fields ! []

        ( EditX xText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | xText = xText } }
                ! []

        ( EditY yText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | yText = yText } }
                ! []

        ( EditZ zText, PositionEditor fields ) ->
            { model | room = PositionEditor { fields | zText = zText } }
                ! []

        _ ->
            model ! []


setPosition : Model -> PositionFields -> Model
setPosition model fields =
    let
        newPosition =
            parseVector fields
    in
        case fields.solid of
            Red ->
                { model | redPosition = newPosition }

            Blue ->
                { model | bluePosition = newPosition }


nudgePosition : Model -> PositionFields -> Model
nudgePosition model fields =
    let
        displacement =
            parseVector fields
    in
        case fields.solid of
            Red ->
                { model | redPosition = Vector.add model.redPosition displacement }

            Blue ->
                { model | bluePosition = Vector.add model.bluePosition displacement }


parseVector : PositionFields -> Vector
parseVector fields =
    let
        toFloat text =
            String.toFloat text
                |> Result.withDefault 0
    in
        Vector.vector
            (toFloat fields.xText)
            (toFloat fields.yText)
            (toFloat fields.zText)


view : Model -> Html Action
view model =
    Html.div [ Attr.style [ ( "display", "flex" ), ( "justify-content", "center" ) ] ]
        [ sidebar model
        , world model
        ]


sidebar : Model -> Html Action
sidebar model =
    Html.div
        [ Attr.style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "justify-content", "space-between" )
            , ( "width", "250px" )
            ]
        ]
        [ controlPanel model, statusPanel model ]


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
    in
        Html.div [ Attr.style [ ( "width", "200px" ) ] ]
            [ Html.h1 [ titleStyle ] [ Html.text title ]
            , divider
            , controls
            ]


divider : Html a
divider =
    Html.hr [ Attr.style [ ( "width", "80%" ) ] ] []


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
    in
        Html.div []
            [ Html.button
                [ Evt.onClick (editPositionFor Red) ]
                [ Html.text "Change Red Position" ]
            , Html.button
                [ Evt.onClick (editPositionFor Blue) ]
                [ Html.text "Change Blue Position" ]
            ]


positionControls : Html Action
positionControls =
    Html.div []
        [ inputField EditX "X = "
        , inputField EditY "Y = "
        , inputField EditZ "Z = "
        , Html.button [ Evt.onClick SetPosition ] [ Html.text "Set position" ]
        , Html.button [ Evt.onClick NudgePosition ] [ Html.text "Nudge position" ]
        , Html.button [ Evt.onClick (ChangeRoom Entrance) ] [ Html.text "Back" ]
        ]


inputField : (String -> msg) -> String -> Html msg
inputField sendMsg label =
    Html.div []
        [ Html.text label
        , Html.input [ Attr.size 10, Evt.onInput sendMsg ] []
        ]


statusPanel : Model -> Html a
statusPanel model =
    let
        red =
            "#FF4137"

        blue =
            "#6F7AC5"
    in
        Html.div []
            [ divider
            , Html.div
                [ Attr.style
                    [ ( "display", "flex" )
                    , ( "flex-wrap", "wrap" )
                    , ( "justify-content", "center" )
                    ]
                ]
                [ displayPosition red model.redPosition
                , displayOrientation red (Quaternion.quaternion 1 0 0 0)
                , displayPosition blue model.bluePosition
                , displayOrientation blue (Quaternion.quaternion 1 0 0 0)
                ]
            ]


displayPosition : String -> Vector -> Html a
displayPosition cssColor position =
    Html.div
        [ Attr.style
            [ ( "color", cssColor )
            , ( "font-weight", "bold" )
            , ( "padding", "10px 15px" )
            ]
        ]
        [ Html.text ("X = " ++ float position.x)
        , Html.br [] []
        , Html.text ("Y = " ++ float position.y)
        , Html.br [] []
        , Html.text ("Z = " ++ float position.z)
        ]


displayOrientation : String -> Quaternion -> Html a
displayOrientation cssColor orientation =
    Html.div
        [ Attr.style
            [ ( "color", cssColor )
            , ( "font-weight", "bold" )
            , ( "padding", "10px 15px" )
            ]
        ]
        [ Html.text ("Qw = " ++ float orientation.scalar)
        , Html.br [] []
        , Html.text ("Qx = " ++ float orientation.vector.x)
        , Html.br [] []
        , Html.text ("Qy = " ++ float orientation.vector.y)
        , Html.br [] []
        , Html.text ("Qz = " ++ float orientation.vector.z)
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
        [ drawSolid Red model.redPosition
        , drawSolid Blue model.bluePosition
        ]


drawSolid : Solid -> Vector -> Renderable
drawSolid solid position =
    WebGL.render vertexShader fragmentShader cube (uniform solid position)


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


uniform : Solid -> Vector -> Uniform
uniform solid position =
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
            Vec3.fromRecord position
                |> Mat4.makeTranslate
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
