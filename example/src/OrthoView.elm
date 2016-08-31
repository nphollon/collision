module OrthoView exposing (draw)

import Html exposing (Html)
import Html.Attributes as Attr
import WebGL exposing (Drawable, Shader, Renderable)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)


-- Collision Library

import Frame exposing (Frame)
import Vector


-- Project Local

import Types exposing (..)


draw : Model -> Html a
draw model =
    Html.div [ Attr.style [ ( "height", "500" ) ] ]
        [ WebGL.toHtml
            [ Attr.width 500
            , Attr.height 500
            , Attr.style [ ( "background-color", "#d0f0ff" ) ]
            ]
            [ drawSolid Red model.red
            , drawSolid Blue model.blue
            ]
        ]


drawSolid : Solid -> Entity -> Renderable
drawSolid solid entity =
    WebGL.render vertexShader
        fragmentShader
        entity.mesh
        (uniform solid entity.frame)


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
